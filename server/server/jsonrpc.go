// Copyright (C) 2026  Muir Manders
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <https://www.gnu.org/licenses/>.

package server

import (
	"context"
	"encoding/json"
	"fmt"
	"io"
	"log"
	"log/slog"
	"net"
	"os"
	"os/user"
	"path/filepath"
	"runtime"
	"strings"
	"sync"
	"time"

	"github.com/creachadair/jrpc2"
	"github.com/creachadair/jrpc2/channel"
	"github.com/creachadair/jrpc2/handler"
	"github.com/muirdm/flit.el/eden"
	"github.com/muirdm/flit.el/exec"
	"github.com/muirdm/flit.el/fs"
	"github.com/muirdm/flit.el/tunnel"
	"github.com/muirdm/flit.el/util"
)

// Server wraps the jrpc2 server with flit-specific functionality
type Server struct {
	idleTimeout  time.Duration
	verbose      bool
	logLevel     slog.Level
	lastActivity time.Time
	mu           sync.Mutex
	done         chan struct{}
	fsHandler    *fs.Handler
	connections  map[net.Conn]struct{}
	connMu       sync.Mutex
}

// SysInfo contains cross-platform system information
type SysInfo struct {
	OS       string   `json:"os"`
	Arch     string   `json:"arch"`
	HomeDir  string   `json:"homeDir"`
	Hostname string   `json:"hostname"`
	Username string   `json:"username"`
	Path     []string `json:"path"` // $PATH split into components
	Pid      int      `json:"pid"`  // Server process ID
}

// PathDirEntry represents a directory listing for one PATH directory
type PathDirEntry struct {
	Path     string         `json:"path"`
	Children *[]fs.DirChild `json:"children,omitempty"` // nil = no data, empty slice = empty dir
	Error    string         `json:"error,omitempty"`
}

// InitResult contains all initial connection info
type InitResult struct {
	SysInfo
	PathDirs []PathDirEntry `json:"pathDirs"`
}

// New creates a new server instance
func New(idleTimeout time.Duration, verbose bool, logLevel slog.Level) *Server {
	s := &Server{
		idleTimeout:  idleTimeout,
		verbose:      verbose,
		logLevel:     logLevel,
		lastActivity: time.Now(),
		done:         make(chan struct{}),
		fsHandler:    fs.NewHandler(),
		connections:  make(map[net.Conn]struct{}),
	}

	// Start idle timeout checker if enabled
	if idleTimeout > 0 {
		go s.idleChecker()
	}

	return s
}

// Done returns a channel that's closed when the server is shutting down
func (s *Server) Done() <-chan struct{} {
	return s.done
}

// Shutdown gracefully shuts down the server
func (s *Server) Shutdown() {
	close(s.done)

	// Close all connections
	s.connMu.Lock()
	for conn := range s.connections {
		conn.Close()
	}
	s.connMu.Unlock()
}

// idleChecker monitors for inactivity and shuts down if idle too long
func (s *Server) idleChecker() {
	ticker := time.NewTicker(time.Minute)
	defer ticker.Stop()

	for {
		select {
		case <-s.done:
			return
		case <-ticker.C:
			s.mu.Lock()
			idle := time.Since(s.lastActivity)
			s.mu.Unlock()

			// Don't shutdown if there are active connections
			s.connMu.Lock()
			hasConnections := len(s.connections) > 0
			s.connMu.Unlock()

			if !hasConnections && idle > s.idleTimeout {
				log.Printf("Idle timeout reached (%v), shutting down", s.idleTimeout)
				s.Shutdown()
				return
			}
		}
	}
}

// touch updates the last activity time
func (s *Server) touch() {
	s.mu.Lock()
	s.lastActivity = time.Now()
	s.mu.Unlock()
}

// WatchMeta holds metadata about a watched path
type WatchMeta struct {
	Listed bool  // true if client fetched entry details for this directory
	Mtime  int64 // last known mtime
	Size   int64 // last known size
}

// Session holds per-connection state (file watcher, process manager, etc.)
type Session struct {
	server        *jrpc2.Server
	logger        *slog.Logger // Logger that sends logs via RPC
	watcher       *fs.Watcher
	edenWatcher   *eden.Watcher
	procManager   *exec.Manager
	tunnelManager *tunnel.Manager
	watchedPaths  map[string]*WatchMeta
	openFiles     map[string]bool // files the client has open in buffers
	richFetched   map[string]bool // directories that have had rich fetch completed
	watchedMu     sync.Mutex
	logLevel      slog.Level
}

// NewSession creates a new session for a connection
func NewSession(logLevel slog.Level) *Session {
	sess := &Session{
		watchedPaths: make(map[string]*WatchMeta),
		openFiles:    make(map[string]bool),
		richFetched:  make(map[string]bool),
		logLevel:     logLevel,
		logger:       NewDiscardLogger(), // Will be replaced when server is set
	}
	// Initialize process manager - callbacks check for nil server
	sess.procManager = exec.NewManager(
		// onOutput callback
		func(procID string, stream string, data string) {
			if sess.server != nil {
				ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
				defer cancel()
				if err := sess.server.Notify(ctx, "exec/output", map[string]string{
					"procId": procID,
					"stream": stream,
					"data":   data,
				}); err != nil {
					sess.logger.Error("exec/output notification failed", "error", err)
				}
			}
		},
		// onExit callback
		func(procID string, exitCode int) {
			sess.logger.Info("exec/exit", "procID", procID, "exitCode", exitCode)
			if sess.server != nil {
				ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
				defer cancel()
				if err := sess.server.Notify(ctx, "exec/exit", map[string]interface{}{
					"procId":   procID,
					"exitCode": exitCode,
				}); err != nil {
					sess.logger.Error("exec/exit notification failed", "error", err)
				}
			}
		},
		sess.logger,
	)
	return sess
}

// SetServer sets the jrpc2 server for sending notifications and creates the RPC logger
func (sess *Session) SetServer(srv *jrpc2.Server) {
	sess.server = srv
	// Create notify function for use by various managers
	notify := func(method string, params any) error {
		ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
		defer cancel()
		return srv.Notify(ctx, method, params)
	}
	// Create RPC logger that sends logs to client
	rpcHandler := NewRPCLogHandler(sess.logLevel, notify)
	sess.logger = slog.New(rpcHandler)
	sess.procManager.SetLogger(sess.logger)
	// Create tunnel manager (handles both forward and reverse tunnels)
	sess.tunnelManager = tunnel.NewManager(notify, sess.logger)
}

// Logger returns the session's logger
func (sess *Session) Logger() *slog.Logger {
	return sess.logger
}

// InitWatcher initializes the file watcher with notification callback
func (sess *Session) InitWatcher(fsHandler *fs.Handler) error {
	watcher, err := fs.NewWatcher(func(event fs.WatchEvent) {
		if sess.server != nil {
			// For "modified" events on watched files, check if mtime/size actually changed.
			// This suppresses duplicate notifications from fsnotify bursts and our own writes.
			if event.Type == "modified" {
				storedMtime, storedSize, isWatched := sess.GetFileState(event.Path)
				if isWatched {
					// Quick stat to check if file actually changed
					fileInfo, err := os.Lstat(event.Path)
					if err != nil {
						// File may have been deleted - let it through
						slog.Debug("watch: stat failed for modified event", "path", event.Path, "err", err)
					} else {
						currentMtime := fileInfo.ModTime().Unix()
						currentSize := fileInfo.Size()
						// Suppress if mtime is older (stale event) or unchanged with same size
						if currentMtime < storedMtime || (currentMtime == storedMtime && currentSize == storedSize) {
							slog.Debug("watch: suppressing", "path", event.Path,
								"currentMtime", currentMtime, "storedMtime", storedMtime,
								"currentSize", currentSize, "storedSize", storedSize)
							return
						}
						// File changed - update stored state
						sess.UpdateFileState(event.Path, currentMtime, currentSize)
					}
				}
			}

			// Determine whether to fetch full info (with content) or basic info:
			// - "modified" events come from individual file watches (fs/open), so
			//   include content for comparison with buffer
			// - For directories the client has listed, include full info
			// - Otherwise use basic info (no content) to save bandwidth
			var info *fs.InfoResult
			var err error
			if event.Type == "modified" || sess.IsDirListed(event.Path) {
				info, err = fsHandler.GetInfo(event.Path)
			} else {
				info, err = fsHandler.GetInfoBasic(event.Path)
			}
			if err != nil {
				// If we can't get info, send basic event only for deletes
				if event.Type == "deleted" {
					ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
					defer cancel()
					sess.server.Notify(ctx, "fs/changed", map[string]interface{}{
						"path":  event.Path,
						"type":  event.Type,
						"cache": []fs.CacheEntry{},
					})
				}
				return
			}

			// Don't notify for non-existent files (create-then-delete race),
			// but still send delete events
			if !info.Exists {
				if event.Type == "deleted" {
					ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
					defer cancel()
					sess.server.Notify(ctx, "fs/changed", map[string]interface{}{
						"path":  event.Path,
						"type":  event.Type,
						"cache": []fs.CacheEntry{},
					})
				}
				return
			}

			cache := []fs.CacheEntry{{Path: info.Path, Info: info}}
			if info.Realpath != "" && info.Realpath != info.Path {
				cache = append(cache, fs.CacheEntry{Path: info.Realpath, Info: info})
			}

			notification := struct {
				Path  string          `json:"path"`
				Type  string          `json:"type"` // "modified", "created", "deleted", "renamed"
				Cache []fs.CacheEntry `json:"cache"`
			}{
				Path:  event.Path,
				Type:  event.Type,
				Cache: cache,
			}

			ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
			defer cancel()
			if err := sess.server.Notify(ctx, "fs/changed", notification); err != nil {
				log.Printf("Failed to send notification: %v", err)
			}
		}
	})
	if err != nil {
		return err
	}
	sess.watcher = watcher
	return nil
}

// noWatchPrefixes are path prefixes for volatile/virtual filesystems
// where file watching is wasteful and caching is inappropriate.
var noWatchPrefixes = []string{"/tmp/", "/dev/", "/proc/", "/sys/"}

// isNoWatchPath returns true if path is under a volatile/virtual filesystem
// (e.g., /tmp, /dev, /proc, /sys) where watches should not be set up.
func isNoWatchPath(path string) bool {
	for _, prefix := range noWatchPrefixes {
		if strings.HasPrefix(path, prefix) || path == prefix[:len(prefix)-1] {
			return true
		}
	}
	return false
}

// Watch adds a path to this session's watch list
func (sess *Session) Watch(path string) error {
	if sess.watcher == nil {
		return fmt.Errorf("file watching not available")
	}

	sess.watchedMu.Lock()
	defer sess.watchedMu.Unlock()

	if sess.watchedPaths[path] != nil {
		return nil // Already watching
	}

	if err := sess.watcher.Watch(path); err != nil {
		return err
	}

	sess.watchedPaths[path] = &WatchMeta{}
	return nil
}

// WatchFile adds a file to this session's watch list (marks as explicitly requested)
func (sess *Session) WatchFile(path string) error {
	if sess.watcher == nil {
		return fmt.Errorf("file watching not available")
	}
	if isNoWatchPath(path) {
		return nil
	}

	sess.watchedMu.Lock()
	defer sess.watchedMu.Unlock()

	if sess.watchedPaths[path] != nil {
		return nil // Already watching
	}

	if err := sess.watcher.WatchFile(path); err != nil {
		return err
	}

	sess.watchedPaths[path] = &WatchMeta{}
	return nil
}

// WatchDir adds a directory to this session's watch list
func (sess *Session) WatchDir(path string) error {
	if sess.watcher == nil {
		return fmt.Errorf("file watching not available")
	}
	if isNoWatchPath(path) {
		return nil
	}

	sess.watchedMu.Lock()
	defer sess.watchedMu.Unlock()

	if sess.watchedPaths[path] != nil {
		return nil // Already watching
	}

	if err := sess.watcher.WatchDir(path); err != nil {
		return err
	}

	sess.watchedPaths[path] = &WatchMeta{}
	return nil
}

// WatchPendingFile watches for a non-existent file to be created.
// Walks up the directory tree to find the closest existing ancestor,
// watches that directory, and registers the pending path.
// Returns true if an ancestor was found and watched, false otherwise.
func (sess *Session) WatchPendingFile(filePath string) bool {
	if sess.watcher == nil {
		return false
	}
	if isNoWatchPath(filePath) {
		return false
	}

	// Walk up to find closest existing ancestor directory
	ancestor := filepath.Dir(filePath)
	for ancestor != "/" && ancestor != "." {
		info, err := os.Stat(ancestor)
		if err == nil && info.IsDir() {
			break
		}
		ancestor = filepath.Dir(ancestor)
	}

	// Check that we found a valid ancestor
	if ancestor == "." {
		return false
	}

	// Watch the ancestor directory (refcounted, so safe to call multiple times)
	sess.watchedMu.Lock()
	if sess.watchedPaths[ancestor] == nil {
		if err := sess.watcher.WatchDir(ancestor); err != nil {
			sess.watchedMu.Unlock()
			return false
		}
		sess.watchedPaths[ancestor] = &WatchMeta{}
	}
	sess.watchedMu.Unlock()

	// Register this path as pending under the ancestor
	sess.watcher.WatchPending(ancestor, filePath)
	return true
}

// Unwatch removes a path from this session's watch list
func (sess *Session) Unwatch(path string) error {
	if sess.watcher == nil {
		return nil
	}

	sess.watchedMu.Lock()
	defer sess.watchedMu.Unlock()

	if sess.watchedPaths[path] == nil {
		return nil // Not watching
	}

	if err := sess.watcher.Unwatch(path); err != nil {
		return err
	}

	delete(sess.watchedPaths, path)
	return nil
}

// OpenFile marks a file as open and starts watching it.
// Called when client opens a file in a buffer.
func (sess *Session) OpenFile(path string) error {
	if sess.watcher == nil {
		return fmt.Errorf("file watching not available")
	}

	// Stat file to get initial mtime/size for change detection
	var mtime, size int64
	if info, err := os.Lstat(path); err == nil {
		mtime = info.ModTime().Unix()
		size = info.Size()
	}

	sess.watchedMu.Lock()
	defer sess.watchedMu.Unlock()

	// Mark as open
	sess.openFiles[path] = true

	// Watch if not already watching
	if sess.watchedPaths[path] == nil {
		if err := sess.watcher.WatchFile(path); err != nil {
			return err
		}
		sess.watchedPaths[path] = &WatchMeta{Mtime: mtime, Size: size}
	} else {
		// Update stored state even if already watching
		sess.watchedPaths[path].Mtime = mtime
		sess.watchedPaths[path].Size = size
	}

	return nil
}

// CloseFile marks a file as closed and stops watching it.
// Called when client closes a buffer.
func (sess *Session) CloseFile(path string) error {
	sess.watchedMu.Lock()
	defer sess.watchedMu.Unlock()

	// Remove from open files
	delete(sess.openFiles, path)

	// Unwatch if we were watching
	if sess.watcher != nil && sess.watchedPaths[path] != nil {
		if err := sess.watcher.Unwatch(path); err != nil {
			return err
		}
		delete(sess.watchedPaths, path)
	}

	return nil
}

// UpdateFileState updates the stored mtime/size for a watched file.
// Called after fs/write so we suppress notifications for our own writes.
func (sess *Session) UpdateFileState(path string, mtime, size int64) {
	sess.watchedMu.Lock()
	defer sess.watchedMu.Unlock()
	if meta := sess.watchedPaths[path]; meta != nil {
		meta.Mtime = mtime
		meta.Size = size
	}
}

// GetFileState returns the stored mtime/size for a watched file.
// Returns (0, 0, false) if the file is not being watched.
func (sess *Session) GetFileState(path string) (mtime, size int64, ok bool) {
	sess.watchedMu.Lock()
	defer sess.watchedMu.Unlock()
	if meta := sess.watchedPaths[path]; meta != nil {
		return meta.Mtime, meta.Size, true
	}
	return 0, 0, false
}

// InitEdenWatcher initializes the Eden watcher if not already initialized.
func (sess *Session) InitEdenWatcher(fsHandler *fs.Handler) {
	if sess.edenWatcher != nil {
		return
	}
	sess.edenWatcher = eden.NewWatcher(func(root string) {
		sess.handleEdenCheckout(root, fsHandler)
	}, sess.logger)
}

// EnsureEdenWatching checks if a path is in an Eden repo and starts watching it.
func (sess *Session) EnsureEdenWatching(path string) {
	if sess.edenWatcher == nil {
		return
	}
	if root := eden.GetEdenRoot(path); root != "" {
		sess.edenWatcher.EnsureWatching(root)
	}
}

// handleEdenCheckout is called when Eden detects a checkout (CommitTransition).
// It checks all watched files and directories under the root for changes and sends notifications.
func (sess *Session) handleEdenCheckout(root string, fsHandler *fs.Handler) {
	start := time.Now()

	sess.watchedMu.Lock()
	paths := make([]string, 0)
	for path := range sess.watchedPaths {
		if strings.HasPrefix(path, root+"/") || path == root {
			paths = append(paths, path)
		}
	}
	sess.watchedMu.Unlock()

	sess.logger.Info("Eden checkout detected", "root", root)

	changed := 0
	for _, path := range paths {
		info, err := os.Lstat(path)
		if err != nil {
			if os.IsNotExist(err) {
				sess.sendFileChangedNotification(path, "deleted", fsHandler)
				changed++
			}
			continue
		}

		currentMtime := info.ModTime().Unix()
		currentSize := info.Size()

		storedMtime, storedSize, ok := sess.GetFileState(path)
		if !ok {
			continue
		}

		// For directories, mtime changes when files are added/removed
		// For files, check both mtime and size
		if currentMtime != storedMtime || (!info.IsDir() && currentSize != storedSize) {
			sess.UpdateFileState(path, currentMtime, currentSize)
			sess.sendFileChangedNotification(path, "modified", fsHandler)
			changed++
		}
	}

	sess.logger.Info("Eden checkout: checked watched paths", "count", len(paths), "changed", changed, "elapsed", time.Since(start).Round(time.Millisecond))
}

// sendFileChangedNotification sends an fs/changed notification for a file.
func (sess *Session) sendFileChangedNotification(path, eventType string, fsHandler *fs.Handler) {
	if sess.server == nil {
		return
	}

	// Get file info:
	// - Full info (with content) for open files
	// - Full info (with children) for listed directories
	// - Basic info for everything else
	var info *fs.InfoResult
	var err error
	if eventType == "modified" && sess.IsFileOpen(path) {
		info, err = fsHandler.GetInfo(path)
	} else if eventType == "modified" && sess.IsDirListed(path) {
		info, err = fsHandler.GetInfo(path)
	} else {
		info, err = fsHandler.GetInfoBasic(path)
	}
	if err != nil && eventType != "deleted" {
		return
	}

	var notification interface{}
	if eventType == "deleted" || info == nil {
		notification = map[string]interface{}{
			"path":  path,
			"type":  eventType,
			"cache": []fs.CacheEntry{},
		}
	} else {
		cache := fs.BuildCacheEntries(info)
		notification = struct {
			Path  string          `json:"path"`
			Type  string          `json:"type"`
			Cache []fs.CacheEntry `json:"cache"`
		}{
			Path:  path,
			Type:  eventType,
			Cache: cache,
		}
	}

	ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
	defer cancel()
	sess.server.Notify(ctx, "fs/changed", notification)
}

// IsFileOpen returns true if the file is currently open in a client buffer
func (sess *Session) IsFileOpen(path string) bool {
	sess.watchedMu.Lock()
	defer sess.watchedMu.Unlock()
	return sess.openFiles[path]
}

// MarkDirListed marks a directory as having its entry details fetched by the client.
// This is used to decide whether to include entry details in watch notifications.
func (sess *Session) MarkDirListed(path string) {
	sess.watchedMu.Lock()
	defer sess.watchedMu.Unlock()
	if meta := sess.watchedPaths[path]; meta != nil {
		meta.Listed = true
	}
}

// IsDirListed returns true if the directory has had its entry details fetched
func (sess *Session) IsDirListed(path string) bool {
	sess.watchedMu.Lock()
	defer sess.watchedMu.Unlock()
	if meta := sess.watchedPaths[path]; meta != nil {
		return meta.Listed
	}
	return false
}

// shouldRichFetch checks if we should do a rich fetch for a directory.
// Returns true if fetch should proceed (marks directory before returning).
// Returns false if already fetched, or if numChildren exceeds limit.
// Must be called BEFORE starting the async fetch to prevent races.
func (sess *Session) shouldRichFetch(path string, numChildren int, limit int) bool {
	sess.watchedMu.Lock()
	defer sess.watchedMu.Unlock()

	// Already fetched
	if sess.richFetched[path] {
		return false
	}

	// Too many children for this limit
	if numChildren > limit {
		return false
	}

	// Mark as fetched and proceed
	sess.richFetched[path] = true
	return true
}

// AsyncFetchChildEntries triggers an async fetch of child entries for a directory.
// Checks if fetch is needed (not already done, within limit), and if so spawns
// a goroutine to fetch entries and send fs/entryInfo notification.
// timeout is the context timeout for the notification.
func (sess *Session) AsyncFetchChildEntries(dirPath string, children []fs.DirChild, limit int, timeout time.Duration, fsHandler *fs.Handler) {
	numChildren := len(children)
	if numChildren == 0 {
		return
	}
	if !sess.shouldRichFetch(dirPath, numChildren, limit) {
		return
	}
	go func() {
		cacheEntries := fsHandler.GetChildEntries(dirPath, children, limit)
		if len(cacheEntries) == 0 {
			return
		}
		ctx, cancel := context.WithTimeout(context.Background(), timeout)
		defer cancel()
		sess.server.Notify(ctx, "fs/entryInfo", map[string]interface{}{
			"path":  dirPath,
			"cache": cacheEntries,
		})
	}()
}

// PrefetchAncestors fetches info for all ancestor directories of path
// that aren't already watched, watches them, and sends a notification
// to the client with the cache entries.
func (sess *Session) PrefetchAncestors(ctx context.Context, path string, fsHandler *fs.Handler) {
	// Compute ancestor paths
	var ancestors []string
	current := filepath.Dir(path)
	for current != "/" && current != "." {
		ancestors = append(ancestors, current)
		current = filepath.Dir(current)
	}
	ancestors = append(ancestors, "/")

	// Filter out already-watched paths and volatile paths
	sess.watchedMu.Lock()
	var toFetch []string
	for _, p := range ancestors {
		if sess.watchedPaths[p] == nil && !isNoWatchPath(p) {
			toFetch = append(toFetch, p)
		}
	}
	sess.watchedMu.Unlock()

	if len(toFetch) == 0 {
		return
	}

	// Watch and fetch info for all ancestors in parallel
	// Watch first in each goroutine to avoid missing changes between fetch and watch
	type result struct {
		path string
		info *fs.InfoResult
		err  error
	}
	results := make(chan result, len(toFetch))
	wg := util.NewBoundedWaitGroup(32)

	for _, p := range toFetch {
		wg.Add(1)
		go func(ancestorPath string) {
			defer wg.Done()
			// Watch before fetching to avoid race
			sess.WatchDir(ancestorPath)
			// Use GetInfoWithEntries: includes child names only (no stat calls)
			// Child entries are NOT sent for ancestors - keeps payload small
			info, err := fsHandler.GetInfoWithEntries(ancestorPath)
			results <- result{path: ancestorPath, info: info, err: err}
		}(p)
	}

	// Close results channel when all fetches complete
	go func() {
		wg.Wait()
		close(results)
	}()

	// Collect results and build cache entries
	var cacheEntries []fs.CacheEntry
	type dirWithChildren struct {
		path     string
		children []fs.DirChild
	}
	var dirsWithChildren []dirWithChildren
	for r := range results {
		if r.err != nil {
			sess.logger.Debug("ancestor prefetch failed", "path", r.path, "error", r.err)
			continue
		}
		cacheEntries = append(cacheEntries, fs.CacheEntry{
			Path: r.path,
			Info: r.info,
		})
		// Track directories with children for async entry fetch
		if r.info.Children != nil && len(*r.info.Children) > 0 {
			dirsWithChildren = append(dirsWithChildren, dirWithChildren{
				path:     r.path,
				children: *r.info.Children,
			})
		}
	}

	if len(cacheEntries) == 0 {
		return
	}

	// Send notification to client
	if err := sess.server.Notify(ctx, "fs/ancestorInfo", map[string]interface{}{
		"cache": cacheEntries,
	}); err != nil {
		sess.logger.Error("fs/ancestorInfo notification failed", "error", err)
	}

	// Async fetch full entries for children of ancestor directories
	for _, dir := range dirsWithChildren {
		sess.AsyncFetchChildEntries(dir.path, dir.children, fs.MaxEntriesDefault, 5*time.Second, fsHandler)
	}
}

// ClearCacheState fully resets session state to match a client cache clear.
// Stops all watches, clears all tracking maps. Watches will be re-established on next access.
func (sess *Session) ClearCacheState() {
	sess.watchedMu.Lock()

	// Count for logging
	watchCount := len(sess.watchedPaths)
	richCount := len(sess.richFetched)
	openCount := len(sess.openFiles)

	// Stop all watches
	if sess.watcher != nil {
		for path := range sess.watchedPaths {
			sess.watcher.Unwatch(path)
		}
		sess.watcher.ClearPending()
	}

	// Reset all maps
	sess.watchedPaths = make(map[string]*WatchMeta)
	sess.richFetched = make(map[string]bool)
	sess.openFiles = make(map[string]bool)

	sess.watchedMu.Unlock()

	sess.logger.Info("session/clearCache: cleared all state",
		"watches", watchCount, "richFetched", richCount, "openFiles", openCount)
}

// Close cleans up the session
func (sess *Session) Close() {
	if sess.watcher != nil {
		sess.watchedMu.Lock()
		for path := range sess.watchedPaths {
			sess.watcher.Unwatch(path)
		}
		sess.watchedPaths = nil
		sess.watchedMu.Unlock()
		sess.watcher.Close()
	}
	if sess.edenWatcher != nil {
		sess.edenWatcher.Stop()
	}
	if sess.procManager != nil {
		sess.procManager.Close()
	}
	if sess.tunnelManager != nil {
		sess.tunnelManager.CloseAll()
	}
}

// buildHandlerMap creates the handler map for the jrpc2 server
func (s *Server) buildHandlerMap(sess *Session) handler.Map {
	return handler.Map{
		// File system operations
		"fs/stat": handler.New(func(ctx context.Context, params json.RawMessage) (interface{}, error) {
			s.touch()
			return s.fsHandler.Stat(params)
		}),
		"fs/read": handler.New(func(ctx context.Context, params json.RawMessage) (interface{}, error) {
			s.touch()
			return s.fsHandler.Read(params)
		}),
		"fs/write": handler.New(func(ctx context.Context, params json.RawMessage) (interface{}, error) {
			s.touch()
			startTime := time.Now()
			var p fs.WriteParams
			if err := json.Unmarshal(params, &p); err != nil {
				return nil, &jrpc2.Error{Code: jrpc2.InvalidParams, Message: "Invalid params"}
			}
			// Pre-set mtime and size to suppress fsnotify notifications during write
			sess.UpdateFileState(p.Path, time.Now().Unix(), int64(len(p.Content)))
			result, err := s.fsHandler.Write(&p)
			if err == nil {
				// Update with actual mtime/size after write
				if infoResp, ok := result.(*fs.InfoResponse); ok && infoResp.InfoResult != nil {
					sess.UpdateFileState(infoResp.Path, infoResp.Mtime, infoResp.Size)
				}
			}
			sess.logger.Info("fs/write done", "path", p.Path, "elapsed", time.Since(startTime))
			return result, err
		}),
		"fs/mkdir": handler.New(func(ctx context.Context, params json.RawMessage) (interface{}, error) {
			s.touch()
			return s.fsHandler.Mkdir(params)
		}),
		"fs/delete": handler.New(func(ctx context.Context, params json.RawMessage) (interface{}, error) {
			s.touch()
			return s.fsHandler.Delete(params)
		}),
		"fs/rename": handler.New(func(ctx context.Context, params json.RawMessage) (interface{}, error) {
			s.touch()
			return s.fsHandler.Rename(params)
		}),
		"fs/copy": handler.New(func(ctx context.Context, params json.RawMessage) (interface{}, error) {
			s.touch()
			return s.fsHandler.Copy(params)
		}),
		"fs/exists": handler.New(func(ctx context.Context, params json.RawMessage) (interface{}, error) {
			s.touch()
			return s.fsHandler.Exists(params)
		}),
		"fs/realpath": handler.New(func(ctx context.Context, params json.RawMessage) (interface{}, error) {
			s.touch()
			return s.fsHandler.Realpath(params)
		}),
		"fs/info": handler.New(func(ctx context.Context, params struct {
			Path string `json:"path"`
		}) (interface{}, error) {
			s.touch()
			// Use GetInfoWithEntries for fast response - entryInfo sent async
			result, err := s.fsHandler.GetInfoWithEntries(params.Path)
			if err != nil {
				return nil, err
			}
			// Auto-watch so we get notifications for changes
			if result.Exists && result.Type == "file" {
				sess.WatchFile(params.Path)
			} else if result.Exists && result.Type == "directory" {
				sess.WatchDir(params.Path)
				// Mark as listed so fs/changed includes children for this directory
				if result.Children != nil {
					sess.MarkDirListed(params.Path)
				}
			}
			// For non-existent files, try to watch parent directory
			// so we can notify when the file is created (negative caching)
			if !result.Exists {
				result.ParentWatched = sess.WatchPendingFile(params.Path)
			}

			// For directories with children, async fetch child entries and send notification
			if result.Children != nil {
				sess.AsyncFetchChildEntries(params.Path, *result.Children, fs.MaxEntriesDefault, 5*time.Second, s.fsHandler)
			}

			// Mark volatile paths as non-cacheable
			if isNoWatchPath(params.Path) {
				result.NoCache = true
			}

			// Return InfoResponse with cache entries for unified caching
			return &fs.InfoResponse{
				InfoResult: result,
				Cache:      fs.BuildCacheEntries(result),
			}, nil
		}),
		"fs/batch": handler.New(func(ctx context.Context, params json.RawMessage) (interface{}, error) {
			s.touch()
			return s.fsHandler.Batch(params)
		}),
		"fs/chmod": handler.New(func(ctx context.Context, params json.RawMessage) (interface{}, error) {
			s.touch()
			return s.fsHandler.Chmod(params)
		}),
		"fs/touch": handler.New(func(ctx context.Context, params json.RawMessage) (interface{}, error) {
			s.touch()
			return s.fsHandler.Touch(params)
		}),
		"fs/copy-dir": handler.New(func(ctx context.Context, params json.RawMessage) (interface{}, error) {
			s.touch()
			return s.fsHandler.CopyDir(params)
		}),

		// Signal that client is browsing a directory (find-file, dired)
		// Returns directory info; triggers async child entry fetch with higher limit
		"fs/openDir": handler.New(func(ctx context.Context, params struct {
			Path string `json:"path"`
		}) (interface{}, error) {
			s.touch()
			result, err := s.fsHandler.GetInfoWithEntries(params.Path)
			if err != nil {
				return nil, err
			}
			// Watch the directory
			if result.Exists && result.Type == "directory" {
				sess.WatchDir(params.Path)
				if result.Children != nil {
					sess.MarkDirListed(params.Path)
				}
			}
			// Async fetch child entries with higher limit for explicit listing
			if result.Children != nil {
				sess.AsyncFetchChildEntries(params.Path, *result.Children, fs.MaxEntriesExplicit, 30*time.Second, s.fsHandler)
			}
			// Return InfoResponse with cache entries
			return &fs.InfoResponse{
				InfoResult: result,
				Cache:      fs.BuildCacheEntries(result),
			}, nil
		}),

		// File watching
		"fs/watch": handler.New(func(ctx context.Context, params struct {
			Path string `json:"path"`
		}) (interface{}, error) {
			s.touch()
			if err := sess.Watch(params.Path); err != nil {
				return nil, err
			}
			return map[string]interface{}{"watching": params.Path}, nil
		}),
		"fs/unwatch": handler.New(func(ctx context.Context, params struct {
			Path string `json:"path"`
		}) (interface{}, error) {
			s.touch()
			if err := sess.Unwatch(params.Path); err != nil {
				return nil, err
			}
			return map[string]interface{}{"unwatched": params.Path}, nil
		}),

		// File lifecycle - client informs server when files are opened/closed
		"fs/open": handler.New(func(ctx context.Context, params struct {
			Path string `json:"path"`
		}) (interface{}, error) {
			s.touch()
			// Kick off ancestor prefetch immediately in background
			go sess.PrefetchAncestors(context.Background(), params.Path, s.fsHandler)
			if err := sess.OpenFile(params.Path); err != nil {
				return nil, err
			}
			// Start Eden watching if this file is in an Eden repo
			sess.EnsureEdenWatching(params.Path)
			return map[string]interface{}{"opened": params.Path}, nil
		}),
		"fs/close": handler.New(func(ctx context.Context, params struct {
			Path string `json:"path"`
		}) (interface{}, error) {
			s.touch()
			if err := sess.CloseFile(params.Path); err != nil {
				return nil, err
			}
			return map[string]interface{}{"closed": params.Path}, nil
		}),
		"fs/forget": handler.New(func(ctx context.Context, params struct {
			Path string `json:"path"`
		}) (interface{}, error) {
			s.touch()
			// Forget decrements refcount for cached paths (directories, negative cache)
			// For now, just unwatch - can add refcounting later if needed
			if err := sess.Unwatch(params.Path); err != nil {
				return nil, err
			}
			return map[string]interface{}{"forgotten": params.Path}, nil
		}),

		// Clear client-side cache state (richFetched, etc.)
		// Called when client clears its cache to re-sync state
		"session/clearCache": handler.New(func(ctx context.Context, params struct{}) (interface{}, error) {
			s.touch()
			sess.ClearCacheState()
			return map[string]interface{}{"ok": true}, nil
		}),

		// Process execution
		"exec/start": handler.New(func(ctx context.Context, params exec.StartParams) (*exec.StartResult, error) {
			s.touch()
			if sess.procManager == nil {
				return nil, fmt.Errorf("process manager not initialized")
			}
			return sess.procManager.Start(params)
		}),
		"exec/input": handler.New(func(ctx context.Context, params exec.InputParams) (interface{}, error) {
			s.touch()
			if sess.procManager == nil {
				return nil, fmt.Errorf("process manager not initialized")
			}
			if err := sess.procManager.Input(params); err != nil {
				return nil, err
			}
			return map[string]bool{"ok": true}, nil
		}),
		"exec/signal": handler.New(func(ctx context.Context, params exec.SignalParams) (interface{}, error) {
			s.touch()
			if sess.procManager == nil {
				return nil, fmt.Errorf("process manager not initialized")
			}
			if err := sess.procManager.Signal(params); err != nil {
				return nil, err
			}
			return map[string]bool{"ok": true}, nil
		}),
		"exec/close-input": handler.New(func(ctx context.Context, params struct {
			ProcID string `json:"procId"`
		}) (interface{}, error) {
			s.touch()
			if sess.procManager == nil {
				return nil, fmt.Errorf("process manager not initialized")
			}
			if err := sess.procManager.CloseInput(params.ProcID); err != nil {
				return nil, err
			}
			return map[string]bool{"ok": true}, nil
		}),
		"exec/ptyctl": handler.New(func(ctx context.Context, params exec.PtyCtlParams) (interface{}, error) {
			s.touch()
			if sess.procManager == nil {
				return nil, fmt.Errorf("process manager not initialized")
			}
			if err := sess.procManager.PtyCtl(params); err != nil {
				return nil, err
			}
			return map[string]bool{"ok": true}, nil
		}),

		// System info (cross-platform)
		"sys/info": handler.New(func(ctx context.Context) (*SysInfo, error) {
			s.touch()
			info := &SysInfo{
				OS:   runtime.GOOS,
				Arch: runtime.GOARCH,
				Pid:  os.Getpid(),
			}
			if home, err := os.UserHomeDir(); err == nil {
				info.HomeDir = home
			}
			if hostname, err := os.Hostname(); err == nil {
				info.Hostname = hostname
			}
			if u, err := user.Current(); err == nil {
				info.Username = u.Username
			}
			// Split PATH into components
			if pathEnv := os.Getenv("PATH"); pathEnv != "" {
				info.Path = strings.Split(pathEnv, string(os.PathListSeparator))
			}
			return info, nil
		}),

		// Initialize connection - returns sys/info plus PATH directory listings
		"init": handler.New(func(ctx context.Context) (*InitResult, error) {
			s.touch()
			result := &InitResult{}

			// Populate sys info
			result.OS = runtime.GOOS
			result.Arch = runtime.GOARCH
			result.Pid = os.Getpid()
			if home, err := os.UserHomeDir(); err == nil {
				result.HomeDir = home
			}
			if hostname, err := os.Hostname(); err == nil {
				result.Hostname = hostname
			}
			if u, err := user.Current(); err == nil {
				result.Username = u.Username
			}

			// Split PATH into components and expand tildes
			var pathDirs []string
			if pathEnv := os.Getenv("PATH"); pathEnv != "" {
				for _, dir := range strings.Split(pathEnv, string(os.PathListSeparator)) {
					// Expand tilde to home directory for consistent cache keys
					if strings.HasPrefix(dir, "~/") && result.HomeDir != "" {
						dir = result.HomeDir + dir[1:]
					} else if dir == "~" && result.HomeDir != "" {
						dir = result.HomeDir
					}
					pathDirs = append(pathDirs, dir)
				}
				result.Path = pathDirs
			}

			// Fetch directory listings for all PATH directories in parallel
			result.PathDirs = make([]PathDirEntry, len(pathDirs))
			wg := util.NewBoundedWaitGroup(32)
			for i, dir := range pathDirs {
				wg.Add(1)
				go func(idx int, dirPath string) {
					defer wg.Done()
					entry := PathDirEntry{Path: dirPath}
					children, err := s.fsHandler.ReadDirWithTypes(dirPath)
					if err != nil {
						entry.Error = err.Error()
					} else {
						entry.Children = &children
					}
					result.PathDirs[idx] = entry
				}(i, dir)
			}
			wg.Wait()

			return result, nil
		}),

		// Tunnel operations (both forward and reverse)
		"tunnel/listen": handler.New(func(ctx context.Context, params tunnel.ListenParams) (*tunnel.ListenResult, error) {
			s.touch()
			if sess.tunnelManager == nil {
				return nil, fmt.Errorf("tunnel manager not initialized")
			}
			return sess.tunnelManager.Listen(params)
		}),
		"tunnel/close": handler.New(func(ctx context.Context, params tunnel.CloseParams) (interface{}, error) {
			s.touch()
			if sess.tunnelManager == nil {
				return nil, fmt.Errorf("tunnel manager not initialized")
			}
			if err := sess.tunnelManager.Close(params); err != nil {
				return nil, err
			}
			return map[string]bool{"ok": true}, nil
		}),
		"tunnel/connect": handler.New(func(ctx context.Context, params tunnel.ConnectParams) (interface{}, error) {
			s.touch()
			if sess.tunnelManager == nil {
				return nil, fmt.Errorf("tunnel manager not initialized")
			}
			if err := sess.tunnelManager.Connect(params); err != nil {
				return nil, err
			}
			return map[string]bool{"ok": true}, nil
		}),
		"tunnel/data": handler.New(func(ctx context.Context, params tunnel.DataParams) (interface{}, error) {
			s.touch()
			if sess.tunnelManager == nil {
				return nil, fmt.Errorf("tunnel manager not initialized")
			}
			if err := sess.tunnelManager.SendData(params); err != nil {
				return nil, err
			}
			return map[string]bool{"ok": true}, nil
		}),
		"tunnel/disconnect": handler.New(func(ctx context.Context, params tunnel.DisconnectParams) (interface{}, error) {
			s.touch()
			if sess.tunnelManager == nil {
				return nil, fmt.Errorf("tunnel manager not initialized")
			}
			if err := sess.tunnelManager.Disconnect(params); err != nil {
				return nil, err
			}
			return map[string]bool{"ok": true}, nil
		}),

		// Shutdown - delay exit to allow in-flight RPCs to complete
		"shutdown": handler.New(func(ctx context.Context) (interface{}, error) {
			sess.Logger().Info("shutdown RPC received, exiting in 5s")
			go func() {
				time.Sleep(5 * time.Second)
				s.Shutdown()
			}()
			return map[string]bool{"ok": true}, nil
		}),
	}
}

// serverOptions returns jrpc2 server options
func (s *Server) serverOptions() *jrpc2.ServerOptions {
	opts := &jrpc2.ServerOptions{
		AllowPush: true, // Enable server-to-client notifications
	}
	if s.verbose {
		opts.Logger = jrpc2.StdLogger(log.Default())
	}
	return opts
}

// HandleConnection handles a single TCP client connection
func (s *Server) HandleConnection(conn net.Conn) {
	s.connMu.Lock()
	s.connections[conn] = struct{}{}
	s.connMu.Unlock()

	sess := NewSession(s.logLevel)
	defer func() {
		conn.Close()
		s.connMu.Lock()
		delete(s.connections, conn)
		s.connMu.Unlock()
		sess.Logger().Info("Connection closed", "remote", conn.RemoteAddr().String())
		sess.Close()
	}()

	if err := sess.InitWatcher(s.fsHandler); err != nil {
		slog.Error("Failed to init watcher", "error", err)
	}

	// Wrap writer with async buffer (see HandleStdio for rationale)
	aw := newAsyncWriter(conn)
	defer aw.Close()

	// Create channel with LSP framing (Content-Length headers)
	ch := channel.Header("")(conn, aw)

	// Create and start the jrpc2 server
	srv := jrpc2.NewServer(s.buildHandlerMap(sess), s.serverOptions())
	sess.SetServer(srv)

	// Initialize Eden watcher after SetServer so it can use the session logger
	sess.InitEdenWatcher(s.fsHandler)

	sess.Logger().Info("Connection established", "remote", conn.RemoteAddr().String())

	// Run until connection closes or server shuts down
	go func() {
		<-s.done
		srv.Stop()
	}()

	srv.Start(ch)
	if err := srv.Wait(); err != nil {
		sess.Logger().Error("Server error", "error", err)
	}
}

// ReadyMessage is sent to stdout in stdio mode to signal the server is ready
type ReadyMessage struct {
	FlitReady bool   `json:"flit_ready"`
	Version   string `json:"version"`
}

// HandleStdio handles JSON-RPC over stdin/stdout
func (s *Server) HandleStdio(stdin io.Reader, stdout io.Writer) {
	sess := NewSession(s.logLevel)
	defer sess.Close()

	if err := sess.InitWatcher(s.fsHandler); err != nil {
		// In stdio mode, only log fatal errors to stderr
		fmt.Fprintf(os.Stderr, "Failed to init watcher: %v\n", err)
	}

	// Send ready message as JSON line (not Content-Length framed)
	// This allows the client to detect when the server is ready
	readyMsg := ReadyMessage{FlitReady: true, Version: "1.0"}
	readyBytes, _ := json.Marshal(readyMsg)
	stdout.Write(readyBytes)
	stdout.Write([]byte("\n"))

	// Wrap stdout with async buffer so jrpc2 writes don't block on
	// slow pipes/transports (prevents mutex-held-during-write deadlocks).
	aw := newAsyncWriter(stdout)
	defer aw.Close()

	// Create channel with Content-Length framing (no Content-Type header)
	// We need a WriteCloser, so wrap stdout
	ch := channel.Header("")(stdin, writeCloser{aw})

	// Create and start the jrpc2 server
	srv := jrpc2.NewServer(s.buildHandlerMap(sess), s.serverOptions())
	sess.SetServer(srv)

	// Initialize Eden watcher after SetServer so it can use the session logger
	sess.InitEdenWatcher(s.fsHandler)

	// Sync heartbeat: periodically send a request and wait for response
	// - If network is down, this blocks until it comes back (that's fine)
	// - If connection is truly broken (client exited), we get ErrConnClosed and exit
	// - We don't use timeouts because we want to survive temporary network outages
	go func() {
		ticker := time.NewTicker(30 * time.Second)
		defer ticker.Stop()
		for {
			select {
			case <-s.done:
				return
			case <-ticker.C:
				// Send heartbeat request and wait for response (no timeout)
				_, err := srv.Callback(context.Background(), "heartbeat", nil)
				if err != nil {
					srv.Stop()
					return
				}
			}
		}
	}()

	// Run until stdin closes or server shuts down
	go func() {
		<-s.done
		srv.Stop()
		// srv.Wait() blocks on stdin.Read() which can't be interrupted
		// by closing the fd on Linux. Force exit after Stop().
		os.Exit(0)
	}()

	srv.Start(ch)
	srv.Wait()
}

// writeCloser wraps an io.Writer to satisfy io.WriteCloser
type writeCloser struct {
	io.Writer
}

func (wc writeCloser) Close() error {
	if c, ok := wc.Writer.(io.Closer); ok {
		return c.Close()
	}
	return nil
}

// asyncWriter wraps an io.Writer with a buffered async queue so that
// writes never block. This prevents bidirectional pipe deadlocks where
// jrpc2 notification senders (tunnel readLoop, fs watcher, exec output)
// block on a full buffer, cascading through jrpc2's internal queues to
// block the stdin reader, while Emacs is blocked writing to stdin
// because stdout isn't being drained.
//
// If the buffer fills up (client not reading), Write returns an error
// which propagates to the caller (e.g. tunnel readLoop, exec output
// callback) causing them to close cleanly.
type asyncWriter struct {
	ch   chan []byte
	done chan struct{}
	err  error
}

var errWriteQueueFull = fmt.Errorf("async write queue full — client not reading")

func newAsyncWriter(w io.Writer) *asyncWriter {
	aw := &asyncWriter{
		ch:   make(chan []byte, 1024),
		done: make(chan struct{}),
	}
	go func() {
		defer close(aw.done)
		for data := range aw.ch {
			if _, err := w.Write(data); err != nil {
				aw.err = err
				for range aw.ch {
				}
				return
			}
		}
	}()
	return aw
}

func (aw *asyncWriter) Write(p []byte) (int, error) {
	if aw.err != nil {
		return 0, aw.err
	}
	data := make([]byte, len(p))
	copy(data, p)
	select {
	case aw.ch <- data:
		return len(p), nil
	default:
		return 0, errWriteQueueFull
	}
}

func (aw *asyncWriter) Close() error {
	close(aw.ch)
	<-aw.done
	return aw.err
}
