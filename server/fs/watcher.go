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

// Package fs provides file system operations for the flit server.
//
// # Watch and Cache Architecture
//
// The watcher provides file system change notifications with a unified tracking
// structure for both existing paths and pending (non-existent) paths.
//
// ## Data Structure
//
// All watches are tracked in a single map:
//
//	watches map[string]*watchInfo
//
// Where watchInfo contains:
//   - refCount: reference count for direct watches (files or directories)
//   - pending: map[childName][]pendingDescendants for tracking non-existent paths
//
// ## Direct Watches (refCount > 0)
//
// When a client requests info for an existing file or directory listing:
//   - The path is added to watches with refCount incremented
//   - fsnotify watches the path directly
//   - Modifications trigger notifications for that exact path
//
// Files must be watched directly because directory watches don't fire on
// file content changes.
//
// ## Pending Watches (for non-existent paths)
//
// When a client requests info for a non-existent path like /a/b/c/d/file.txt
// where only /a/b exists:
//   - Find the closest existing ancestor: /a/b
//   - Watch that ancestor directory
//   - Add to pending: watches["/a/b"].pending["c"] = ["/a/b/c/d/file.txt"]
//
// When a create event fires for /a/b/c:
//   - Look up watches["/a/b"].pending["c"]
//   - Notify all pending descendants (they'll re-query and may set up new watches)
//   - Remove the pending entry
//
// ## Watch Cleanup
//
// A watch is removed from fsnotify when:
//   - refCount reaches 0 AND
//   - pending map is empty
//
// This ensures watches stay active as long as either:
//   - A direct watch exists (for cached listings or file info)
//   - Pending paths are waiting for creation events
package fs

import (
	"log/slog"
	"os"
	"path/filepath"
	"strings"
	"sync"
	"time"

	"github.com/fsnotify/fsnotify"
)

// WatchEvent represents a file system change notification
type WatchEvent struct {
	Path string `json:"path"`
	Type string `json:"type"` // "modified", "created", "deleted", "renamed"
}

// NotifyFunc is called when a file system event occurs
type NotifyFunc func(event WatchEvent)

// watchInfo tracks watch state for a single path
type watchInfo struct {
	refCount int                 // direct watch refcount (for files or directory listings)
	pending  map[string][]string // childName -> []pendingDescendants (only for directories)
}

// debounceState tracks notification history for a path
type debounceState struct {
	timer     *time.Timer // pending debounced notification
	count     int         // notifications in current burst window
	windowEnd time.Time   // when current burst window expires
}

// Watcher manages file system watches with debouncing
type Watcher struct {
	watcher        *fsnotify.Watcher
	notify         NotifyFunc
	watches        map[string]*watchInfo     // path -> watch info
	debounceStates map[string]*debounceState // path -> debounce state
	mu             sync.Mutex
	done           chan struct{}
}

const (
	debounceInterval = time.Second // Debounce delay after burst exhausted
	burstLimit       = 3           // Allow this many instant notifications before debouncing
	burstWindow      = time.Second // Reset burst count after this duration of quiet
)

// NewWatcher creates a new file watcher
func NewWatcher(notify NotifyFunc) (*Watcher, error) {
	fsw, err := fsnotify.NewWatcher()
	if err != nil {
		return nil, err
	}

	w := &Watcher{
		watcher:        fsw,
		notify:         notify,
		watches:        make(map[string]*watchInfo),
		debounceStates: make(map[string]*debounceState),
		done:           make(chan struct{}),
	}

	go w.run()

	return w, nil
}

// getOrCreateWatch returns the watchInfo for a path, creating it if needed.
// Caller must hold w.mu.
func (w *Watcher) getOrCreateWatch(path string) *watchInfo {
	info := w.watches[path]
	if info == nil {
		info = &watchInfo{
			pending: make(map[string][]string),
		}
		w.watches[path] = info
	}
	return info
}

// Watch adds a path to the watch list with refcounting.
// Used for both files and directories.
func (w *Watcher) Watch(path string) error {
	w.mu.Lock()
	defer w.mu.Unlock()

	info := w.getOrCreateWatch(path)
	if info.refCount == 0 && len(info.pending) == 0 {
		// New watch - add to fsnotify
		if err := w.watcher.Add(path); err != nil {
			// Clean up empty watchInfo
			if info.refCount == 0 && len(info.pending) == 0 {
				delete(w.watches, path)
			}
			return err
		}
		slog.Info("watch: started", "path", path)
	}
	info.refCount++

	return nil
}

// WatchFile adds a file to the watch list.
// The file will receive direct notifications for modifications.
func (w *Watcher) WatchFile(path string) error {
	return w.Watch(path)
}

// WatchDir adds a directory to the watch list.
// The directory will receive notifications for listing changes.
func (w *Watcher) WatchDir(path string) error {
	return w.Watch(path)
}

// WatchPending registers a non-existent path to watch for creation.
// ancestor is the closest existing directory that will be watched.
// pendingPath is the full path that doesn't exist yet.
// The ancestor must be watched (via WatchDir) before calling this.
func (w *Watcher) WatchPending(ancestor, pendingPath string) {
	w.mu.Lock()
	defer w.mu.Unlock()

	info := w.watches[ancestor]
	if info == nil {
		// Ancestor should have been watched first
		return
	}

	// Extract the first non-existent component (child of ancestor)
	rel, err := filepath.Rel(ancestor, pendingPath)
	if err != nil {
		return
	}
	// Get first component of relative path
	parts := strings.SplitN(rel, string(filepath.Separator), 2)
	if len(parts) == 0 {
		return
	}
	child := parts[0]

	// Add to pending map
	info.pending[child] = append(info.pending[child], pendingPath)
}

// ClearPending removes all pending path registrations.
// Called when client clears its cache - pending paths are no longer relevant.
func (w *Watcher) ClearPending() {
	w.mu.Lock()
	defer w.mu.Unlock()

	for _, info := range w.watches {
		if info != nil && len(info.pending) > 0 {
			info.pending = make(map[string][]string)
		}
	}
}

// Unwatch removes a path from the watch list.
// Decrements refcount and removes fsnotify watch when no longer needed.
func (w *Watcher) Unwatch(path string) error {
	w.mu.Lock()
	defer w.mu.Unlock()

	info := w.watches[path]
	if info == nil || info.refCount <= 0 {
		return nil // Not watching
	}

	info.refCount--
	if info.refCount == 0 && len(info.pending) == 0 {
		// No more direct watchers and no pending paths
		delete(w.watches, path)
		if state, ok := w.debounceStates[path]; ok {
			if state.timer != nil {
				state.timer.Stop()
			}
			delete(w.debounceStates, path)
		}
		slog.Info("watch: stopped", "path", path)
		if err := w.watcher.Remove(path); err != nil {
			return err
		}
	}

	return nil
}

// Close shuts down the watcher
func (w *Watcher) Close() error {
	close(w.done)
	w.mu.Lock()
	defer w.mu.Unlock()
	for _, state := range w.debounceStates {
		if state.timer != nil {
			state.timer.Stop()
		}
	}
	w.debounceStates = make(map[string]*debounceState)
	return w.watcher.Close()
}

// triggerDebouncedNotification handles notification with burst allowance.
// The first burstLimit notifications within burstWindow are sent immediately.
// After that, notifications are debounced until the window expires.
func (w *Watcher) triggerDebouncedNotification(path string, eventType string, isDirectEvent bool) {
	w.mu.Lock()
	defer w.mu.Unlock()

	now := time.Now()
	state := w.debounceStates[path]

	// Initialize or reset state if window expired
	if state == nil || now.After(state.windowEnd) {
		state = &debounceState{
			count:     0,
			windowEnd: now.Add(burstWindow),
		}
		w.debounceStates[path] = state
	}

	// If we have a pending debounced notification, just reset the timer
	if state.timer != nil {
		state.timer.Reset(debounceInterval)
		return
	}

	// Within burst limit - send immediately
	if state.count < burstLimit {
		state.count++
		if w.notify != nil {
			if isDirectEvent {
				slog.Info("watch: event (burst)", "type", eventType, "path", path, "count", state.count)
			} else {
				slog.Debug("watch: event (burst)", "type", eventType, "path", path, "count", state.count)
			}
			w.notify(WatchEvent{
				Path: path,
				Type: eventType,
			})
		}
		return
	}

	// Burst exhausted - start debouncing
	state.timer = time.AfterFunc(debounceInterval, func() {
		if w.notify != nil {
			if isDirectEvent {
				slog.Info("watch: event (debounced)", "type", eventType, "path", path)
			} else {
				slog.Debug("watch: event (debounced)", "type", eventType, "path", path)
			}
			w.notify(WatchEvent{
				Path: path,
				Type: eventType,
			})
		}
		w.mu.Lock()
		if s, ok := w.debounceStates[path]; ok {
			s.timer = nil
			// Reset burst count after debounced notification fires
			s.count = 0
			s.windowEnd = time.Now().Add(burstWindow)
		}
		w.mu.Unlock()
	})
}

// isDirectlyWatched returns true if the path has a direct watch (refCount > 0)
func (w *Watcher) isDirectlyWatched(path string) bool {
	w.mu.Lock()
	defer w.mu.Unlock()
	info := w.watches[path]
	return info != nil && info.refCount > 0
}

// popPendingForChild removes and returns all pending paths for a child of ancestor.
// Also cleans up the ancestor watch if no longer needed.
func (w *Watcher) popPendingForChild(ancestor, child string) []string {
	w.mu.Lock()
	defer w.mu.Unlock()

	info := w.watches[ancestor]
	if info == nil {
		return nil
	}

	pending := info.pending[child]
	delete(info.pending, child)

	// Clean up ancestor watch if no longer needed
	if info.refCount == 0 && len(info.pending) == 0 {
		delete(w.watches, ancestor)
		if state, ok := w.debounceStates[ancestor]; ok {
			if state.timer != nil {
				state.timer.Stop()
			}
			delete(w.debounceStates, ancestor)
		}
		// Remove fsnotify watch - ignore error as path may no longer exist
		_ = w.watcher.Remove(ancestor)
	}

	return pending
}

// watchParentForPending sets up a pending watch for a file that was deleted/renamed.
// This allows us to detect when the file is recreated.
// Returns true if the file was found to already exist (and watch was re-added).
// Caller must NOT hold w.mu.
func (w *Watcher) watchParentForPending(filePath string) bool {
	parentDir := filepath.Dir(filePath)
	childName := filepath.Base(filePath)

	w.mu.Lock()
	info := w.getOrCreateWatch(parentDir)
	needsAdd := info.refCount == 0 && len(info.pending) == 0
	info.pending[childName] = append(info.pending[childName], filePath)
	w.mu.Unlock()

	if needsAdd {
		if err := w.watcher.Add(parentDir); err != nil {
			slog.Debug("watch: failed to watch parent for pending", "parent", parentDir, "file", filePath, "err", err)
			// Clean up the pending entry we just added
			w.mu.Lock()
			delete(info.pending, childName)
			if info.refCount == 0 && len(info.pending) == 0 {
				delete(w.watches, parentDir)
			}
			w.mu.Unlock()
			return false
		}
		slog.Info("watch: watching parent for recreation", "parent", parentDir, "file", filePath)
	}

	// Race check: file may have been recreated between our initial check and
	// setting up the parent watch. Check again now that parent watch is active.
	if _, err := os.Stat(filePath); err == nil {
		// File exists now! Re-add the direct watch and clean up pending.
		if err := w.watcher.Add(filePath); err != nil {
			slog.Debug("watch: failed to re-add after race", "path", filePath, "err", err)
		} else {
			slog.Info("watch: re-added after race (file already recreated)", "path", filePath)
		}

		// Clean up pending entry
		w.mu.Lock()
		pending := info.pending[childName]
		// Remove this specific path from pending list
		for i, p := range pending {
			if p == filePath {
				info.pending[childName] = append(pending[:i], pending[i+1:]...)
				break
			}
		}
		if len(info.pending[childName]) == 0 {
			delete(info.pending, childName)
		}
		// Clean up parent watch if no longer needed
		if info.refCount == 0 && len(info.pending) == 0 {
			delete(w.watches, parentDir)
			if state, ok := w.debounceStates[parentDir]; ok {
				if state.timer != nil {
					state.timer.Stop()
				}
				delete(w.debounceStates, parentDir)
			}
			_ = w.watcher.Remove(parentDir)
		}
		w.mu.Unlock()

		// Send notification directly, bypassing debounce.
		// This is a significant event that the client needs to know about.
		if w.notify != nil {
			slog.Info("watch: notifying recreation (race)", "path", filePath)
			w.notify(WatchEvent{
				Path: filePath,
				Type: "modified",
			})
		}

		return true
	}

	return false
}

// run processes file system events
func (w *Watcher) run() {
	for {
		select {
		case <-w.done:
			return

		case event, ok := <-w.watcher.Events:
			if !ok {
				return
			}

			// Convert fsnotify event to our event type
			var eventType string
			switch {
			case event.Op&fsnotify.Write != 0:
				eventType = "modified"
			case event.Op&fsnotify.Create != 0:
				eventType = "created"
			case event.Op&fsnotify.Remove != 0:
				eventType = "deleted"
				// When a file is deleted, the kernel removes the inotify watch.
				// If the file still exists (atomic rewrite), re-add the watch.
				// If it doesn't exist yet (delete-then-create race), watch the
				// parent directory so we catch when it's recreated.
				if w.isDirectlyWatched(event.Name) {
					if _, err := os.Stat(event.Name); err == nil {
						// File still exists - re-add the watch
						if err := w.watcher.Add(event.Name); err != nil {
							slog.Debug("watch: failed to re-add after delete", "path", event.Name, "err", err)
						} else {
							slog.Info("watch: re-added after atomic rewrite", "path", event.Name)
						}
					} else if os.IsNotExist(err) {
						// File doesn't exist yet - watch parent for its creation.
						// If file was already recreated, watchParentForPending sends
						// notification directly and returns true - skip normal flow.
						if w.watchParentForPending(event.Name) {
							continue
						}
					}
				}
			case event.Op&fsnotify.Rename != 0:
				eventType = "renamed"
				// When a file is renamed away, the kernel removes the inotify watch.
				// Same handling as Remove - check if file exists or watch for creation.
				if w.isDirectlyWatched(event.Name) {
					if _, err := os.Stat(event.Name); err == nil {
						if err := w.watcher.Add(event.Name); err != nil {
							slog.Debug("watch: failed to re-add after rename", "path", event.Name, "err", err)
						} else {
							slog.Info("watch: re-added after rename", "path", event.Name)
						}
					} else if os.IsNotExist(err) {
						// File was renamed away and doesn't exist yet.
						// Set up pending watch on parent directory so we catch
						// when it's recreated. Always continue after this - we'll
						// get a Create event later when the file reappears.
						w.watchParentForPending(event.Name)
						continue
					}
				}
			case event.Op&fsnotify.Chmod != 0:
				// Skip chmod events - not usually interesting
				continue
			default:
				continue
			}

			eventPath := event.Name
			parentDir := filepath.Dir(eventPath)
			childName := filepath.Base(eventPath)

			// Check for pending paths on create OR rename events.
			// Rename is needed because atomic writes do: write temp, rename temp -> target.
			// The rename generates IN_MOVED_TO which fsnotify reports as Rename, not Create.
			if eventType == "created" || eventType == "renamed" {
				pending := w.popPendingForChild(parentDir, childName)
				for _, pendingPath := range pending {
					// Re-add direct watch if this path was previously watched
					if w.isDirectlyWatched(pendingPath) {
						if err := w.watcher.Add(pendingPath); err != nil {
							slog.Debug("watch: failed to re-add after recreation", "path", pendingPath, "err", err)
						} else {
							slog.Info("watch: re-added after recreation", "path", pendingPath)
						}
					}
					// Always notify for pending path recreation - bypass debouncing.
					// This is a significant event (file was deleted and recreated) that
					// the client needs to know about, even if it happened quickly.
					if w.notify != nil {
						slog.Info("watch: notifying recreation", "path", pendingPath)
						w.notify(WatchEvent{
							Path: pendingPath,
							Type: "modified", // Content changed, use modified not created
						})
					}
				}
			}

			// Determine notification path
			notifyPath := eventPath
			isDirectEvent := true // true for file events, false for directory events
			if w.isDirectlyWatched(eventPath) {
				// Path is directly watched - notify for it specifically
				notifyPath = eventPath
			} else if w.isDirectlyWatched(parentDir) {
				// Parent directory is watched but this specific file isn't
				// Notify for directory instead (so dired/listings can refresh)
				notifyPath = parentDir
				eventType = "modified" // Directory contents changed
				isDirectEvent = false
			} else {
				// Neither path nor parent is directly watched
				// This could happen for pending-only watches; skip notification
				continue
			}

			// Trigger debounced notification
			w.triggerDebouncedNotification(notifyPath, eventType, isDirectEvent)

		case err, ok := <-w.watcher.Errors:
			if !ok {
				return
			}
			// Error during watch - silently ignore to avoid corrupting stdio stream
			_ = err
		}
	}
}
