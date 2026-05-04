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

package fs

import (

	"encoding/json"
	"fmt"
	"io"
	"log/slog"
	"os"
	"os/user"
	"path/filepath"
	"strconv"
	"strings"
	"sync"
	"syscall"
	"time"

	"github.com/muirdm/flit.el/util"
)

// strPtr returns a pointer to a string (helper for optional string fields)
func strPtr(s string) *string {
	return &s
}

// Handler handles file system operations
type Handler struct{}

// NewHandler creates a new file system handler
func NewHandler() *Handler {
	return &Handler{}
}

// RPCError represents a JSON-RPC error
type RPCError struct {
	Code    int
	Message string
	Data    interface{}
}

func (e *RPCError) Error() string {
	return e.Message
}

// Error codes
const (
	InvalidParams = -32602
	InternalError = -32603
	FileNotFound  = -32001
	PermissionErr = -32002
	FileExists    = -32003
)

// StatParams represents parameters for fs/stat
type StatParams struct {
	Path string `json:"path"`
}

// StatResult represents the result of fs/stat
type StatResult struct {
	Size   int64  `json:"size"`
	Mtime  int64  `json:"mtime"`
	Atime  int64  `json:"atime"`
	Mode   uint32 `json:"mode"`
	Type   string `json:"type"`
	IsDir  bool   `json:"isDir"`
	Uid    uint32 `json:"uid"`
	Gid    uint32 `json:"gid"`
	User   string `json:"user,omitempty"`
	Group  string `json:"group,omitempty"`
	Nlink  uint64 `json:"nlink"`
	Target string `json:"target,omitempty"`
}

// Stat returns file information
func (h *Handler) Stat(params json.RawMessage) (interface{}, error) {
	var p StatParams
	if err := json.Unmarshal(params, &p); err != nil {
		return nil, &RPCError{Code: InvalidParams, Message: "Invalid params", Data: err.Error()}
	}

	slog.Info("fs/stat", "path", p.Path)

	info, err := os.Lstat(p.Path)
	if err != nil {
		return nil, fileError(err)
	}

	result := &StatResult{}
	populateStatFromFileInfo(result, p.Path, info)
	return result, nil
}

// statFields is an interface for types that can receive stat information
type statFields interface {
	setStatFields(size int64, mtime int64, mode uint32, typ string, isDir bool, target string)
	setSyscallFields(uid, gid uint32, user, group string, nlink uint64, atime int64)
}

func (r *StatResult) setStatFields(size int64, mtime int64, mode uint32, typ string, isDir bool, target string) {
	r.Size = size
	r.Mtime = mtime
	r.Mode = mode
	r.Type = typ
	r.IsDir = isDir
	r.Target = target
}

func (r *StatResult) setSyscallFields(uid, gid uint32, user, group string, nlink uint64, atime int64) {
	r.Uid = uid
	r.Gid = gid
	r.User = user
	r.Group = group
	r.Nlink = nlink
	r.Atime = atime
}

func (r *InfoResult) setStatFields(size int64, mtime int64, mode uint32, typ string, isDir bool, target string) {
	r.Size = size
	r.Mtime = mtime
	r.Mode = mode
	r.Type = typ
	r.IsDir = isDir
	r.Target = target
}

func (r *InfoResult) setSyscallFields(uid, gid uint32, user, group string, nlink uint64, atime int64) {
	r.Uid = uid
	r.Gid = gid
	r.User = user
	r.Group = group
	r.Nlink = nlink
	r.Atime = atime
}

// populateStatFromFileInfo fills in stat fields from os.FileInfo
// This is the single source of truth for stat field population
func populateStatFromFileInfo(result statFields, path string, info os.FileInfo) {
	size := info.Size()
	mtime := info.ModTime().Unix()
	mode := uint32(info.Mode().Perm())

	typ, isDir, target := determineTypeAndIsDir(path, info)

	result.setStatFields(size, mtime, mode, typ, isDir, target)

	// Get platform-specific stat info
	if stat, ok := info.Sys().(*syscall.Stat_t); ok {
		userName := lookupUser(stat.Uid)
		groupName := lookupGroup(stat.Gid)
		result.setSyscallFields(stat.Uid, stat.Gid, userName, groupName, uint64(stat.Nlink), getAtime(stat))
	}
}

// lookupUser returns the username for a uid, or the numeric uid as a string if lookup fails
func lookupUser(uid uint32) string {
	if u, err := user.LookupId(strconv.FormatUint(uint64(uid), 10)); err == nil {
		return u.Username
	}
	return strconv.FormatUint(uint64(uid), 10)
}

// lookupGroup returns the group name for a gid, or the numeric gid as a string if lookup fails
func lookupGroup(gid uint32) string {
	if g, err := user.LookupGroupId(strconv.FormatUint(uint64(gid), 10)); err == nil {
		return g.Name
	}
	return strconv.FormatUint(uint64(gid), 10)
}

// determineTypeAndIsDir determines the file type and whether it's a directory.
// For symlinks, it follows the link to check if the target is a directory.
// Returns (type, isDir, symlinkTarget).
func determineTypeAndIsDir(path string, info os.FileInfo) (typ string, isDir bool, target string) {
	isDir = info.IsDir()

	switch {
	case info.Mode().IsDir():
		typ = "directory"
	case info.Mode().IsRegular():
		typ = "file"
	case info.Mode()&os.ModeSymlink != 0:
		typ = "symlink"
		if t, err := os.Readlink(path); err == nil {
			target = t
		}
		// For symlinks, check if target is a directory
		if targetInfo, err := os.Stat(path); err == nil {
			isDir = targetInfo.IsDir()
		}
	default:
		typ = "other"
	}
	return
}

// ReadParams represents parameters for fs/read
type ReadParams struct {
	Path   string `json:"path"`
	Offset int64  `json:"offset,omitempty"`
	Length int64  `json:"length,omitempty"`
}

// ReadResult represents the result of fs/read, combining content and stat info
type ReadResult struct {
	Content []byte     `json:"content"` // Raw binary file content
	Stat    InfoResult `json:"stat"`    // Full info including path/realpath/exists
}

// Read reads file contents and returns it along with fresh stat info
func (h *Handler) Read(params json.RawMessage) (interface{}, error) {
	var p ReadParams
	if err := json.Unmarshal(params, &p); err != nil {
		return nil, &RPCError{Code: InvalidParams, Message: "Invalid params", Data: err.Error()}
	}

	slog.Info("fs/read", "path", p.Path, "offset", p.Offset, "length", p.Length)

	// Read the file content first
	content, err := os.ReadFile(p.Path)
	if err != nil {
		return nil, fileError(err)
	}

	// Now that we have the content, stat the file to get the most up-to-date
	// metadata. This avoids a race condition where the file could change
	// between a separate stat and read call.
	info, err := os.Lstat(p.Path)
	if err != nil {
		return nil, fileError(err)
	}

	result := &ReadResult{
		Content: content,
	}
	result.Stat.Exists = true
	result.Stat.Path = p.Path
	populateStatFromFileInfo(&result.Stat, p.Path, info)

	// Get realpath
	if resolved, err := filepath.EvalSymlinks(p.Path); err == nil {
		if abs, err := filepath.Abs(resolved); err == nil {
			result.Stat.Realpath = abs
		}
	}

	return result, nil
}

// WriteParams represents parameters for fs/write
type WriteParams struct {
	Path           string   `json:"path"`
	Content        []byte   `json:"content"`
	Mode           uint32   `json:"mode,omitempty"`
	Append         bool     `json:"append,omitempty"`
	ExpectedMtime  *float64 `json:"expectedMtime,omitempty"`  // Expected file mtime (seconds since epoch)
	ExpectNotExist bool     `json:"expectNotExist,omitempty"` // True if client expects file to not exist
	Force          bool     `json:"force,omitempty"`          // Skip mtime check (user confirmed overwrite)
}

// WriteMismatchResult is returned when the file's state doesn't match expectations
type WriteMismatchResult struct {
	Mismatch bool        `json:"mismatch"`
	Current  *InfoResult `json:"current,omitempty"` // Current file info
}

// Write writes content to a file and returns updated file info
func (h *Handler) Write(p *WriteParams) (interface{}, error) {
	slog.Info("fs/write", "path", p.Path, "size", len(p.Content), "mode", p.Mode, "append", p.Append,
		"expectedMtime", p.ExpectedMtime, "expectNotExist", p.ExpectNotExist, "force", p.Force)

	// Check for mtime mismatch before writing (unless force is set)
	if !p.Force && (p.ExpectedMtime != nil || p.ExpectNotExist) {
		stat, err := os.Stat(p.Path)
		fileExists := err == nil

		var mismatch bool
		if p.ExpectNotExist {
			// Client expects file to not exist - mismatch if it does
			mismatch = fileExists
			if mismatch {
				slog.Info("fs/write mismatch: expected not exist but file exists", "path", p.Path)
			}
		} else if p.ExpectedMtime != nil {
			if !fileExists {
				// Client expects file to exist but it doesn't
				mismatch = true
				slog.Info("fs/write mismatch: expected file to exist", "path", p.Path)
			} else {
				// Compare mtimes (2 second tolerance like TRAMP)
				actualMtime := float64(stat.ModTime().Unix())
				diff := actualMtime - *p.ExpectedMtime
				if diff < 0 {
					diff = -diff
				}
				mismatch = diff > 2
				if mismatch {
					slog.Info("fs/write mismatch: mtime differs", "path", p.Path,
						"expected", *p.ExpectedMtime, "actual", actualMtime, "diff", diff)
				}
			}
		}

		if mismatch {
			// Return current file info without writing
			var current *InfoResult
			if fileExists {
				current, _ = h.getInfo(p.Path)
			}
			return &WriteMismatchResult{
				Mismatch: true,
				Current:  current,
			}, nil
		}
	}

	mode := os.FileMode(0644)
	if p.Mode != 0 {
		mode = os.FileMode(p.Mode)
	}

	flags := os.O_WRONLY | os.O_CREATE
	if p.Append {
		flags |= os.O_APPEND
	} else {
		flags |= os.O_TRUNC
	}

	f, err := os.OpenFile(p.Path, flags, mode)
	if err != nil {
		return nil, fileError(err)
	}
	defer f.Close()

	_, err = f.Write(p.Content)
	if err != nil {
		return nil, fileError(err)
	}

	// Return updated file info (without content - client already has it)
	info, err := h.getInfo(p.Path)
	if err != nil {
		return nil, err
	}
	// Clear content field - client already knows it
	info.Content = nil

	return &InfoResponse{
		InfoResult: info,
		Cache:      BuildCacheEntries(info),
	}, nil
}

// DirChild represents a directory entry with minimal info (name and isDir).
// IsDir is true for directories and symlinks that point to directories.
type DirChild struct {
	Name  string `json:"name"`
	IsDir bool   `json:"isDir"`
}

// readDirWithTypes returns directory entries with name and isDir info.
// For regular files/dirs, isDir comes from DirEntry (no stat needed).
// For symlinks, follows the link to check if target is a directory (parallel stat).
func (h *Handler) readDirWithTypes(dirPath string) ([]DirChild, error) {
	entries, err := os.ReadDir(dirPath)
	if err != nil {
		return nil, fileError(err)
	}

	results := make([]DirChild, len(entries))
	bwg := util.NewBoundedWaitGroup(32)

	for i, entry := range entries {
		results[i].Name = entry.Name()
		if entry.Type()&os.ModeSymlink != 0 {
			// Symlink - need to follow to check if target is dir
			bwg.Add(1)
			go func(idx int) {
				defer bwg.Done()
				childPath := filepath.Join(dirPath, results[idx].Name)
				// os.Stat follows symlinks
				if info, err := os.Stat(childPath); err == nil {
					results[idx].IsDir = info.IsDir()
				}
				// On error (broken symlink), IsDir stays false
			}(i)
		} else {
			// Regular file or directory - IsDir() is available from dirent
			results[i].IsDir = entry.IsDir()
		}
	}

	bwg.Wait()
	return results, nil
}

// readDir returns just the names of entries in a directory (no stat calls)
// This is the minimal operation for directory listing
func (h *Handler) readDir(path string) ([]string, error) {
	entries, err := os.ReadDir(path)
	if err != nil {
		return nil, fileError(err)
	}

	names := make([]string, len(entries))
	for i, entry := range entries {
		names[i] = entry.Name()
	}
	return names, nil
}

// ReadDir is exported for use by the server
func (h *Handler) ReadDir(path string) ([]string, error) {
	return h.readDir(path)
}

// ReadDirWithTypes is exported for use by the server
func (h *Handler) ReadDirWithTypes(dirPath string) ([]DirChild, error) {
	return h.readDirWithTypes(dirPath)
}

// MkdirParams represents parameters for fs/mkdir
type MkdirParams struct {
	Path string `json:"path"`
	Mode uint32 `json:"mode,omitempty"`
}

// Mkdir creates a directory
func (h *Handler) Mkdir(params json.RawMessage) (interface{}, error) {
	var p MkdirParams
	if err := json.Unmarshal(params, &p); err != nil {
		return nil, &RPCError{Code: InvalidParams, Message: "Invalid params", Data: err.Error()}
	}

	slog.Info("fs/mkdir", "path", p.Path, "mode", p.Mode)

	mode := os.FileMode(0755)
	if p.Mode != 0 {
		mode = os.FileMode(p.Mode)
	}

	if err := os.MkdirAll(p.Path, mode); err != nil {
		return nil, fileError(err)
	}

	return map[string]bool{"ok": true}, nil
}

// DeleteParams represents parameters for fs/delete
type DeleteParams struct {
	Path      string `json:"path"`
	Recursive bool   `json:"recursive,omitempty"`
}

// Delete deletes a file or directory
func (h *Handler) Delete(params json.RawMessage) (interface{}, error) {
	var p DeleteParams
	if err := json.Unmarshal(params, &p); err != nil {
		return nil, &RPCError{Code: InvalidParams, Message: "Invalid params", Data: err.Error()}
	}

	slog.Info("fs/delete", "path", p.Path, "recursive", p.Recursive)

	var err error
	if p.Recursive {
		err = os.RemoveAll(p.Path)
	} else {
		err = os.Remove(p.Path)
	}

	if err != nil {
		return nil, fileError(err)
	}

	return map[string]bool{"ok": true}, nil
}

// RenameParams represents parameters for fs/rename
type RenameParams struct {
	OldPath string `json:"oldPath"`
	NewPath string `json:"newPath"`
}

// Rename renames/moves a file or directory
func (h *Handler) Rename(params json.RawMessage) (interface{}, error) {
	var p RenameParams
	if err := json.Unmarshal(params, &p); err != nil {
		return nil, &RPCError{Code: InvalidParams, Message: "Invalid params", Data: err.Error()}
	}

	slog.Info("fs/rename", "old", p.OldPath, "new", p.NewPath)

	if err := os.Rename(p.OldPath, p.NewPath); err != nil {
		return nil, fileError(err)
	}

	return map[string]bool{"ok": true}, nil
}

// CopyParams represents parameters for fs/copy
type CopyParams struct {
	Src  string `json:"src"`
	Dest string `json:"dest"`
	Mode uint32 `json:"mode,omitempty"`
}

// Copy copies a file
func (h *Handler) Copy(params json.RawMessage) (interface{}, error) {
	var p CopyParams
	if err := json.Unmarshal(params, &p); err != nil {
		return nil, &RPCError{Code: InvalidParams, Message: "Invalid params", Data: err.Error()}
	}

	slog.Info("fs/copy", "src", p.Src, "dest", p.Dest)

	src, err := os.Open(p.Src)
	if err != nil {
		return nil, fileError(err)
	}
	defer src.Close()

	srcInfo, err := src.Stat()
	if err != nil {
		return nil, fileError(err)
	}

	mode := srcInfo.Mode()
	if p.Mode != 0 {
		mode = os.FileMode(p.Mode)
	}

	dest, err := os.OpenFile(p.Dest, os.O_WRONLY|os.O_CREATE|os.O_TRUNC, mode)
	if err != nil {
		return nil, fileError(err)
	}
	defer dest.Close()

	written, err := io.Copy(dest, src)
	if err != nil {
		return nil, fileError(err)
	}

	return map[string]int64{"written": written}, nil
}

// ExistsParams represents parameters for fs/exists
type ExistsParams struct {
	Path string `json:"path"`
}

// Exists checks if a path exists
func (h *Handler) Exists(params json.RawMessage) (interface{}, error) {
	var p ExistsParams
	if err := json.Unmarshal(params, &p); err != nil {
		return nil, &RPCError{Code: InvalidParams, Message: "Invalid params", Data: err.Error()}
	}

	_, err := os.Stat(p.Path)
	exists := !os.IsNotExist(err)

	return map[string]bool{"exists": exists}, nil
}

// RealpathParams represents parameters for fs/realpath
type RealpathParams struct {
	Path string `json:"path"`
}

// Realpath resolves a path to its canonical form
func (h *Handler) Realpath(params json.RawMessage) (interface{}, error) {
	var p RealpathParams
	if err := json.Unmarshal(params, &p); err != nil {
		return nil, &RPCError{Code: InvalidParams, Message: "Invalid params", Data: err.Error()}
	}

	resolved, err := filepath.EvalSymlinks(p.Path)
	if err != nil {
		return nil, fileError(err)
	}

	abs, err := filepath.Abs(resolved)
	if err != nil {
		return nil, fileError(err)
	}

	return map[string]string{"path": abs}, nil
}

// InfoParams represents parameters for fs/info
type InfoParams struct {
	Path string `json:"path"`
}

// CacheEntry represents a single cache entry with path and info
type CacheEntry struct {
	Path string      `json:"path"`
	Info *InfoResult `json:"info"`
}

// InfoResponse wraps the result with cache entries for unified caching
type InfoResponse struct {
	*InfoResult
	Cache []CacheEntry `json:"cache,omitempty"`
}

// InfoResult represents the combined result of fs/info
// This is the unified response for exists + stat + realpath + content
type InfoResult struct {
	Exists   bool   `json:"exists"`
	Path     string `json:"path"`               // Original path
	Realpath string `json:"realpath,omitempty"` // Resolved canonical path
	// Stat info (only if exists)
	Size   int64  `json:"size,omitempty"`
	Mtime  int64  `json:"mtime,omitempty"`
	Atime  int64  `json:"atime,omitempty"`
	Mode   uint32 `json:"mode,omitempty"`
	Type   string `json:"type,omitempty"` // "file", "directory", "symlink", "other"
	IsDir  bool   `json:"isDir,omitempty"`
	Uid    uint32 `json:"uid,omitempty"`
	Gid    uint32 `json:"gid,omitempty"`
	User   string `json:"user,omitempty"`
	Group  string `json:"group,omitempty"`
	Nlink  uint64 `json:"nlink,omitempty"`
	Target string `json:"target,omitempty"` // Symlink target
	// Content (only for files < 1MB, raw binary)
	// nil = content not included
	Content []byte `json:"content,omitempty"`
	// Directory children (only for directories) - name and isDir for each entry
	// Individual child entries with full stat are sent via :cache for caching by full path
	Children *[]DirChild `json:"children,omitempty"` // nil = no data, empty = empty dir
	// ParentWatched indicates if the parent directory is being watched
	// (for non-existent files, enables safe negative caching)
	ParentWatched bool `json:"parentWatched,omitempty"`
	// NoCache indicates the client should not cache this result
	// (e.g., volatile paths like /tmp, /dev, /proc, /sys)
	NoCache bool `json:"noCache,omitempty"`
}

const maxContentSize = 1024 * 1024      // 1MB
const maxEntriesForContentPrefetch = 10 // Only prefetch content for directories with <= this many entries

// BuildCacheEntries creates a flat list of cache entries from an InfoResult
func BuildCacheEntries(info *InfoResult) []CacheEntry {
	if info == nil {
		return nil
	}

	entries := []CacheEntry{{Path: info.Path, Info: info}}

	// Add realpath as separate entry if different
	if info.Realpath != "" && info.Realpath != info.Path {
		entries = append(entries, CacheEntry{Path: info.Realpath, Info: info})
	}

	return entries
}

// Info returns combined file information: exists, stat, realpath, and content
func (h *Handler) Info(params json.RawMessage) (interface{}, error) {
	var p InfoParams
	if err := json.Unmarshal(params, &p); err != nil {
		return nil, &RPCError{Code: InvalidParams, Message: "Invalid params", Data: err.Error()}
	}

	slog.Info("fs/info", "path", p.Path)

	info, err := h.getInfo(p.Path)
	if err != nil {
		return nil, err
	}

	return &InfoResponse{
		InfoResult: info,
		Cache:      BuildCacheEntries(info),
	}, nil
}

// getInfo is the internal implementation that can be reused
func (h *Handler) getInfo(path string) (*InfoResult, error) {
	result := &InfoResult{
		Path: path,
	}

	// Check if file exists using Lstat (don't follow symlinks for type detection)
	info, err := os.Lstat(path)
	if err != nil {
		if os.IsNotExist(err) {
			result.Exists = false
			// For non-existent files, compute would-be realpath from parent
			parentDir := filepath.Dir(path)
			baseName := filepath.Base(path)
			if resolved, err := filepath.EvalSymlinks(parentDir); err == nil {
				if abs, err := filepath.Abs(resolved); err == nil {
					result.Realpath = filepath.Join(abs, baseName)
				}
			}
			return result, nil
		}
		return nil, fileError(err)
	}

	result.Exists = true
	populateStatFromFileInfo(result, path, info)

	// Get realpath
	if resolved, err := filepath.EvalSymlinks(path); err == nil {
		if abs, err := filepath.Abs(resolved); err == nil {
			result.Realpath = abs
		}
	}

	// For regular files < 1MB, include content
	if result.Type == "file" && result.Size <= maxContentSize {
		if content, err := os.ReadFile(path); err == nil {
			result.Content = content
			// Re-stat after reading content to catch metadata updates that may have
			// propagated after the initial Lstat. This handles timing issues on
			// filesystems like EdenFS where fsnotify events may fire before metadata
			// is fully updated.
			if info2, err := os.Lstat(path); err == nil {
				newMtime := info2.ModTime().Unix()
				if newMtime != result.Mtime {
					slog.Debug("getInfo: mtime updated after read", "path", path, "old", result.Mtime, "new", newMtime)
					result.Mtime = newMtime
					result.Size = info2.Size()
				}
			}
		}
	}

	// For directories, include child names and isDir (symlinks need stat)
	// Full child entries will be sent separately via :cache in async notifications
	if result.Type == "directory" || (result.Type == "symlink" && result.IsDir) {
		if children, err := h.readDirWithTypes(path); err == nil {
			result.Children = &children
		}
	}

	return result, nil
}

// getInfoNoContent returns info without file content (for prefetching)
func (h *Handler) getInfoNoContent(path string) (*InfoResult, error) {
	result := &InfoResult{
		Path: path,
	}

	info, err := os.Lstat(path)
	if err != nil {
		if os.IsNotExist(err) {
			result.Exists = false
			return result, nil
		}
		return nil, err
	}

	result.Exists = true
	populateStatFromFileInfo(result, path, info)

	// Get realpath
	if resolved, err := filepath.EvalSymlinks(path); err == nil {
		if abs, err := filepath.Abs(resolved); err == nil {
			result.Realpath = abs
		}
	}

	// No content fetching, no directory entry listing
	return result, nil
}

// GetInfo is exported for use by the watcher
func (h *Handler) GetInfo(path string) (*InfoResult, error) {
	return h.getInfo(path)
}

// GetInfoBasic returns info without file content or directory entries (for watch notifications)
func (h *Handler) GetInfoBasic(path string) (*InfoResult, error) {
	return h.getInfoNoContent(path)
}

// GetInfoWithEntries returns info with directory entries but without entryInfo
// (entryInfo can be fetched async and sent as notification)
func (h *Handler) GetInfoWithEntries(path string) (*InfoResult, error) {
	result := &InfoResult{
		Path: path,
	}

	// Check if file exists using Lstat (don't follow symlinks for type detection)
	info, err := os.Lstat(path)
	if err != nil {
		if os.IsNotExist(err) {
			result.Exists = false
			// For non-existent files, compute would-be realpath from parent
			parentDir := filepath.Dir(path)
			baseName := filepath.Base(path)
			if resolved, err := filepath.EvalSymlinks(parentDir); err == nil {
				if abs, err := filepath.Abs(resolved); err == nil {
					result.Realpath = filepath.Join(abs, baseName)
				}
			}
			return result, nil
		}
		return nil, fileError(err)
	}

	result.Exists = true
	populateStatFromFileInfo(result, path, info)

	// Get realpath
	if resolved, err := filepath.EvalSymlinks(path); err == nil {
		if abs, err := filepath.Abs(resolved); err == nil {
			result.Realpath = abs
		}
	}

	// For regular files < 1MB, include content
	if result.Type == "file" && result.Size <= maxContentSize {
		if content, err := os.ReadFile(path); err == nil {
			result.Content = content
			// Re-stat after reading content
			if info2, err := os.Lstat(path); err == nil {
				newMtime := info2.ModTime().Unix()
				if newMtime != result.Mtime {
					result.Mtime = newMtime
					result.Size = info2.Size()
				}
			}
		}
	}

	// For directories, include child names and isDir (symlinks need stat)
	// Full child entries will be sent via :cache in async notification
	if result.Type == "directory" || (result.Type == "symlink" && result.IsDir) {
		if children, err := h.readDirWithTypes(path); err == nil {
			result.Children = &children
		}
	}

	return result, nil
}

// GetChildEntries fetches full info for each child in parallel
// Returns cache entries that can be sent as async notification
// Skips directories with > maxEntries entries
// For small directories (≤ maxEntriesForContentPrefetch), includes file content
const (
	MaxEntriesDefault  = 1000  // Default limit for implicit directory fetches
	MaxEntriesExplicit = 10000 // Higher limit for explicit list operations (find-file, dired)
)

func (h *Handler) GetChildEntries(dirPath string, children []DirChild, maxEntries int) []CacheEntry {
	if len(children) == 0 || len(children) > maxEntries {
		return nil
	}

	// Include content for small directories
	includeContent := len(children) <= maxEntriesForContentPrefetch

	results := make([]*InfoResult, len(children))
	bwg := util.NewBoundedWaitGroup(32)

	for i, child := range children {
		bwg.Add(1)
		go func(idx int, childName string) {
			defer bwg.Done()
			childPath := filepath.Join(dirPath, childName)
			var info *InfoResult
			var err error
			if includeContent {
				// Get full info including content for files
				info, err = h.getInfo(childPath)
			} else {
				// Get info without content (to keep payloads small)
				info, err = h.getInfoNoContent(childPath)
			}
			if err == nil {
				results[idx] = info
			}
		}(i, child.Name)
	}

	bwg.Wait()

	// Build cache entries from results
	var cacheEntries []CacheEntry
	for _, info := range results {
		if info != nil {
			cacheEntries = append(cacheEntries, CacheEntry{Path: info.Path, Info: info})
			if info.Realpath != "" && info.Realpath != info.Path {
				cacheEntries = append(cacheEntries, CacheEntry{Path: info.Realpath, Info: info})
			}
		}
	}
	return cacheEntries
}

// BatchParams represents parameters for fs/batch
type BatchParams struct {
	Paths []string `json:"paths"`
}

// BatchResult represents the result of fs/batch with unified cache format
type BatchResult struct {
	Cache  []CacheEntry      `json:"cache"`
	Errors map[string]string `json:"errors,omitempty"`
}

// Batch fetches multiple files and directories in parallel
// For files: returns full info including content
// For directories: returns info with entries
// Also fetches parent directories of all specified files
func (h *Handler) Batch(params json.RawMessage) (interface{}, error) {
	var p BatchParams
	if err := json.Unmarshal(params, &p); err != nil {
		return nil, &RPCError{Code: InvalidParams, Message: "Invalid params", Data: err.Error()}
	}

	slog.Info("fs/batch", "count", len(p.Paths))

	if len(p.Paths) == 0 {
		return &BatchResult{
			Cache: []CacheEntry{},
		}, nil
	}

	// Collect all paths to fetch: original paths + parent directories
	// This is pure path manipulation, no I/O
	allPaths := make(map[string]bool)
	for _, path := range p.Paths {
		allPaths[path] = true
		allPaths[filepath.Dir(path)] = true
	}

	// Fetch all paths in parallel - getInfo handles files and directories correctly
	var cache []CacheEntry
	errors := make(map[string]string)
	var mu sync.Mutex
	bwg := util.NewBoundedWaitGroup(32)

	for path := range allPaths {
		bwg.Add(1)
		go func(p string) {
			defer bwg.Done()
			info, err := h.getInfo(p)
			mu.Lock()
			defer mu.Unlock()
			if err != nil {
				errors[p] = err.Error()
			} else {
				// Add all cache entries (includes realpath and entryInfo)
				cache = append(cache, BuildCacheEntries(info)...)
			}
		}(path)
	}

	bwg.Wait()

	result := &BatchResult{Cache: cache}
	if len(errors) > 0 {
		result.Errors = errors
	}

	return result, nil
}

// fileError converts an OS error to a JSON-RPC error
func fileError(err error) *RPCError {
	if os.IsNotExist(err) {
		return &RPCError{Code: FileNotFound, Message: "File not found", Data: err.Error()}
	}
	if os.IsPermission(err) {
		return &RPCError{Code: PermissionErr, Message: "Permission denied", Data: err.Error()}
	}
	if os.IsExist(err) {
		return &RPCError{Code: FileExists, Message: "File exists", Data: err.Error()}
	}
	return &RPCError{Code: InternalError, Message: fmt.Sprintf("File operation failed: %v", err)}
}

// ChmodParams represents parameters for fs/chmod
type ChmodParams struct {
	Path string `json:"path"`
	Mode uint32 `json:"mode"`
}

// Chmod changes file permissions
func (h *Handler) Chmod(params json.RawMessage) (interface{}, error) {
	var p ChmodParams
	if err := json.Unmarshal(params, &p); err != nil {
		return nil, &RPCError{Code: InvalidParams, Message: "Invalid params", Data: err.Error()}
	}

	slog.Info("fs/chmod", "path", p.Path, "mode", p.Mode)

	if err := os.Chmod(p.Path, os.FileMode(p.Mode)); err != nil {
		return nil, fileError(err)
	}

	return map[string]bool{"ok": true}, nil
}

// TouchParams represents parameters for fs/touch
type TouchParams struct {
	Path  string `json:"path"`
	Mtime int64  `json:"mtime,omitempty"` // Unix timestamp, 0 means current time
	Atime int64  `json:"atime,omitempty"` // Unix timestamp, 0 means current time
}

// Touch updates file access and modification times
func (h *Handler) Touch(params json.RawMessage) (interface{}, error) {
	var p TouchParams
	if err := json.Unmarshal(params, &p); err != nil {
		return nil, &RPCError{Code: InvalidParams, Message: "Invalid params", Data: err.Error()}
	}

	slog.Info("fs/touch", "path", p.Path, "mtime", p.Mtime, "atime", p.Atime)

	// Determine times to set
	now := time.Now()
	atime := now
	mtime := now
	if p.Atime != 0 {
		atime = time.Unix(p.Atime, 0)
	}
	if p.Mtime != 0 {
		mtime = time.Unix(p.Mtime, 0)
	}

	if err := os.Chtimes(p.Path, atime, mtime); err != nil {
		return nil, fileError(err)
	}

	return map[string]bool{"ok": true}, nil
}

// CopyDirParams represents parameters for fs/copy-dir
type CopyDirParams struct {
	Src           string `json:"src"`
	Dest          string `json:"dest"`
	KeepTime      bool   `json:"keepTime,omitempty"`
	PreserveLinks bool   `json:"preserveLinks,omitempty"`
}

// CopyDirResult contains statistics about the copy operation
type CopyDirResult struct {
	FilesCopied int64 `json:"filesCopied"`
	DirsCopied  int64 `json:"dirsCopied"`
	BytesCopied int64 `json:"bytesCopied"`
}

// CopyDir recursively copies a directory
func (h *Handler) CopyDir(params json.RawMessage) (interface{}, error) {
	var p CopyDirParams
	if err := json.Unmarshal(params, &p); err != nil {
		return nil, &RPCError{Code: InvalidParams, Message: "Invalid params", Data: err.Error()}
	}

	slog.Info("fs/copy-dir", "src", p.Src, "dest", p.Dest)

	// Security: prevent recursive copy
	srcAbs, err := filepath.Abs(p.Src)
	if err != nil {
		return nil, fileError(err)
	}
	destAbs, err := filepath.Abs(p.Dest)
	if err != nil {
		return nil, fileError(err)
	}
	if strings.HasPrefix(destAbs, srcAbs) {
		return nil, &RPCError{Code: InvalidParams, Message: "Recursive copy: destination is inside source"}
	}

	result := &CopyDirResult{}

	err = filepath.Walk(p.Src, func(path string, info os.FileInfo, err error) error {
		if err != nil {
			return err
		}

		// Compute destination path
		relPath, err := filepath.Rel(p.Src, path)
		if err != nil {
			return err
		}
		destPath := filepath.Join(p.Dest, relPath)

		// Handle symlinks
		if info.Mode()&os.ModeSymlink != 0 {
			if p.PreserveLinks {
				target, err := os.Readlink(path)
				if err != nil {
					return err
				}
				return os.Symlink(target, destPath)
			}
			// Follow symlink - get the target info
			info, err = os.Stat(path)
			if err != nil {
				return err
			}
		}

		if info.IsDir() {
			// Create directory with same permissions
			if err := os.MkdirAll(destPath, info.Mode()); err != nil {
				return err
			}
			result.DirsCopied++
			return nil
		}

		// Copy file
		src, err := os.Open(path)
		if err != nil {
			return err
		}
		defer src.Close()

		dest, err := os.OpenFile(destPath, os.O_WRONLY|os.O_CREATE|os.O_TRUNC, info.Mode())
		if err != nil {
			return err
		}
		defer dest.Close()

		written, err := io.Copy(dest, src)
		if err != nil {
			return err
		}

		result.FilesCopied++
		result.BytesCopied += written

		// Preserve times if requested
		if p.KeepTime {
			if err := os.Chtimes(destPath, info.ModTime(), info.ModTime()); err != nil {
				return err
			}
		}

		return nil
	})

	if err != nil {
		return nil, fileError(err)
	}

	return result, nil
}
