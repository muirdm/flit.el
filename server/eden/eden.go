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

package eden

import (
	"bufio"
	"encoding/json"
	"log/slog"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"sync"
)

// CheckoutCallback is called when a checkout (CommitTransition) is detected.
// The root parameter is the Eden mount root where the checkout occurred.
type CheckoutCallback func(root string)

// Watcher monitors Eden repositories for checkout events.
type Watcher struct {
	callback CheckoutCallback
	logger   *slog.Logger
	mu       sync.Mutex
	roots    map[string]*rootWatcher // root path -> watcher
}

type rootWatcher struct {
	root   string
	cmd    *exec.Cmd
	cancel chan struct{}
}

// NewWatcher creates a new Eden watcher.
func NewWatcher(callback CheckoutCallback, logger *slog.Logger) *Watcher {
	return &Watcher{
		callback: callback,
		logger:   logger,
		roots:    make(map[string]*rootWatcher),
	}
}

// GetEdenRoot checks if a path is inside an Eden repo and returns the root.
// Returns empty string if not in an Eden repo.
func GetEdenRoot(path string) string {
	// Walk up the directory tree looking for .eden
	dir := path
	if info, err := os.Stat(path); err == nil && !info.IsDir() {
		dir = filepath.Dir(path)
	}

	for {
		edenDir := filepath.Join(dir, ".eden")
		if info, err := os.Stat(edenDir); err == nil && info.IsDir() {
			// Found .eden directory, read the root symlink
			rootLink := filepath.Join(edenDir, "root")
			if target, err := os.Readlink(rootLink); err == nil {
				return target
			}
			// If we can't read the symlink, use the parent of .eden
			return dir
		}

		parent := filepath.Dir(dir)
		if parent == dir {
			// Reached filesystem root
			return ""
		}
		dir = parent
	}
}

// EnsureWatching starts watching an Eden root if not already watching.
func (w *Watcher) EnsureWatching(root string) {
	w.mu.Lock()
	defer w.mu.Unlock()

	if _, exists := w.roots[root]; exists {
		return
	}

	rw := &rootWatcher{
		root:   root,
		cancel: make(chan struct{}),
	}
	w.roots[root] = rw

	go w.watchRoot(rw)
}

// Stop stops all Eden watchers.
func (w *Watcher) Stop() {
	w.mu.Lock()
	defer w.mu.Unlock()

	for _, rw := range w.roots {
		close(rw.cancel)
		if rw.cmd != nil && rw.cmd.Process != nil {
			rw.cmd.Process.Kill()
		}
	}
	w.roots = make(map[string]*rootWatcher)
}

func (w *Watcher) watchRoot(rw *rootWatcher) {
	w.logger.Info("Subscribing to Eden notifications", "root", rw.root)

	for {
		select {
		case <-rw.cancel:
			return
		default:
		}

		cmd := exec.Command("eden", "notify", "changes-since", "--subscribe", "--json", "--deferred-states", "hg.transaction")
		cmd.Dir = rw.root

		stdout, err := cmd.StdoutPipe()
		if err != nil {
			w.logger.Error("Failed to create stdout pipe for eden", "root", rw.root, "error", err)
			return
		}

		if err := cmd.Start(); err != nil {
			w.logger.Error("Failed to start eden notify", "root", rw.root, "error", err)
			return
		}

		rw.cmd = cmd

		scanner := bufio.NewScanner(stdout)
		// Increase buffer size for potentially large JSON lines
		scanner.Buffer(make([]byte, 1024*1024), 1024*1024)

		for scanner.Scan() {
			select {
			case <-rw.cancel:
				cmd.Process.Kill()
				return
			default:
			}

			line := scanner.Text()
			if w.hasCommitTransition(line) {
				w.logger.Info("Eden checkout detected", "root", rw.root)
				w.callback(rw.root)
			}
		}

		if err := scanner.Err(); err != nil {
			w.logger.Error("Eden scanner error", "root", rw.root, "error", err)
		}

		cmd.Wait()

		// Check if we should stop
		select {
		case <-rw.cancel:
			return
		default:
			w.logger.Info("Eden notify exited, reconnecting", "root", rw.root)
		}
	}
}

// hasCommitTransition checks if a JSON line contains a CommitTransition event.
func (w *Watcher) hasCommitTransition(line string) bool {
	// Quick check before parsing JSON
	if !strings.Contains(line, "CommitTransition") {
		return false
	}

	// Parse to verify it's a valid CommitTransition
	var msg struct {
		Changes []json.RawMessage `json:"changes"`
	}
	if err := json.Unmarshal([]byte(line), &msg); err != nil {
		return false
	}

	for _, change := range msg.Changes {
		var largeChange struct {
			LargeChange struct {
				CommitTransition json.RawMessage `json:"CommitTransition"`
			} `json:"LargeChange"`
		}
		if err := json.Unmarshal(change, &largeChange); err == nil {
			if largeChange.LargeChange.CommitTransition != nil {
				return true
			}
		}
	}

	return false
}
