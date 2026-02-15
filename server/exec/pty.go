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

package exec

import (
	"fmt"
	"io"
	"os"
	"os/exec"
	"sync"
	"syscall"

	"github.com/creack/pty"
)

// PTY represents a running PTY process
type PTY struct {
	ID     string
	cmd    *exec.Cmd
	pty    *os.File
	closed bool
	mu     sync.Mutex
}

// PTYManager manages running PTY sessions
type PTYManager struct {
	ptys     map[string]*PTY
	mu       sync.RWMutex
	nextID   int
	onOutput OutputFunc // Reuse OutputFunc from handler.go
	onExit   ExitFunc   // Reuse ExitFunc from handler.go
}

// NewPTYManager creates a new PTY manager
func NewPTYManager(onOutput OutputFunc, onExit ExitFunc) *PTYManager {
	return &PTYManager{
		ptys:     make(map[string]*PTY),
		onOutput: onOutput,
		onExit:   onExit,
	}
}

// CreateParams contains parameters for creating a PTY
type CreateParams struct {
	Cmd  string            `json:"cmd"`
	Args []string          `json:"args"`
	Cwd  string            `json:"cwd"`
	Env  map[string]string `json:"env"`
	Rows uint16            `json:"rows"`
	Cols uint16            `json:"cols"`
}

// CreateResult contains the result of creating a PTY
type CreateResult struct {
	PtyID string `json:"ptyId"`
}

// Create creates a new PTY session
func (m *PTYManager) Create(params CreateParams) (*CreateResult, error) {
	cmd := exec.Command(params.Cmd, params.Args...)

	if params.Cwd != "" {
		cmd.Dir = params.Cwd
	}

	// Set up environment
	env := os.Environ()
	if params.Env != nil {
		for k, v := range params.Env {
			env = append(env, k+"="+v)
		}
	}
	// Ensure TERM is set
	hasTerm := false
	for _, e := range env {
		if len(e) >= 5 && e[:5] == "TERM=" {
			hasTerm = true
			break
		}
	}
	if !hasTerm {
		env = append(env, "TERM=xterm-256color")
	}
	cmd.Env = env

	// Set initial window size
	winSize := &pty.Winsize{
		Rows: params.Rows,
		Cols: params.Cols,
	}
	if winSize.Rows == 0 {
		winSize.Rows = 24
	}
	if winSize.Cols == 0 {
		winSize.Cols = 80
	}

	// Start command with PTY
	ptmx, err := pty.StartWithSize(cmd, winSize)
	if err != nil {
		return nil, fmt.Errorf("failed to start PTY: %w", err)
	}

	// Generate PTY ID
	m.mu.Lock()
	m.nextID++
	ptyID := fmt.Sprintf("pty-%d", m.nextID)
	p := &PTY{
		ID:  ptyID,
		cmd: cmd,
		pty: ptmx,
	}
	m.ptys[ptyID] = p
	m.mu.Unlock()

	// Start goroutine to read output
	go m.readOutput(ptyID, ptmx)

	// Start goroutine to wait for exit
	go m.waitForExit(ptyID, cmd, ptmx)

	return &CreateResult{PtyID: ptyID}, nil
}

// readOutput reads from the PTY and sends output notifications
func (m *PTYManager) readOutput(ptyID string, r io.Reader) {
	buf := make([]byte, 4096)
	for {
		n, err := r.Read(buf)
		if n > 0 && m.onOutput != nil {
			// For PTY, we use "stdout" as the stream name
			m.onOutput(ptyID, "stdout", string(buf[:n]))
		}
		if err != nil {
			break
		}
	}
}

// waitForExit waits for the PTY process to exit
func (m *PTYManager) waitForExit(ptyID string, cmd *exec.Cmd, ptmx *os.File) {
	err := cmd.Wait()

	exitCode := 0
	if err != nil {
		if exitErr, ok := err.(*exec.ExitError); ok {
			if status, ok := exitErr.Sys().(syscall.WaitStatus); ok {
				exitCode = status.ExitStatus()
			} else {
				exitCode = 1
			}
		} else {
			exitCode = -1
		}
	}

	// Close PTY
	ptmx.Close()

	// Remove from active PTYs
	m.mu.Lock()
	if p, exists := m.ptys[ptyID]; exists {
		p.mu.Lock()
		p.closed = true
		p.mu.Unlock()
		delete(m.ptys, ptyID)
	}
	m.mu.Unlock()

	// Send exit notification
	if m.onExit != nil {
		m.onExit(ptyID, exitCode)
	}
}

// PTYInputParams contains parameters for sending input to a PTY
type PTYInputParams struct {
	PtyID string `json:"ptyId"`
	Data  string `json:"data"`
}

// Input sends data to a PTY's stdin
func (m *PTYManager) Input(params PTYInputParams) error {
	m.mu.RLock()
	p, exists := m.ptys[params.PtyID]
	m.mu.RUnlock()

	if !exists {
		return fmt.Errorf("PTY not found: %s", params.PtyID)
	}

	p.mu.Lock()
	defer p.mu.Unlock()

	if p.closed {
		return fmt.Errorf("PTY is closed: %s", params.PtyID)
	}

	_, err := p.pty.Write([]byte(params.Data))
	return err
}

// PTYResizeParams contains parameters for resizing a PTY
type PTYResizeParams struct {
	PtyID string `json:"ptyId"`
	Rows  uint16 `json:"rows"`
	Cols  uint16 `json:"cols"`
}

// Resize changes the window size of a PTY
func (m *PTYManager) Resize(params PTYResizeParams) error {
	m.mu.RLock()
	p, exists := m.ptys[params.PtyID]
	m.mu.RUnlock()

	if !exists {
		return fmt.Errorf("PTY not found: %s", params.PtyID)
	}

	p.mu.Lock()
	defer p.mu.Unlock()

	if p.closed {
		return fmt.Errorf("PTY is closed: %s", params.PtyID)
	}

	return pty.Setsize(p.pty, &pty.Winsize{
		Rows: params.Rows,
		Cols: params.Cols,
	})
}

// PTYSignalParams contains parameters for sending a signal to a PTY
type PTYSignalParams struct {
	PtyID  string `json:"ptyId"`
	Signal string `json:"signal"`
}

// Signal sends a signal to a PTY process
func (m *PTYManager) Signal(params PTYSignalParams) error {
	m.mu.RLock()
	p, exists := m.ptys[params.PtyID]
	m.mu.RUnlock()

	if !exists {
		return fmt.Errorf("PTY not found: %s", params.PtyID)
	}

	var sig syscall.Signal
	switch params.Signal {
	case "kill":
		sig = syscall.SIGKILL
	case "interrupt", "int":
		sig = syscall.SIGINT
	case "term", "terminate":
		sig = syscall.SIGTERM
	case "hup", "hangup":
		sig = syscall.SIGHUP
	default:
		return fmt.Errorf("unknown signal: %s", params.Signal)
	}

	return p.cmd.Process.Signal(sig)
}

// Close terminates all running PTY sessions
func (m *PTYManager) Close() {
	m.mu.Lock()
	defer m.mu.Unlock()

	for _, p := range m.ptys {
		p.mu.Lock()
		if !p.closed {
			p.cmd.Process.Kill()
			p.pty.Close()
			p.closed = true
		}
		p.mu.Unlock()
	}
	m.ptys = make(map[string]*PTY)
}
