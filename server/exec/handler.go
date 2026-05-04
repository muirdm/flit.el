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
	"context"

	"fmt"
	"io"
	"log/slog"
	"os"
	"os/exec"
	"sync"
	"syscall"
	"time"

	"github.com/creack/pty"
)

// OutputFunc is called when a process produces output
type OutputFunc func(procID string, stream string, data []byte)

// ExitFunc is called when a process exits
type ExitFunc func(procID string, exitCode int)

// Process represents a running async process
type Process struct {
	ID       string
	cmd      *exec.Cmd
	cmdName  string // command name for logging
	stdin    io.WriteCloser
	ptyFile  *os.File // master PTY fd, non-nil if running in PTY mode
	ptySlave *os.File // slave PTY fd, for flow control ioctls
	cancel   context.CancelFunc
	mu       sync.Mutex

	// Async input: inputs are queued to inputCh and written by a dedicated
	// goroutine, so a blocked stdin write doesn't block the RPC server.
	inputCh        chan string
	inputClosed    bool // protected by mu, prevents send to closed channel
	inputCloseOnce sync.Once

	// Input ordering: buffer out-of-order inputs and write in sequence
	inputExpectedIdx int64            // next expected input index
	inputBuffer      map[int64]string // buffered out-of-order inputs
	inputBufferSize  int              // total bytes buffered
}

const (
	// Maximum bytes to buffer for out-of-order inputs per process
	maxInputBufferSize = 10 * 1024 * 1024 // 10 MB
	// Maximum number of out-of-order inputs to buffer
	maxInputBufferCount = 1000
)

// Manager manages running async processes
type Manager struct {
	processes map[string]*Process
	mu        sync.RWMutex
	nextID    int
	onOutput  OutputFunc
	onExit    ExitFunc
	logger    *slog.Logger
}

// NewManager creates a new process manager
func NewManager(onOutput OutputFunc, onExit ExitFunc, logger *slog.Logger) *Manager {
	return &Manager{
		processes: make(map[string]*Process),
		onOutput:  onOutput,
		onExit:    onExit,
		logger:    logger,
	}
}

// SetLogger updates the logger used for warnings (e.g. when the RPC logger becomes available).
func (m *Manager) SetLogger(logger *slog.Logger) {
	m.logger = logger
}

// StartParams contains parameters for starting an async process
type StartParams struct {
	ProcID string            `json:"procId"` // Client-provided ID (optional, server generates if empty)
	Cmd    string            `json:"cmd"`
	Args   []string          `json:"args"`
	Cwd    string            `json:"cwd"`
	Env    map[string]string `json:"env"`
	Pty    bool              `json:"pty"`  // If true, allocate a PTY
	Rows   uint16            `json:"rows"` // PTY rows (default 24)
	Cols   uint16            `json:"cols"` // PTY cols (default 80)
}

// StartResult contains the result of starting a process
type StartResult struct {
	ProcID string `json:"procId"`
}

// Start starts an async process and returns its ID
func (m *Manager) Start(params StartParams) (*StartResult, error) {
	ctx, cancel := context.WithCancel(context.Background())
	cmd := exec.CommandContext(ctx, params.Cmd, params.Args...)

	if params.Cwd != "" {
		cmd.Dir = params.Cwd
	}

	// Set up environment
	env := cmd.Environ()
	if params.Env != nil {
		for k, v := range params.Env {
			env = append(env, k+"="+v)
		}
	}

	// Use client-provided ID or generate one
	var procID string
	if params.ProcID != "" {
		procID = params.ProcID
	} else {
		m.mu.Lock()
		m.nextID++
		procID = fmt.Sprintf("proc-%d", m.nextID)
		m.mu.Unlock()
	}

	var proc *Process

	if params.Pty {
		// PTY mode: allocate a pseudo-terminal
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
			cancel()
			return nil, fmt.Errorf("failed to start process with PTY: %w", err)
		}

		// Open slave side for flow control ioctls (TCOOFF/TCOON)
		slavePath, err := ptsname(ptmx)
		if err != nil {
			ptmx.Close()
			cancel()
			return nil, fmt.Errorf("failed to get slave PTY name: %w", err)
		}
		ptySlave, err := os.OpenFile(slavePath, os.O_RDWR|syscall.O_NOCTTY, 0)
		if err != nil {
			ptmx.Close()
			cancel()
			return nil, fmt.Errorf("failed to open slave PTY %s: %w", slavePath, err)
		}

		proc = &Process{
			ID:       procID,
			cmd:      cmd,
			cmdName:  params.Cmd,
			ptyFile:  ptmx,
			ptySlave: ptySlave,
			cancel: func() {
				cancel()
				ptmx.Close() // unblock any blocked write/read on the PTY
			},
			inputCh: make(chan string, 1024),
		}
		go m.inputWriter(proc)

		// Read PTY output (combined stdout/stderr)
		var wg sync.WaitGroup
		wg.Add(1)
		go func() {
			m.readOutput(procID, "stdout", ptmx)
			wg.Done()
		}()

		// Wait for process to exit in background
		go m.waitForExitWithWg(procID, cmd, &wg)
	} else {
		// Pipe mode: use separate stdin/stdout/stderr pipes
		cmd.Env = env

		stdin, err := cmd.StdinPipe()
		if err != nil {
			cancel()
			return nil, fmt.Errorf("failed to create stdin pipe: %w", err)
		}

		stdout, err := cmd.StdoutPipe()
		if err != nil {
			cancel()
			return nil, fmt.Errorf("failed to create stdout pipe: %w", err)
		}

		stderr, err := cmd.StderrPipe()
		if err != nil {
			cancel()
			return nil, fmt.Errorf("failed to create stderr pipe: %w", err)
		}

		// Start the process
		if err := cmd.Start(); err != nil {
			cancel()
			return nil, fmt.Errorf("failed to start process: %w", err)
		}

		proc = &Process{
			ID:      procID,
			cmd:     cmd,
			cmdName: params.Cmd,
			stdin:   stdin,
			cancel: func() {
				cancel()
				stdin.Close() // unblock any blocked write on stdin
			},
			inputCh: make(chan string, 1024),
		}
		go m.inputWriter(proc)

		// Start goroutines to read stdout/stderr
		var wg sync.WaitGroup
		wg.Add(2)
		go func() {
			m.readOutput(procID, "stdout", stdout)
			wg.Done()
		}()
		go func() {
			m.readOutput(procID, "stderr", stderr)
			wg.Done()
		}()

		// Wait for process to exit in background
		go m.waitForExitWithWg(procID, cmd, &wg)
	}

	// Store process
	m.mu.Lock()
	m.processes[procID] = proc
	m.mu.Unlock()

	slog.Info("exec/start", "cmd", params.Cmd, "args", params.Args, "procId", procID, "pty", params.Pty)

	return &StartResult{ProcID: procID}, nil
}

// readOutput reads from a pipe and sends output notifications.
func (m *Manager) readOutput(procID string, stream string, r io.Reader) {
	buf := make([]byte, 4096)
	for {
		n, err := r.Read(buf)
		if n > 0 && m.onOutput != nil {
			// Send raw bytes — msgpack encodes []byte as binary natively
			data := make([]byte, n)
			copy(data, buf[:n])
			m.onOutput(procID, stream, data)
		}
		if err != nil {
			break
		}
	}
}

// waitForExit waits for a process to exit and sends exit notification
func (m *Manager) waitForExit(procID string, cmd *exec.Cmd) {
	m.waitForExitWithWg(procID, cmd, nil)
}

// waitForExitWithWg waits for a process to exit, optionally waits for output
// goroutines to complete, then sends exit notification
func (m *Manager) waitForExitWithWg(procID string, cmd *exec.Cmd, wg *sync.WaitGroup) {
	err := cmd.Wait()

	// Close slave PTY fd so the master read returns EIO.
	// Must happen before wg.Wait() - otherwise the read goroutine
	// blocks forever because the kernel keeps the PTY open.
	m.mu.RLock()
	proc := m.processes[procID]
	m.mu.RUnlock()
	if proc != nil && proc.ptySlave != nil {
		proc.ptySlave.Close()
	}

	// Close input channel so the writer goroutine exits
	if proc != nil {
		proc.closeInputCh()
	}

	// Wait for output goroutines to finish sending all data before
	// sending exit notification - prevents race where exit arrives
	// before final output
	if wg != nil {
		wg.Wait()
	}

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

	// Remove from active processes
	m.mu.Lock()
	delete(m.processes, procID)
	m.mu.Unlock()

	// Send exit notification
	if m.onExit != nil {
		m.onExit(procID, exitCode)
	}
}

// InputParams contains parameters for sending input to a process
type InputParams struct {
	ProcID string `json:"procId"`
	Data   string `json:"data"`
	Idx    int64  `json:"idx"` // Sequence number for ordering (0-based)
}

// Input sends data to a process's stdin (or PTY)
// Inputs are buffered and written in order based on Idx.
// Data is queued to a per-process channel and written by a dedicated goroutine,
// so a blocked stdin write never blocks the RPC server.
func (m *Manager) Input(params InputParams) error {
	m.mu.RLock()
	proc, exists := m.processes[params.ProcID]
	m.mu.RUnlock()

	if !exists {
		return fmt.Errorf("process not found: %s", params.ProcID)
	}

	proc.mu.Lock()
	defer proc.mu.Unlock()

	if proc.inputClosed {
		return nil // process exiting, discard silently
	}

	// Initialize buffer on first use
	if proc.inputBuffer == nil {
		proc.inputBuffer = make(map[int64]string)
	}

	// Check if this is the expected input
	if params.Idx == proc.inputExpectedIdx {
		// Queue this input and any buffered sequential ones
		m.queueInput(proc, params.Data)
		proc.inputExpectedIdx++

		// Drain any buffered inputs that are now in sequence
		for {
			if data, ok := proc.inputBuffer[proc.inputExpectedIdx]; ok {
				m.queueInput(proc, data)
				proc.inputBufferSize -= len(data)
				delete(proc.inputBuffer, proc.inputExpectedIdx)
				proc.inputExpectedIdx++
			} else {
				break
			}
		}
	} else if params.Idx > proc.inputExpectedIdx {
		// Future input - buffer it if we have room
		if len(proc.inputBuffer) >= maxInputBufferCount {
			return fmt.Errorf("input buffer count exceeded (%d pending)", len(proc.inputBuffer))
		}
		if proc.inputBufferSize+len(params.Data) > maxInputBufferSize {
			return fmt.Errorf("input buffer size exceeded (%d bytes pending)", proc.inputBufferSize)
		}
		proc.inputBuffer[params.Idx] = params.Data
		proc.inputBufferSize += len(params.Data)
		slog.Debug("exec/input: buffered out-of-order input",
			"procId", params.ProcID, "idx", params.Idx,
			"expected", proc.inputExpectedIdx, "buffered", len(proc.inputBuffer))
	}
	// else: params.Idx < proc.inputExpectedIdx - duplicate, ignore silently

	return nil
}

// queueInput sends data to the process's input channel for async writing.
// If the channel is full (writer goroutine blocked on stdin), waits up to 5s
// then kills the process — a corrupted input stream can't be recovered.
// Caller must hold proc.mu.
func (m *Manager) queueInput(proc *Process, data string) {
	select {
	case proc.inputCh <- data:
		return
	default:
	}

	// Channel full — give the process time to drain before killing it.
	timer := time.NewTimer(5 * time.Second)
	defer timer.Stop()
	select {
	case proc.inputCh <- data:
		return
	case <-timer.C:
		m.logger.Warn("exec/input: stdin write blocked for 5s, killing process",
			"procId", proc.ID, "cmd", proc.cmdName)
		proc.inputClosed = true // caller holds proc.mu
		proc.cancel()
	}
}

// inputWriter is a dedicated goroutine that reads from inputCh and writes
// to the process's stdin/PTY. This goroutine is the only writer to stdin,
// so a blocked write only blocks this goroutine, not the RPC server.
func (m *Manager) inputWriter(proc *Process) {
	for data := range proc.inputCh {
		if err := proc.writeInput(data); err != nil {
			slog.Debug("inputWriter: write failed", "procId", proc.ID, "error", err)
			return
		}
	}
	// Channel closed - close stdin to signal EOF to the process
	if proc.stdin != nil {
		proc.stdin.Close()
	}
}

// writeInput writes data to the process's stdin or PTY.
// Caller must hold proc.mu.
func (proc *Process) writeInput(data string) error {
	if proc.ptyFile != nil {
		_, err := proc.ptyFile.Write([]byte(data))
		return err
	}
	if proc.stdin != nil {
		_, err := proc.stdin.Write([]byte(data))
		return err
	}
	return fmt.Errorf("process has no input stream")
}

// SignalParams contains parameters for sending a signal to a process
type SignalParams struct {
	ProcID string `json:"procId"`
	Signal string `json:"signal"` // "kill", "interrupt", "term"
}

// Signal sends a signal to a process
func (m *Manager) Signal(params SignalParams) error {
	m.mu.RLock()
	proc, exists := m.processes[params.ProcID]
	m.mu.RUnlock()

	if !exists {
		return fmt.Errorf("process not found: %s", params.ProcID)
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

	return proc.cmd.Process.Signal(sig)
}

// closeInputCh closes the input channel, signaling the writer goroutine to
// drain remaining data and exit. Safe to call multiple times.
func (proc *Process) closeInputCh() {
	proc.mu.Lock()
	proc.inputClosed = true
	proc.mu.Unlock()
	proc.inputCloseOnce.Do(func() {
		close(proc.inputCh)
	})
}

// CloseInput closes a process's stdin
func (m *Manager) CloseInput(procID string) error {
	m.mu.RLock()
	proc, exists := m.processes[procID]
	m.mu.RUnlock()

	if !exists {
		return fmt.Errorf("process not found: %s", procID)
	}

	proc.closeInputCh()
	return nil
}

// PtyCtlParams contains parameters for PTY control operations.
type PtyCtlParams struct {
	ProcID string `json:"procId"`
	Op     string `json:"op"`   // "resize", "TCOOFF", "TCOON"
	Rows   uint16 `json:"rows"` // for resize
	Cols   uint16 `json:"cols"` // for resize
}

// PtyCtl performs a control operation on a PTY process.
// Supported ops:
//   - "resize": resize the PTY to Rows x Cols
//   - "TCOOFF": suspend output (tcflow TCOOFF)
//   - "TCOON":  resume output (tcflow TCOON)
func (m *Manager) PtyCtl(params PtyCtlParams) error {
	m.mu.RLock()
	proc, exists := m.processes[params.ProcID]
	m.mu.RUnlock()

	if !exists {
		return fmt.Errorf("process not found: %s", params.ProcID)
	}

	proc.mu.Lock()
	defer proc.mu.Unlock()

	if proc.ptyFile == nil {
		return fmt.Errorf("process is not a PTY: %s", params.ProcID)
	}

	switch params.Op {
	case "resize":
		return pty.Setsize(proc.ptyFile, &pty.Winsize{
			Rows: params.Rows,
			Cols: params.Cols,
		})
	case "TCOOFF":
		return suspendOutput(proc.ptySlave)
	case "TCOON":
		return resumeOutput(proc.ptySlave)
	default:
		return fmt.Errorf("unknown ptyctl op: %s", params.Op)
	}
}

// Close terminates all running processes
func (m *Manager) Close() {
	m.mu.Lock()
	defer m.mu.Unlock()

	for _, proc := range m.processes {
		proc.closeInputCh()
		if proc.ptySlave != nil {
			proc.ptySlave.Close()
		}
		proc.cancel()
	}
	m.processes = make(map[string]*Process)
}
