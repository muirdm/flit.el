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

package main

import (
	"bufio"
	"encoding/json"
	"flag"
	"fmt"
	"io"
	"log/slog"
	"os"
	"os/exec"
	"os/signal"
	"regexp"
	"strings"
	"syscall"
	"time"

	"github.com/creack/pty"
)

// pty-bridge bridges stdin/stdout to a command running in a PTY.
// This enables interactive authentication (password prompts) to be
// forwarded to the client via JSON messages.
//
// Password prompts are detected and sent to the client as:
//
//	{"pty_password_prompt": "Password: "}
//
// The client responds with:
//
//	{"pty_password": "secret"}
//
// Once the command outputs a line containing "flit_ready", the bridge
// switches to transparent passthrough mode.

func runPtyBridge(args []string) {
	fs := flag.NewFlagSet("pty-bridge", flag.ExitOnError)
	debug := fs.Bool("debug", false, "Enable debug logging")

	fs.Usage = func() {
		fmt.Fprintln(os.Stderr, `Usage: flit pty-bridge [options] -- <program> [args...]

Bridge stdin/stdout to a command running in a PTY.

This enables interactive authentication (password prompts) to be handled
by the client. Password prompts are sent as JSON and responses are read
from stdin.

The bridge waits for "flit_ready" in the command output before switching
to transparent passthrough mode.

Options:`)
		fs.PrintDefaults()
		fmt.Fprintln(os.Stderr, `
Example:
  flit pty-bridge -- ssh host flit server --stdio`)
	}

	fs.Parse(args)

	if fs.NArg() < 1 {
		fs.Usage()
		os.Exit(1)
	}

	cmdArgs := fs.Args()

	// Setup logging to stderr
	level := slog.LevelInfo
	if *debug {
		level = slog.LevelDebug
	}
	slog.SetDefault(slog.New(slog.NewTextHandler(os.Stderr, &slog.HandlerOptions{Level: level})))

	slog.Info("Starting pty-bridge", "args", cmdArgs)

	if err := runBridge(cmdArgs); err != nil {
		slog.Error("Bridge failed", "err", err)
		os.Exit(1)
	}
}

func runBridge(cmdArgs []string) error {
	reader := bufio.NewReader(os.Stdin)
	writer := os.Stdout

	// Start the command in a PTY
	ptmx, cmd, err := startPtyCommand(cmdArgs)
	if err != nil {
		return fmt.Errorf("failed to start command: %w", err)
	}

	// Set up signal handling to send Ctrl-D before cleanup
	sigChan := make(chan os.Signal, 1)
	signal.Notify(sigChan, syscall.SIGTERM, syscall.SIGINT, syscall.SIGHUP)
	defer signal.Stop(sigChan)

	go func() {
		sig := <-sigChan
		slog.Info("Received signal, cleaning up", "signal", sig)
		cleanupPty(ptmx, cmd)
		os.Exit(0)
	}()

	defer cleanupPty(ptmx, cmd)

	// Handle auth and wait for ready signal
	pendingData, err := handleAuthAndWaitForReady(ptmx, writer, reader)
	if err != nil {
		return fmt.Errorf("auth/ready failed: %w", err)
	}

	// Forward any pending data (including flit_ready) to the client
	if len(pendingData) > 0 {
		slog.Info("Forwarding pending data to client", "len", len(pendingData), "data", fmt.Sprintf("%q", pendingData))
		if _, err := writer.Write([]byte(pendingData)); err != nil {
			return fmt.Errorf("failed to forward pending data: %w", err)
		}
	}

	slog.Info("Ready, bridging")

	// Bridge client I/O to PTY
	done := make(chan struct{}, 2)

	go func() {
		n, err := io.Copy(ptmx, reader)
		slog.Info("Client->PTY copy done", "bytes", n, "err", err)
		done <- struct{}{}
	}()

	go func() {
		n, err := io.Copy(writer, ptmx)
		slog.Info("PTY->Client copy done", "bytes", n, "err", err)
		done <- struct{}{}
	}()

	<-done
	ptmx.Close()
	// Don't wg.Wait() — the client→PTY goroutine is likely blocked
	// on reader.Read() (os.Stdin) which can't be unblocked by closing
	// the PTY. Just exit; the process is done.

	return nil
}

func startPtyCommand(cmdArgs []string) (*os.File, *exec.Cmd, error) {
	cmd := exec.Command(cmdArgs[0], cmdArgs[1:]...)

	ptmx, err := pty.Start(cmd)
	if err != nil {
		return nil, nil, err
	}

	slog.Debug("PTY started", "pid", cmd.Process.Pid)
	return ptmx, cmd, nil
}

func cleanupPty(ptmx *os.File, cmd *exec.Cmd) {
	slog.Debug("Cleaning up", "pid", cmd.Process.Pid)

	// Send Ctrl-D (EOF) to trigger remote shutdown
	ptmx.Write([]byte{0x04})
	ptmx.Close()

	// Try graceful termination first
	if err := cmd.Process.Signal(syscall.SIGTERM); err != nil {
		slog.Debug("SIGTERM failed", "err", err)
		cmd.Process.Kill()
		return
	}

	// Wait briefly for graceful exit
	done := make(chan error, 1)
	go func() {
		_, err := cmd.Process.Wait()
		done <- err
	}()

	select {
	case <-done:
		slog.Debug("Process exited gracefully")
	case <-time.After(500 * time.Millisecond):
		slog.Debug("Process did not exit, sending SIGKILL")
		cmd.Process.Kill()
		<-done
	}
}

func handleAuthAndWaitForReady(ptmx *os.File, writer io.Writer, reader *bufio.Reader) (string, error) {
	var pending strings.Builder

	slog.Info("Waiting for ready signal")

	for {
		buf := make([]byte, 1024)
		n, err := ptmx.Read(buf)
		if err != nil {
			return "", fmt.Errorf("read error: %w", err)
		}
		output := string(buf[:n])
		pending.WriteString(output)

		slog.Info("PTY output", "bytes", n, "data", fmt.Sprintf("%q", output), "pending_len", pending.Len())

		// Check for password prompt in raw output (not JSON — this is
		// the PTY itself prompting, not a flit protocol message).
		if isPasswordPrompt(output) {
			if err := handlePasswordPrompt(output, ptmx, writer, reader); err != nil {
				return "", err
			}
			pending.Reset()
			continue
		}

		// Parse each complete line as JSON (stripping PTY junk first).
		// This cleanly ignores command echoes, ANSI escapes, etc.
		pendingStr := pending.String()
		for _, l := range strings.Split(pendingStr, "\n") {
			cleaned := stripPtyJunk(l)
			if cleaned == "" {
				continue
			}
			var obj map[string]interface{}
			if err := json.Unmarshal([]byte(cleaned), &obj); err != nil {
				continue
			}

			slog.Info("Parsed JSON from PTY", "obj", cleaned)

			if _, ok := obj["flit_ready"]; ok {
				slog.Info("Ready signal detected")
				return pendingStr, nil
			}

			if _, ok := obj["flit_not_found"]; ok {
				slog.Info("Deploy signal detected", "line", cleaned)
				if err := handleDeploy(cleaned, ptmx, writer, reader); err != nil {
					return "", err
				}
				pending.Reset()
				goto nextRead
			}
		}

		slog.Info("No match yet, continuing to read")
	nextRead:
	}
}

// handlePasswordPrompt relays a password prompt to the client and sends
// the response to the PTY.
func handlePasswordPrompt(prompt string, ptmx *os.File, writer io.Writer, reader *bufio.Reader) error {
	slog.Info("Password prompt detected", "prompt", strings.TrimSpace(prompt))

	promptJSON, err := json.Marshal(map[string]string{"pty_password_prompt": prompt})
	if err != nil {
		return fmt.Errorf("failed to marshal prompt: %w", err)
	}
	promptJSON = append(promptJSON, '\n')
	if _, err := writer.Write(promptJSON); err != nil {
		return fmt.Errorf("failed to send prompt: %w", err)
	}

	slog.Info("Waiting for password response from client")
	line, err := reader.ReadString('\n')
	if err != nil {
		return fmt.Errorf("failed to read password response: %w", err)
	}
	slog.Info("Received password response", "len", len(line))

	var response map[string]string
	if err := json.Unmarshal([]byte(line), &response); err != nil {
		return fmt.Errorf("failed to parse password response: %w", err)
	}

	password, ok := response["pty_password"]
	if !ok {
		return fmt.Errorf("password response missing 'pty_password' field")
	}

	slog.Info("Sending password to PTY")
	ptmx.Write([]byte(password + "\n"))
	return nil
}

// handleDeploy relays a flit_not_found signal to the client, receives the
// binary, and transfers it to the PTY for the remote deploy script.
func handleDeploy(deployLine string, ptmx *os.File, writer io.Writer, reader *bufio.Reader) error {
	if _, err := writer.Write([]byte(deployLine + "\n")); err != nil {
		return fmt.Errorf("failed to relay deploy signal: %w", err)
	}

	slog.Info("Waiting for deploy response from client")
	responseLine, err := reader.ReadString('\n')
	if err != nil {
		return fmt.Errorf("failed to read deploy response: %w", err)
	}
	slog.Info("Received deploy response", "response", strings.TrimSpace(responseLine))

	var deployResponse struct {
		FlitDeploy struct {
			Size int64 `json:"size"`
		} `json:"flit_deploy"`
	}
	if err := json.Unmarshal([]byte(responseLine), &deployResponse); err != nil {
		return fmt.Errorf("failed to parse deploy response: %w", err)
	}
	size := deployResponse.FlitDeploy.Size
	if size <= 0 {
		return fmt.Errorf("invalid deploy size: %d", size)
	}
	slog.Info("Deploying binary", "size", size)

	if _, err := fmt.Fprintf(ptmx, "%d\n", size); err != nil {
		return fmt.Errorf("failed to send deploy size to PTY: %w", err)
	}

	// Wait for remote script to signal it's ready (stty raw -echo done).
	// Without this, binary data written to the PTY would be mangled by
	// the cooked-mode line discipline.
	slog.Info("Waiting for deploy_ready signal from remote")
	readyBuf := make([]byte, 256)
	for {
		n, err := ptmx.Read(readyBuf)
		if err != nil {
			return fmt.Errorf("failed to read deploy_ready signal: %w", err)
		}
		chunk := string(readyBuf[:n])
		slog.Info("PTY deploy output", "data", fmt.Sprintf("%q", chunk))
		if strings.Contains(chunk, "deploy_ready") {
			break
		}
	}
	slog.Info("Deploy ready, starting binary transfer")

	copied, err := io.CopyN(ptmx, reader, size)
	if err != nil {
		return fmt.Errorf("failed to transfer binary (copied %d/%d): %w", copied, size, err)
	}
	slog.Info("Binary transfer complete", "bytes", copied)
	return nil
}

// ansiEscapeRe matches ANSI escape sequences (CSI and OSC).
var ansiEscapeRe = regexp.MustCompile(`\x1b\[[0-9;?]*[a-zA-Z]|\x1b\][^\x07]*\x07`)

// stripPtyJunk strips ANSI escape sequences and control characters from s,
// leaving printable text suitable for JSON parsing.
func stripPtyJunk(s string) string {
	s = ansiEscapeRe.ReplaceAllString(s, "")
	var b strings.Builder
	for _, r := range s {
		if r >= 32 || r == '\t' {
			b.WriteRune(r)
		}
	}
	return strings.TrimSpace(b.String())
}

func isPasswordPrompt(s string) bool {
	lower := strings.ToLower(s)
	return strings.Contains(lower, "password") ||
		strings.Contains(lower, "passphrase") ||
		strings.Contains(lower, "passcode") ||
		(strings.Contains(lower, "enter") && strings.Contains(lower, "key"))
}
