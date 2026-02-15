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
	"flag"
	"fmt"
	"io"
	"log"
	"log/slog"
	"net"
	"os"
	"os/signal"
	"syscall"
	"time"

	"github.com/muirdm/flit.el/server"
)

func runServer(args []string) {
	fs := flag.NewFlagSet("server", flag.ExitOnError)
	port := fs.Int("port", 9999, "Port to listen on")
	host := fs.String("host", "127.0.0.1", "Host to bind to")
	idleTimeout := fs.Duration("idle-timeout", 30*time.Minute, "Shutdown after this duration of inactivity (0 to disable)")
	verbose := fs.Bool("verbose", false, "Enable verbose logging (includes debug messages)")
	quiet := fs.Bool("quiet", false, "Quiet mode (only show errors)")
	stdio := fs.Bool("stdio", false, "Use stdin/stdout instead of TCP")

	fs.Usage = func() {
		fmt.Fprintln(os.Stderr, `Usage: flit server [options]

Start the JSON-RPC server for remote file operations.

Options:`)
		fs.PrintDefaults()
	}

	fs.Parse(args)

	// Configure slog level based on flags
	var level slog.Level
	if *quiet {
		level = slog.LevelError
	} else if *verbose {
		level = slog.LevelDebug
	} else {
		level = slog.LevelInfo
	}

	// Configure logging based on mode
	if *stdio {
		// In stdio mode, discard global logs - they would corrupt the JSON-RPC stream
		// Session-specific logs are sent back via RPC notifications
		slog.SetDefault(slog.New(server.DiscardHandler{}))
		log.SetOutput(io.Discard)
	} else {
		// TCP mode: log to stderr
		handler := slog.NewTextHandler(os.Stderr, &slog.HandlerOptions{
			Level: level,
		})
		slog.SetDefault(slog.New(handler))
		log.SetOutput(os.Stderr)
		if *verbose {
			log.SetFlags(log.LstdFlags | log.Lmicroseconds | log.Lshortfile)
		} else {
			log.SetFlags(log.LstdFlags)
		}
	}

	// Create the server (no idle timeout in stdio mode)
	timeout := *idleTimeout
	if *stdio {
		timeout = 0
	}
	srv := server.New(timeout, *verbose, level)

	// Handle stdio mode
	if *stdio {
		// No startup log - the ready JSON message signals readiness
		srv.HandleStdio(os.Stdin, os.Stdout)
		return
	}

	// TCP mode
	// Security: only allow listening on loopback interfaces.
	ip := net.ParseIP(*host)
	if ip == nil || !ip.IsLoopback() {
		log.Fatalf("Security: For TCP mode, host must be a loopback address (e.g., 127.0.0.1 or ::1), not '%s'", *host)
	}

	addr := fmt.Sprintf("%s:%d", *host, *port)
	listener, err := net.Listen("tcp", addr)
	if err != nil {
		log.Fatalf("Failed to listen on %s: %v", addr, err)
	}
	defer listener.Close()

	slog.Info("Flit server listening", "addr", addr)

	// Handle shutdown signals
	sigChan := make(chan os.Signal, 1)
	signal.Notify(sigChan, syscall.SIGINT, syscall.SIGTERM)
	go func() {
		sig := <-sigChan
		slog.Info("Received signal, shutting down", "signal", sig)
		listener.Close()
		srv.Shutdown()
		os.Exit(0)
	}()

	// Accept connections
	for {
		conn, err := listener.Accept()
		if err != nil {
			// Check if we're shutting down
			select {
			case <-srv.Done():
				return
			default:
				slog.Error("Failed to accept connection", "error", err)
				continue
			}
		}

		// Disable Nagle's algorithm for lower latency (important for interactive use)
		if tcpConn, ok := conn.(*net.TCPConn); ok {
			tcpConn.SetNoDelay(true)
		}

		slog.Info("New connection", "remote", conn.RemoteAddr())
		go srv.HandleConnection(conn)
	}
}
