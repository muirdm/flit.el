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

// The server and local sidecar companion for flit.el.
//
// Usage:
//
//	flit server [flags]       Start the JSON-RPC server
//	flit pty-bridge <cmd>     Bridge stdin/stdout to a PTY running <cmd>
package main

import (
	"fmt"
	"os"
)

func main() {
	if len(os.Args) < 2 {
		printUsage()
		os.Exit(1)
	}

	switch os.Args[1] {
	case "server":
		runServer(os.Args[2:])
	case "pty-bridge":
		runPtyBridge(os.Args[2:])
	case "version":
		fmt.Println(protoVersion)
	case "-h", "--help", "help":
		printUsage()
	default:
		fmt.Fprintf(os.Stderr, "Unknown command: %s\n\n", os.Args[1])
		printUsage()
		os.Exit(1)
	}
}

func printUsage() {
	fmt.Fprintln(os.Stderr, `Usage: flit <command> [options]

Commands:
  server       Start the flitrpc server for remote file operations
  pty-bridge   Bridge stdin/stdout to a command running in a PTY
  version      Print the protocol version number

Run 'flit <command> -h' for help on a specific command.`)
}
