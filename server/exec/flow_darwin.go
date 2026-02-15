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
	"bytes"
	"fmt"
	"os"
	"syscall"
	"unsafe"
)

// Darwin uses TIOCSTOP/TIOCSTART ioctls for output flow control.
const (
	tiocptygname = 0x40807453 // _IOC(IOC_OUT, 't', 'S', 128) - get slave PTY name
	tiocstop     = 0x2000746f // _IO('t', 111) - suspend output
	tiocstart    = 0x2000746e // _IO('t', 110) - resume output
)

// ptsname returns the slave PTY device path for the given master fd.
func ptsname(master *os.File) (string, error) {
	var buf [128]byte
	_, _, errno := syscall.Syscall(syscall.SYS_IOCTL, master.Fd(), tiocptygname, uintptr(unsafe.Pointer(&buf[0])))
	if errno != 0 {
		return "", fmt.Errorf("TIOCPTYGNAME: %w", errno)
	}
	return string(buf[:bytes.IndexByte(buf[:], 0)]), nil
}

func suspendOutput(slave *os.File) error {
	_, _, errno := syscall.Syscall(syscall.SYS_IOCTL, slave.Fd(), tiocstop, 0)
	if errno != 0 {
		return errno
	}
	return nil
}

func resumeOutput(slave *os.File) error {
	_, _, errno := syscall.Syscall(syscall.SYS_IOCTL, slave.Fd(), tiocstart, 0)
	if errno != 0 {
		return errno
	}
	return nil
}
