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
	"os"
	"syscall"
	"unsafe"
)

// Linux uses TCXONC ioctl for output flow control.
const (
	tcxonc = 0x540A
	tcooff = 0 // Suspend output
	tcoon  = 1 // Resume output
)

// ptsname returns the slave PTY device path for the given master fd.
func ptsname(master *os.File) (string, error) {
	var n uint32
	_, _, errno := syscall.Syscall(syscall.SYS_IOCTL, master.Fd(), syscall.TIOCGPTN, uintptr(unsafe.Pointer(&n)))
	if errno != 0 {
		return "", fmt.Errorf("TIOCGPTN: %w", errno)
	}
	return fmt.Sprintf("/dev/pts/%d", n), nil
}

func suspendOutput(slave *os.File) error {
	_, _, errno := syscall.Syscall(syscall.SYS_IOCTL, slave.Fd(), tcxonc, tcooff)
	if errno != 0 {
		return errno
	}
	return nil
}

func resumeOutput(slave *os.File) error {
	_, _, errno := syscall.Syscall(syscall.SYS_IOCTL, slave.Fd(), tcxonc, tcoon)
	if errno != 0 {
		return errno
	}
	return nil
}
