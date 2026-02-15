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

package util

import (
	"sync"
)

// BoundedWaitGroup is a WaitGroup with a bounded number of workers.
type BoundedWaitGroup struct {
	wg      sync.WaitGroup
	sem     chan struct{}
	maxSize int
}

// NewBoundedWaitGroup creates a new BoundedWaitGroup.
// If maxSize <= 0, it will be treated as having no limit.
func NewBoundedWaitGroup(maxSize int) *BoundedWaitGroup {
	b := &BoundedWaitGroup{maxSize: maxSize}
	if maxSize > 0 {
		b.sem = make(chan struct{}, maxSize)
	}
	return b
}

// Add adds a new worker to the group. It will block if the max size is reached.
func (b *BoundedWaitGroup) Add(delta int) {
	if b.maxSize <= 0 {
		b.wg.Add(delta)
		return
	}
	for i := 0; i < delta; i++ {
		b.sem <- struct{}{}
		b.wg.Add(1)
	}
}

// Done signals that a worker is finished.
func (b *BoundedWaitGroup) Done() {
	if b.maxSize > 0 {
		<-b.sem
	}
	b.wg.Done()
}

// Wait waits for all workers to finish.
func (b *BoundedWaitGroup) Wait() {
	b.wg.Wait()
}
