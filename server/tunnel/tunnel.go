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

package tunnel

import (
	"context"
	"encoding/base64"
	"fmt"
	"io"
	"log/slog"
	"net"
	"sync"
	"sync/atomic"
)

// NotifyFunc is called to send notifications to the client.
type NotifyFunc func(ctx context.Context, method string, params any) error

// Manager handles tunnel listeners and connections for both directions.
type Manager struct {
	notify    NotifyFunc
	logger    *slog.Logger
	mu        sync.Mutex
	listeners map[string]*Listener   // tunnelID -> listener (reverse tunnels)
	conns     map[string]*Connection // connID -> connection (both directions)
	connIDGen atomic.Uint64
}

// Listener represents a TCP listener that forwards connections via RPC.
type Listener struct {
	ID       string
	Port     int
	listener net.Listener
	manager  *Manager
	closed   atomic.Bool
}

// Connection represents a single tunneled connection (either direction).
type Connection struct {
	ID       string
	TunnelID string
	conn     net.Conn
	manager  *Manager
	closed   atomic.Bool
}

// NewManager creates a new tunnel manager.
func NewManager(notify NotifyFunc, logger *slog.Logger) *Manager {
	return &Manager{
		notify:    notify,
		logger:    logger,
		listeners: make(map[string]*Listener),
		conns:     make(map[string]*Connection),
	}
}

// ListenParams contains parameters for starting a tunnel listener (reverse tunnels).
type ListenParams struct {
	TunnelID string `json:"tunnelId"`
	Port     int    `json:"port"` // Port to listen on (0 for random)
}

// ListenResult contains the result of starting a listener.
type ListenResult struct {
	TunnelID string `json:"tunnelId"`
	Port     int    `json:"port"` // Actual port (useful when requested 0)
}

// Listen starts a TCP listener on the specified port (reverse tunnels).
func (m *Manager) Listen(params ListenParams) (*ListenResult, error) {
	m.mu.Lock()
	defer m.mu.Unlock()

	if _, exists := m.listeners[params.TunnelID]; exists {
		return nil, fmt.Errorf("tunnel %s already exists", params.TunnelID)
	}

	addr := fmt.Sprintf("127.0.0.1:%d", params.Port)
	listener, err := net.Listen("tcp", addr)
	if err != nil {
		return nil, fmt.Errorf("failed to listen on %s: %w", addr, err)
	}

	actualPort := listener.Addr().(*net.TCPAddr).Port

	l := &Listener{
		ID:       params.TunnelID,
		Port:     actualPort,
		listener: listener,
		manager:  m,
	}

	m.listeners[params.TunnelID] = l

	go l.acceptLoop()

	m.logger.Info("Tunnel listener started", "tunnelId", params.TunnelID, "port", actualPort)

	return &ListenResult{
		TunnelID: params.TunnelID,
		Port:     actualPort,
	}, nil
}

// CloseParams contains parameters for closing a tunnel listener.
type CloseParams struct {
	TunnelID string `json:"tunnelId"`
}

// Close stops a tunnel listener and all its connections.
func (m *Manager) Close(params CloseParams) error {
	m.mu.Lock()
	l, exists := m.listeners[params.TunnelID]
	if !exists {
		m.mu.Unlock()
		return fmt.Errorf("tunnel %s not found", params.TunnelID)
	}
	delete(m.listeners, params.TunnelID)
	m.mu.Unlock()

	l.close()
	return nil
}

// ConnectParams contains parameters for dialing a local port (forward tunnels).
type ConnectParams struct {
	ConnID   string `json:"connId"`
	TunnelID string `json:"tunnelId"`
	Port     int    `json:"port"`
}

// Connect dials a local port on the server and starts forwarding data (forward tunnels).
func (m *Manager) Connect(params ConnectParams) error {
	m.mu.Lock()
	if _, exists := m.conns[params.ConnID]; exists {
		m.mu.Unlock()
		return fmt.Errorf("connection %s already exists", params.ConnID)
	}
	m.mu.Unlock()

	addr := fmt.Sprintf("127.0.0.1:%d", params.Port)
	conn, err := net.Dial("tcp", addr)
	if err != nil {
		return fmt.Errorf("failed to connect to %s: %w", addr, err)
	}

	c := &Connection{
		ID:       params.ConnID,
		TunnelID: params.TunnelID,
		conn:     conn,
		manager:  m,
	}

	m.mu.Lock()
	m.conns[params.ConnID] = c
	m.mu.Unlock()

	m.logger.Info("Tunnel connected", "connId", params.ConnID, "tunnelId", params.TunnelID, "port", params.Port)

	go c.readLoop()

	return nil
}

// DataParams contains data to send to a tunnel connection.
type DataParams struct {
	ConnID string `json:"connId"`
	Data   []byte `json:"data"`
}

// SendData sends data to a specific connection.
func (m *Manager) SendData(params DataParams) error {
	m.mu.Lock()
	conn, exists := m.conns[params.ConnID]
	m.mu.Unlock()

	if !exists {
		return fmt.Errorf("connection %s not found", params.ConnID)
	}

	_, err := conn.conn.Write(params.Data)
	return err
}

// DisconnectParams contains parameters for disconnecting a connection.
type DisconnectParams struct {
	ConnID string `json:"connId"`
}

// Disconnect closes a specific connection.
func (m *Manager) Disconnect(params DisconnectParams) error {
	m.mu.Lock()
	conn, exists := m.conns[params.ConnID]
	if exists {
		delete(m.conns, params.ConnID)
	}
	m.mu.Unlock()

	if !exists {
		return nil // Already disconnected
	}

	conn.close()
	return nil
}

// CloseAll closes all listeners and connections.
func (m *Manager) CloseAll() {
	m.mu.Lock()
	listeners := make([]*Listener, 0, len(m.listeners))
	for _, l := range m.listeners {
		listeners = append(listeners, l)
	}
	m.listeners = make(map[string]*Listener)

	conns := make([]*Connection, 0, len(m.conns))
	for _, c := range m.conns {
		conns = append(conns, c)
	}
	m.conns = make(map[string]*Connection)
	m.mu.Unlock()

	for _, l := range listeners {
		l.close()
	}
	for _, c := range conns {
		c.close()
	}
}

// acceptLoop accepts connections on the listener (reverse tunnels).
func (l *Listener) acceptLoop() {
	for {
		conn, err := l.listener.Accept()
		if err != nil {
			if l.closed.Load() {
				return
			}
			l.manager.logger.Error("Accept failed", "tunnelId", l.ID, "error", err)
			continue
		}

		connID := fmt.Sprintf("%s-%d", l.ID, l.manager.connIDGen.Add(1))

		c := &Connection{
			ID:       connID,
			TunnelID: l.ID,
			conn:     conn,
			manager:  l.manager,
		}

		l.manager.mu.Lock()
		l.manager.conns[connID] = c
		l.manager.mu.Unlock()

		// Notify client of new connection
		l.manager.notify(context.Background(), "tunnel/accept", map[string]string{
			"tunnelId": l.ID,
			"connId":   connID,
		})

		l.manager.logger.Info("Tunnel connection accepted", "tunnelId", l.ID, "connId", connID)

		go c.readLoop()
	}
}

func (l *Listener) close() {
	if l.closed.Swap(true) {
		return
	}
	l.listener.Close()
	l.manager.logger.Info("Tunnel listener closed", "tunnelId", l.ID)
}

// readLoop reads data from the connection and sends it to the client.
func (c *Connection) readLoop() {
	defer c.cleanup()

	buf := make([]byte, 32*1024) // 32KB buffer
	for {
		n, err := c.conn.Read(buf)
		if n > 0 {
			encoded := base64.StdEncoding.EncodeToString(buf[:n])
			c.manager.notify(context.Background(), "tunnel/data", map[string]string{
				"tunnelId": c.TunnelID,
				"connId":   c.ID,
				"data":     encoded,
			})
		}
		if err != nil {
			if err != io.EOF && !c.closed.Load() {
				c.manager.logger.Debug("Tunnel read error", "connId", c.ID, "error", err)
			}
			return
		}
	}
}

func (c *Connection) cleanup() {
	if c.closed.Swap(true) {
		return
	}
	c.conn.Close()

	c.manager.mu.Lock()
	delete(c.manager.conns, c.ID)
	c.manager.mu.Unlock()

	c.manager.notify(context.Background(), "tunnel/disconnect", map[string]string{
		"tunnelId": c.TunnelID,
		"connId":   c.ID,
	})

	c.manager.logger.Debug("Tunnel connection closed", "connId", c.ID)
}

func (c *Connection) close() {
	c.closed.Store(true)
	c.conn.Close()
}
