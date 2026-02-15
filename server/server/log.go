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

package server

import (
	"context"
	"io"
	"log/slog"
	"time"
)

// RPCLogHandler is a slog handler that sends logs via JSON-RPC notifications
type RPCLogHandler struct {
	level  slog.Leveler
	notify func(method string, params any) error
	attrs  []slog.Attr
	groups []string
}

// LogNotification is the structure sent to the client
type LogNotification struct {
	Time    string         `json:"time"`
	Level   string         `json:"level"`
	Message string         `json:"msg"`
	Attrs   map[string]any `json:"attrs,omitempty"`
}

// NewRPCLogHandler creates a new handler that sends logs via RPC
func NewRPCLogHandler(level slog.Leveler, notify func(method string, params any) error) *RPCLogHandler {
	return &RPCLogHandler{
		level:  level,
		notify: notify,
	}
}

func (h *RPCLogHandler) Enabled(_ context.Context, level slog.Level) bool {
	return level >= h.level.Level()
}

func (h *RPCLogHandler) Handle(_ context.Context, r slog.Record) error {
	attrs := make(map[string]any)

	// Add pre-stored attrs
	for _, a := range h.attrs {
		attrs[a.Key] = a.Value.Any()
	}

	// Add record attrs
	r.Attrs(func(a slog.Attr) bool {
		attrs[a.Key] = a.Value.Any()
		return true
	})

	notification := LogNotification{
		Time:    r.Time.Format(time.RFC3339Nano),
		Level:   r.Level.String(),
		Message: r.Message,
	}
	if len(attrs) > 0 {
		notification.Attrs = attrs
	}

	return h.notify("log", notification)
}

func (h *RPCLogHandler) WithAttrs(attrs []slog.Attr) slog.Handler {
	newHandler := &RPCLogHandler{
		level:  h.level,
		notify: h.notify,
		attrs:  make([]slog.Attr, len(h.attrs)+len(attrs)),
		groups: h.groups,
	}
	copy(newHandler.attrs, h.attrs)
	copy(newHandler.attrs[len(h.attrs):], attrs)
	return newHandler
}

func (h *RPCLogHandler) WithGroup(name string) slog.Handler {
	newHandler := &RPCLogHandler{
		level:  h.level,
		notify: h.notify,
		attrs:  h.attrs,
		groups: append(h.groups[:len(h.groups):len(h.groups)], name),
	}
	return newHandler
}

// MultiHandler is a slog handler that writes to multiple handlers
type MultiHandler struct {
	handlers []slog.Handler
}

// NewMultiHandler creates a handler that writes to all provided handlers
func NewMultiHandler(handlers ...slog.Handler) *MultiHandler {
	return &MultiHandler{handlers: handlers}
}

func (h *MultiHandler) Enabled(ctx context.Context, level slog.Level) bool {
	for _, handler := range h.handlers {
		if handler.Enabled(ctx, level) {
			return true
		}
	}
	return false
}

func (h *MultiHandler) Handle(ctx context.Context, r slog.Record) error {
	for _, handler := range h.handlers {
		if handler.Enabled(ctx, r.Level) {
			handler.Handle(ctx, r)
		}
	}
	return nil
}

func (h *MultiHandler) WithAttrs(attrs []slog.Attr) slog.Handler {
	handlers := make([]slog.Handler, len(h.handlers))
	for i, handler := range h.handlers {
		handlers[i] = handler.WithAttrs(attrs)
	}
	return &MultiHandler{handlers: handlers}
}

func (h *MultiHandler) WithGroup(name string) slog.Handler {
	handlers := make([]slog.Handler, len(h.handlers))
	for i, handler := range h.handlers {
		handlers[i] = handler.WithGroup(name)
	}
	return &MultiHandler{handlers: handlers}
}

// DiscardHandler is a slog handler that discards all logs
type DiscardHandler struct{}

func (h DiscardHandler) Enabled(_ context.Context, _ slog.Level) bool {
	return false
}

func (h DiscardHandler) Handle(_ context.Context, _ slog.Record) error {
	return nil
}

func (h DiscardHandler) WithAttrs(_ []slog.Attr) slog.Handler {
	return h
}

func (h DiscardHandler) WithGroup(_ string) slog.Handler {
	return h
}

// NewDiscardLogger creates a logger that discards all output
func NewDiscardLogger() *slog.Logger {
	return slog.New(DiscardHandler{})
}

// NewStderrLogger creates a logger that writes to stderr
func NewStderrLogger(level slog.Level) *slog.Logger {
	return slog.New(slog.NewTextHandler(io.Discard, &slog.HandlerOptions{Level: level}))
}

// loggerKey is the context key for the logger
type loggerKey struct{}

// WithLogger returns a context with the given logger
func WithLogger(ctx context.Context, logger *slog.Logger) context.Context {
	return context.WithValue(ctx, loggerKey{}, logger)
}

// Logger returns the logger from context, or a discard logger if none
func Logger(ctx context.Context) *slog.Logger {
	if logger, ok := ctx.Value(loggerKey{}).(*slog.Logger); ok {
		return logger
	}
	return NewDiscardLogger()
}
