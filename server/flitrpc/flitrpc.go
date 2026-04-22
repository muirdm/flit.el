// Package flitrpc implements a binary RPC protocol.
//
// Wire format:
//
//	[4 bytes: magic 0x464C5452 "FLTR"]
//	[4 bytes: meta_len (big-endian)]
//	[4 bytes: payload_len (big-endian)]
//	[meta_len bytes: msgpack-encoded metadata]
//	[payload_len bytes: raw binary payload]
//
// Message types (metadata "t" field):
//
//	1=request, 2=response, 3=notification, 4=chunk-continue, 5=chunk-end
package flitrpc

import (
	"bufio"
	"encoding/binary"
	"encoding/json"
	"fmt"
	"io"
	"log/slog"
	"sync"
	"sync/atomic"

	"github.com/vmihailenco/msgpack/v5"
)

var magic = [4]byte{'F', 'L', 'T', 'R'}

const (
	headerSize = 12

	TypeRequest       = 1
	TypeResponse      = 2
	TypeNotification  = 3
	TypeChunkContinue = 4
	TypeChunkEnd      = 5
)

// Meta is the metadata for a frame. Encoded as msgpack on the wire.
type Meta struct {
	Type   int            `msgpack:"t"`
	ID     int64          `msgpack:"id,omitempty"`
	Method string         `msgpack:"method,omitempty"`
	Params map[string]any `msgpack:"params,omitempty"`
	Result any            `msgpack:"result,omitempty"`
	Error  *ErrorInfo     `msgpack:"error,omitempty"`
}

// ErrorInfo is an error in a response frame.
type ErrorInfo struct {
	Code    int    `msgpack:"code,omitempty"`
	Message string `msgpack:"message"`
}

// Frame is a parsed flitrpc frame.
type Frame struct {
	Meta    Meta
	Payload []byte // raw binary payload (nil if none)
}

// ReadFrame reads one frame from r.
func ReadFrame(r *bufio.Reader) (*Frame, error) {
	var hdr [headerSize]byte
	if _, err := io.ReadFull(r, hdr[:]); err != nil {
		return nil, err
	}
	if hdr[0] != magic[0] || hdr[1] != magic[1] || hdr[2] != magic[2] || hdr[3] != magic[3] {
		return nil, fmt.Errorf("flitrpc: bad magic %x%x%x%x", hdr[0], hdr[1], hdr[2], hdr[3])
	}
	metaLen := binary.BigEndian.Uint32(hdr[4:8])
	payloadLen := binary.BigEndian.Uint32(hdr[8:12])

	metaBuf := make([]byte, metaLen)
	if _, err := io.ReadFull(r, metaBuf); err != nil {
		return nil, fmt.Errorf("flitrpc: reading meta: %w", err)
	}

	var meta Meta
	if err := msgpack.Unmarshal(metaBuf, &meta); err != nil {
		return nil, fmt.Errorf("flitrpc: decoding meta: %w", err)
	}

	var payload []byte
	if payloadLen > 0 {
		payload = make([]byte, payloadLen)
		if _, err := io.ReadFull(r, payload); err != nil {
			return nil, fmt.Errorf("flitrpc: reading payload: %w", err)
		}
	}

	return &Frame{Meta: meta, Payload: payload}, nil
}

// Writer writes flitrpc frames. Safe for concurrent use.
type Writer struct {
	w  io.Writer
	mu sync.Mutex
}

// NewWriter creates a Writer wrapping w.
func NewWriter(w io.Writer) *Writer {
	return &Writer{w: w}
}

// WriteFrame writes a single frame.
func (fw *Writer) WriteFrame(meta Meta, payload []byte) error {
	metaBuf, err := msgpack.Marshal(meta)
	if err != nil {
		return fmt.Errorf("flitrpc: encoding meta: %w", err)
	}
	return fw.writeRaw(metaBuf, payload)
}

func (fw *Writer) writeRaw(metaBuf, payload []byte) error {
	var hdr [headerSize]byte
	copy(hdr[:4], magic[:])
	binary.BigEndian.PutUint32(hdr[4:8], uint32(len(metaBuf)))
	binary.BigEndian.PutUint32(hdr[8:12], uint32(len(payload)))

	fw.mu.Lock()
	defer fw.mu.Unlock()
	if _, err := fw.w.Write(hdr[:]); err != nil {
		return err
	}
	if _, err := fw.w.Write(metaBuf); err != nil {
		return err
	}
	if len(payload) > 0 {
		if _, err := fw.w.Write(payload); err != nil {
			return err
		}
	}
	return nil
}

// Handler processes an RPC request and returns a result.
type Handler func(params json.RawMessage, payload []byte) (result any, resultPayload []byte, err error)

// Server handles flitrpc requests.
type Server struct {
	reader   *bufio.Reader
	writer   *Writer
	handlers map[string]Handler
	nextID   atomic.Int64
	logger   *slog.Logger
	done     chan struct{}

	// RequestHandler is called for client->server requests not in the handler map.
	// If nil, unknown methods return an error.
	RequestHandler func(method string, params json.RawMessage, payload []byte) (any, []byte, error)
}

// NewServer creates a server reading from r and writing to w.
func NewServer(r io.Reader, w io.Writer, logger *slog.Logger) *Server {
	return &Server{
		reader:   bufio.NewReaderSize(r, 256*1024),
		writer:   NewWriter(w),
		handlers: make(map[string]Handler),
		logger:   logger,
		done:     make(chan struct{}),
	}
}

// Handle registers a handler for a method.
func (s *Server) Handle(method string, h Handler) {
	s.handlers[method] = h
}

// HandleFunc registers a simple handler that takes json params and returns a result.
func (s *Server) HandleFunc(method string, fn func(params json.RawMessage) (any, error)) {
	s.handlers[method] = func(params json.RawMessage, _ []byte) (any, []byte, error) {
		result, err := fn(params)
		return result, nil, err
	}
}

// HandleTyped registers a handler with typed params (unmarshaled from JSON).
func HandleTyped[P any](s *Server, method string, fn func(params P) (any, error)) {
	s.HandleFunc(method, func(raw json.RawMessage) (any, error) {
		var p P
		if len(raw) > 0 {
			if err := json.Unmarshal(raw, &p); err != nil {
				return nil, fmt.Errorf("invalid params: %w", err)
			}
		}
		return fn(p)
	})
}

// Notify sends a notification to the client (no payload).
func (s *Server) Notify(method string, params any) error {
	return s.NotifyWithPayload(method, params, nil)
}

// NotifyWithPayload sends a notification with optional binary payload.
func (s *Server) NotifyWithPayload(method string, params any, payload []byte) error {
	var paramsMap map[string]any
	if params != nil {
		// Convert params to map[string]any via JSON roundtrip
		b, err := json.Marshal(params)
		if err != nil {
			return fmt.Errorf("flitrpc: marshaling notification params: %w", err)
		}
		if err := json.Unmarshal(b, &paramsMap); err != nil {
			return fmt.Errorf("flitrpc: unmarshaling notification params: %w", err)
		}
	}
	meta := Meta{
		Type:   TypeNotification,
		Method: method,
		Params: paramsMap,
	}
	return s.writer.WriteFrame(meta, payload)
}

// Callback sends a request to the client and waits for a response.
// This is used for heartbeats and other server-initiated requests.
func (s *Server) Callback(method string, params any) (any, error) {
	// For now, heartbeat is the only use case.
	// We don't implement full bidirectional request/response.
	// The heartbeat response is handled in the read loop.
	return nil, fmt.Errorf("flitrpc: Callback not yet implemented")
}

// Serve reads and dispatches frames until the connection closes.
func (s *Server) Serve() error {
	defer close(s.done)
	for {
		frame, err := ReadFrame(s.reader)
		if err != nil {
			return err
		}
		switch frame.Meta.Type {
		case TypeRequest:
			go s.handleRequest(frame)
		case TypeResponse:
			// Server-initiated request responses (e.g., heartbeat)
			s.handleCallbackResponse(frame)
		case TypeNotification:
			// Client-to-server notification (e.g., exec/input)
			go s.handleRequest(frame)
		default:
			s.logger.Warn("flitrpc: unknown frame type", "type", frame.Meta.Type)
		}
	}
}

// Done returns a channel closed when Serve exits.
func (s *Server) Done() <-chan struct{} {
	return s.done
}

// Stop causes Serve to exit. The caller should close the underlying
// reader/writer to unblock ReadFrame.
func (s *Server) Stop() {
	// Serve exits when ReadFrame returns io.EOF from closed reader.
}

func (s *Server) handleRequest(frame *Frame) {
	method := frame.Meta.Method
	id := frame.Meta.ID

	handler, ok := s.handlers[method]
	if !ok {
		if s.RequestHandler != nil {
			s.handleWithFallback(frame)
			return
		}
		s.sendError(id, -32601, fmt.Sprintf("method not found: %s", method))
		return
	}

	// Marshal params to JSON for handler consumption
	paramsJSON, err := json.Marshal(frame.Meta.Params)
	if err != nil {
		s.sendError(id, -32600, fmt.Sprintf("invalid params: %v", err))
		return
	}

	result, resultPayload, err := handler(paramsJSON, frame.Payload)
	if err != nil {
		s.sendError(id, -32603, err.Error())
		return
	}

	// For notifications (type 3), don't send a response
	if frame.Meta.Type == TypeNotification {
		return
	}

	s.sendResult(id, result, resultPayload)
}

func (s *Server) handleWithFallback(frame *Frame) {
	paramsJSON, err := json.Marshal(frame.Meta.Params)
	if err != nil {
		s.sendError(frame.Meta.ID, -32600, fmt.Sprintf("invalid params: %v", err))
		return
	}
	result, resultPayload, err := s.RequestHandler(frame.Meta.Method, paramsJSON, frame.Payload)
	if err != nil {
		s.sendError(frame.Meta.ID, -32603, err.Error())
		return
	}
	if frame.Meta.Type == TypeNotification {
		return
	}
	s.sendResult(frame.Meta.ID, result, resultPayload)
}

func (s *Server) sendResult(id int64, result any, payload []byte) {
	meta := Meta{
		Type:   TypeResponse,
		ID:     id,
		Result: result,
	}
	if err := s.writer.WriteFrame(meta, payload); err != nil {
		s.logger.Error("flitrpc: failed to send response", "id", id, "error", err)
	}
}

func (s *Server) sendError(id int64, code int, message string) {
	meta := Meta{
		Type: TypeResponse,
		ID:   id,
		Error: &ErrorInfo{
			Code:    code,
			Message: message,
		},
	}
	if err := s.writer.WriteFrame(meta, nil); err != nil {
		s.logger.Error("flitrpc: failed to send error", "id", id, "error", err)
	}
}

// Heartbeat support: server sends request, client responds.
var (
	callbackMu       sync.Mutex
	callbackWaiters  = make(map[int64]chan *Frame)
)

func (s *Server) handleCallbackResponse(frame *Frame) {
	callbackMu.Lock()
	ch, ok := callbackWaiters[frame.Meta.ID]
	callbackMu.Unlock()
	if ok {
		ch <- frame
	}
}

// SendRequest sends a request to the client and waits for a response.
func (s *Server) SendRequest(method string, params any) (any, error) {
	id := s.nextID.Add(1)

	ch := make(chan *Frame, 1)
	callbackMu.Lock()
	callbackWaiters[id] = ch
	callbackMu.Unlock()
	defer func() {
		callbackMu.Lock()
		delete(callbackWaiters, id)
		callbackMu.Unlock()
	}()

	var paramsMap map[string]any
	if params != nil {
		b, _ := json.Marshal(params)
		json.Unmarshal(b, &paramsMap)
	}

	meta := Meta{
		Type:   TypeRequest,
		ID:     id,
		Method: method,
		Params: paramsMap,
	}
	if err := s.writer.WriteFrame(meta, nil); err != nil {
		return nil, err
	}

	resp := <-ch
	if resp.Meta.Error != nil {
		return nil, fmt.Errorf("%s", resp.Meta.Error.Message)
	}
	return resp.Meta.Result, nil
}
