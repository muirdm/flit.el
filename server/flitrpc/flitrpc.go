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
	"bytes"
	"encoding/binary"

	"fmt"
	"io"
	"log/slog"
	"sync"
	"sync/atomic"


	"github.com/vmihailenco/msgpack/v5"
)

const (
	headerSize = 8 // meta_len(4) + payload_len(4)

	TypeRequest       = 1
	TypeResponse      = 2
	TypeNotification  = 3
	TypeChunkContinue = 4
	TypeChunkEnd      = 5
)

// Meta is the metadata for a frame. Encoded as msgpack on the wire.
type Meta struct {
	Type   int                `msgpack:"t"`
	ID     int64              `msgpack:"id,omitempty"`
	Method string             `msgpack:"method,omitempty"`
	Params msgpack.RawMessage `msgpack:"params,omitempty"` // raw msgpack, decoded by handler
	Result any                `msgpack:"result,omitempty"`
	Error  *ErrorInfo         `msgpack:"error,omitempty"`
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
	metaLen := binary.BigEndian.Uint32(hdr[0:4])
	payloadLen := binary.BigEndian.Uint32(hdr[4:8])

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
	metaBuf, err := marshalMsgpack(meta)
	if err != nil {
		return fmt.Errorf("flitrpc: encoding meta: %w", err)
	}
	return fw.writeRaw(metaBuf, payload)
}

// marshalMsgpack encodes v using msgpack with json struct tag fallback.
// This allows Go structs with json:"..." tags (but no msgpack tags) to
// produce lowercase field names matching the JSON convention.
func marshalMsgpack(v any) ([]byte, error) {
	var buf bytes.Buffer
	enc := msgpack.NewEncoder(&buf)
	enc.SetCustomStructTag("json")
	if err := enc.Encode(v); err != nil {
		return nil, err
	}
	return buf.Bytes(), nil
}

func (fw *Writer) writeRaw(metaBuf, payload []byte) error {
	var hdr [headerSize]byte
	binary.BigEndian.PutUint32(hdr[0:4], uint32(len(metaBuf)))
	binary.BigEndian.PutUint32(hdr[4:8], uint32(len(payload)))

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

const chunkSize = 256 * 1024 // 256KB chunks for multiplexing

// writeChunked writes a message with a large payload as multiple frames,
// releasing the mutex between chunks so other messages can interleave.
func (fw *Writer) writeChunked(metaBuf []byte, payload []byte) error {
	// First frame: metadata + first chunk of payload
	first := payload
	if len(first) > chunkSize {
		first = payload[:chunkSize]
	}
	if err := fw.writeRaw(metaBuf, first); err != nil {
		return err
	}
	payload = payload[len(first):]

	// Continuation frames
	for len(payload) > 0 {
		chunk := payload
		if len(chunk) > chunkSize {
			chunk = payload[:chunkSize]
		}
		contMeta, _ := marshalMsgpack(map[string]any{"t": TypeChunkContinue})
		if err := fw.writeRaw(contMeta, chunk); err != nil {
			return err
		}
		payload = payload[len(chunk):]
	}

	// End frame
	endMeta, _ := marshalMsgpack(map[string]any{"t": TypeChunkEnd})
	return fw.writeRaw(endMeta, nil)
}

// Handler processes an RPC request and returns a result.
// Params is raw msgpack bytes for the "params" field.
type Handler func(params msgpack.RawMessage, payload []byte) (result any, resultPayload []byte, err error)

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
	RequestHandler func(method string, params msgpack.RawMessage, payload []byte) (any, []byte, error)
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

// HandleFunc registers a handler that takes raw msgpack params.
func (s *Server) HandleFunc(method string, fn func(params msgpack.RawMessage) (any, error)) {
	s.handlers[method] = func(params msgpack.RawMessage, _ []byte) (any, []byte, error) {
		result, err := fn(params)
		return result, nil, err
	}
}

// HandleTyped registers a handler with typed params.
// Params are unmarshaled from msgpack directly into the typed struct.
func HandleTyped[P any](s *Server, method string, fn func(params P) (any, error)) {
	s.HandleFunc(method, func(raw msgpack.RawMessage) (any, error) {
		var p P
		if len(raw) > 0 {
			if err := UnmarshalParams(raw, &p); err != nil {
				return nil, fmt.Errorf("invalid params: %w", err)
			}
		}
		return fn(p)
	})
}

// UnmarshalParams decodes raw msgpack params into a typed struct.
// Uses json struct tags since Go structs have json:"..." tags.
func UnmarshalParams(raw msgpack.RawMessage, v any) error {
	dec := msgpack.NewDecoder(bytes.NewReader(raw))
	dec.SetCustomStructTag("json")
	return dec.Decode(v)
}

// Notify sends a notification to the client (no payload).
func (s *Server) Notify(method string, params any) error {
	return s.NotifyWithPayload(method, params, nil)
}

// NotifyWithPayload sends a notification with optional binary payload.
func (s *Server) NotifyWithPayload(method string, params any, payload []byte) error {
	metaMap := map[string]any{
		"t":      TypeNotification,
		"id":     0,
		"method": method,
		"params": params,
	}
	if len(payload) > chunkSize {
		metaMap["chunked"] = true
	}
	metaBuf, err := marshalMsgpack(metaMap)
	if err != nil {
		return fmt.Errorf("flitrpc: encoding notification: %w", err)
	}
	if len(payload) > chunkSize {
		return s.writer.writeChunked(metaBuf, payload)
	}
	return s.writer.writeRaw(metaBuf, payload)
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

	result, resultPayload, err := handler(frame.Meta.Params, frame.Payload)
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
	result, resultPayload, err := s.RequestHandler(frame.Meta.Method, frame.Meta.Params, frame.Payload)
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
	metaMap := map[string]any{
		"t":      TypeResponse,
		"id":     id,
		"result": result,
	}
	if len(payload) > chunkSize {
		metaMap["chunked"] = true
	}
	metaBuf, err := marshalMsgpack(metaMap)
	if err != nil {
		s.logger.Error("flitrpc: failed to encode response", "id", id, "error", err)
		return
	}
	if len(payload) > chunkSize {
		if err := s.writer.writeChunked(metaBuf, payload); err != nil {
			s.logger.Error("flitrpc: failed to send chunked response", "id", id, "error", err)
		}
	} else {
		if err := s.writer.writeRaw(metaBuf, payload); err != nil {
			s.logger.Error("flitrpc: failed to send response", "id", id, "error", err)
		}
	}
}

func (s *Server) sendError(id int64, code int, message string) {
	metaMap := map[string]any{
		"t":  TypeResponse,
		"id": id,
		"error": map[string]any{
			"code":    code,
			"message": message,
		},
	}
	metaBuf, err := marshalMsgpack(metaMap)
	if err != nil {
		s.logger.Error("flitrpc: failed to encode error", "id", id, "error", err)
		return
	}
	if err := s.writer.writeRaw(metaBuf, nil); err != nil {
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

	metaMap := map[string]any{
		"t":      TypeRequest,
		"id":     id,
		"method": method,
		"params": params,
	}
	metaBuf, err := marshalMsgpack(metaMap)
	if err != nil {
		return nil, err
	}
	if err := s.writer.writeRaw(metaBuf, nil); err != nil {
		return nil, err
	}

	resp := <-ch
	if resp.Meta.Error != nil {
		return nil, fmt.Errorf("%s", resp.Meta.Error.Message)
	}
	return resp.Meta.Result, nil
}
