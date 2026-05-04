;;; flitrpc.el --- Binary RPC protocol for flit -*- lexical-binding: t; -*-

(require 'cl-lib)

;; Custom binary protocol replacing JSON-RPC.  Eliminates base64 overhead,
;; nested request deadlocks (jsonrpc.el bug#67945), and head-of-line blocking.
;;
;; Wire format:
;;   [4 bytes: magic 0x464C5452 "FLTR"]
;;   [4 bytes: meta_len (big-endian)]
;;   [4 bytes: payload_len (big-endian)]
;;   [meta_len bytes: msgpack-encoded metadata]
;;   [payload_len bytes: raw binary payload]
;;
;; Message types (in metadata "t" field):
;;   1 = request, 2 = response, 3 = notification
;;   4 = chunk-continue, 5 = chunk-end

;;; Msgpack reader — minimal subset for flitrpc metadata

(defun flitrpc--read-msgpack (buf pos)
  "Read one msgpack value from BUF starting at POS.
Return (VALUE . NEW-POS).  Operates on unibyte buffer."
  (with-current-buffer buf
    (let ((byte (char-after pos)))
      (cond
       ;; positive fixint 0x00-0x7f
       ((<= byte #x7f)
        (cons byte (1+ pos)))
       ;; fixmap 0x80-0x8f
       ((and (>= byte #x80) (<= byte #x8f))
        (flitrpc--read-map buf (1+ pos) (logand byte #x0f)))
       ;; fixarray 0x90-0x9f
       ((and (>= byte #x90) (<= byte #x9f))
        (flitrpc--read-array buf (1+ pos) (logand byte #x0f)))
       ;; fixstr 0xa0-0xbf
       ((and (>= byte #xa0) (<= byte #xbf))
        (flitrpc--read-str buf (1+ pos) (logand byte #x1f)))
       ;; nil 0xc0
       ((= byte #xc0)
        (cons nil (1+ pos)))
       ;; false 0xc2
       ((= byte #xc2)
        (cons :json-false (1+ pos)))
       ;; true 0xc3
       ((= byte #xc3)
        (cons t (1+ pos)))
       ;; bin8 0xc4
       ((= byte #xc4)
        (let ((len (char-after (1+ pos))))
          (flitrpc--read-bin buf (+ pos 2) len)))
       ;; bin16 0xc5
       ((= byte #xc5)
        (let ((len (flitrpc--read-u16-at buf (1+ pos))))
          (flitrpc--read-bin buf (+ pos 3) len)))
       ;; bin32 0xc6
       ((= byte #xc6)
        (let ((len (flitrpc--read-u32-at buf (1+ pos))))
          (flitrpc--read-bin buf (+ pos 5) len)))
       ;; float64 0xcb
       ((= byte #xcb)
        (flitrpc--read-float64 buf (1+ pos)))
       ;; uint8 0xcc
       ((= byte #xcc)
        (cons (char-after (1+ pos)) (+ pos 2)))
       ;; uint16 0xcd
       ((= byte #xcd)
        (cons (flitrpc--read-u16-at buf (1+ pos)) (+ pos 3)))
       ;; uint32 0xce
       ((= byte #xce)
        (cons (flitrpc--read-u32-at buf (1+ pos)) (+ pos 5)))
       ;; uint64 0xcf
       ((= byte #xcf)
        (flitrpc--read-u64 buf (1+ pos)))
       ;; int8 0xd0
       ((= byte #xd0)
        (let ((v (char-after (1+ pos))))
          (cons (if (>= v 128) (- v 256) v) (+ pos 2))))
       ;; int16 0xd1
       ((= byte #xd1)
        (let ((v (flitrpc--read-u16-at buf (1+ pos))))
          (cons (if (>= v 32768) (- v 65536) v) (+ pos 3))))
       ;; int32 0xd2
       ((= byte #xd2)
        (let ((v (flitrpc--read-u32-at buf (1+ pos))))
          (cons (if (>= v #x80000000) (- v #x100000000) v) (+ pos 5))))
       ;; int64 0xd3
       ((= byte #xd3)
        (flitrpc--read-i64 buf (1+ pos)))
       ;; str8 0xd9
       ((= byte #xd9)
        (let ((len (char-after (1+ pos))))
          (flitrpc--read-str buf (+ pos 2) len)))
       ;; str16 0xda
       ((= byte #xda)
        (let ((len (flitrpc--read-u16-at buf (1+ pos))))
          (flitrpc--read-str buf (+ pos 3) len)))
       ;; str32 0xdb
       ((= byte #xdb)
        (let ((len (flitrpc--read-u32-at buf (1+ pos))))
          (flitrpc--read-str buf (+ pos 5) len)))
       ;; array16 0xdc
       ((= byte #xdc)
        (let ((len (flitrpc--read-u16-at buf (1+ pos))))
          (flitrpc--read-array buf (+ pos 3) len)))
       ;; array32 0xdd
       ((= byte #xdd)
        (let ((len (flitrpc--read-u32-at buf (1+ pos))))
          (flitrpc--read-array buf (+ pos 5) len)))
       ;; map16 0xde
       ((= byte #xde)
        (let ((len (flitrpc--read-u16-at buf (1+ pos))))
          (flitrpc--read-map buf (+ pos 3) len)))
       ;; map32 0xdf
       ((= byte #xdf)
        (let ((len (flitrpc--read-u32-at buf (1+ pos))))
          (flitrpc--read-map buf (+ pos 5) len)))
       ;; negative fixint 0xe0-0xff
       ((>= byte #xe0)
        (cons (- byte 256) (1+ pos)))
       (t (error "flitrpc: unsupported msgpack type 0x%02x at pos %d" byte pos))))))

(defun flitrpc--read-str (buf pos len)
  "Read LEN-byte UTF-8 string from BUF at POS."
  (with-current-buffer buf
    (cons (decode-coding-string
           (buffer-substring-no-properties pos (+ pos len))
           'utf-8)
          (+ pos len))))

(defun flitrpc--read-bin (buf pos len)
  "Read LEN-byte binary data from BUF at POS."
  (with-current-buffer buf
    (cons (buffer-substring-no-properties pos (+ pos len))
          (+ pos len))))

(defun flitrpc--read-map (buf pos count)
  "Read COUNT key-value pairs as a plist from BUF at POS."
  (with-current-buffer buf
    (let ((result nil))
      (dotimes (_ count)
        (let* ((key-pair (flitrpc--read-msgpack buf pos))
               (key-str (car key-pair))
               (key (intern (concat ":" key-str))))
          (setq pos (cdr key-pair))
          (let ((val-pair (flitrpc--read-msgpack buf pos)))
            (setq result (plist-put result key (car val-pair)))
            (setq pos (cdr val-pair)))))
      (cons result pos))))

(defun flitrpc--read-array (buf pos count)
  "Read COUNT values as a vector from BUF at POS."
  (with-current-buffer buf
    (let ((result (make-vector count nil)))
      (dotimes (i count)
        (let ((pair (flitrpc--read-msgpack buf pos)))
          (aset result i (car pair))
          (setq pos (cdr pair))))
      (cons result pos))))

(defun flitrpc--read-u16-at (buf pos)
  "Read big-endian u16 from BUF at POS."
  (with-current-buffer buf
    (logior (ash (char-after pos) 8)
            (char-after (1+ pos)))))

(defun flitrpc--read-u32-at (buf pos)
  "Read big-endian u32 from BUF at POS."
  (with-current-buffer buf
    (logior (ash (char-after pos) 24)
            (ash (char-after (+ pos 1)) 16)
            (ash (char-after (+ pos 2)) 8)
            (char-after (+ pos 3)))))

(defun flitrpc--read-u64 (buf pos)
  "Read big-endian u64 from BUF at POS.  Return (VALUE . NEW-POS)."
  (with-current-buffer buf
    (let ((hi (flitrpc--read-u32-at buf pos))
          (lo (flitrpc--read-u32-at buf (+ pos 4))))
      (cons (logior (ash hi 32) lo) (+ pos 8)))))

(defun flitrpc--read-i64 (buf pos)
  "Read big-endian signed i64 from BUF at POS.  Return (VALUE . NEW-POS)."
  (let* ((pair (flitrpc--read-u64 buf pos))
         (v (car pair)))
    (cons (if (>= v (ash 1 63)) (- v (ash 1 64)) v) (cdr pair))))

(defun flitrpc--read-float64 (buf pos)
  "Read IEEE 754 float64 from BUF at POS.  Return (VALUE . NEW-POS)."
  (with-current-buffer buf
    (let* ((bytes (buffer-substring-no-properties pos (+ pos 8)))
           (val (string-to-number
                 (format "%S" (flitrpc--decode-float64 bytes)))))
      (cons val (+ pos 8)))))

(defun flitrpc--decode-float64 (bytes)
  "Decode 8-byte big-endian IEEE 754 double from unibyte string BYTES."
  (let* ((b0 (aref bytes 0)) (b1 (aref bytes 1))
         (b2 (aref bytes 2)) (b3 (aref bytes 3))
         (b4 (aref bytes 4)) (b5 (aref bytes 5))
         (b6 (aref bytes 6)) (b7 (aref bytes 7))
         (sign (ash b0 -7))
         (exp (logand (logior (ash b0 4) (ash b1 -4)) #x7ff))
         (frac (+ (* (logand b1 #x0f) (expt 2.0 48))
                  (* b2 (expt 2.0 40))
                  (* b3 (expt 2.0 32))
                  (* b4 (expt 2.0 24))
                  (* b5 (expt 2.0 16))
                  (* b6 (expt 2.0 8))
                  (float b7))))
    (cond
     ((= exp 0)
      (if (= frac 0.0) (if (= sign 0) 0.0 -0.0)
        (* (if (= sign 0) 1 -1) (ldexp frac (- 1 1023 52)))))
     ((= exp #x7ff)
      (if (= frac 0.0)
          (if (= sign 0) 1.0e+INF -1.0e+INF)
        0.0e+NaN))
     (t
      (* (if (= sign 0) 1 -1)
         (ldexp (+ (expt 2.0 52) frac) (- exp 1023 52)))))))


;;; Msgpack writer — serialize Elisp values to msgpack bytes

(defun flitrpc--write-msgpack (value)
  "Serialize VALUE to a msgpack unibyte string."
  (let ((parts nil))
    (flitrpc--write-value value (lambda (s) (push s parts)))
    (apply #'concat (nreverse parts))))

(defun flitrpc--write-value (value emit)
  "Serialize VALUE, calling EMIT with unibyte string fragments."
  (cond
   ((null value)
    (funcall emit (unibyte-string #xc0)))
   ((eq value t)
    (funcall emit (unibyte-string #xc3)))
   ((eq value :json-false)
    (funcall emit (unibyte-string #xc2)))
   ((integerp value)
    (flitrpc--write-int value emit))
   ((floatp value)
    (flitrpc--write-float64 value emit))
   ((and (consp value) (eq (car value) :bin))
    ;; Explicit binary data wrapper — encode as msgpack bin
    (flitrpc--write-bin-data (cdr value) emit))
   ((stringp value)
    (flitrpc--write-str value emit))
   ((vectorp value)
    (flitrpc--write-array value emit))
   ((listp value)
    (flitrpc--write-plist value emit))
   (t (error "flitrpc: cannot serialize %S" value))))

(defun flitrpc--write-int (n emit)
  "Serialize integer N."
  (cond
   ((and (>= n 0) (<= n 127))
    (funcall emit (unibyte-string n)))
   ((and (>= n -32) (< n 0))
    (funcall emit (unibyte-string (logand n #xff))))
   ((and (>= n 0) (<= n #xff))
    (funcall emit (unibyte-string #xcc n)))
   ((and (>= n 0) (<= n #xffff))
    (funcall emit (unibyte-string #xcd (ash n -8) (logand n #xff))))
   ((and (>= n 0) (<= n #xffffffff))
    (funcall emit (unibyte-string #xce
                                  (logand (ash n -24) #xff)
                                  (logand (ash n -16) #xff)
                                  (logand (ash n -8) #xff)
                                  (logand n #xff))))
   ((>= n 0)
    (funcall emit (unibyte-string #xcf))
    (flitrpc--write-u64-bytes n emit))
   ((>= n -128)
    (funcall emit (unibyte-string #xd0 (logand n #xff))))
   ((>= n -32768)
    (let ((u (logand n #xffff)))
      (funcall emit (unibyte-string #xd1 (ash u -8) (logand u #xff)))))
   ((>= n (- #x80000000))
    (let ((u (logand n #xffffffff)))
      (funcall emit (unibyte-string #xd2
                                    (logand (ash u -24) #xff)
                                    (logand (ash u -16) #xff)
                                    (logand (ash u -8) #xff)
                                    (logand u #xff)))))
   (t
    (funcall emit (unibyte-string #xd3))
    (let ((u (logand n (1- (ash 1 64)))))
      (flitrpc--write-u64-bytes u emit)))))

(defun flitrpc--write-u64-bytes (n emit)
  "Write 8 big-endian bytes for unsigned 64-bit N."
  (funcall emit (unibyte-string
                 (logand (ash n -56) #xff)
                 (logand (ash n -48) #xff)
                 (logand (ash n -40) #xff)
                 (logand (ash n -32) #xff)
                 (logand (ash n -24) #xff)
                 (logand (ash n -16) #xff)
                 (logand (ash n -8) #xff)
                 (logand n #xff))))

(defun flitrpc--write-float64 (f emit)
  "Serialize float F as float64."
  (funcall emit (unibyte-string #xcb))
  (let ((bytes (flitrpc--encode-float64 f)))
    (funcall emit bytes)))

(defun flitrpc--encode-float64 (f)
  "Encode float F as 8-byte big-endian IEEE 754 double."
  (let (sign exp frac-bits)
    (cond
     ((isnan f)
      (setq sign 0 exp #x7ff frac-bits 1))
     ((= f 1.0e+INF)
      (setq sign 0 exp #x7ff frac-bits 0))
     ((= f -1.0e+INF)
      (setq sign 1 exp #x7ff frac-bits 0))
     ((= f 0.0)
      (setq sign (if (< (copysign 1.0 f) 0) 1 0) exp 0 frac-bits 0))
     (t
      (setq sign (if (< f 0) 1 0))
      (let* ((af (abs f))
             (e (floor (log af 2)))
             (e2 (max e -1022)))
        (setq exp (+ e2 1023))
        (setq frac-bits (round (* (- (/ af (expt 2.0 e2)) 1.0) (expt 2.0 52)))))))
    (let* ((b0 (logior (ash sign 7) (ash exp -4)))
           (frac-hi (truncate (/ frac-bits (expt 2.0 32))))
           (frac-lo (truncate (mod frac-bits (expt 2.0 32))))
           (b1 (logior (ash (logand exp #xf) 4) (logand (ash frac-hi -16) #xf)))
           (b2 (logand (ash frac-hi -8) #xff))
           (b3 (logand frac-hi #xff))
           (b4 (logand (ash frac-lo -24) #xff))
           (b5 (logand (ash frac-lo -16) #xff))
           (b6 (logand (ash frac-lo -8) #xff))
           (b7 (logand frac-lo #xff)))
      (unibyte-string b0 b1 b2 b3 b4 b5 b6 b7))))

(defun flitrpc--write-bin-data (s emit)
  "Serialize unibyte string S as msgpack bin."
  (let ((len (length s)))
    (cond
     ((<= len #xff)
      (funcall emit (unibyte-string #xc4 len)))
     ((<= len #xffff)
      (funcall emit (unibyte-string #xc5 (ash len -8) (logand len #xff))))
     (t
      (funcall emit (unibyte-string #xc6
                                    (logand (ash len -24) #xff)
                                    (logand (ash len -16) #xff)
                                    (logand (ash len -8) #xff)
                                    (logand len #xff)))))
    (funcall emit s)))

(defun flitrpc--write-str (s emit)
  "Serialize string S."
  (let* ((encoded (encode-coding-string s 'utf-8))
         (len (length encoded)))
    (cond
     ((<= len 31)
      (funcall emit (unibyte-string (logior #xa0 len))))
     ((<= len #xff)
      (funcall emit (unibyte-string #xd9 len)))
     ((<= len #xffff)
      (funcall emit (unibyte-string #xda (ash len -8) (logand len #xff))))
     (t
      (funcall emit (unibyte-string #xdb
                                    (logand (ash len -24) #xff)
                                    (logand (ash len -16) #xff)
                                    (logand (ash len -8) #xff)
                                    (logand len #xff)))))
    (funcall emit encoded)))

(defun flitrpc--write-array (vec emit)
  "Serialize vector VEC as msgpack array."
  (let ((len (length vec)))
    (cond
     ((<= len 15)
      (funcall emit (unibyte-string (logior #x90 len))))
     ((<= len #xffff)
      (funcall emit (unibyte-string #xdc (ash len -8) (logand len #xff))))
     (t
      (funcall emit (unibyte-string #xdd
                                    (logand (ash len -24) #xff)
                                    (logand (ash len -16) #xff)
                                    (logand (ash len -8) #xff)
                                    (logand len #xff)))))
    (dotimes (i len)
      (flitrpc--write-value (aref vec i) emit))))

(defun flitrpc--write-plist (plist emit)
  "Serialize PLIST as msgpack map.  Keyword keys have colon stripped."
  (let* ((pairs nil)
         (pl plist))
    (while pl
      (let* ((key (car pl))
             (key-name (symbol-name key))
             (key-str (if (eq (aref key-name 0) ?:)
                          (substring key-name 1)
                        key-name)))
        (push (cons key-str (cadr pl)) pairs))
      (setq pl (cddr pl)))
    (setq pairs (nreverse pairs))
    (let ((len (length pairs)))
      (cond
       ((<= len 15)
        (funcall emit (unibyte-string (logior #x80 len))))
       ((<= len #xffff)
        (funcall emit (unibyte-string #xde (ash len -8) (logand len #xff))))
       (t
        (funcall emit (unibyte-string #xdf
                                      (logand (ash len -24) #xff)
                                      (logand (ash len -16) #xff)
                                      (logand (ash len -8) #xff)
                                      (logand len #xff)))))
      (dolist (pair pairs)
        (flitrpc--write-str (car pair) emit)
        (flitrpc--write-value (cdr pair) emit)))))


;;; Frame parser and writer

(defconst flitrpc--magic #x464C5452
  "Magic bytes \"FLTR\" as u32.")

(defconst flitrpc--header-size 8
  "Frame header size in bytes (meta_len + payload_len).")

(defconst flitrpc--type-request 1)
(defconst flitrpc--type-response 2)
(defconst flitrpc--type-notification 3)
(defconst flitrpc--type-chunk-continue 4)
(defconst flitrpc--type-chunk-end 5)

(defun flitrpc--write-frame (conn msg-type id meta &optional payload)
  "Write a frame to CONN's process.
MSG-TYPE is 1-5.  ID is the message id.  META is a plist.
PAYLOAD is an optional unibyte string of raw bytes."
  (let* ((process (flitrpc-conn-process conn))
         (full-meta (plist-put (plist-put (copy-sequence meta) :t msg-type) :id id))
         (meta-bytes (flitrpc--write-msgpack full-meta))
         (meta-len (length meta-bytes))
         (payload-len (if payload (length payload) 0))
         (header (unibyte-string
                  (logand (ash meta-len -24) #xff)
                  (logand (ash meta-len -16) #xff)
                  (logand (ash meta-len -8) #xff)
                  (logand meta-len #xff)
                  (logand (ash payload-len -24) #xff)
                  (logand (ash payload-len -16) #xff)
                  (logand (ash payload-len -8) #xff)
                  (logand payload-len #xff))))
    (process-send-string process header)
    (process-send-string process meta-bytes)
    (when payload
      (process-send-string process payload))))


;;; Connection and dispatch

(cl-defstruct flitrpc-conn
  "A flitrpc connection."
  process         ; the Emacs process (SSH, etc.)
  buffer          ; unibyte process buffer for parsing
  host            ; remote host name
  pending         ; hash table: id -> (success-fn . error-fn)
  next-id         ; next request ID counter
  notification-fn ; function called for notifications: (fn method params payload-start payload-end)
  request-fn      ; function called for server->client requests: (fn method params) -> result
  chunks          ; hash table: id -> (meta . payload-parts) for chunked reassembly
  on-shutdown)    ; function called when connection closes

(defun flitrpc-make-conn (process host &rest args)
  "Create a flitrpc connection over PROCESS for HOST.
ARGS are keyword args: :notification-fn, :request-fn, :on-shutdown."
  (let* ((buf (generate-new-buffer (format " *flitrpc-%s*" host)))
         (conn (make-flitrpc-conn
                :process process
                :buffer buf
                :host host
                :pending (make-hash-table :test 'eql)
                :next-id 0
                :notification-fn (plist-get args :notification-fn)
                :request-fn (plist-get args :request-fn)
                :chunks (make-hash-table :test 'eql)
                :on-shutdown (plist-get args :on-shutdown))))
    (with-current-buffer buf
      (set-buffer-multibyte nil))
    (set-process-buffer process buf)
    (set-process-filter process (flitrpc--make-filter conn))
    (set-process-sentinel process (flitrpc--make-sentinel conn))
    (set-process-coding-system process 'binary 'binary)
    conn))

(defun flitrpc--make-filter (conn)
  "Return a process filter function for CONN."
  (lambda (_proc data)
    (let ((buf (flitrpc-conn-buffer conn)))
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (goto-char (point-max))
          (insert data)
          (flitrpc--parse-frames conn))))))

(defun flitrpc--make-sentinel (conn)
  "Return a process sentinel function for CONN."
  (lambda (_proc event)
    (when (string-match-p "\\(?:exited\\|killed\\|finished\\|deleted\\|connection broken\\)" event)
      (when-let ((fn (flitrpc-conn-on-shutdown conn)))
        (funcall fn conn))
      ;; Fail all pending requests
      (maphash (lambda (id entry)
                 (ignore id)
                 (funcall (cdr entry) '(:message "Connection closed")))
               (flitrpc-conn-pending conn))
      (clrhash (flitrpc-conn-pending conn)))))

(defun flitrpc--parse-frames (conn)
  "Parse all complete frames from CONN's buffer."
  (let ((buf (flitrpc-conn-buffer conn)))
    (with-current-buffer buf
      (goto-char (point-min))
      (catch 'flitrpc--incomplete
        (while (>= (- (point-max) (point)) flitrpc--header-size)
          (let* ((hdr-start (point))
                 (meta-len (flitrpc--read-u32-at buf hdr-start))
                 (payload-len (flitrpc--read-u32-at buf (+ hdr-start 4)))
                 (frame-len (+ flitrpc--header-size meta-len payload-len)))
            (when (< (- (point-max) hdr-start) frame-len)
              (goto-char hdr-start)
              (throw 'flitrpc--incomplete nil))
            (let* ((meta-start (+ hdr-start flitrpc--header-size))
                   (meta-pair (flitrpc--read-msgpack buf meta-start))
                   (meta (car meta-pair))
                   (payload-start (+ meta-start meta-len))
                   (payload-end (+ payload-start payload-len)))
              (goto-char payload-end)
              (flitrpc--dispatch-frame conn meta payload-start payload-end)))))
      (delete-region (point-min) (point)))))

(defun flitrpc--dispatch-frame (conn meta payload-start payload-end)
  "Dispatch a parsed frame from CONN."
  (let ((msg-type (plist-get meta :t))
        (id (plist-get meta :id)))
    (pcase msg-type
      ;; Response
      ((pred (= flitrpc--type-response))
       (let ((err (plist-get meta :error)))
         (if err
             (flitrpc--complete-request conn id nil err)
           (flitrpc--complete-request conn id meta nil payload-start payload-end))))
      ;; Notification
      ((pred (= flitrpc--type-notification))
       (when-let ((fn (flitrpc-conn-notification-fn conn)))
         (let ((method (plist-get meta :method))
               (params (plist-get meta :params)))
           (funcall fn conn method params payload-start payload-end))))
      ;; Request (server -> client, e.g. heartbeat)
      ((pred (= flitrpc--type-request))
       (let* ((method (plist-get meta :method))
              (params (plist-get meta :params))
              (req-fn (flitrpc-conn-request-fn conn))
              (result (if req-fn (funcall req-fn conn method params) t)))
         (flitrpc--write-frame conn
                               flitrpc--type-response id
                               (list :result result))))
      ;; Chunk continue
      ((pred (= flitrpc--type-chunk-continue))
       (flitrpc--accumulate-chunk conn id payload-start payload-end))
      ;; Chunk end
      ((pred (= flitrpc--type-chunk-end))
       (flitrpc--finish-chunks conn id)))))

(defun flitrpc--complete-request (conn id meta err &optional payload-start payload-end)
  "Complete pending request ID on CONN with META/ERR."
  (when-let ((entry (gethash id (flitrpc-conn-pending conn))))
    (remhash id (flitrpc-conn-pending conn))
    (if err
        (funcall (cdr entry) err)
      (funcall (car entry) meta payload-start payload-end))))

;; Chunked message reassembly

(defun flitrpc--accumulate-chunk (conn id payload-start payload-end)
  "Accumulate a chunk for message ID."
  (let* ((chunks (flitrpc-conn-chunks conn))
         (entry (gethash id chunks)))
    (unless entry
      (error "flitrpc: chunk-continue for unknown id %d" id))
    ;; Append payload bytes to accumulation buffer
    (let ((chunk-data (buffer-substring-no-properties payload-start payload-end)))
      (setcdr entry (cons chunk-data (cdr entry))))))

(defun flitrpc--finish-chunks (conn id)
  "Reassemble and dispatch completed chunked message for ID."
  (let* ((chunks (flitrpc-conn-chunks conn))
         (entry (gethash id chunks)))
    (when entry
      (remhash id chunks)
      (let* ((meta (car entry))
             (parts (nreverse (cdr entry)))
             (full-payload (apply #'concat parts))
             ;; Insert reassembled payload into buffer so callbacks
             ;; can use insert-buffer-substring
             (buf (flitrpc-conn-buffer conn)))
        (with-current-buffer buf
          (save-excursion
            (goto-char (point-max))
            (let ((start (point)))
              (insert full-payload)
              (let ((end (point)))
                (flitrpc--dispatch-reassembled conn meta start end)))))))))

(defun flitrpc--dispatch-reassembled (conn meta payload-start payload-end)
  "Dispatch a reassembled chunked message."
  (let ((msg-type (plist-get meta :t))
        (id (plist-get meta :id)))
    (pcase msg-type
      ((pred (= flitrpc--type-response))
       (let ((err (plist-get meta :error)))
         (if err
             (flitrpc--complete-request conn id nil err)
           (flitrpc--complete-request conn id meta nil payload-start payload-end))))
      ((pred (= flitrpc--type-notification))
       (when-let ((fn (flitrpc-conn-notification-fn conn)))
         (funcall fn conn (plist-get meta :method) (plist-get meta :params)
                  payload-start payload-end))))))


;;; Public API

(defun flitrpc-request (conn method params success-fn &optional error-fn)
  "Send async request to CONN.  Return msg-id.
SUCCESS-FN is called with (meta payload-start payload-end).
ERROR-FN is called with (error-plist)."
  (let ((id (cl-incf (flitrpc-conn-next-id conn))))
    (puthash id (cons success-fn (or error-fn #'ignore))
             (flitrpc-conn-pending conn))
    (flitrpc--write-frame conn
                          flitrpc--type-request id
                          (list :method method :params params))
    id))

(defun flitrpc-request-with-payload (conn method params payload success-fn &optional error-fn)
  "Like `flitrpc-request' but include raw PAYLOAD bytes."
  (let ((id (cl-incf (flitrpc-conn-next-id conn))))
    (puthash id (cons success-fn (or error-fn #'ignore))
             (flitrpc-conn-pending conn))
    (flitrpc--write-frame conn
                          flitrpc--type-request id
                          (list :method method :params params)
                          payload)
    id))

(defun flitrpc-notify (conn method params &optional payload)
  "Send notification to CONN (fire-and-forget)."
  (flitrpc--write-frame conn
                        flitrpc--type-notification 0
                        (list :method method :params params)
                        payload))

(defun flitrpc-request-sync (conn method params &optional timeout)
  "Send request to CONN and wait for response.
TIMEOUT defaults to 5 seconds.  Returns the result meta plist.
Nested calls work: each has its own done/result variables."
  (let ((done nil)
        (result nil)
        (err nil)
        (deadline (+ (float-time) (or timeout 5)))
        (proc (flitrpc-conn-process conn)))
    (flitrpc-request conn method params
                     (lambda (meta _ps _pe) (setq result meta done t))
                     (lambda (e) (setq err e done t)))
    (while (not done)
      (when (> (float-time) deadline)
        (setq err '(:message "Timed out") done t))
      (accept-process-output proc 0.5))
    (when err
      (signal 'flitrpc-error (list err)))
    result))

(defun flitrpc-request-sync-with-payload (conn method params &optional timeout)
  "Like `flitrpc-request-sync' but also return payload region.
Returns (META PAYLOAD-START PAYLOAD-END) list."
  (let ((done nil)
        (result-meta nil)
        (result-ps nil)
        (result-pe nil)
        (err nil)
        (deadline (+ (float-time) (or timeout 5)))
        (proc (flitrpc-conn-process conn)))
    (flitrpc-request conn method params
                     (lambda (meta ps pe)
                       (setq result-meta meta result-ps ps result-pe pe done t))
                     (lambda (e) (setq err e done t)))
    (while (not done)
      (when (> (float-time) deadline)
        (setq err '(:message "Timed out") done t))
      (accept-process-output proc 0.5))
    (when err
      (signal 'flitrpc-error (list err)))
    (list result-meta result-ps result-pe)))

(define-error 'flitrpc-error "flitrpc error")

(defun flitrpc-close (conn)
  "Close CONN and clean up."
  (when-let ((proc (flitrpc-conn-process conn)))
    (when (process-live-p proc)
      (delete-process proc)))
  (when-let ((buf (flitrpc-conn-buffer conn)))
    (when (buffer-live-p buf)
      (kill-buffer buf))))

(provide 'flitrpc)

;;; flitrpc.el ends here
