;;; flit.el --- Modern remote file editing -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Muir Manders

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;; Copyright (C) 2025
;; Author: Muir Manders
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.1"))
;; Keywords: files, remote, tools
;; URL: https://github.com/muirdm/flit.el

;;; Code:

(require 'cl-lib)
(require 'flitrpc)

;; Forward declarations for variables defined later in the file.
;; These are needed because the public API section uses them before
;; their canonical defvar.
(defvar flit--connections)
(defvar flit--connection-states)
(defvar flit--sys-info)
(defvar flit--cache)
(defvar flit--tunnels)
(defvar flit--processes)
(defvar flit--process-host)
(defvar flit--home-directory-cache)
(defvar flit--deferred-buffers)
(defvar flit--current-path)
(defvar flit--force-allow-prompt)
(defvar flit--pending-restore nil
  "Non-nil while a desktop restore is pending.")

;; Forward declarations for functions defined inside with-eval-after-load.
(declare-function lsp-session "ext:lsp-mode")
(declare-function lsp-session-folders "ext:lsp-mode")
(declare-function marginalia-annotate-file "ext:marginalia")
(declare-function marginalia-annotate-buffer "ext:marginalia")
(declare-function flit--setup-desktop-save "flit")
(declare-function flit--desktop-read-advice "flit")
(declare-function flit--desktop-create-buffer-advice "flit")
(declare-function flit--lsp-mode-hook "flit")
(declare-function flit--maybe-prefetch-lsp-workspace-folders "flit")
(declare-function flit--prefetch-lsp-workspace-folders "flit")
(declare-function comint-output-filter "comint")
(declare-function shell-command-sentinel "simple")

;; Forward declarations for variables from external packages.
(defvar savehist-additional-variables)
(defvar marginalia-remote-file-regexps)
(defvar marginalia-annotators)

;;; Customization

(defgroup flit nil
  "Lightweight remote file editing."
  :group 'files
  :prefix "flit-")

(defcustom flit-connection-methods
  '((t . :ssh))
  "Alist mapping host patterns to connection methods.
Each entry is (PATTERN . METHOD) where PATTERN is a regexp string
or t (to match any host).  Entries are matched in order.

METHOD can be:

High-level modes (auto-deploy, resolve to low-level):

  :et or (:et [SPEC])
    Use ET transport with pty-bridge, password handling, and in-band deploy.
    Example: :et
    Example: (:et \"ssh-hostname\")
    Example: (:et my-transport-fn)

  :ssh or (:ssh [SPEC])
    Use SSH with BatchMode fast path.  Tries direct SSH first (no sidecar
    process); if that fails (auth needed, flit not installed), falls back
    to pty-bridge with password handling and in-band deploy.
    Example: :ssh
    Example: (:ssh \"ssh-hostname\")
    Example: (:ssh my-transport-fn)

  SPEC can be nil (use host from flit path), a string (explicit SSH hostname),
  or a function (called with HOST, returns transport args list).
  Bare keywords (e.g. :ssh) are equivalent to the list form with no spec.

Low-level modes (no auto-deploy):

  (:stdio COMMAND [:handshake FN])
    Run COMMAND as subprocess, communicate via stdin/stdout.
    COMMAND is a list of strings (program and arguments).
    Example: (:stdio (\"ssh\" \"myhost\" \"flit\" \"server\" \"--stdio\"))

  (:tcp HOST PORT [:handshake FN])
    Connect directly to HOST:PORT via TCP.
    Example: (:tcp \"127.0.0.1\" 19902)

  FUNCTION
    A function called with (HOST CALLBACK) where CALLBACK expects
    (METHOD ERROR-MSG).

The optional :handshake FN is called asynchronously before flitrpc starts.
Signature: (FN HOST PROC CALLBACK) where:
- HOST is the flit host name
- PROC is the Emacs process object
- CALLBACK expects (PROC ERROR-MSG)

On success, the handshake should call:
  (funcall CALLBACK PROC nil)
  (or a different process if the handshake creates one)
On failure, delete PROC and call:
  (funcall CALLBACK nil ERROR-MSG)

The handshake can check the `flit-allow-prompt' process property to decide
whether interactive password prompts are allowed.

Example configuration:
  (setq flit-connection-methods
        \\='((\"localhost\" . (:stdio (\"flit\" \"server\" \"--stdio\")))
          (\"devvm\" . :et)
          (\"myhost\" . (:ssh \"actual-ssh-hostname\"))
          (t . :ssh)))"
  :type '(alist :key-type (choice regexp (const t))
                :value-type (choice
                             (const :et)
                             (const :ssh)
                             (list (const :et))
                             (list (const :ssh))
                             (list (const :stdio) (repeat string))
                             (list (const :tcp) string integer)
                             function))
  :group 'flit)

(defcustom flit-timeout 5
  "Default timeout in seconds for flit RPC operations."
  :type 'integer
  :group 'flit)

(defcustom flit-log-level 'info
  "Logging verbosity level.
Levels (each includes all levels above it):
  nil   - Only errors and exceptional situations
  info  - RPC calls and significant events (default)
  debug - Handler calls, notifications from server
  trace - Cache operations and very verbose output"
  :type '(choice (const :tag "Errors only" nil)
                 (const :tag "Info (RPC calls)" info)
                 (const :tag "Debug (handlers, notifications)" debug)
                 (const :tag "Trace (cache, verbose)" trace))
  :group 'flit)

(defcustom flit-log-filter nil
  "Optional filter for log messages at all levels.
When non-nil, only log messages matching this filter are recorded.
Can be:
  - A string/regexp: only messages matching this regexp are logged
  - A function: called with the message, returns non-nil to log

Useful for debugging specific files, e.g.:
  (setq flit-log-filter \"myfile.txt\")
  (setq flit-log-filter (rx \"cache\" (or \"hit\" \"miss\")))
  (setq flit-log-filter (lambda (msg) (string-match-p \"foo\" msg)))"
  :type '(choice (const :tag "No filter" nil)
                 (string :tag "Regexp filter")
                 (function :tag "Predicate function"))
  :group 'flit)


;;; Logging

(defvar flit--log-buffer-name "*flit-log*"
  "Name of the buffer for flit debug logs.")

(defvar flit--log-level-context nil
  "Dynamically bound to the current log level context (info, debug, trace).
Used by `flit--log-should-log' to apply level-specific filtering.")

(defvar flit--log-last-msg nil
  "The last logged message (without timestamp), for repeat compression.")

(defvar flit--log-repeat-count 0
  "Count of consecutive repeats of `flit--log-last-msg'.")

(define-derived-mode flit-log-mode special-mode "Flit-Log"
  "Major mode for viewing flit debug logs.
\\{flit-log-mode-map}"
  (setq-local truncate-lines t))

(defun flit--clean-for-log (obj)
  "Recursively strip text properties from strings in OBJ for clean logging."
  (cond
   ((stringp obj) (substring-no-properties obj))
   ((consp obj) (cons (flit--clean-for-log (car obj))
                      (flit--clean-for-log (cdr obj))))
   ((vectorp obj) (apply #'vector (mapcar #'flit--clean-for-log obj)))
   (t obj)))

(defun flit--log-fn-label (fn)
  "Return a short label for FN suitable for logging."
  (cond
   ((null fn) nil)
   ((symbolp fn) (symbol-name fn))
   (t "<lambda>")))

(defun flit--log-should-log (msg _level)
  "Return non-nil if MSG should be logged.
Respects `flit-log-filter' when set.  MSG is the formatted string including the
\"[flit]\" prefix."
  (cond
   ((null flit-log-filter) t)
   ((stringp flit-log-filter)
    (string-match-p flit-log-filter msg))
   ((functionp flit-log-filter)
    (funcall flit-log-filter msg))
   (t t)))

(defun flit--log (format-string &rest args)
  "Log a message to the flit log buffer if `flit-log-level' is non-nil.
Consecutive identical messages are compressed with a repeat count."
  (when flit-log-level
    (let* ((time (current-time))
           (ms (/ (nth 2 time) 1000))
           (msg (apply #'format (concat "[flit] " format-string)
                       (mapcar #'flit--clean-for-log args))))
      (when (flit--log-should-log msg flit--log-level-context)
        (with-current-buffer (get-buffer-create flit--log-buffer-name)
          (unless (derived-mode-p 'flit-log-mode)
            (flit-log-mode)
            ;; Ensure local default-directory so log buffer isn't a "remote" buffer
            (setq default-directory (expand-file-name "~/")))
          (let ((inhibit-read-only t))
            (if (and flit--log-last-msg
                     (string= msg flit--log-last-msg))
                ;; Same message - increment counter and update in place
                (progn
                  (setq flit--log-repeat-count (1+ flit--log-repeat-count))
                  (save-excursion
                    (goto-char (point-max))
                    (forward-line -1)
                    (end-of-line)
                    (when (looking-back " ([0-9]+x)" (line-beginning-position))
                      (delete-region (match-beginning 0) (match-end 0)))
                    (insert (format " (%dx)" (1+ flit--log-repeat-count)))))
              ;; Different message - log new line
              (setq flit--log-last-msg msg
                    flit--log-repeat-count 0)
              (save-excursion
                (goto-char (point-max))
                (insert (format-time-string "%H:%M:%S" time)
                        (format ".%03d " ms)
                        msg "\n")))))))))

(defsubst flit--log-level-p (level)
  "Return non-nil if LEVEL should be logged given current `flit-log-level'."
  ;; Most common: log-level=info, level=trace -> return nil fast
  (and flit-log-level
       (cond
        ((eq flit-log-level 'trace) t)
        ((eq level 'info) t)  ; info logged at any non-nil level
        ((eq flit-log-level 'debug) (eq level 'debug)))))

(defmacro flit--log-info (format-string &rest args)
  "Log an info-level message (RPC calls, significant events).
Arguments are not evaluated if info logging is disabled."
  `(when (flit--log-level-p 'info)
     (let ((flit--log-level-context 'info))
       (flit--log ,format-string ,@args))))

(defmacro flit--log-debug (format-string &rest args)
  "Log a debug-level message (handlers, notifications).
Arguments are not evaluated if debug logging is disabled."
  `(when (flit--log-level-p 'debug)
     (let ((flit--log-level-context 'debug))
       (flit--log ,format-string ,@args))))

(defmacro flit--log-trace (format-string &rest args)
  "Log a trace-level message (cache ops, very verbose).
Arguments are not evaluated if trace logging is disabled."
  `(when (flit--log-level-p 'trace)
     (let ((flit--log-level-context 'trace))
       (flit--log ,format-string ,@args))))

(defmacro flit--log-error (format-string &rest args)
  "Log an error message (always logged regardless of level)."
  `(flit--log (concat "ERROR: " ,format-string) ,@args))

(defun flit--sanitize-params-for-log (params)
  "Return a copy of PARAMS with large :content values truncated for logging."
  (let ((content (plist-get params :content)))
    (if (and content (stringp content) (> (length content) 100))
        (plist-put (copy-sequence params) :content
                   (format "<%d bytes>" (length content)))
      params)))

(defun flit--simple-backtrace ()
  "Return a simple backtrace string with just function names.
Excludes special forms and macros to show only actual function calls."
  (let ((frames nil)
        (n 0))
    (while (and (< n 60)
                (let ((frame (backtrace-frame n)))
                  (when frame
                    (let ((fn (cadr frame)))
                      (when (and fn (symbolp fn)
                                 (not (special-form-p fn))
                                 (not (macrop fn))
                                 (not (string-prefix-p "flit--log" (symbol-name fn)))
                                 (not (string-prefix-p "flit--simple" (symbol-name fn))))
                        (push (symbol-name fn) frames)))
                    t)))
      (cl-incf n))
    (string-join (nreverse frames) " <- ")))

(defmacro flit--with-quit-log (desc &rest body)
  "Execute BODY.  If interrupted by C-g, log DESC with backtrace, then re-quit."
  (declare (indent 1))
  `(condition-case nil
       (progn ,@body)
     (quit
      (flit--log-info "C-g interrupted: %s\n  backtrace: %s"
                      ,desc (flit--simple-backtrace))
      (signal 'quit nil))))

;;; Public API

(defvar flit-known-hosts nil
  "List of known flit host names for completion.")

(with-eval-after-load 'savehist
  (add-to-list 'savehist-additional-variables 'flit-known-hosts))

;;;###autoload
(defun flit-connect (host &optional callback)
  "Establish a flit connection to HOST.
Use this to connect before opening files, especially when authentication
prompts (like duo) would interfere with minibuffer input.
If already connected, this is a no-op.  If failed, this retries.

If CALLBACK is provided, connect asynchronously:
- Returns immediately after starting the connection process
- Does not prompt for authentication (fails if auth is needed)
- Calls CALLBACK with (HOST SUCCESS ERROR-MSG) when done

If CALLBACK is nil, connect synchronously:
- Blocks until connection completes or fails
- May prompt for password if needed"
  (interactive
   (let* ((file (buffer-file-name))
          (current-host (and file
                             (flit--file-name-p file)
                             (flit--host file)))
          (candidates (cl-remove-if
                       (lambda (host)
                         (and (eq (flit--connection-state host) 'connected)
                              (flit--connection-alive-p host)))
                       (hash-table-keys flit--connection-states)))
          (input (completing-read "Connect to host: "
                                  candidates
                                  nil nil
                                  current-host)))
     ;; Interactive calls are always sync (no callback)
     (list (if (string-empty-p input)
               (user-error "No host specified")
             input))))
  ;; Sync flit-connect always allows prompting (user explicitly asked to connect).
  ;; Async (with callback) does not — it's used by background prefetch etc.
  (let ((state (flit--connection-state host))
        (flit--force-allow-prompt (not callback)))
    (cond
     ;; Already connected and alive - nothing to do
     ((and (eq state 'connected) (flit--connection-alive-p host))
      (if callback
          (funcall callback host t nil)
        (message "[flit] Already connected to %s" host)))
     ;; Otherwise, clean up and connect
     (t
      (flit--close-connection host)
      (flit--cache-invalidate-host host)
      (if callback
          (flit--do-connect
           host
           (lambda (host success error-msg)
             (if success
                 (flit--log-info "Async connected to %s" host)
               (flit--log-info "Async connection to %s failed: %s" host error-msg))
             (funcall callback host success error-msg)))
        (flit--do-connect host)
        (message "[flit] Connected to %s" host))))))

;;;###autoload
(defun flit-disconnect (host)
  "Disconnect from HOST, closing the connection.
Uses `disconnected' state - explicit user action (find-file,
flit-connect) needed to reconnect."
  (interactive
   (list (completing-read "Disconnect from host: "
                          (hash-table-keys flit--connections)
                          nil t)))
  (flit--close-connection host)
  (flit--cache-invalidate-host host)
  ;; Use 'disconnected state - user explicitly disconnected
  (flit--set-connection-state host 'disconnected)
  (message "[flit] Disconnected from %s" host))

;;;###autoload
(defun flit-status ()
  "Display status of all flit connections in a buffer."
  (interactive)
  (let ((buf (get-buffer-create "*flit-status*"))
        (hosts (delete-dups
                (append (hash-table-keys flit--connections)
                        (hash-table-keys flit--connection-states))))
        (cache-stats (flit--cache-memory-stats)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "Flit Status\n")
        (insert "===========\n\n")
        (if (null hosts)
            (insert "No connections\n")
          (dolist (host hosts)
            (let* ((state (flit--connection-state host))
                   (conn (gethash host flit--connections))
                   (alive (and conn (flit--connection-alive-p host)))
                   (proc (and conn (flit-conn-process conn)))
                   (method-chain (and (processp proc) (process-get proc 'flit-method)))
                   (sys-info (gethash host flit--sys-info))
                   (os (plist-get sys-info :os))
                   (hostname (plist-get sys-info :hostname))
                   (stats (cdr (assoc host cache-stats)))
                   (files (or (plist-get stats :files) 0))
                   (dirs (or (plist-get stats :dirs) 0))
                   (file-bytes (or (plist-get stats :file-bytes) 0))
                   (dir-bytes (or (plist-get stats :dir-bytes) 0)))
              (insert (format "%s:\n" host))
              (insert (format "  State: %s%s\n" state (if alive " (alive)" "")))
              (when method-chain
                (insert (format "  Connection: %s\n" method-chain)))
              (when hostname
                (insert (format "  Remote: %s %s\n" hostname (or os ""))))
              (insert (format "  Cache: %d files (%s), %d dirs (%s)\n"
                              files (flit--format-bytes file-bytes)
                              dirs (flit--format-bytes dir-bytes)))
              (insert "\n"))))
        (goto-char (point-min))
        (flit-status-mode)))
    (pop-to-buffer buf)))

;;;###autoload
(defun flit-clear-cache (&optional host)
  "Clear flit cache.
If HOST is provided, clear only entries for that host.
Otherwise, clear all cache entries.
Also notifies connected servers to clear their cache state."
  (interactive)
  (if host
      (progn
        (flit--cache-invalidate-host host)
        ;; Notify server to clear its cache state
        (when (gethash host flit--connections)
          (flit--with-quit-log (format "clearCache RPC to %s" host)
            (condition-case nil
                (flit--send-request host "session/clearCache" nil 5)
              (error nil)))))
    (let ((count (hash-table-count flit--cache)))
      (clrhash flit--cache)
      ;; Notify all connected servers
      (maphash (lambda (host _conn)
                 (flit--with-quit-log (format "clearCache RPC to %s" host)
                   (condition-case nil
                       (flit--send-request host "session/clearCache" nil 5)
                     (error nil))))
               flit--connections)
      (message "[flit] Cleared %d cache entries" count))))

;;;###autoload
(defun flit-clear-all-state ()
  "Clear all flit state - connections, caches, tunnels, and control sockets."
  (interactive)
  ;; Close all tunnels (before closing connections)
  (maphash (lambda (_tunnel-id tunnel)
             (ignore-errors
               (when-let ((listener (plist-get tunnel :listener)))
                 (when (process-live-p listener)
                   (delete-process listener)))
               (maphash (lambda (_conn-id proc)
                          (when (process-live-p proc)
                            (delete-process proc)))
                        (plist-get tunnel :connections))))
           flit--tunnels)
  (clrhash flit--tunnels)
  ;; Close all connections - this also cleans up associated processes
  (dolist (host (hash-table-keys flit--connections))
    (ignore-errors (flit--close-connection host)))
  (clrhash flit--connections)
  ;; Mark all known hosts as disconnected before clearing, so that
  ;; buffers referencing flit paths don't trigger reconnection (pending
  ;; is the default for unknown hosts and allows auto-connect).
  (let ((hosts (hash-table-keys flit--connection-states)))
    (clrhash flit--connection-states)
    (dolist (host hosts)
      (flit--set-connection-state host 'disconnected)))
  (clrhash flit--sys-info)
  (clrhash flit--cache)
  (clrhash flit--processes)
  (clrhash flit--process-host)
  (clrhash flit--home-directory-cache)
  (clrhash flit--deferred-buffers)
  (setq flit--pending-restore nil)
  (setq flit-known-hosts nil)
  (message "[flit] State cleared"))

;;;###autoload
(defun flit-open-frame (host &optional dir)
  "Open a new frame dedicated to HOST.
The frame's default-directory is set to DIR on HOST.
DIR defaults to the home directory. DIR can start with ~ which
expands to the home directory (e.g., \"~/projects\").

The frame opens with a scratch buffer, and all file operations
in this frame will use the flit connection to HOST."
  (interactive
   (list (read-string "Host: " "localhost")))
  (let* ((expanded-dir (flit--expand-home host dir))
         (flit-path (flit--format-path host (file-name-as-directory expanded-dir)))
         (frame (make-frame `((title . ,(format "flit: %s" host))))))
    (select-frame-set-input-focus frame)
    (let ((buf (get-buffer-create (format "*scratch [%s]*" host))))
      (switch-to-buffer buf)
      (setq default-directory flit-path)
      (when (= (buffer-size) 0)
        (insert (format ";; Flit scratch buffer for %s\n;; default-directory: %s\n\n"
                        host flit-path))
        (set-buffer-modified-p nil))
      (lisp-interaction-mode))
    frame))

;;; Internal variables

(defvar flit--home-directory-cache (make-hash-table :test 'equal)
  "Cache of home directories per host.")

(defun flit--get-home-directory (host)
  "Get the home directory on HOST.
Uses cached value if available, otherwise queries the server."
  (or (gethash host flit--home-directory-cache)
      (let* ((result (flit--send-request host "sys/info" nil))
             (home (plist-get result :homeDir)))
        (when (and home (not (string-empty-p home)))
          (puthash host home flit--home-directory-cache))
        (or home (format "/home/%s" (user-login-name))))))

(defun flit--expand-home (host dir)
  "Expand ~ in DIR to the home directory on HOST.
Returns the expanded path."
  (let ((home (flit--get-home-directory host)))
    (if (and dir (string-prefix-p "~" dir))
        (concat home (substring dir 1))
      (or dir home))))

(defvar flit-status-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "q" #'quit-window)
    (define-key map "g" #'flit-status)
    map)
  "Keymap for `flit-status-mode'.")

(define-derived-mode flit-status-mode special-mode "Flit-Status"
  "Major mode for displaying flit connection status.
\\{flit-status-mode-map}")

(defvar flit--connections (make-hash-table :test 'equal)
  "Hash table mapping host to flit-connection object.")

(defvar flit--connection-states (make-hash-table :test 'equal)
  "Hash table mapping host to connection state.
Values: `pending', `connecting', `connected', `failed', or `disconnected'.
- `pending': initial state, never connected
- `connecting': connection attempt in progress
- `connected': live connection exists
- `failed': connection attempt failed
- `disconnected': user explicitly ran flit-disconnect")

(defun flit--known-hosts ()
  "Return list of all known flit hosts."
  (delete-dups
   (append (copy-sequence flit-known-hosts)
           (hash-table-keys flit--connection-states))))

(defvar flit--connection-tier nil
  "Current connection tier: `connect' or `passive'.
Bound by callers so connection functions know the context.
- `connect': Attempt sync connection (find-file, dired minibuffer)
- `passive': Only use existing live connections, never initiate new ones
Defaults to `passive' when unbound.")

(defvar flit--after-connect-functions nil
  "Functions to run after a flit connection is established.
Each function is called with one argument: HOST (the host that connected).
Use `flit-run-after-connect' to register functions.")

(defun flit--run-after-connect-functions (host)
  "Run after-connect functions for HOST.
Each function is called with HOST as the argument.
Errors are caught and logged."
  (when flit--after-connect-functions
    (flit--log-info "Running %d after-connect functions for %s"
                    (length flit--after-connect-functions) host)
    (dolist (fn flit--after-connect-functions)
      (condition-case err
          (funcall fn host)
        (error
         (flit--log-error "After-connect function error (%s): %s"
                          fn (error-message-string err)))))))

(defun flit-run-after-connect (fn)
  "Register FN to run after flit connections are established.
FN is called with one argument: HOST (the connected host).
If any hosts are already connected, FN is called immediately for each.
FN is also registered to run for future connections."
  (add-hook 'flit--after-connect-functions fn)
  ;; Run immediately for any already-connected hosts
  (maphash (lambda (host _conn)
             (when (flit--connection-alive-p host)
               (condition-case err
                   (funcall fn host)
                 (error
                  (flit--log-error "After-connect function error (%s): %s"
                                   fn (error-message-string err))))))
           flit--connections))

(defvar flit--sys-info (make-hash-table :test 'equal)
  "Hash table mapping host to sys/info response.
Contains :os, :arch, :homeDir, :hostname, :username, :path, :pid.")

(defconst flit--sep ?!
  "Separator between method and host in flit paths.")

(defconst flit--prefix (concat "/flit" (char-to-string flit--sep))
  "Canonical flit prefix, e.g. \"/flit!\".")

(defconst flit--prefix-length (length flit--prefix)
  "Length of `flit--prefix'.")

(defconst flit--short-prefix (concat "/" (char-to-string flit--sep))
  "Short flit prefix, e.g. \"/!\".")

(defconst flit--file-name-regexp
  (concat "\\`/\\(?:flit\\)?"
          (regexp-quote (char-to-string flit--sep))
          "\\([^/:~]+\\)\\(:[0-9]+\\)?\\([/~].*\\)\\'")
  "Regexp matching flit file names (both canonical and short form).
Group 1: host, Group 2: optional :port, Group 3: path (/ or ~).

Path is required — this won't match while the user is still typing
the host, avoiding premature connections.

Examples:
  /flit!dev/home/user  - host with absolute path
  /flit!dev~/foo       - host with home-relative path
  /flit!dev:9999/path  - host:port with path
  /flit!dev            - does NOT match (no path yet)")

;;; Caching
;;
;; We use a single unified cache keyed by (host . path).
;; Each cache entry contains the full file info from fs/info.
;; The cache is updated by:
;; 1. RPC calls to fs/info
;; 2. fs/changed notifications from the server (push updates)
;; Cache memory monitoring - logs size every 10 minutes if grown significantly.

(defvar flit--cache (make-hash-table :test 'equal)
  "Cache for file info. Keys are (host . path), values are info plists.")

(defvar flit--cache-last-reported-size (make-hash-table :test 'equal)
  "Last reported cache size per host, for change detection.")

(defvar flit--cache-monitor-timer nil
  "Timer for periodic cache size monitoring.")

(defun flit--normalize-path (path)
  "Normalize PATH by removing trailing slashes (except for root)."
  (if (and path
           (> (length path) 1)
           (string-suffix-p "/" path))
      (substring path 0 -1)
    path))

(defun flit--cache-key (host path)
  "Create a cache key for HOST and PATH."
  (cons host (flit--normalize-path path)))

(defun flit--cache-get (host path)
  "Get cached info for HOST PATH, or nil if not cached."
  (let* ((normalized (flit--normalize-path path))
         (result (gethash (flit--cache-key host normalized) flit--cache)))
    (when (flit--log-level-p 'trace)
      (flit--log-trace "Cache GET %s: %s (stat=%s children=%s content=%s)"
                       normalized
                       (if result "HIT" "MISS")
                       (if (plist-get result :exists) "y" "n")
                       (if (plist-get result :children) "y" "n")
                       (if (plist-get result :content) "y" "n")))
    result))

(defun flit--cache-put (host path info)
  "Cache INFO for HOST PATH.
Don't overwrite if existing entry has more complete data (e.g., has children).
Rejects entries missing required :path field (indicates server bug).
Decodes base64 content on cache so it's ready for use."
  (let* ((normalized (flit--normalize-path path))
         (key (flit--cache-key host normalized))
         (existing (gethash key flit--cache)))
    ;; Validate required fields - :path is needed for file-truename to work
    (if (not (plist-get info :path))
        (flit--log-error "cache-put: rejecting entry for %s - missing :path field (keys: %s)"
                         normalized (mapcar #'car (seq-partition (copy-sequence info) 2)))
      ;; Don't overwrite if existing has children and new one doesn't
      (unless (and existing
                   (plist-get existing :children)
                   (not (plist-get info :children)))
        ;; Content arrives as raw bytes from msgpack — no decoding needed
        (when (flit--log-level-p 'trace)
          (flit--log-trace "Cache PUT %s (stat=%s children=%s content=%s realpath=%s)"
                           normalized
                           (if (eq (plist-get info :exists) t) "y" "n")
                           (if (plist-get info :children) "y" "n")
                           (if (plist-get info :content) "y" "n")
                           (plist-get info :realpath)))
        (puthash key info flit--cache)))))

(defun flit--cache-invalidate (host path)
  "Invalidate cache entry for HOST PATH."
  (remhash (flit--cache-key host path) flit--cache))

(defun flit--cache-invalidate-host (host)
  "Invalidate all cache entries for HOST."
  (let ((keys-to-remove '()))
    (maphash (lambda (key _value)
               (when (equal (car key) host)
                 (push key keys-to-remove)))
             flit--cache)
    (dolist (key keys-to-remove)
      (remhash key flit--cache))
    ;; Reset reported size when cache is significantly emptied
    (when (> (length keys-to-remove) 0)
      (remhash host flit--cache-last-reported-size))
    (flit--log-info "Cleared %d cache entries for %s" (length keys-to-remove) host)))

(defun flit--estimate-object-size (obj)
  "Estimate memory size of OBJ in bytes, recursively."
  (cond
   ((null obj) 0)
   ((stringp obj) (+ 32 (length obj)))  ; header + characters
   ((symbolp obj) (+ 32 (length (symbol-name obj))))
   ((numberp obj) 8)
   ((consp obj)
    ;; Cons cell (16 bytes) + car + cdr
    (+ 16
       (flit--estimate-object-size (car obj))
       (flit--estimate-object-size (cdr obj))))
   ((vectorp obj)
    ;; Vector header + elements
    (let ((size 32))
      (dotimes (i (length obj))
        (cl-incf size (flit--estimate-object-size (aref obj i))))
      size))
   ((hash-table-p obj)
    ;; Hash table base + per-entry overhead
    (+ 100 (* 50 (hash-table-count obj))))
   (t 8)))  ; unknown type, guess small

(defun flit--cache-memory-stats ()
  "Calculate approximate memory usage of the cache per host.
Returns alist of (host . plist) where plist has:
  :files :dirs - entry counts by type
  :file-bytes :dir-bytes - memory sizes by type"
  (let ((stats (make-hash-table :test 'equal))
        ;; Hash table overhead: ~50 bytes per entry
        (hash-entry-overhead 50))
    (maphash
     (lambda (key value)
       (let* ((host (car key))
              (_path (cdr key))
              (is-dir (plist-get value :children))
              (s (or (gethash host stats)
                     (puthash host (list :files 0 :dirs 0 :file-bytes 0 :dir-bytes 0) stats)))
              ;; Entry size = hash overhead + key + value
              (entry-size (+ hash-entry-overhead
                             (flit--estimate-object-size key)
                             (flit--estimate-object-size value))))
         ;; Count and accumulate by type
         (if is-dir
             (progn
               (cl-incf (plist-get s :dirs))
               (cl-incf (plist-get s :dir-bytes) entry-size))
           (cl-incf (plist-get s :files))
           (cl-incf (plist-get s :file-bytes) entry-size))))
     flit--cache)
    (let (result)
      (maphash (lambda (host plist) (push (cons host plist) result)) stats)
      result)))

(defun flit--format-bytes (bytes)
  "Format BYTES as human-readable string."
  (cond
   ((>= bytes (* 1024 1024 1024)) (format "%.1fGB" (/ bytes (* 1024.0 1024 1024))))
   ((>= bytes (* 1024 1024)) (format "%.1fMB" (/ bytes (* 1024.0 1024))))
   ((>= bytes 1024) (format "%.1fKB" (/ bytes 1024.0)))
   (t (format "%dB" bytes))))

(defun flit--cache-monitor ()
  "Check cache size and log if significantly grown."
  (dolist (entry (flit--cache-memory-stats))
    (let* ((host (car entry))
           (stats (cdr entry))
           (total (+ (plist-get stats :file-bytes) (plist-get stats :dir-bytes)))
           (last-size (gethash host flit--cache-last-reported-size 0)))
      ;; Only log if more than 2x previous size (and at least 1MB to avoid noise)
      (when (and (> total (* 2 last-size))
                 (> total (* 1024 1024)))
        (message "[flit] Cache for %s: %d files (%s), %d dirs (%s)"
                 host
                 (plist-get stats :files)
                 (flit--format-bytes (plist-get stats :file-bytes))
                 (plist-get stats :dirs)
                 (flit--format-bytes (plist-get stats :dir-bytes)))
        (puthash host total flit--cache-last-reported-size)))))

(defun flit--start-cache-monitor ()
  "Start the cache monitoring timer."
  (unless flit--cache-monitor-timer
    (setq flit--cache-monitor-timer
          (run-with-timer 600 600 #'flit--cache-monitor))))

(defun flit--stop-cache-monitor ()
  "Stop the cache monitoring timer."
  (when flit--cache-monitor-timer
    (cancel-timer flit--cache-monitor-timer)
    (setq flit--cache-monitor-timer nil)))

;; Start monitor when flit is loaded
(flit--start-cache-monitor)

(defun flit--cache-from-response (host response)
  "Process unified cache entries from RESPONSE for HOST.
RESPONSE may contain a :cache field with a list of cache entries.
Each entry has :path and :info fields."
  (let ((cache (plist-get response :cache)))
    (flit--log-trace "cache-from-response: cache field = %s" (if cache "present" "nil"))
    (when cache
      (seq-doseq (entry cache)
        (let* ((path (plist-get entry :path))
               (info (plist-get entry :info))
               (exists-val (plist-get info :exists))
               (parent-watched (plist-get info :parentWatched)))
          (flit--log-trace "cache-from-response: path=%s exists=%S parent-watched=%S"
                           path exists-val parent-watched)
          (when (and path info)
            ;; Skip caching for volatile paths (server sets noCache flag)
            (unless (eq (plist-get info :noCache) t)
              ;; Always cache the result - watch-based invalidation handles staleness
              ;; This prevents repeated RPCs for the same path within a short window
              (flit--cache-put host path info))))))))

(defun flit--find-entry (parent-info basename)
  "Check if BASENAME exists in PARENT-INFO's children list.
PARENT-INFO is a cache entry plist with :children (list of {name, isDir}).
Returns the child entry if found, nil otherwise."
  (let ((children (plist-get parent-info :children)))
    (when children
      (seq-find (lambda (child)
                  (string= (plist-get child :name) basename))
                children))))

;;; Async process management
;;
;; Track async processes started via start-file-process.
;; Maps procID (from server) to Emacs process object.

(defvar flit--processes (make-hash-table :test 'equal)
  "Hash table mapping server procID to Emacs pipe process.")

(defvar flit--process-host (make-hash-table :test 'equal)
  "Hash table mapping server procID to host.")

(defvar flit--proc-id-counter 0
  "Counter for generating unique client-side process IDs.
These IDs are unique across server restarts, unlike server-generated IDs.")

(defun flit--generate-proc-id ()
  "Generate a unique process ID for use with exec/start.
Uses Emacs PID and counter to ensure uniqueness across sessions."
  (setq flit--proc-id-counter (1+ flit--proc-id-counter))
  (format "emacs-%d-%d" (emacs-pid) flit--proc-id-counter))

(defun flit--compute-env-delta ()
  "Compute environment variable delta to send to remote.
Returns an alist of (NAME . VALUE) for variables that have been
added or modified compared to the default `process-environment'.
This follows TRAMP's approach of only sending the difference."
  (let ((default-env (default-toplevel-value 'process-environment))
        (delta nil))
    (dolist (elt process-environment)
      (unless (member elt default-env)
        (when (string-match "\\`\\([^=]+\\)=\\(.*\\)\\'" elt)
          (push (cons (match-string 1 elt) (match-string 2 elt)) delta))))
    (nreverse delta)))

;; Advise process-status to return expected values for flit processes.
;; Pipe processes return 'open instead of 'run, but VC expects 'run.
;; After exit, we return 'exit based on our stored flag.
(defun flit--process-status-advice (orig-fn proc)
  "Return expected status for flit processes."
  (if (and (processp proc)
           (process-get proc 'flit-proc-id))
      ;; This is a flit process
      (cond
       ((process-get proc 'flit-exited) 'exit)
       ;; Pipe processes show 'open, but VC expects 'run
       ((memq (funcall orig-fn proc) '(open run)) 'run)
       (t (funcall orig-fn proc)))
    (funcall orig-fn proc)))

(advice-add 'process-status :around #'flit--process-status-advice)

;; Advise process-exit-status to return the stored exit code for flit processes.
(defun flit--process-exit-status-advice (orig-fn proc)
  "Return stored exit code for flit processes."
  (if (and (processp proc)
           (process-get proc 'flit-exited))
      (or (process-get proc 'flit-exit-code) 0)
    (funcall orig-fn proc)))

(advice-add 'process-exit-status :around #'flit--process-exit-status-advice)

;; Advise process-send-string to send input to flit processes via exec/input.
(defun flit--process-valid-for-rpc-p (proc operation)
  "Check if PROC is a valid flit process for RPC OPERATION.
Returns the proc-id if valid, nil otherwise.
Logs a message if the process is not valid for RPC.
IMPORTANT: This function must NOT trigger reconnection - it uses direct
hash lookup instead of flit--get-connection to avoid side effects."
  (when (and (processp proc) (process-get proc 'flit-proc-id))
    (let ((proc-id (process-get proc 'flit-proc-id)))
      (cond
       ((process-get proc 'flit-exited)
        (flit--log-info "Ignoring %s to flit process %s: process has exited" operation proc-id)
        nil)
       (t
        (let* ((host (process-get proc 'flit-host))
               (orig-conn-proc (process-get proc 'flit-conn-proc))
               ;; Direct lookup - do NOT use flit--get-connection which triggers reconnect
               (curr-conn (and host (gethash host flit--connections)))
               (curr-conn-proc (and curr-conn (flit-conn-process curr-conn))))
          (cond
           ((null orig-conn-proc)
            ;; Old process without conn tracking - allow only if connection is alive
            (if (and curr-conn-proc (process-live-p curr-conn-proc))
                proc-id
              (flit--log-info "Ignoring %s to flit process %s: connection is dead" operation proc-id)
              nil))
           ((and (eq orig-conn-proc curr-conn-proc)
                 (process-live-p curr-conn-proc))
            ;; Same connection and still alive
            proc-id)
           ((eq orig-conn-proc curr-conn-proc)
            ;; Same connection but dead - don't allow, don't trigger reconnect
            (flit--log-info "Ignoring %s to flit process %s: connection is dead" operation proc-id)
            nil)
           (t
            (flit--log-info "Ignoring %s to flit process %s: connection has changed" operation proc-id)
            nil))))))))

(defun flit--process-send-string-advice (orig-fn proc string)
  "Send STRING to flit process PROC via exec/input, or call ORIG-FN."
  (if-let* ((proc-id (flit--process-valid-for-rpc-p proc "send"))
            (host (process-get proc 'flit-host)))
      (let (;; Ensure multibyte string for msgpack encoding.
            ;; LSP messages containing non-ASCII may be unibyte if
            ;; lsp-mode encoded them for the wire.
            (data (if (multibyte-string-p string)
                      string
                    (decode-coding-string string 'utf-8-unix))))
        (flit--log-debug "process-send-string: proc-id=%s len=%d"
                         proc-id (length string))
        (flit--send-notify host "exec/input"
                           `(:procId ,proc-id :data ,data)))
    (funcall orig-fn proc string)))

(advice-add 'process-send-string :around #'flit--process-send-string-advice)

;; Advise process-send-eof to close stdin for flit processes.
(defun flit--process-send-eof-advice (orig-fn &optional proc)
  "Close stdin for flit process PROC, or call ORIG-FN."
  (if-let* ((proc (or proc (get-buffer-process (current-buffer))))
            (proc-id (flit--process-valid-for-rpc-p proc "send-eof"))
            (host (process-get proc 'flit-host)))
      (flit--send-request-async host "exec/close-input"
                                `(:procId ,proc-id))
    (funcall orig-fn proc)))

(advice-add 'process-send-eof :around #'flit--process-send-eof-advice)

;; Advise accept-process-output to handle flit processes.
;; Our pipe processes don't receive output directly - output comes via flitrpc
;; notifications on the flit connection. So we poll the connection until
;; output arrives for the specific process we're waiting on.
(defun flit--accept-process-output-advice (orig-fn &optional process timeout timeout-msecs just-this-one)
  "Handle accept-process-output for flit processes.
For flit processes, poll the flitrpc connection until this specific
process receives output, or until timeout expires."
  (if (and (processp process)
           (process-get process 'flit-proc-id))
      ;; This is a flit process - poll connection until THIS process gets output
      (let* ((host (process-get process 'flit-host))
             (conn (condition-case nil
                       (and host (flit--get-connection host))
                     (error nil)))
             (conn-proc (and conn (flit-conn-process conn))))
        (if conn-proc
            ;; Flit process with active connection - poll until THIS process gets output
            (let* ((output-count-before (or (process-get process 'flit-output-count) 0))
                   (timeout-secs (when (or timeout timeout-msecs)
                                   (+ (or timeout 0) (/ (or timeout-msecs 0) 1000.0))))
                   (deadline (when (and timeout-secs (> timeout-secs 0))
                               (+ (float-time) timeout-secs)))
                   (poll-interval (if (and timeout-secs (= timeout-secs 0)) 0 0.001))
                   (got-output nil))
              (if (and timeout-secs (= timeout-secs 0))
                  ;; Non-blocking: just one check
                  (progn
                    (funcall orig-fn conn-proc 0 nil conn-proc)
                    (setq got-output (> (or (process-get process 'flit-output-count) 0)
                                        output-count-before)))
                ;; Blocking: poll until this process gets output, exits, or deadline
                (flit--with-quit-log
                    (format "accept-process-output for %s" (process-name process))
                  (while (and (not got-output)
                              (not (process-get process 'flit-exited))
                              (or (null deadline) (< (float-time) deadline)))
                    (funcall orig-fn conn-proc poll-interval nil conn-proc)
                    (when (> (or (process-get process 'flit-output-count) 0)
                             output-count-before)
                      (setq got-output t)))))
              got-output)
          ;; No connection available - fall through to default
          (funcall orig-fn process timeout timeout-msecs just-this-one)))
    ;; Not a flit process - use default, then process pending timers.
    ;; Not a flit process - use default
    (funcall orig-fn process timeout timeout-msecs just-this-one)))

(advice-add 'accept-process-output :around #'flit--accept-process-output-advice)

(defun flit--non-essential-advice (orig-fn &rest args)
  "Bind `non-essential' to t so flit uses passive connection tier.
Used as :around advice for timer-driven functions like auto-revert,
eldoc, flymake, and flycheck to prevent them from initiating new connections."
  (let ((non-essential t))
    (apply orig-fn args)))

(advice-add 'auto-revert-handler :around #'flit--non-essential-advice)
(advice-add 'eldoc-print-current-symbol-info :around #'flit--non-essential-advice)
(advice-add 'flymake-start :around #'flit--non-essential-advice)
(advice-add 'flycheck--handle-idle-trigger :around #'flit--non-essential-advice)

(defun flit--signal-name (signal)
  "Convert SIGNAL (symbol or number) to a flit signal name string."
  (cond
   ((eq signal 'SIGINT) "int")
   ((eq signal 'SIGTERM) "term")
   ((eq signal 'SIGKILL) "kill")
   ((eq signal 'SIGHUP) "hup")
   ((eq signal 2) "int")
   ((eq signal 15) "term")
   ((eq signal 9) "kill")
   ((eq signal 1) "hup")
   (t "term")))

;; Advise signal-process to send signals to flit processes via exec/signal.
(defun flit--signal-process-advice (orig-fn process signal)
  "Send SIGNAL to flit PROCESS via exec/signal, or call ORIG-FN."
  (if-let* ((proc (cond
                   ((processp process) process)
                   ((integerp process) nil)  ; PID - can't be a flit process
                   ((stringp process) (get-process process))
                   (t nil)))
            (proc-id (flit--process-valid-for-rpc-p proc "signal"))
            (host (process-get proc 'flit-host)))
      (progn
        (flit--send-request-async host "exec/signal"
                                  `(:procId ,proc-id :signal ,(flit--signal-name signal)))
        0)  ; Return 0 for success
    (funcall orig-fn process signal)))

(advice-add 'signal-process :around #'flit--signal-process-advice)

;; Advise kill-process to handle flit processes.
;; kill-process sends SIGKILL to terminate a process, but our pipe processes
;; aren't real subprocesses so this fails with "not a subprocess".
(defun flit--kill-process-advice (orig-fn process &optional current-group)
  "Handle kill-process for flit processes."
  (if-let* ((proc (cond
                   ((processp process) process)
                   ((stringp process) (get-process process))
                   (t nil)))
            ((process-get proc 'flit-proc-id)))
      ;; This is a flit process - send SIGKILL if valid, then always clean up
      (progn
        (when-let* ((proc-id (flit--process-valid-for-rpc-p proc "kill"))
                    (host (process-get proc 'flit-host)))
          (condition-case nil
              (flit--send-request-async host "exec/signal"
                                        `(:procId ,proc-id :signal "kill"))
            (error nil)))
        ;; Clean up stderr process if one exists
        (when-let* ((stderr-proc (process-get proc 'flit-stderr-proc))
                    ((process-live-p stderr-proc)))
          (delete-process stderr-proc))
        ;; Always clean up local state
        (process-put proc 'flit-exited t)
        (process-put proc 'flit-exit-code -1)
        (delete-process proc))
    (funcall orig-fn process current-group)))

(advice-add 'kill-process :around #'flit--kill-process-advice)

;;; File name parsing helpers

(defun flit--file-name-p (filename)
  "Return non-nil if FILENAME is a canonical flit file name."
  (and (string-prefix-p flit--prefix filename)
       (> (length filename) (1+ flit--prefix-length))
       (let ((c (aref filename flit--prefix-length)))
         (not (memq c '(?/ ?: ?~))))
       (flit--find-path-start filename)))

(defun flit--find-path-start (filename)
  "Return position of first / or ~ after the host in FILENAME, or nil."
  (cl-loop for i from flit--prefix-length below (length filename)
           for c = (aref filename i)
           when (or (eq c ?/) (eq c ?~)) return i))

(defun flit--parse-file-name (filename)
  "Parse FILENAME and return (HOST . PATH).
Port is included in HOST as host:port when present."
  (if-let ((path-start (flit--find-path-start filename)))
      (cons (substring filename flit--prefix-length path-start)
            (substring filename path-start))
    (error "Invalid flit file name: %s" filename)))

(defmacro flit--with-parsed (bindings filename &rest body)
  "Parse FILENAME and bind (HOST PATH) from BINDINGS, then execute BODY."
  (declare (indent 2))
  (let ((parsed-sym (make-symbol "parsed")))
    `(let* ((,parsed-sym (flit--parse-file-name ,filename))
            (,(car bindings) (car ,parsed-sym))
            (,(cadr bindings) (cdr ,parsed-sym)))
       ,@body)))

(defvar flit--inhibit-handlers
  '(flit--file-name-handler
    flit--completion-handler
    tramp-file-name-handler
    tramp-completion-file-name-handler)
  "Handlers to inhibit when calling default file operations.")

(defun flit--call-default (operation args)
  "Call default handler for OPERATION with ARGS list, inhibiting flit and TRAMP."
  (let ((inhibit-file-name-handlers
         (if inhibit-file-name-handlers
             (append flit--inhibit-handlers inhibit-file-name-handlers)
           flit--inhibit-handlers))
        (inhibit-file-name-operation operation))
    (apply operation args)))

(defun flit--host (filename)
  "Extract host from FILENAME."
  (car (flit--parse-file-name filename)))

(defun flit--path (filename)
  "Extract remote path from FILENAME."
  (cdr (flit--parse-file-name filename)))

(defun flit--format-path (host path)
  "Format HOST and PATH into a flit file name."
  (if (or (null path) (string-empty-p path))
      (concat flit--prefix host)
    (concat flit--prefix host path)))

;;; Connection management

(cl-defstruct flit-connection
  "A flitrpc connection to a flit server."
  host      ; hostname string
  rpc)      ; flitrpc-conn struct

(defun flit-conn-process (conn)
  "Return the underlying Emacs process for CONN."
  (flitrpc-conn-process (flit-connection-rpc conn)))

(defun flit-conn-running-p (conn)
  "Return non-nil if CONN is running."
  (when-let ((proc (flit-conn-process conn)))
    (process-live-p proc)))

(defun flit--await (async-fn)
  "Drive ASYNC-FN synchronously, waiting for its callback.
ASYNC-FN is called with a single callback argument.
The callback is expected to be called with (RESULT ERROR-MSG).
Returns RESULT on success, signals error on failure."
  (let ((done nil)
        (result nil)
        (error-msg nil))
    (funcall async-fn
             (lambda (r e)
               (setq result r error-msg e done t)))
    (flit--with-quit-log "flit--await"
      (while (not done)
        (accept-process-output nil 0.1)))
    (if error-msg
        (error "%s" error-msg)
      result)))

(defun flit--get-connection-method (host)
  "Get connection method for HOST from `flit-connection-methods'.
Entries are matched in order.  Use t as a pattern to match any host.
Returns the method (which may be a function, :stdio spec, or :tcp spec)."
  (or (cl-some (lambda (entry)
                 (let ((pattern (car entry))
                       (method (cdr entry)))
                   (when (if (eq pattern t)
                             t
                           (string-match-p pattern host))
                     method)))
               flit-connection-methods)
      (error "No connection method configured for host: %s" host)))

(defun flit--connection-state (host)
  "Get the connection state for HOST.
Returns `connected', `connecting', `pending', `failed', or `disconnected'.
- `pending': initial state, never connected
- `connecting': connection attempt in progress
- `connected': live connection exists
- `failed': connection attempt failed
- `disconnected': user explicitly ran flit-disconnect"
  (or (gethash host flit--connection-states) 'pending))

(defun flit--set-connection-state (host state)
  "Set the connection state for HOST to STATE."
  (puthash host state flit--connection-states)
  (flit--log-info "Connection state for %s: %s" host state))

(defun flit--set-connection-failed (host _error-msg)
  "Set connection state for HOST to `failed'.
ERROR-MSG is logged but does not affect the state."
  (flit--set-connection-state host 'failed))

(defun flit--connection-alive-p (host)
  "Return non-nil if the connection to HOST is alive."
  (let ((conn (gethash host flit--connections)))
    (and conn
         (flit-conn-running-p conn)
         (let ((proc (flit-conn-process conn)))
           (and proc (process-live-p proc))))))

(define-error 'flit-disconnected "Not connected to flit host")

(defun flit--signal-disconnected (host)
  "Signal an error indicating HOST is disconnected."
  (signal 'flit-disconnected (list host)))

(defun flit--do-connect (host &optional callback)
  "Attempt to connect to HOST.
Sets state to `connecting' during attempt, then `connected' or `failed'.

If CALLBACK is provided, connect asynchronously:
- Returns immediately after starting the connection process
- Calls CALLBACK with (HOST SUCCESS ERROR-MSG) when done

If CALLBACK is nil, connect synchronously:
- Blocks until connection completes or fails
- Returns the connection or signals an error

Password prompting is determined by context at connection start
via `flit--allow-prompt-p'."
  (let ((async callback)
        (allow-prompt (flit--allow-prompt-p)))

    ;; State for waiting (sync mode)
    (let ((done nil)
          (result-conn nil)
          (result-error nil))

      (unwind-protect
          (progn
            (flit--set-connection-state host 'connecting)
            (if async
                (flit--log-info "Async connecting to %s (allow-prompt=%s)..." host allow-prompt)
              (flit--log-info "Sync connecting to %s (allow-prompt=%s)..." host allow-prompt)
              ;; Show message - use minibuffer-message when in minibuffer to avoid being overwritten
              ;; minibuffer-message adds its own brackets, so omit ours there
              (if (active-minibuffer-window)
                  (minibuffer-message "flit: Connecting to %s..." host)
                (message "[flit] Connecting to %s..." host)))

            ;; Start async connection flow
            (condition-case err
                (let ((method (flit--get-connection-method host)))
                  (flit--create-connection
                   host method
                   (lambda (conn error-msg)
                     (if conn
                         ;; Connection created, now initialize
                         (flit--initialize-connection
                          host conn
                          (lambda (init-success init-error)
                            (if init-success
                                (progn
                                  (puthash host conn flit--connections)
                                  (flit--set-connection-state host 'connected)
                                  (cl-pushnew host flit-known-hosts :test #'equal)
                                  (flit--reregister-watches host)
                                  (flit--log-info "Connection to %s complete" host)
                                  (flit--run-after-connect-functions host)
                                  (setq result-conn conn done t)
                                  (when callback (funcall callback host t nil)))
                              (flit--set-connection-failed host init-error)
                              (setq result-error init-error done t)
                              (when callback (funcall callback host nil init-error)))))
                       ;; Connection creation failed
                       (flit--set-connection-failed host error-msg)
                       (setq result-error error-msg done t)
                       (when callback (funcall callback host nil error-msg))))))
              (error
               (let ((msg (error-message-string err)))
                 (flit--set-connection-failed host msg)
                 (setq result-error msg done t)
                 (when callback (funcall callback host nil result-error)))))

            ;; For sync mode, wait for completion
            (unless async
              (flit--with-quit-log (format "connecting to %s" host)
                (while (not done)
                  (accept-process-output nil 0.1)))
              (if result-error
                  (error "Failed to connect to %s: %s" host result-error)
                result-conn)))

        ;; Cleanup: ensure state never stays stuck at connecting
        (when (eq (flit--connection-state host) 'connecting)
          (flit--set-connection-state host 'failed))))))

(defun flit--get-connection (host)
  "Get or create a connection to HOST.
Behavior depends on `flit--connection-tier' (must be bound by caller):
- `connect': Attempts sync connection (find-file/dired minibuffer)
- `passive': Only returns existing live connections, never initiates new ones

Defaults to `passive' when `flit--connection-tier' is unbound.
`flit-connect' and `flit-deferred-reload' bypass this function entirely."
  (let* ((state (flit--connection-state host))
         (tier (or flit--connection-tier 'passive)))
    (let ((conn-log-detail
           (lambda ()
             (format "(state=%s tier=%s this-command=%s) path=%s buffer=%s\n  callers: %s"
                     state tier this-command
                     flit--current-path (buffer-name)
                     (flit--simple-backtrace)))))
      (cl-flet ((do-connect (reason)
                  (flit--log-info "Connecting to %s: %s %s" host reason (funcall conn-log-detail))
                  (flit--do-connect host))
                (skip-connect (reason)
                  (flit--log-debug "Not connecting to %s: %s %s" host reason (funcall conn-log-detail))))
        (if (eq tier 'passive)
            ;; Passive: only use existing live connections
            (if (and (eq state 'connected) (flit--connection-alive-p host))
                (gethash host flit--connections)
              (skip-connect "passive tier")
              (flit--signal-disconnected host))
          ;; Connect tier: attempt connection
          (pcase state
            ('connected
             (if (flit--connection-alive-p host)
                 (gethash host flit--connections)
               ;; Connection died - clean up and reconnect
               ;; Set state to 'pending FIRST to prevent infinite recursion:
               ;; cleanup-host-processes calls sentinels, which may call
               ;; flit--send-request-async, which calls flit--get-connection
               (flit--set-connection-state host 'pending)
               (remhash host flit--connections)
               (remhash host flit--sys-info)
               (flit--cache-invalidate-host host)
               (flit--cleanup-host-processes host)
               (do-connect "connection died")))

            ('connecting
             ;; Connection in progress — reentrant call from accept-process-output
             ;; during sync connect.  Just signal disconnected; the outer call is
             ;; waiting and will complete the connection.
             (flit--signal-disconnected host))

            (_
             ;; pending, failed, disconnected — attempt connection
             (flit--cache-invalidate-host host)
             (do-connect (format "state=%s" state)))))))))
(defun flit--on-shutdown (host)
  "Return an on-shutdown callback for connection to HOST."
  (lambda (conn)
    (flit--log-info "Connection to %s closed" host)
    (flit--cleanup-host-tunnels host)
    (flit--cleanup-host-processes host)
    (remhash host flit--connections)
    (remhash host flit--sys-info)
    ;; Use 'pending so next operation can reconnect
    (flit--set-connection-state host 'pending)
    ;; Kill the underlying process (et-bridge, ssh, etc.) to ensure
    ;; the remote flit-server receives EOF and exits
    (when-let* ((proc (flit-conn-process conn)))
      (when (process-live-p proc)
        (flit--log-info "Killing process for %s" host)
        ;; Send EOF first to allow graceful shutdown
        (ignore-errors (process-send-eof proc))
        ;; Give it a moment to exit gracefully
        (flit--with-quit-log (format "shutdown wait for %s" host)
          (with-timeout (0.5 nil)
            (while (process-live-p proc)
              (accept-process-output proc 0.05))))
        ;; Force kill if still alive
        (when (process-live-p proc)
          (delete-process proc))))))

(defun flit--make-connection (host proc)
  "Create a flit-connection for HOST using PROC."
  ;; Save any leftover data from the handshake filter (flitrpc frames
  ;; that arrived in the same chunk as the ready message).
  (let ((leftover (process-get proc 'flit-sm-pending)))
    (let* ((conn (make-flit-connection :host host))
           (rpc (flitrpc-make-conn
                 proc host
                 :notification-fn
                 (lambda (_rpc-conn method params payload-data)
                   (when payload-data
                     (setq params (plist-put (copy-sequence params)
                                            :payload payload-data)))
                   (flit--handle-notification conn (intern method) params))
                 :request-fn
                 (lambda (rpc-conn method _params)
                   (ignore rpc-conn)
                   (flit--handle-request conn (intern method) nil))
                 :on-shutdown
                 (lambda (_rpc-conn)
                   (funcall (flit--on-shutdown host) conn)))))
      (setf (flit-connection-rpc conn) rpc)
      (set-process-query-on-exit-flag proc nil)
      ;; Increase read chunk size for better throughput
      (when (process-buffer proc)
        (with-current-buffer (process-buffer proc)
          (setq-local read-process-output-max (* 1024 1024))))
      ;; Inject leftover handshake data into the flitrpc buffer
      (when (and leftover (> (length leftover) 0))
        (with-current-buffer (flitrpc-conn-buffer rpc)
          (goto-char (point-max))
          (insert leftover))
        (flitrpc--parse-frames rpc))
      conn)))

;;; Connection type: :stdio

(defvar flit--force-allow-prompt nil
  "When non-nil, `flit--allow-prompt-p' always returns t.
Bound by `flit-connect' to ensure explicit user connections always prompt.")

(defun flit--allow-prompt-p ()
  "Return non-nil if interactive prompting is allowed in current context.
Prompting is allowed when actively connecting (tier=connect or flit-connect)."
  (or flit--force-allow-prompt
      (eq flit--connection-tier 'connect)))

(defun flit--create-stdio-connection (host command callback)
  "Create a stdio connection to HOST using COMMAND (async).
COMMAND is a list of strings (program and arguments).
CALLBACK is called with (PROC ERROR-MSG) when the process is ready.

The caller is responsible for any handshake logic and creating
the final connection with `flit--make-connection'."
  (let* ((allow-prompt (flit--allow-prompt-p))
         (proc-name (format "flit-%s" host))
         (default-directory "/"))
    (flit--log-info "Creating stdio connection to %s: %S (allow-prompt=%s)" host command allow-prompt)
    (condition-case err
        (let* ((stderr-name (format "*flit-stderr-%s*" host))
               (stderr-buf (get-buffer-create stderr-name))
               (_ (with-current-buffer stderr-buf (erase-buffer)))
               (proc (make-process
                      :name proc-name
                      :command command
                      :coding 'utf-8-emacs-unix
                      :connection-type 'pipe
                      :noquery t
                      :stderr stderr-buf)))
          (flit--log-info "stdio process started, stderr buffer: %s" (buffer-name stderr-buf))
          ;; Capture context for handshake to check later
          (process-put proc 'flit-allow-prompt allow-prompt)
          (funcall callback proc nil))
      (error
       (funcall callback nil (error-message-string err))))))

;;; Password reading (used by pty-bridge handshake)

(defun flit--read-passwd-with-timeout (prompt timeout)
  "Read password with PROMPT, aborting after TIMEOUT seconds.
Returns the password string, or nil if timed out or aborted."
  (let ((timer (run-with-timer
                timeout nil
                (lambda ()
                  (when (active-minibuffer-window)
                    (message "[flit] Password prompt timed out")
                    (abort-recursive-edit))))))
    (unwind-protect
        (condition-case nil
            (read-passwd prompt)
          (quit nil))  ; Return nil on abort/quit
      (cancel-timer timer))))

;;; Connection type: :ssh remote script

(defun flit--remote-server-script-ssh ()
  "Return the remote shell script for SSH fast path.
Checks for flit binary and runs it directly.  Exits with flit_not_found
JSON if the binary is not available (no in-band deploy support).
Does not use stty, so works over a pipe without a TTY."
  (concat
   "FLIT=~/.local/share/flit/flit; "
   "command -v flit >/dev/null 2>&1 && FLIT=flit; "
   "if { ! test -x \"$FLIT\" || ! test -s \"$FLIT\"; } && ! command -v flit >/dev/null 2>&1; then "
   "printf '{\"flit_not_found\":true,\"uname\":\"%s\"}\\n' \"$(uname -sm)\"; "
   "exit 1; "
   "fi; "
   "exec \"$FLIT\" server --stdio"))

;;; Connection type: :tcp

(defun flit--create-tcp-connection (host tcp-host tcp-port callback)
  "Create a TCP connection for HOST to TCP-HOST:TCP-PORT (async-style).
CALLBACK is called with (PROC ERROR-MSG) when the process is ready.

The caller is responsible for any handshake logic and creating
the final connection with `flit--make-connection'."
  (let ((default-directory "/"))
    (flit--log-info "Connecting to %s via TCP %s:%d" host tcp-host tcp-port)
    (condition-case err
        (let ((proc (apply
                     #'make-network-process
                     (append
                      (list :name (format "flit-%s" host)
                            :host tcp-host
                            :service tcp-port
                            :coding 'utf-8-emacs-unix
                            :noquery t)
                      (when (>= emacs-major-version 28)
                        '(:nodelay t))))))
          (funcall callback proc nil))
      (error
       (funcall callback nil (error-message-string err))))))

;;; Binary discovery and deployment
;;
;; Infrastructure for finding/compiling the local flit binary and
;; deploying it to remote hosts.

(defvar flit--source-directory
  (let ((lisp-dir (file-name-directory (or load-file-name buffer-file-name ""))))
    (expand-file-name "server" lisp-dir))
  "Directory containing the flit Go source code.")

(defun flit--uname-to-goos (name)
  "Convert uname system NAME to Go GOOS value."
  (pcase (downcase (or name ""))
    ("linux" "linux")
    ("darwin" "darwin")
    (_ (downcase (or name "")))))

(defun flit--uname-to-goarch (name)
  "Convert uname machine NAME to Go GOARCH value."
  (pcase (downcase (or name ""))
    ("x86_64" "amd64")
    ((or "aarch64" "arm64") "arm64")
    (_ (downcase (or name "")))))

(defun flit--find-local-binary ()
  "Find the local flit binary.
Search order: `exec-path', then source-dir/flit.
If not found and interactive, offer to compile.
Checks proto version matches — prompts to recompile if stale."
  (let ((binary (or (executable-find "flit")
                    (let ((in-source (expand-file-name "flit" flit--source-directory)))
                      (and (file-executable-p in-source) in-source))
                    (when (y-or-n-p "Local flit binary not found. Compile now?")
                      (flit--compile-local))
                    (error "flit binary not found"))))
    (flit--check-local-binary-version binary)
    binary))

(defun flit--check-local-binary-version (binary)
  "Check that BINARY has the expected proto version.
Signals an error with a recompile suggestion if mismatched."
  (let ((output (string-trim
                 (with-output-to-string
                   (with-current-buffer standard-output
                     (call-process binary nil t nil "version"))))))
    (unless (equal output (number-to-string flit--proto-version))
      (if (y-or-n-p (format "Local flit binary (proto %s) doesn't match client (proto %d). Recompile?"
                            output flit--proto-version))
          (flit--compile-local)
        (error "Flit binary version mismatch")))))

(defun flit--compile-local ()
  "Compile flit for the local platform.  Returns binary path."
  (let ((default-directory flit--source-directory)
        (output (expand-file-name "flit" flit--source-directory)))
    (message "Compiling flit...")
    (let ((result (call-process "go" nil "*flit-compile*" nil
                                "build" "-o" output ".")))
      (unless (= result 0)
        (pop-to-buffer "*flit-compile*")
        (error "Compilation failed (exit %d)" result)))
    (message "Compiled flit → %s" output)
    output))

(defun flit--compile-for-remote (goos goarch)
  "Cross-compile flit for GOOS/GOARCH.  Returns binary path."
  (let* ((default-directory flit--source-directory)
         (output (expand-file-name (format "flit-%s-%s" goos goarch)
                                   flit--source-directory)))
    (message "Cross-compiling flit for %s/%s..." goos goarch)
    (let ((result (let ((process-environment
                         (append (list (format "GOOS=%s" goos)
                                       (format "GOARCH=%s" goarch)
                                       "CGO_ENABLED=0")
                                 process-environment)))
                    (call-process "go" nil "*flit-compile*" nil
                                  "build" "-o" output "."))))
      (unless (= result 0)
        (pop-to-buffer "*flit-compile*")
        (error "Cross-compilation failed (exit %d)" result)))
    (message "Compiled flit for %s/%s → %s" goos goarch output)
    output))

(defun flit--send-binary-data (proc binary-path)
  "Send binary file contents to PROC."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally binary-path)
    (let ((coding-system-for-write 'binary))
      (process-send-region proc (point-min) (point-max)))))

(defun flit--remote-server-script ()
  "Return the remote shell script that checks for flit and handles deploy."
  (concat
   "FLIT=~/.local/share/flit/flit; "
   "command -v flit >/dev/null 2>&1 && FLIT=flit; "
   "if { ! test -x \"$FLIT\" || ! test -s \"$FLIT\"; } && ! command -v flit >/dev/null 2>&1; then "
   "printf '{\"flit_not_found\":true,\"uname\":\"%s\"}\\n' \"$(uname -sm)\"; "
   "read DEPLOY_SIZE; "
   "mkdir -p ~/.local/share/flit; "
   "stty raw -echo; "
   "printf 'deploy_ready\\n'; "
   "head -c \"$DEPLOY_SIZE\" > \"$FLIT.tmp\"; "
   "stty sane; "
   "chmod +x \"$FLIT.tmp\"; "
   "mv -f \"$FLIT.tmp\" \"$FLIT\"; "
   "fi; "
   "stty raw -echo && exec \"$FLIT\" server --stdio"))

(defun flit--deploy-only-script ()
  "Return the remote shell script for deploy-only mode (no server start)."
  (concat
   "printf '{\"flit_not_found\":true,\"uname\":\"%s\"}\\n' \"$(uname -sm)\"; "
   "read DEPLOY_SIZE; "
   "mkdir -p ~/.local/share/flit; "
   "stty raw -echo; "
   "printf 'deploy_ready\\n'; "
   "head -c \"$DEPLOY_SIZE\" > ~/.local/share/flit/flit.tmp; "
   "stty sane; "
   "chmod +x ~/.local/share/flit/flit.tmp; "
   "mv -f ~/.local/share/flit/flit.tmp ~/.local/share/flit/flit; "
   "echo '{\"flit_deploy_done\":true}'"))

;;; Process filter state machine
;;
;; Thin state-machine infrastructure for async process handshakes.
;; Each state is a handler closure that captures its own config.
;; The SM infrastructure only stores the handler, the line buffer,
;; and the timer on the process.

(defun flit--sm-start (proc handler host callback &optional timeout)
  "Start a state machine on PROC.
HANDLER is the initial state handler closure, called (PROC JSON-OBJ LINE).
HOST is used for logging.
CALLBACK is called (PROC ERROR-MSG) on timeout or unexpected exit.
TIMEOUT is seconds before automatic failure (default 120)."
  (let ((timeout (or timeout 120)))
    (flit--log-info "sm[%s]: Starting timeout=%d" host timeout)
    (process-put proc 'flit-sm-handler handler)
    (process-put proc 'flit-sm-pending "")
    (set-process-filter proc #'flit--sm-filter)
    (set-process-sentinel proc
                          (lambda (proc event)
                            (when (process-get proc 'flit-sm-handler)
                              (let ((msg (format "Process exited during handshake: %s"
                                                 (string-trim event))))
                                (flit--log-info "sm[%s]: %s" host msg)
                                (flit--sm-cleanup proc)
                                (when callback
                                  (funcall callback nil msg))))))
    (process-put proc 'flit-sm-timer
                 (run-at-time timeout nil
                              (lambda ()
                                (flit--log-info "sm[%s]: Timeout" host)
                                (flit--sm-cleanup proc)
                                (delete-process proc)
                                (when callback
                                  (funcall callback nil
                                           "Timeout waiting for flit server ready")))))))

(defun flit--sm-filter (proc output)
  "Universal process filter for state-machine handshakes.
Accumulates OUTPUT, extracts complete lines, parses JSON, calls handler."
  (let ((pending (or (process-get proc 'flit-sm-pending) "")))
    (setq pending (concat pending output))
    (process-put proc 'flit-sm-pending pending)
    (flit--log-info "sm filter: output=%S pending-len=%d" output (length pending))
    (while (string-match "^\\([^\n]*\\)\n" pending)
      (let* ((raw-line (match-string 1 pending))
             (line (string-trim-right raw-line "\r"))
             ;; Strip ANSI escape sequences that may be prepended by PTY output.
             (json-line (replace-regexp-in-string "\x1b\\[[0-9;?]*[a-zA-Z]" "" line))
             (json-line (string-trim json-line))
             (json-obj (condition-case err
                           (json-parse-string json-line :object-type 'plist)
                         (error
                          (flit--log-info "sm filter: JSON parse failed for %S: %s"
                                          line (error-message-string err))
                          nil)))
             (handler (process-get proc 'flit-sm-handler)))
        (setq pending (substring pending (match-end 0)))
        (process-put proc 'flit-sm-pending pending)
        (if handler
            (progn
              (flit--log-info "sm filter: dispatching line=%S json-keys=%S"
                              line (and json-obj (cl-loop for (k _v) on json-obj by #'cddr
                                                          collect k)))
              (funcall handler proc json-obj line))
          (flit--log-info "sm filter: no handler, dropping line=%S" line))))))

(defun flit--sm-goto (proc handler)
  "Set PROC's current state handler to HANDLER."
  (process-put proc 'flit-sm-handler handler))

(defun flit--sm-complete (proc callback &optional result-proc)
  "Signal successful completion.
Cleans up SM on PROC, then calls CALLBACK with RESULT-PROC (or PROC)."
  (flit--sm-cleanup proc)
  (when callback
    (funcall callback (or result-proc proc) nil)))

(defun flit--sm-fail (proc callback error-msg)
  "Signal failure with ERROR-MSG.
Cleans up SM, kills PROC, calls CALLBACK with error."
  (flit--sm-cleanup proc)
  (delete-process proc)
  (when callback
    (funcall callback nil error-msg)))

(defun flit--sm-cleanup (proc)
  "Remove state-machine filter, sentinel, and timer from PROC."
  (set-process-filter proc nil)
  (set-process-sentinel proc nil)
  (let ((timer (process-get proc 'flit-sm-timer)))
    (when timer
      (cancel-timer timer)))
  (process-put proc 'flit-sm-handler nil)
  (process-put proc 'flit-sm-pending nil)
  (process-put proc 'flit-sm-timer nil))

(defconst flit--proto-version 1
  "Expected flitrpc protocol version.
Bump when making breaking wire format changes.")

(defun flit--check-proto-version (proc json-obj callback host _deploy-handler)
  "Check proto_version in JSON-OBJ, then complete or fail the handshake."
  (let ((server-version (plist-get json-obj :proto_version)))
    (if (eq server-version flit--proto-version)
        (flit--sm-complete proc callback)
      (flit--sm-fail proc callback
                     (format "Server protocol version %s does not match client version %d. Run flit-compile-and-deploy to update."
                             (or server-version "missing") flit--proto-version)))))

;;; Handshake state handlers

(defun flit--hs-waiting-ready (host callback handle-password deploy-handler
                                    allow-prompt deadline)
  "Return a handler closure for the waiting-ready handshake state.
All parameters are captured in the closure."
  (lambda (proc json-obj line)
    (cond
     ;; Ready signal from flit server
     ((and json-obj (plist-get json-obj :flit_ready))
      (flit--log-info "handshake[%s]: flit_ready received" host)
      (flit--check-proto-version proc json-obj callback host deploy-handler))

     ;; Deploy completion signal — from the deploy shell script, not the server.
     ;; No proto_version check needed here since the binary we just deployed
     ;; is the one we compiled locally.
     ((and json-obj (plist-get json-obj :flit_deploy_done))
      (flit--log-info "handshake[%s]: Deploy complete" host)
      (flit--sm-complete proc callback))

     ;; Password prompt
     ((and json-obj (plist-get json-obj :pty_password_prompt))
      (if (and handle-password allow-prompt)
          (let* ((raw-prompt (plist-get json-obj :pty_password_prompt))
                 (prompt (string-replace "\r" "" raw-prompt))
                 (remaining (max 1 (- deadline (float-time))))
                 (password (flit--read-passwd-with-timeout prompt remaining)))
            (if password
                (progn
                  (flit--log-info "handshake[%s]: Sending password response" host)
                  (process-send-string
                   proc
                   (concat (json-serialize `(:pty_password ,password)) "\n")))
              (flit--sm-fail proc callback "Password prompt cancelled")))
        (flit--sm-fail proc callback "Authentication required")))

     ;; Deploy signal - flit not found on remote
     ((and json-obj (plist-get json-obj :flit_not_found))
      (if deploy-handler
          (progn
            (flit--log-info "handshake[%s]: flit_not_found, calling deploy handler" host)
            (flit--sm-goto proc (flit--hs-deploying host callback))
            (funcall deploy-handler host proc json-obj callback))
        (flit--sm-fail proc callback "flit binary not available on remote")))

     (t
      (flit--log-info "handshake[%s]: unrecognized (waiting-ready): %S" host line)))))

(defun flit--hs-deploying (host callback)
  "Return a handler closure for the deploying handshake state.
Only handles terminal signals; the deploy handler does the actual work."
  (lambda (proc json-obj line)
    (cond
     ((and json-obj (plist-get json-obj :flit_ready))
      (flit--log-info "handshake[%s]: flit_ready received (deploying)" host)
      (flit--sm-complete proc callback))

     ((and json-obj (plist-get json-obj :flit_deploy_done))
      (flit--log-info "handshake[%s]: Deploy complete" host)
      (flit--sm-complete proc callback))

     (t
      (flit--log-info "handshake[%s]: unrecognized (deploying): %S" host line)))))

;;; Handshake setup

(defun flit--setup-handshake (host proc callback &rest options)
  "Set up a handshake on PROC for HOST.
CALLBACK is called with (PROC ERROR-MSG) when done.

OPTIONS is a plist with:
  :handle-password t  - handle pty_password_prompt messages
  :deploy-handler FN  - called (HOST PROC JSON-OBJ CALLBACK) on flit_not_found
  :timeout N          - seconds, default 120"
  (let* ((timeout (or (plist-get options :timeout) 120))
         (handle-password (plist-get options :handle-password))
         (deploy-handler (plist-get options :deploy-handler))
         (allow-prompt (process-get proc 'flit-allow-prompt))
         (deadline (+ (float-time) timeout)))
    (flit--log-info "handshake[%s]: Starting (allow-prompt=%s password=%s deploy=%s timeout=%d)"
                    host allow-prompt
                    (if handle-password "yes" "no")
                    (if deploy-handler "yes" "no")
                    timeout)
    (flit--sm-start proc
                    (flit--hs-waiting-ready host callback handle-password
                                            deploy-handler allow-prompt deadline)
                    host callback timeout)))

(defun flit-pty-bridge-handshake (host proc callback)
  "Handshake for pty-bridge connections (async).
HOST is the flit host name (for logging).
PROC is the process running `flit pty-bridge'.
CALLBACK is called with (PROC ERROR-MSG) when done.

Handles password prompts but not deploy.  For deploy support,
use `flit--deploy-handshake' instead."
  (flit--setup-handshake host proc callback :handle-password t))

;;; Deploy handlers

(defun flit--in-band-deploy-handler (host proc json-obj callback)
  "Deploy handler for in-band deploy via pty-bridge.
Extracts uname from JSON-OBJ, cross-compiles, and sends binary in-band.
Does NOT call CALLBACK — the state machine continues waiting for flit_ready."
  (let* ((allow-prompt (process-get proc 'flit-allow-prompt))
         (uname (plist-get json-obj :uname))
         (parts (split-string (or uname "") " "))
         (goos (flit--uname-to-goos (nth 0 parts)))
         (goarch (flit--uname-to-goarch (nth 1 parts))))
    (flit--log-info "handshake[%s]: flit not found (uname=%s, %s/%s)"
                    host uname goos goarch)
    (if (and allow-prompt
             (y-or-n-p (format "flit not found on %s. Deploy (%s/%s)? "
                               host goos goarch)))
        (condition-case err
            (let* ((binary-path (flit--compile-for-remote goos goarch))
                   (size (file-attribute-size
                          (file-attributes binary-path))))
              (flit--log-info "handshake[%s]: Deploying %d bytes" host size)
              (process-send-string
               proc
               (concat (json-serialize `(:flit_deploy (:size ,size))) "\n"))
              (flit--send-binary-data proc binary-path)
              (flit--log-info "handshake[%s]: Binary sent, waiting for ready" host))
          (error
           (flit--log-info "handshake[%s]: Deploy failed: %s"
                           host (error-message-string err))
           (flit--sm-fail proc callback (format "Deploy failed: %s"
                                                (error-message-string err)))))
      ;; User declined or prompting not allowed
      (flit--log-info "handshake[%s]: Deploy declined" host)
      (flit--sm-fail proc callback "flit binary not available on remote"))))

;;; Named handshakes

(defun flit--deploy-handshake (host proc callback)
  "Handshake with password handling and in-band deploy support.
For use with :et connections or manual :pty-bridge with deploy."
  (flit--setup-handshake host proc callback
                         :handle-password t
                         :deploy-handler #'flit--in-band-deploy-handler))

(defun flit--stdio-ready-handshake (host proc callback)
  "Wait for flit_ready on PROC, then call CALLBACK.
Used for :stdio connections without an explicit handshake."
  (flit--setup-handshake host proc callback
                         :handle-password nil))

;;; SSH fast path

(defun flit--ssh-fast-handshake (host proc callback method-chain ssh-args)
  "Try fast SSH handshake on PROC for HOST.
On flit_ready, succeed with pure stdio path.
On any failure (flit_not_found, process exit, timeout), fall back to pty-bridge.
SSH-ARGS is the base SSH args for the pty-bridge fallback."
  (flit--setup-handshake host proc
                         ;; Wrap callback: success goes through, any failure triggers fallback
                         (lambda (result-proc error-msg)
                           (if error-msg
                               (flit--ssh-fallback host ssh-args callback method-chain)
                             (process-put result-proc 'flit-method
                                          (string-join (reverse (cons "ssh" method-chain)) " → "))
                             (funcall callback (flit--make-connection host result-proc) nil)))
                         :handle-password nil
                         :deploy-handler
                         (lambda (_host deploy-proc _json-obj _callback)
                           ;; flit_not_found — kill process and fall back
                           (flit--sm-cleanup deploy-proc)
                           (delete-process deploy-proc)
                           (flit--ssh-fallback host ssh-args callback method-chain))
                         :timeout 15))

(defun flit--ssh-fallback (host ssh-args callback method-chain)
  "Fall back to pty-bridge for SSH connection to HOST.
SSH-ARGS is the base SSH command (e.g. (\"ssh\" \"hostname\")).
Adds -t to force remote PTY allocation (needed for stty in the remote script)
and -e none to disable escape character processing (prevents binary corruption)."
  (flit--log-info "SSH fast path failed for %s, falling back to pty-bridge" host)
  ;; Insert -t -e none after program name but before hostname/other args
  (let ((transport (append (list (car ssh-args) "-t" "-e" "none") (cdr ssh-args))))
    (flit--create-connection
     host
     (list :pty-bridge transport
           :handshake #'flit--deploy-handshake)
     callback (cons "ssh-fallback" method-chain))))

;;; Connection dispatch

(defun flit--create-connection (host method callback &optional method-chain)
  "Create a connection to HOST using METHOD (async).
CALLBACK is called with (CONN ERROR-MSG) when done.

METHOD can be:

High-level modes (expand to low-level with deploy handshakes):
- (:et [SPEC])       - ET transport with pty-bridge and deploy
- (:ssh [SPEC])      - SSH with BatchMode fast path, pty-bridge fallback

Low-level modes:
- (:stdio COMMAND [:handshake FN])  - stdin/stdout
- (:tcp HOST PORT [:handshake FN])  - TCP
- (:pty-bridge TRANSPORT [:handshake FN]) - pty-bridge (internal)

SPEC can be nil (use HOST), a string (explicit hostname),
or a function (called with HOST, returns transport args list).

All connection types support :handshake FN which is called with the
process before flitrpc starts.

A function or process can also be used directly as METHOD.

METHOD-CHAIN accumulates the method resolution path for display."
  ;; Helper to wrap process callback with handshake and connection creation
  (cl-flet ((wrap-callback (handshake-fn chain)
              (lambda (proc error-msg)
                (if error-msg
                    (funcall callback nil error-msg)
                  (when (processp proc)
                    (process-put proc 'flit-method
                                 (string-join (reverse chain) " → ")))
                  (if handshake-fn
                      (funcall handshake-fn host proc
                               (lambda (result-proc handshake-error)
                                 (if result-proc
                                     (progn
                                       (when (and (processp result-proc)
                                                  (not (eq result-proc proc)))
                                         (process-put result-proc 'flit-method
                                                      (string-join (reverse chain) " → ")))
                                       (funcall callback (flit--make-connection host result-proc) nil))
                                   (funcall callback nil handshake-error))))
                    (funcall callback (flit--make-connection host proc) nil))))))
    (cond
     ;; Already a process - use directly
     ((processp method)
      (flit--log-info "Using provided process for %s" host)
      (funcall callback (flit--make-connection host method) nil))

     ;; Bare keyword - normalize to list form
     ((memq method '(:et :ssh))
      (flit--create-connection host (list method) callback method-chain))

     ;; Function - call it with callback to get the actual method
     ((functionp method)
      (flit--log-info "Calling connector function for %s" host)
      (condition-case err
          (funcall method host
                   (lambda (result error-msg)
                     (if error-msg
                         (funcall callback nil error-msg)
                       (flit--create-connection host result callback method-chain))))
        (error
         (funcall callback nil (format "Connector function failed: %s" (error-message-string err))))))

     ;; (:et [SPEC]) - ET transport with pty-bridge and deploy
     ((and (consp method) (eq (car method) :et))
      (let* ((spec (nth 1 method))
             (transport (cond
                         ((or (null spec) (stringp spec))
                          (list "et" (or spec host) "-c"))
                         ((functionp spec) (funcall spec host))
                         (t (error "Invalid :et spec: %S" spec)))))
        (flit--log-info "ET connection to %s via %S" host transport)
        (flit--create-connection
         host
         (list :pty-bridge transport
               :handshake #'flit--deploy-handshake)
         callback (cons "et" method-chain))))

     ;; (:ssh [SPEC]) - SSH with BatchMode fast-path, pty-bridge fallback
     ((and (consp method) (eq (car method) :ssh))
      (let* ((spec (nth 1 method))
             (ssh-args (cond
                        ((or (null spec) (stringp spec))
                         (list "ssh" (or spec host)))
                        ((functionp spec) (funcall spec host))
                        (t (error "Invalid :ssh spec: %S" spec)))))
        (flit--log-info "SSH connection to %s via %S" host ssh-args)
        ;; Try fast path: BatchMode SSH with no sidecar
        (let ((command (append ssh-args
                               (list "-o" "BatchMode=yes"
                                     (flit--remote-server-script-ssh)))))
          (flit--create-stdio-connection
           host command
           (lambda (proc error-msg)
             (if error-msg
                 ;; Process creation failed — fall back to pty-bridge
                 (flit--ssh-fallback host ssh-args callback method-chain)
               ;; Process started — try fast handshake
               (flit--ssh-fast-handshake
                host proc callback method-chain ssh-args)))))))

     ;; (:stdio COMMAND [:handshake FN])
     ((and (consp method) (eq (car method) :stdio))
      (let ((command (nth 1 method))
            (handshake-fn (or (plist-get (cddr method) :handshake)
                              #'flit--stdio-ready-handshake))
            (chain (cons "stdio" method-chain)))
        (flit--create-stdio-connection host command (wrap-callback handshake-fn chain))))

     ;; (:tcp HOST PORT [:handshake FN]) - TCP
     ((and (consp method) (eq (car method) :tcp))
      (let ((tcp-host (nth 1 method))
            (tcp-port (nth 2 method))
            (handshake-fn (plist-get (cdddr method) :handshake))
            (chain (cons "tcp" method-chain)))
        (flit--create-tcp-connection host tcp-host tcp-port (wrap-callback handshake-fn chain))))

     ;; (:pty-bridge TRANSPORT [:handshake FN]) - pty-bridge
     ((and (consp method) (eq (car method) :pty-bridge))
      (let* ((transport (nth 1 method))
             (handshake-fn (or (plist-get (cddr method) :handshake)
                               #'flit-pty-bridge-handshake))
             (local-flit (flit--find-local-binary))
             (remote-script (flit--remote-server-script))
             (command (append (list local-flit "pty-bridge" "--")
                              transport
                              (list remote-script)))
             (chain (cons "stdio" (cons "pty-bridge" method-chain))))
        (flit--create-stdio-connection
         host command (wrap-callback handshake-fn chain))))

     (t
      (funcall callback nil (format "Unknown connection method: %S" method))))))

;;; Deployment

(defun flit-compile-and-deploy (host)
  "Compile flit and deploy to HOST.
Compiles the local flit binary first, then cross-compiles for the remote
architecture and transfers the binary.
Uses the connection method configured for HOST."
  (interactive
   (list (read-string "Deploy flit to host: ")))
  (when (string-empty-p host)
    (user-error "No host specified"))
  ;; Compile local binary first
  (flit--compile-local)
  (let ((method (flit--get-connection-method host)))
    ;; Normalize bare keywords to list form
    (when (memq method '(:et :ssh))
      (setq method (list method)))
    (cond
     ;; :et → deploy via pty-bridge in-band
     ((and (consp method) (eq (car method) :et))
      (let* ((spec (nth 1 method))
             (transport (cond
                         ((or (null spec) (stringp spec))
                          (list "et" (or spec host) "-c"))
                         ((functionp spec) (funcall spec host)))))
        (flit--deploy-via-pty-bridge host transport)))

     ;; :ssh → deploy via pty-bridge in-band
     ((and (consp method) (eq (car method) :ssh))
      (let* ((spec (nth 1 method))
             (transport (cond
                         ((or (null spec) (stringp spec))
                          (list "ssh" (or spec host) "-t"))
                         ((functionp spec) (funcall spec host)))))
        (flit--deploy-via-pty-bridge host transport)))

     ;; :pty-bridge → deploy via pty-bridge in-band
     ((and (consp method) (eq (car method) :pty-bridge))
      (flit--deploy-via-pty-bridge host (nth 1 method)))

     (t
      (user-error "Don't know how to deploy to %s (method: %S)" host method)))))

(defun flit--deploy-via-pty-bridge (host transport)
  "Deploy flit to HOST using pty-bridge with TRANSPORT prefix."
  (let* ((local-flit (flit--find-local-binary))
         (remote-script (flit--deploy-only-script))
         (command (append (list local-flit "pty-bridge" "--")
                          transport
                          (list remote-script)))
         (flit--force-allow-prompt t)
         (done nil)
         (result-error nil))
    (flit--create-stdio-connection
     host command
     (lambda (proc error-msg)
       (if error-msg
           (progn (setq result-error error-msg done t))
         (flit--deploy-handshake
          host proc
          (lambda (result-proc handshake-error)
            (if handshake-error
                (setq result-error handshake-error done t)
              ;; Deploy done - clean up
              (when (process-live-p result-proc)
                (delete-process result-proc))
              (setq done t)))))))
    ;; Wait synchronously
    (flit--with-quit-log (format "deploying to %s" host)
      (while (not done)
        (accept-process-output nil 0.1)))
    (if result-error
        (error "Deploy to %s failed: %s" host result-error)
      (message "[flit] Deployed to %s" host))))


(defun flit--cleanup-host-processes (host)
  "Clean up all processes associated with HOST.
Marks them as exited, calls their sentinels, and deletes them."
  (let ((to-remove nil))
    ;; Find all processes for this host
    (maphash (lambda (proc-id proc)
               (when (equal (gethash proc-id flit--process-host) host)
                 (push (cons proc-id proc) to-remove)
                 ;; Only call sentinel if not already exited (e.g., via kill-process)
                 ;; This prevents double sentinel calls during Emacs shutdown
                 (unless (process-get proc 'flit-exited)
                   (process-put proc 'flit-exited t)
                   (process-put proc 'flit-exit-code -1)
                   (let ((sentinel (process-sentinel proc)))
                     (when sentinel
                       (condition-case err
                           (funcall sentinel proc "connection closed\n")
                         (error (flit--log-debug "Error in sentinel: %s" err))))))
                 ;; Delete the pipe process so it's no longer "live"
                 (condition-case nil
                     (delete-process proc)
                   (error nil))))
             flit--processes)
    ;; Remove from hash tables
    (dolist (entry to-remove)
      (remhash (car entry) flit--processes)
      (remhash (car entry) flit--process-host))))

(defun flit--send-shutdown (host)
  "Send shutdown RPC to HOST.  Fire-and-forget; errors are ignored."
  (condition-case nil
      (with-timeout (1 nil)
        (flit--send-notify host "shutdown" nil))
    (error nil)))

(defun flit--close-connection (host)
  "Close the connection to HOST."
  ;; Tell the server to shut down (delayed exit gives in-flight RPCs time)
  (flit--send-shutdown host)
  ;; Clean up any processes associated with this host
  (flit--cleanup-host-processes host)
  (let ((conn (gethash host flit--connections)))
    (when conn
      (remhash host flit--connections)
      (remhash host flit--sys-info)
      (let ((proc (flit-conn-process conn)))
        (when (and proc (process-live-p proc))
          ;; Close stdin to signal server to exit gracefully
          (process-send-eof proc)
          ;; Wait for process to exit on its own
          (flit--with-quit-log (format "closing connection to %s" host)
            (with-timeout (0.5 nil)
              (while (process-live-p proc)
                (accept-process-output proc 0.05))))
          ;; If still alive after timeout, force kill
          (when (process-live-p proc)
            (delete-process proc)))))))

(defun flit--initialize-connection (host conn callback)
  "Initialize connection CONN for HOST asynchronously.
CALLBACK is called with (SUCCESS ERROR-MSG) when done.
Fetches sys/info and PATH dirs, caches results."
  (flit--log-info "Async initializing connection to %s" host)
  (flitrpc-request
   (flit-connection-rpc conn) "init" nil
   (lambda (meta _payload)
     (let ((result (plist-get meta :result)))
       (flit--log-info "Async RPC init success for %s" host)
       ;; Cache sys-info (everything except pathDirs)
       (let ((sys-info (list :os (plist-get result :os)
                             :arch (plist-get result :arch)
                             :homeDir (plist-get result :homeDir)
                             :hostname (plist-get result :hostname)
                             :username (plist-get result :username)
                             :path (plist-get result :path)
                             :pid (plist-get result :pid))))
         (puthash host sys-info flit--sys-info))
       ;; Cache PATH directory listings
       (let ((path-dirs (plist-get result :pathDirs)))
         (seq-doseq (dir-entry path-dirs)
           (let ((path (plist-get dir-entry :path))
                 (children (plist-get dir-entry :children))
                 (error-msg (plist-get dir-entry :error)))
             (unless error-msg
               (let ((info (list :exists t
                                 :type "directory"
                                 :isDir t
                                 :path path
                                 :children children)))
                 (flit--log-debug "PATH prefetch: caching %s with %d children"
                                  path (if children (length children) 0))
                 (flit--cache-put host path info)))))
         (flit--log-info "Cached %d PATH directories for %s"
                         (length path-dirs) host))
       (funcall callback t nil)))
   (lambda (err)
     (flit--log "Async init failed for %s: %s" host err)
     (funcall callback nil (format "Init failed: %s" err)))))

(defun flit--get-sys-info (host)
  "Get cached sys-info for HOST, or nil if not available."
  (gethash host flit--sys-info))

(defun flit--get-remote-exec-path (host)
  "Get the remote exec-path (PATH) for HOST as a list of raw directory paths.
Emacs expects raw paths from exec-path and adds the remote prefix itself."
  (let ((sys-info (flit--get-sys-info host)))
    (when sys-info
      (let ((path (plist-get sys-info :path)))
        (append path nil)))))

(defun flit--handle-notification (conn method params)
  "Handle a server notification METHOD with PARAMS from CONN."
  ;; Skip logging exec/output and log - too noisy
  (unless (memq method '(exec/output log))
    (flit--log-debug "Notification: %s %S" method params))
  (let ((start-time (float-time)))
    (cond
     ((eq method 'fs/changed)
      (flit--handle-file-changed conn params))
     ((eq method 'exec/output)
      (flit--handle-exec-output params))
     ((eq method 'exec/exit)
      (flit--handle-exec-exit params))
     ((eq method 'log)
      (flit--handle-server-log conn params))
     ((eq method 'tunnel/accept)
      (flit--handle-tunnel-accept conn params))
     ((eq method 'tunnel/data)
      (flit--handle-tunnel-data conn params))
     ((eq method 'tunnel/disconnect)
      (flit--handle-tunnel-disconnect conn params))
     ((eq method 'fs/ancestorInfo)
      (flit--handle-ancestor-info conn params))
     ((eq method 'fs/entryInfo)
      (flit--handle-entry-info conn params))
     (t
      (flit--log-debug "Unknown notification: %s" method)))
    (let ((elapsed (- (float-time) start-time)))
      (when (> elapsed 0.05)
        (flit--log-info "Slow notification: %s on %s took %.3fs"
                        method (flit-connection-host conn) elapsed)))))

(defun flit--handle-server-log (conn params)
  "Handle a log notification from CONN with PARAMS.
Server logs are forwarded via RPC and displayed in the flit log buffer."
  (let* ((host (flit-connection-host conn))
         (level (plist-get params :level))
         (msg (plist-get params :msg))
         (attrs (plist-get params :attrs)))
    ;; Map server log levels to flit log functions
    (cond
     ((string= level "ERROR")
      (flit--log-error "[%s] %s %S" host msg attrs))
     ((string= level "WARN")
      (flit--log-info "[%s] WARN: %s %S" host msg attrs))
     ((string= level "DEBUG")
      (flit--log-debug "[%s] %s %S" host msg attrs))
     (t ; INFO and others
      (flit--log-info "[%s] %s%s" host msg
                      (if attrs (format " %S" attrs) ""))))))

(defun flit--handle-request (_conn method _params)
  "Handle a server request.
METHOD is the RPC method name."
  (pcase method
    ("heartbeat" t)  ; Just return true to acknowledge
    (_ nil)))

(defun flit--handle-ancestor-info (conn params)
  "Handle ancestor info notification from CONN with PARAMS.
Caches ancestor directory info pushed by server during fs/open."
  (let* ((host (flit-connection-host conn))
         (cache (plist-get params :cache))
         (count (length cache)))
    (flit--log-debug "Ancestor info: caching %d entries for %s" count host)
    (flit--cache-from-response host params)))

(defun flit--handle-entry-info (conn params)
  "Handle entry info notification from CONN with PARAMS.
Caches entry info pushed by server async after fs/info on directories."
  (let* ((host (flit-connection-host conn))
         (path (plist-get params :path))
         (cache (plist-get params :cache))
         (count (length cache)))
    (flit--log-debug "Entry info: caching %d entries for %s:%s" count host path)
    (flit--cache-from-response host params)))

(defun flit--handle-file-changed (conn params)
  "Handle a file change notification from CONN with PARAMS."
  (let* ((host (flit-connection-host conn))
         (path (plist-get params :path))
         (type (plist-get params :type))
         (flit-path (flit--format-path host path))
         (cache (plist-get params :cache))
         (cache-info (and cache (vectorp cache) (> (length cache) 0)
                          (aref cache 0)))
         (info (and cache-info (plist-get cache-info :info)))
         (info-type (plist-get info :type))
         (is-dir (and info-type (string= info-type "directory"))))
    ;; Log file changes at info level (skip directories - too noisy)
    (unless is-dir
      (let ((new-mtime (plist-get info :mtime))
            (has-content (plist-get info :content)))
        (flit--log-info "fs/changed: %s %s mtime=%s content=%s"
                        type flit-path new-mtime (if has-content "yes" "no"))))
    ;; Invalidate directory cache on modification - children may have changed
    ;; and the incoming info (from GetInfoBasic) lacks children, so cache-put
    ;; would skip the update due to its "don't overwrite richer data" guard.
    (when (and is-dir (string= type "modified"))
      (flit--cache-invalidate host path))
    ;; Use unified cache function to process the cache entries
    (flit--cache-from-response host params)
    ;; Invalidate on delete
    (when (string= type "deleted")
      (flit--cache-invalidate host path))
    ;; Find buffers visiting this file and mark them for revert
    (let* ((new-mtime (plist-get info :mtime))
           (new-content-raw (or (plist-get params :payload)
                                (plist-get info :content))))
      (dolist (buf (buffer-list))
        (with-current-buffer buf
          (when (and buffer-file-name
                     (string= buffer-file-name flit-path))
            ;; Check if we can determine the buffer is in sync without reverting
            ;; Case 1: Content provided and matches buffer -> in sync (our save came back)
            ;; Case 2: Content not provided but mtime matches visited-file-modtime -> in sync
            (let* ((visited-mtime (float-time (visited-file-modtime)))
                   (mtime-matches (and new-mtime (= (truncate visited-mtime) new-mtime)))
                   (buffer-content (save-restriction
                                     (widen)
                                     (buffer-substring-no-properties (point-min) (point-max))))
                   (new-content (and new-content-raw
                                     (condition-case nil
                                         (let ((coding (or buffer-file-coding-system 'utf-8-unix)))
                                           (decode-coding-string new-content-raw coding))
                                       (error nil))))
                   (content-unchanged (and new-content
                                           (string= new-content buffer-content)))
                   ;; Buffer is in sync if content matches OR (no content provided AND mtime matches)
                   (already-in-sync (or content-unchanged
                                        (and (null new-content-raw) mtime-matches))))
              ;; Debug: log content comparison details when content is provided but doesn't match
              (when (and new-content-raw (not content-unchanged))
                (flit--log-info "watcher: %s content-mismatch: new-content-len=%s buffer-len=%s new-content-raw-len=%s"
                                (buffer-name)
                                (if new-content (length new-content) "nil")
                                (length buffer-content)
                                (length new-content-raw)))
              (if already-in-sync
                  ;; Buffer is already in sync - skip revert
                  ;; Update modtime so Emacs knows file is in sync
                  (progn
                    (when new-mtime
                      (let ((file-name-handler-alist nil))
                        (set-visited-file-modtime (seconds-to-time new-mtime))))
                    (flit--log-info "watcher: %s already-in-sync (content=%s mtime-matches=%s), updated visited-modtime to %s"
                                    (buffer-name)
                                    (if new-content-raw "matched" "not-provided")
                                    mtime-matches
                                    new-mtime))
                ;; Buffer needs revert - only do so if no unsaved changes
                ;; If buffer is modified, user has typed since last save - don't lose their work!
                (if (buffer-modified-p)
                    (flit--log-info "watcher: %s SKIPPING revert (buffer-modified-p=t) visited-modtime=%s file-mtime=%s"
                                    (buffer-name) (float-time (visited-file-modtime)) new-mtime)
                  ;; Buffer is unmodified - safe to mark for revert
                  (setq-local flit--file-changed t)
                  (flit--log-info "watcher: %s marking stale, visited-modtime=%s file-mtime=%s"
                                  (buffer-name) (float-time (visited-file-modtime)) new-mtime)
                  ;; If auto-revert-mode is enabled, trigger an immediate revert.
                  ;; We call revert-buffer directly rather than auto-revert-handler
                  ;; because auto-revert-handler has a sit-for that creates a race
                  ;; window where user input during the sit-for could trigger a
                  ;; revert even though buffer-modified-p was checked before sit-for.
                  ;; Preserve mark ring to avoid polluting navigation history.
                  (when (or (and (boundp 'auto-revert-mode) auto-revert-mode)
                            (and (boundp 'global-auto-revert-mode) global-auto-revert-mode))
                    (let ((saved-mark-ring mark-ring)
                          (saved-mark (mark-marker)))
                      (unless (buffer-modified-p)
                        (flit--log-info "watcher: %s REVERTING (auto-revert-mode=t)"
                                        (buffer-name))
                        (revert-buffer 'ignore-auto 'dont-ask 'preserve-modes)
                        (setq flit--file-changed nil))
                      (setq mark-ring saved-mark-ring)
                      (when (marker-position saved-mark)
                        (set-marker (mark-marker) (marker-position saved-mark))))))))))))))

(defun flit--handle-exec-output (params)
  "Handle exec/output notification with PARAMS.
PARAMS contains :procId, :stream, and :payload (raw bytes)."
  (let* ((proc-id (plist-get params :procId))
         (stream (plist-get params :stream))
         (data (or (plist-get params :payload) (plist-get params :data)))
         (proc (gethash proc-id flit--processes)))
    (flit--log-debug "exec-output: proc-id=%s stream=%s len=%d live=%s name=%s"
                     proc-id stream (if data (length data) -1)
                     (and proc (process-live-p proc))
                     (and proc (process-name proc)))
    (when (and proc (process-live-p proc))
      ;; Increment output counter so accept-process-output knows this process got data
      (process-put proc 'flit-output-count
                   (1+ (or (process-get proc 'flit-output-count) 0)))
      ;; Route stderr to stderr process if one exists
      (let* ((target-proc (if (and (equal stream "stderr")
                                   (process-get proc 'flit-stderr-proc))
                              (process-get proc 'flit-stderr-proc)
                            proc))
             (filter (process-filter target-proc))
             ;; Decode using process's coding system (car is for decoding output)
             (coding (car (process-coding-system target-proc)))
             (decoded-data (if coding
                               (decode-coding-string data coding)
                             data)))
        (flit--log-debug "exec-output: target=%s filter=%s"
                         (and target-proc (process-name target-proc))
                         (flit--log-fn-label filter))
        (when filter
          (funcall filter target-proc decoded-data))))))

(defun flit--handle-exec-exit (params)
  "Handle exec/exit notification with PARAMS.
PARAMS contains :procId and :exitCode."
  (let* ((proc-id (plist-get params :procId))
         (exit-code (plist-get params :exitCode))
         (proc (gethash proc-id flit--processes)))
    (if proc
        (flit--process-exit proc proc-id exit-code)
      ;; Process not found - this shouldn't happen with client-generated IDs
      ;; but log it for debugging
      (flit--log-debug "exec-exit: unknown proc-id=%s (already exited or never registered)" proc-id))))

(defun flit--process-exit (proc proc-id exit-code)
  "Handle exit of PROC with PROC-ID and EXIT-CODE."
  ;; Remove from tracking
  (remhash proc-id flit--processes)
  (remhash proc-id flit--process-host)
  ;; Mark process as exited with the exit code
  (process-put proc 'flit-exited t)
  (process-put proc 'flit-exit-code exit-code)
  ;; Clean up stderr process if one exists
  (when-let* ((stderr-proc (process-get proc 'flit-stderr-proc))
              ((process-live-p stderr-proc)))
    (delete-process stderr-proc))
  ;; Call the sentinel with the appropriate event string
  ;; Note: process-live-p may return nil for our fake pipe processes,
  ;; so we call the sentinel regardless
  (let ((sentinel (process-sentinel proc))
        (event (if (= exit-code 0)
                   "finished\n"
                 (format "exited abnormally with code %d\n" exit-code))))
    ;; Set sentinel to #'ignore before delete-process to prevent duplicate calls
    ;; (delete-process triggers sentinel with "deleted\n")
    ;; Note: nil would cause Emacs to insert "Process NAME finished" in buffer
    (set-process-sentinel proc #'ignore)
    ;; Delete the process (if still "live")
    (when (process-live-p proc)
      (delete-process proc))
    ;; Then call sentinel with proper event
    (when sentinel
      (funcall sentinel proc event))))

;;; RPC helpers

(defun flit--format-rpc-result-extra (method result)
  "Format extra info for RPC RESULT based on METHOD."
  (pcase method
    ("fs/info"
     (let* ((exists (plist-get result :exists))
            (type (plist-get result :type))
            (size (plist-get result :size))
            (has-content (and (plist-get result :content) t))
            (num-children (length (plist-get result :children)))
            (num-cached (length (plist-get result :cache))))
       (format " [exists=%s type=%s size=%s content=%s children=%d cached=%d]"
               exists type size has-content num-children num-cached)))
    (_ "")))

(defun flit--send-request (host method params &optional timeout)
  "Send RPC request to HOST and wait for response.
METHOD is the RPC method name, PARAMS is a plist of parameters.
TIMEOUT is optional seconds to wait (default `flit-timeout').
Returns the result plist or signals an error."
  (let ((conn (flit--get-connection host)))
    (unless conn
      (error "Cannot connect to flit-server on %s" host))
    (let ((start-time (float-time)))
      (condition-case err
          (let ((result (flitrpc-request-sync
                         (flit-connection-rpc conn) method params
                         (or timeout flit-timeout))))
            (flit--log-info "RPC %s %S -> %.3fs%s"
                            method (flit--sanitize-params-for-log params)
                            (- (float-time) start-time)
                            (flit--format-rpc-result-extra method result))
            ;; flitrpc wraps result in (:t 2 :id N :result RESULT :payload BYTES)
            (let ((extracted (plist-get result :result))
                  (payload (plist-get result :payload)))
              (when payload
                (setq extracted (plist-put (copy-sequence extracted) :content payload)))
              extracted))
        (flitrpc-error
         (flit--log-error "RPC %s %S -> ERROR after %.3fs: %S"
                          method (flit--sanitize-params-for-log params)
                          (- (float-time) start-time) err)
         (signal 'flitrpc-error (cdr err)))
        (quit
         (flit--log-info "C-g interrupted RPC %s on %s after %.3fs\n  backtrace: %s"
                          method host (- (float-time) start-time)
                          (flit--simple-backtrace))
         (signal 'quit nil))))))

(defun flit--send-request-async (host method params &optional success-fn error-fn)
  "Send an async RPC request to HOST.
METHOD is the RPC method name, PARAMS is a plist of parameters.
SUCCESS-FN is called with the result on success.
ERROR-FN is called with the error on failure."
  (let ((conn (flit--get-connection host)))
    (unless conn
      (error "Cannot connect to flit-server on %s" host))
    (flit--log-info "RPC async %s %S" method (flit--sanitize-params-for-log params))
    (flitrpc-request (flit-connection-rpc conn) method params
                     (lambda (meta _payload)
                       (funcall (or success-fn #'ignore) (plist-get meta :result)))
                     (or error-fn #'ignore))))

(defun flit--send-notify (host method params)
  "Send notification to HOST (fire-and-forget).
METHOD is the RPC method name, PARAMS is a plist of parameters."
  (let ((conn (flit--get-connection host)))
    (when conn
      (flitrpc-notify (flit-connection-rpc conn) method params))))

;;; File operations

(defun flit--get-info (filename &optional force-refresh)
  "Get full file info for FILENAME, using cache unless FORCE-REFRESH.
Returns the info plist from fs/info, or nil if file doesn't exist."
  (flit--with-parsed (host path) filename
    (let ((cached (unless force-refresh (flit--cache-get host path))))
      (cond
       ;; Direct cache hit
       (cached cached)
       ;; Check if parent directory has entries or is known non-existent
       ((let* ((norm-path (flit--normalize-path path))
               (parent-path (directory-file-name (file-name-directory norm-path)))
               (basename (file-name-nondirectory norm-path))
               (parent-info (and (not (string= norm-path "/"))
                                 (flit--cache-get host parent-path)))
               ;; Also check grandparent to see if parent exists
               (grandparent-path (and (not (string= parent-path "/"))
                                      (directory-file-name (file-name-directory parent-path))))
               (parent-basename (file-name-nondirectory parent-path))
               (grandparent-info (and grandparent-path
                                      (flit--cache-get host grandparent-path))))
          (flit--log-trace "Checking ancestors for %s: parent=%s gp=%s"
                           path parent-path grandparent-path)
          (cond
           ;; Parent is known to not exist - file can't exist either
           ((eq (plist-get parent-info :exists) :json-false)
            (flit--log-trace "File %s: parent doesn't exist - returning exists=false" norm-path)
            (list :exists :json-false :path norm-path))
           ;; Parent has children list and file not in it - file doesn't exist
           ((and (plist-get parent-info :children)
                 (not (flit--find-entry parent-info basename)))
            (flit--log-trace "File %s not in parent children - returning exists=false" norm-path)
            (list :exists :json-false :path norm-path))
           ;; Grandparent has children list and parent not in it - parent doesn't exist
           ((and (plist-get grandparent-info :children)
                 (not (flit--find-entry grandparent-info parent-basename)))
            (flit--log-trace "File %s: parent not in grandparent children - returning exists=false" norm-path)
            (list :exists :json-false :path norm-path)))))
       ;; Cache miss - make RPC call
       (t
        (let ((result (flit--send-request host "fs/info" `(:path ,path))))
          ;; Cache the main result unless server says not to (volatile paths)
          (when (and (plist-get result :path)
                     (not (eq (plist-get result :noCache) t)))
            (flit--cache-put host (plist-get result :path) result))
          ;; Also process any additional cache entries from :cache field
          (flit--cache-from-response host result)
          ;; Return cached value (has decoded content) not original result
          (or (flit--cache-get host path) result)))))))

(defun flit--stat (filename)
  "Get file attributes for FILENAME (from cached info)."
  (flit--get-info filename))

(defun flit--read (filename)
  "Read contents of FILENAME.
Returns a plist with :content and :stat, or nil if read fails.
Uses cached content if available, otherwise fetches via fs/read."
  (flit--with-parsed (host path) filename
    (let* ((info (flit--get-info filename))
           (cached-content (plist-get info :content)))
      (if cached-content
          ;; Use cached content (already decoded on cache) and existing stat info
          (progn
            (flit--log-trace "Using cached content for %s" path)
            (list :content cached-content
                  :stat info))
        ;; Need to fetch content (file > 1MB or not cached)
        (let* ((result (flit--send-request host "fs/read" `(:path ,path)))
               (content (plist-get result :content))
               (stat (plist-get result :stat)))
          (when content
            (flit--cache-put host path stat)
            (list :content content
                  :stat stat)))))))

(defun flit--write (filename content &optional expected-mtime)
  "Write CONTENT to FILENAME and update cache.
EXPECTED-MTIME is the mtime we expect the file to have
\(from visited-file-modtime).
If nil or 0, we expect the file to not exist (new file).
If the file's actual state doesn't match, prompts user before
overwriting."
  (flit--with-parsed (host path) filename
    ;; Encode using buffer's coding system — sent as raw bytes via msgpack
    ;; If coding system is undecided or nil, use UTF-8
    (let* ((coding (or buffer-file-coding-system 'utf-8-unix))
           (base (coding-system-base coding))
           (actual-coding (if (eq base 'undecided) 'utf-8-unix coding))
           (raw-bytes (encode-coding-string content actual-coding))
           ;; Convert expected-mtime: nil/0 means "expect non-existent"
           (expect-mtime (when (and expected-mtime
                                    (not (equal expected-mtime 0))
                                    (not (equal expected-mtime '(0 0))))
                           (if (listp expected-mtime)
                               (float-time expected-mtime)
                             expected-mtime)))
           (params `(:path ,path :content (:bin . ,raw-bytes)))
           result)
      ;; Add expectedMtime if we have an expectation
      (when expect-mtime
        (setq params (plist-put params :expectedMtime expect-mtime)))
      ;; If no expected mtime (new file), tell server we expect file to not exist
      (unless expect-mtime
        (setq params (plist-put params :expectNotExist t)))
      (setq result (flit--send-request host "fs/write" params))
      ;; Handle mismatch response
      (when (eq (plist-get result :mismatch) t)
        (flit--log-info "Write mismatch for %s - file changed on disk" path)
        ;; Update cache with server's current file info
        (when-let ((current (plist-get result :current)))
          (flit--cache-put host path current))
        ;; Use Emacs's standard supersession handling
        ;; This prompts "File has changed since visited. Save anyway?"
        ;; If user says no, it signals 'file-supersession and aborts
        ;; If user says yes, it returns normally
        (ask-user-about-supersession-threat filename)
        ;; User said yes - retry with force
        (setq params (plist-put params :force t))
        (setq result (flit--send-request host "fs/write" params)))
      ;; Server returns updated file info (without content since client already has it)
      ;; Add the content we just wrote (raw bytes) before caching
      (when (and result (not (plist-get result :mismatch)) (< (length content) (* 1024 1024)))
        (setq result (plist-put result :content raw-bytes)))
      ;; Use unified cache function
      (when (and result (not (plist-get result :mismatch)))
        (flit--cache-from-response host result))
      result)))

(defun flit--list (filename)
  "List directory contents of FILENAME.
Returns a list of child cache entries (plists with :name, :type, :size, etc.).
Each child entry is looked up from the cache by its full path.
Children from server have :name and :isDir, used as fallback if not cached."
  (let* ((parsed (flit--parse-file-name filename))
         (host (car parsed))
         (dir-path (cdr parsed))
         (info (flit--get-info filename))
         (children (plist-get info :children)))
    ;; If no children cached, force refresh
    (unless children
      (setq info (flit--get-info filename t))
      (setq children (plist-get info :children)))
    ;; Look up each child's cache entry
    (when children
      (let ((dir-with-slash (file-name-as-directory dir-path))
            (results '()))
        (seq-doseq (child children)
          ;; Each child is a plist with :name and :isDir
          (let* ((name (plist-get child :name))
                 (is-dir (eq (plist-get child :isDir) t))
                 (child-path (concat dir-with-slash name))
                 (child-info (flit--cache-get host child-path)))
            ;; If child info is cached, use it; otherwise use info from children list
            (if child-info
                (push (plist-put (copy-sequence child-info) :name name) results)
              ;; Use name and isDir from children list (no full stat, but enough for completions)
              (push (list :name name :exists t :isDir is-dir) results))))
        (nreverse results)))))

(defun flit--open-dir-async (filename)
  "Signal that client is browsing directory FILENAME.
Fire-and-forget call to fs/openDir, which triggers async child entry fetch
with higher limit (10k vs 1k default). Used by find-file and dired."
  (condition-case nil
      (flit--with-parsed (host path) filename
        (flit--log-info "RPC fs/openDir (async): %s" path)
        (flit--send-request-async host "fs/openDir" `(:path ,path)
                                  (lambda (_result)
                                    (flit--log-debug "fs/openDir complete: %s" path))
                                  (lambda (err)
                                    (flit--log-debug "fs/openDir error: %s - %s" path err))))
    (error nil)))  ; Silently ignore errors - this is best-effort

(defun flit--format-mode-string (mode is-dir)
  "Format MODE as a mode string like ls -l output.
IS-DIR indicates if this is a directory."
  (let ((type-char (if is-dir "d" "-"))
        (user-r (if (> (logand mode #o400) 0) "r" "-"))
        (user-w (if (> (logand mode #o200) 0) "w" "-"))
        (user-x (if (> (logand mode #o100) 0) "x" "-"))
        (group-r (if (> (logand mode #o040) 0) "r" "-"))
        (group-w (if (> (logand mode #o020) 0) "w" "-"))
        (group-x (if (> (logand mode #o010) 0) "x" "-"))
        (other-r (if (> (logand mode #o004) 0) "r" "-"))
        (other-w (if (> (logand mode #o002) 0) "w" "-"))
        (other-x (if (> (logand mode #o001) 0) "x" "-")))
    (concat type-char user-r user-w user-x group-r group-w group-x other-r other-w other-x)))

(defun flit--stat-to-attributes (stat &optional id-format)
  "Convert STAT plist to Emacs file-attributes list.
STAT can be a full stat result or a list entry - handles both formats.
ID-FORMAT controls uid/gid format: `integer' for numeric, otherwise strings.
Returns a 12-element list matching `file-attributes' return value."
  (let* ((type (plist-get stat :type))
         (is-dir (or (string= type "directory")
                     (eq (plist-get stat :isDir) t)))
         (size (or (plist-get stat :size) 0))
         (mtime (or (plist-get stat :mtime) 0))
         (mode (or (plist-get stat :mode) (if is-dir #o755 #o644)))
         ;; Use numeric IDs if requested, otherwise prefer string names
         (uid (if (eq id-format 'integer)
                  (or (plist-get stat :uid) 0)
                (or (plist-get stat :user) (plist-get stat :uid) 0)))
         (gid (if (eq id-format 'integer)
                  (or (plist-get stat :gid) 0)
                (or (plist-get stat :group) (plist-get stat :gid) 0)))
         (nlink (or (plist-get stat :nlink) 1))
         (target (plist-get stat :target)))
    (list
     (cond (is-dir t)
           ((string= type "symlink") target)
           (t nil))                      ; 0: file type
     nlink                                ; 1: link count
     uid                                  ; 2: uid (string or int)
     gid                                  ; 3: gid (string or int)
     (list 0 mtime)                       ; 4: atime
     (list 0 mtime)                       ; 5: mtime
     (list 0 mtime)                       ; 6: ctime
     size                                 ; 7: size
     (flit--format-mode-string mode is-dir) ; 8: modes
     nil                                  ; 9: gid change
     0                                    ; 10: inode
     0)))                                 ; 11: device

(defun flit--insert-directory-entry (name attrs)
  "Insert a directory entry for NAME with ATTRS in ls -l format."
  (let* ((is-dir (car attrs))
         (nlink (or (nth 1 attrs) 1))
         (uid (or (nth 2 attrs) 0))
         (gid (or (nth 3 attrs) 0))
         (size (or (nth 7 attrs) 0))
         (mtime (nth 5 attrs))
         (mode-str (or (nth 8 attrs) (if is-dir "drwxr-xr-x" "-rw-r--r--")))
         (time-str (if mtime
                       (format-time-string "%b %e %H:%M" (seconds-to-time (if (listp mtime) (cadr mtime) mtime)))
                     "Jan  1 00:00"))
         ;; Format uid/gid - can be string (username) or integer
         (uid-str (if (stringp uid) uid (format "%d" uid)))
         (gid-str (if (stringp gid) gid (format "%d" gid))))
    (insert (format "%s %3d %-8s %-8s %8d %s %s\n"
                    mode-str nlink uid-str gid-str size time-str name))))

(defun flit--exists (filename)
  "Check if FILENAME exists (from cached info)."
  (let ((info (flit--get-info filename)))
    (eq (plist-get info :exists) t)))

(defun flit--realpath (filename)
  "Get the realpath for FILENAME (from cached info)."
  (let ((info (flit--get-info filename)))
    (or (plist-get info :realpath)
        (plist-get info :path))))

(defun flit--exec-run (host cmd args cwd stdin)
  "Execute CMD with ARGS on HOST in directory CWD.
STDIN is optional input string.
Returns plist with :stdout :stderr :exitCode.
Implemented using exec/start and waiting for completion."
  (let* ((stdout-buf (generate-new-buffer " *flit-exec-stdout*"))
         (stderr-buf (generate-new-buffer " *flit-exec-stderr*"))
         ;; Create stderr pipe process with buffer
         (stderr-proc (make-pipe-process
                       :name "flit-sync-exec-stderr"
                       :buffer stderr-buf
                       :noquery t
                       :filter #'flit--default-process-filter))
         ;; Start the main process - setup buffer/filter/stderr before RPC
         ;; to handle exec/exit arriving during the synchronous RPC call
         (proc (flit--exec-start host "flit-sync-exec" cmd args cwd nil
                                 nil nil nil
                                 (lambda (p)
                                   (set-process-buffer p stdout-buf)
                                   (set-process-filter p #'flit--default-process-filter)
                                   (process-put p 'flit-stderr-proc stderr-proc)))))
    ;; Send stdin if provided
    (when stdin
      (process-send-string proc stdin)
      (process-send-eof proc))
    ;; Wait for process to exit
    (flit--with-quit-log (format "sync exec %s on %s" cmd host)
      (while (process-live-p proc)
        (accept-process-output nil 0.1)))
    ;; Extract output
    (let ((stdout (with-current-buffer stdout-buf (buffer-string)))
          (stderr (with-current-buffer stderr-buf (buffer-string)))
          (exit-code (process-exit-status proc)))
      ;; Clean up
      (delete-process stderr-proc)
      (kill-buffer stdout-buf)
      (kill-buffer stderr-buf)
      ;; Return collected output
      (list :stdout stdout
            :stderr stderr
            :exitCode exit-code))))

(defun flit--exec-start (host name cmd args cwd env &optional pty rows cols setup-fn)
  "Start async process CMD with ARGS on HOST.
NAME is the process name for Emacs.
CWD is the working directory on the remote.
ENV is an alist of environment variables.
If PTY is non-nil, allocate a pseudo-terminal with ROWS x COLS dimensions.
SETUP-FN, if non-nil, is called with the process object before the RPC call.
Use this to set buffer, filter, sentinel, etc. before the exec/start RPC,
so that if exec/exit arrives during the synchronous RPC, the correct
sentinel and filter are already installed.
Returns an Emacs process object."
  (let* (;; Generate unique proc-id on client side to avoid race conditions
         ;; between exec/exit notifications and process registration
         (proc-id (flit--generate-proc-id))
         ;; Ensure we have a valid process name
         (proc-name (if (or (null name) (string= name ""))
                        (format "flit-%s" (file-name-nondirectory cmd))
                      name))
         ;; Create a pipe process for receiving output and attaching sentinels.
         ;; We advise process-status/process-exit-status to return correct values.
         (proc (make-pipe-process
                :name proc-name
                :noquery t
                :coding 'utf-8-unix
                :filter #'flit--default-process-filter
                :sentinel #'flit--process-sentinel))
         (params `(:procId ,proc-id :cmd ,cmd :args ,(vconcat args))))
    ;; Store mapping BEFORE RPC so notifications can find the process
    (puthash proc-id proc flit--processes)
    (puthash proc-id host flit--process-host)
    ;; Store proc-id on process for later lookup
    (process-put proc 'flit-proc-id proc-id)
    (process-put proc 'flit-host host)
    (process-put proc 'flit-pty pty)  ; Remember if this is a PTY process
    ;; Store connection process for identity verification - if server restarts,
    ;; the new connection will have a different process object
    (let ((conn (flit--get-connection host)))
      (when conn
        (process-put proc 'flit-conn-proc (flit-conn-process conn))))
    ;; Build remaining params
    (when cwd
      (setq params (plist-put params :cwd cwd)))
    (when env
      ;; Convert alist to plist for JSON
      (let ((env-plist nil))
        (dolist (pair env)
          (setq env-plist (plist-put env-plist (intern (car pair)) (cdr pair))))
        (setq params (plist-put params :env env-plist))))
    (when pty
      (setq params (plist-put params :pty t))
      (setq params (plist-put params :rows (or rows 24)))
      (setq params (plist-put params :cols (or cols 80))))
    ;; Call setup-fn before RPC so filter/sentinel/buffer are set before
    ;; any exec/exit notification arrives during the synchronous RPC call
    (when setup-fn
      (funcall setup-fn proc))
    ;; Start the remote process - notifications may arrive during this call,
    ;; but the process is already registered so they'll be handled correctly
    (flit--log-debug "exec-start: proc=%s proc-id=%s" proc proc-id)
    (condition-case err
        (flit--send-request host "exec/start" params)
      (error
       ;; Clean up on failure
       (remhash proc-id flit--processes)
       (remhash proc-id flit--process-host)
       (delete-process proc)
       (signal (car err) (cdr err))))
    proc))

(defun flit--default-process-filter (proc output)
  "Default filter that inserts OUTPUT into PROC's buffer.
Uses `inhibit-read-only' because flit delivers output manually via
notification handlers, not through Emacs's internal process handling
which sets inhibit-read-only itself.  The process buffer may be
read-only (e.g., compilation-mode sets it before make-process returns
and before the real filter is installed)."
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (let* ((inhibit-read-only t)
             (mark (process-mark proc))
             ;; Ensure mark is in this buffer
             (mark-pos (if (and (markerp mark)
                                (eq (marker-buffer mark) (current-buffer)))
                           (marker-position mark)
                         (point-max)))
             (moving (= (point) mark-pos)))
        (save-excursion
          (goto-char mark-pos)
          (insert output)
          ;; Update mark to new position
          (set-marker (process-mark proc) (point)))
        (when moving
          (goto-char (process-mark proc)))))))

(defun flit--process-sentinel (proc event)
  "Sentinel for flit processes.
PROC is the process, EVENT describes what happened."
  (flit--log-debug "Process %s sentinel: %s" (process-name proc) event))

;;; File watching

(defvar-local flit--file-changed nil
  "Non-nil if the file has changed on disk since last read/write.")

(defun flit--watch (filename)
  "Notify server that we opened FILENAME in a buffer.
Server will watch the file and push notifications when it changes."
  (when (flit--file-name-p filename)
    (flit--with-parsed (host path) filename
      ;; Use async request to avoid blocking and interfering with other requests
      (flit--send-request-async host "fs/open" `(:path ,path)
                                (lambda (_result)
                                  (flit--log-debug "Opened (watching): %s" filename))
                                (lambda (err)
                                  (flit--log-error "Failed to open %s: %s" filename err))))))

(defun flit--unwatch (filename)
  "Notify server that we closed FILENAME's buffer.
Server will stop watching and stop pushing notifications."
  (when (flit--file-name-p filename)
    ;; Ignore errors if disconnected - server doesn't need to know
    (condition-case nil
        (flit--with-parsed (host path) filename
          ;; Use async request - no need to block on close
          (flit--send-request-async host "fs/close" `(:path ,path)
                                    (lambda (_result)
                                      (flit--log-debug "Closed (unwatching): %s" filename))
                                    (lambda (err)
                                      (flit--log-error "Failed to close %s: %s" filename err))))
      (flit-disconnected nil))))

(defun flit--setup-file-watch ()
  "Set up file watching for the current buffer if it's a flit file."
  (when (and buffer-file-name (flit--file-name-p buffer-file-name))
    ;; Use async watch request - it won't block or interfere with other requests
    (flit--watch buffer-file-name)
    ;; Set buffer-stale-function for auto-revert integration
    ;; This is the proper hook - verify-visited-file-modtime doesn't go
    ;; through file-name-handler-alist
    (setq-local buffer-stale-function #'flit--buffer-stale-p)
    ;; Enable auto-revert for this buffer even though it's "remote"
    ;; This is safe because flit uses push-based file watching, not polling
    (setq-local auto-revert-remote-files t)
    ;; Unwatch when buffer is killed
    (add-hook 'kill-buffer-hook #'flit--teardown-file-watch nil t)))

(defun flit--buffer-stale-p (&optional _noconfirm)
  "Return non-nil if the current flit buffer needs reverting.
This is called by `auto-revert-mode' to check if the file changed."
  ;; If buffer has unsaved modifications, don't report as stale.
  ;; This prevents auto-revert from attempting to revert (which it would skip
  ;; anyway due to buffer-modified-p check), and preserves the flit--file-changed
  ;; flag so we can revert later when the buffer becomes unmodified.
  (if (buffer-modified-p)
      (progn
        (when flit--file-changed
          (flit--log-info "buffer-stale-p: %s has pending change but buffer-modified-p=t, preserving flag"
                          (buffer-name)))
        nil)
    (let ((stale flit--file-changed))
      (when stale
        ;; Clear the flag - auto-revert will revert the buffer
        (flit--log-info "buffer-stale-p: %s reporting stale=t (timer-based revert)"
                        (buffer-name))
        (setq flit--file-changed nil))
      (flit--log-trace "buffer-stale-p: %s stale=%s visited-modtime=%s"
                       (buffer-name) stale (visited-file-modtime))
      stale)))

(defun flit--teardown-file-watch ()
  "Tear down file watching for the current buffer."
  (when (and buffer-file-name (flit--file-name-p buffer-file-name))
    (flit--unwatch buffer-file-name)))

(defun flit--reregister-watches (host)
  "Re-register file watches for all buffers visiting files on HOST.
Call this after reconnecting to ensure server knows about open files.
Also marks buffers as potentially stale to trigger revert check."
  (let ((count 0))
    (dolist (buf (buffer-list))
      (when-let* ((file (buffer-file-name buf)))
        (when (and (flit--file-name-p file)
                   (equal (flit--host file) host))
          (flit--watch file)
          ;; Mark buffer as potentially changed so auto-revert will check
          ;; BUT only if buffer has no unsaved modifications - don't lose user's work!
          (with-current-buffer buf
            (unless (buffer-modified-p)
              (setq flit--file-changed t)))
          (setq count (1+ count)))))
    (when (> count 0)
      (flit--log-info "Re-registered watches for %d buffers on %s" count host))))

;; Set up watching when visiting flit files
(add-hook 'find-file-hook #'flit--setup-file-watch)

;;; File name handler

;; Operations that need special handling (filename not in first arg position)
(defconst flit--special-operations
  '(expand-file-name set-visited-file-modtime verify-visited-file-modtime
    file-truename write-region
    lock-file unlock-file make-auto-save-file-name start-file-process
    process-file file-name-completion file-name-all-completions
    file-newer-than-file-p make-process exec-path
    shell-command)
  "Operations where the filename isn't simply the first argument.")

(defvar flit--current-path nil
  "The current flit path being accessed. Used for debugging.")

(defconst flit--probe-operations
  '(file-exists-p file-readable-p file-writable-p file-directory-p
    file-regular-p file-symlink-p file-attributes file-modes
    file-newer-than-file-p file-executable-p)
  "Operations that probe file state without loading content.")

(defun flit--file-name-handler (operation &rest args)
  "Handle file OPERATION for flit files."
  ;; Normalize /! to /flit!
  (when (and (stringp (car args))
             (string-prefix-p flit--short-prefix (car args)))
    (setcar args (concat flit--prefix
                         (substring (car args) (length flit--short-prefix)))))
  (let* ((filename (car args))
         ;; Special operations (make-process, etc.) don't have a file path as
         ;; first arg — use default-directory instead.
         (flit--current-path (if (and (stringp filename)
                                      (flit--file-name-p filename))
                                 filename
                               default-directory))
         ;; Suppress connection attempts for probe ops on deferred buffers
         (non-essential (or non-essential
                            (and (memq operation flit--probe-operations)
                                 buffer-file-name
                                 (gethash buffer-file-name flit--deferred-buffers))))
         ;; Connection tier: connect for explicit user actions, passive otherwise.
         ;; Respect an already-set tier (e.g., from tests or flit-connect).
         (flit--connection-tier
          (or flit--connection-tier
              (if (and this-command
                       (not non-essential))
                  ;; Interactive command.  Connect for non-probe operations.
                  ;; For probe operations, only connect if the flit buffer is
                  ;; visible — this ensures save-buffer/revert-buffer trigger
                  ;; connections (their early probes like file-exists-p run on
                  ;; the visible buffer), while background probes (marginalia,
                  ;; vc-mode) stay passive.
                  (if (or (not (memq operation flit--probe-operations))
                          (and (zerop (recursion-depth))
                               (stringp flit--current-path)
                               (flit--file-name-p flit--current-path)
                               (let ((buf (cl-find-if
                                           (lambda (b)
                                             (equal (buffer-file-name b)
                                                    flit--current-path))
                                           (buffer-list))))
                                 (and buf (get-buffer-window buf 'visible)))))
                      'connect
                    'passive)
                'passive))))
    (flit--log-trace "Handler: %s %S (filename-type=%s flit-p=%s)"
                     operation
                     (if (> (length args) 3) (list (car args) '...) args)
                     (type-of filename)
                     (and (stringp filename) (flit--file-name-p filename)))
    ;; Fast path: non-flit filename, non-special operation
    (if (and (not (memq operation flit--special-operations))
             (or (not (stringp filename))
                 (not (flit--file-name-p filename))))
        ;; Not a flit path - pass through with inhibited handlers
        (flit--call-default operation args)
      ;; Handle flit paths
      (cond
       ;; File existence checks
       ((eq operation 'file-exists-p)
        (condition-case nil
            (flit--exists filename)
          (flit-disconnected nil)))

       ((eq operation 'file-readable-p)
        (condition-case nil
            (flit--exists filename)
          (flit-disconnected nil)))

       ((eq operation 'file-writable-p)
        t) ; Assume writable for now

       ;; File type checks
       ((eq operation 'file-directory-p)
        (condition-case nil
            (let ((stat (flit--stat filename)))
              ;; Check isDir which is true for directories AND symlinks to directories
              (eq (plist-get stat :isDir) t))
          (flit-disconnected nil)))

       ((eq operation 'file-regular-p)
        (condition-case nil
            (let ((stat (flit--stat filename)))
              (string= (plist-get stat :type) "file"))
          (flit-disconnected nil)))

       ((eq operation 'file-symlink-p)
        (condition-case nil
            (let ((stat (flit--stat filename)))
              (when (string= (plist-get stat :type) "symlink")
                (plist-get stat :target)))
          (flit-disconnected nil)))

       ;; File attributes
       ((eq operation 'file-attributes)
        (condition-case nil
            (let* ((id-format (nth 1 args))
                   (stat (flit--stat filename)))
              (when (eq (plist-get stat :exists) t)
                (flit--stat-to-attributes stat id-format)))
          (flit-disconnected nil)))

       ;; Reading files
       ((eq operation 'insert-file-contents)
        (flit--log-debug "insert-file-contents: %s" filename)
        (let* ((visit (nth 1 args))
               (beg (nth 2 args))
               (end (nth 3 args))
               (replace-arg (nth 4 args))
               ;; REPLACE can be t, nil, or 'if-regular (replace if file is regular)
               (replace (or (eq replace-arg t)
                            (eq replace-arg 'if-regular)))
               (info (flit--get-info filename)))
          ;; If cache says file doesn't exist, bust parent cache and retry.
          ;; This handles newly created files (like commit temp files) that
          ;; aren't in the cached parent directory listing yet.
          (when (not (eq (plist-get info :exists) t))
            (flit--log-debug "insert-file-contents: cache miss for %s, busting parent cache" filename)
            (flit--with-parsed (host path) filename
              (let ((parent-path (directory-file-name (file-name-directory path))))
                (flit--cache-invalidate host parent-path)))
            (setq info (flit--get-info filename t)))  ; force refresh
          (cond
           ;; File doesn't exist (confirmed by RPC after cache bust)
           ((not (eq (plist-get info :exists) t))
            (when visit
              (setq buffer-file-name filename))
            (signal 'file-missing (list "Opening input file"
                                        "No such file or directory"
                                        filename)))
           ;; It's a directory - signal error so Emacs opens dired
           ((string= (plist-get info :type) "directory")
            (signal 'file-error (list "Opening input file"
                                      "Is a directory"
                                      filename)))
           ;; Regular file - read it
           (t
            (let* ((read-result (flit--read filename))
                   (content (plist-get read-result :content))
                   (stat (plist-get read-result :stat))
                   (len (length content))
                   (pt-before (point))
                   (mtime (plist-get stat :mtime))
                   inserted-chars)
              (when (and beg end)
                (setq content (substring content beg (min end len))))
              (if replace
                  ;; Replace mode (revert): use replace-buffer-contents to preserve
                  ;; point, markers, and window positions via minimal diff.
                  ;; Also preserve mark ring to avoid polluting navigation history.
                  (let ((source-buf (generate-new-buffer " *flit-source*"))
                        (saved-mark-ring mark-ring)
                        (saved-mark (mark-marker)))
                    (flit--log-info "insert-file-contents: %s REPLACING content (revert) file-mtime=%s"
                                    (buffer-name) mtime)
                    (unwind-protect
                        (progn
                          (with-current-buffer source-buf
                            (insert content)
                            (decode-coding-inserted-region (point-min) (point-max)
                                                           filename nil nil nil t))
                          (replace-buffer-contents source-buf)
                          (setq inserted-chars (buffer-size))
                          ;; Restore mark ring
                          (setq mark-ring saved-mark-ring)
                          (when (marker-position saved-mark)
                            (set-marker (mark-marker) (marker-position saved-mark))))
                      (kill-buffer source-buf)))
                ;; Insert mode: just insert at point
                (let ((start (point)))
                  (insert content)
                  (decode-coding-inserted-region start (point) filename visit nil nil nil)
                  (setq inserted-chars (- (point) start))
                  (goto-char pt-before)))
              (when visit
                (let ((old-modtime (float-time (visited-file-modtime))))
                  (setq buffer-file-name filename)
                  ;; Set modtime directly using the mtime we already have.
                  ;; Bypass the handler since we have the value.
                  (let ((file-name-handler-alist nil))
                    (set-visited-file-modtime (seconds-to-time mtime)))
                  (set-buffer-modified-p nil)
                  ;; Clear deferred state if this file was deferred - it's now loaded
                  (remhash filename flit--deferred-buffers)
                  (flit--log-trace "insert-file-contents visit: %s old-modtime=%s new-mtime=%s visited-modtime=%s modified=%s"
                                   filename old-modtime mtime (visited-file-modtime)
                                   (buffer-modified-p))))
              (list filename inserted-chars))))))

       ;; Writing files
       ((eq operation 'write-region)
        (let* ((start (nth 0 args))
               (end (nth 1 args))
               (filename (nth 2 args))
               (_append (nth 3 args))
               (visit (nth 4 args)))
          ;; Check if filename is a flit path
          (if (and (stringp filename) (flit--file-name-p filename))
              (let ((content (if (stringp start)
                                 start
                               ;; Widen to ensure we capture the full buffer,
                               ;; not just the narrowed region
                               (save-restriction
                                 (widen)
                                 (buffer-substring-no-properties
                                  (or start (point-min))
                                  (or end (point-max))))))
                    ;; Get expected modtime if we're saving the buffer's visited file
                    (expected-mtime (when (and buffer-file-name
                                               (string= buffer-file-name filename))
                                      (visited-file-modtime))))
                ;; DEBUG: Log suspicious writes that should probably be local
                (when (string-match-p "\\.emacs\\.desktop\\|/\\.emacs\\.d/" filename)
                  (flit--log-error "SUSPICIOUS WRITE to %s\nBacktrace:\n%s"
                                   filename
                                   (with-output-to-string (backtrace))))
                (flit--log-info "write-region: %s expected-mtime=%s visit=%s"
                                (file-name-nondirectory filename)
                                (and expected-mtime (float-time expected-mtime))
                                visit)
                (flit--write filename content expected-mtime)
                (when (or (eq visit t) (stringp visit))
                  (set-visited-file-modtime)
                  (flit--log-info "write-region: %s saved, new visited-modtime=%s"
                                  (file-name-nondirectory filename)
                                  (float-time (visited-file-modtime)))
                  (set-buffer-modified-p nil))
                nil)
            ;; Not a flit path - pass through
            (flit--call-default operation args))))

       ;; Directory listing
       ((eq operation 'directory-files)
        (let* ((full (nth 1 args))
               (match (nth 2 args))
               (nosort (nth 3 args))
               (entries (flit--list filename))
               (names (mapcar (lambda (e) (plist-get e :name)) entries)))
          ;; Add . and ..
          (setq names (append '("." "..") names))
          ;; Filter by match
          (when match
            (setq names (cl-remove-if-not
                         (lambda (n) (string-match-p match n))
                         names)))
          ;; Make full paths if requested
          (when full
            (let ((dir (file-name-as-directory filename)))
              (setq names (mapcar (lambda (n) (concat dir n)) names))))
          ;; Sort unless nosort
          (unless nosort
            (setq names (sort names #'string<)))
          names))

       ;; Path manipulation (local operations)
       ((eq operation 'file-name-directory)
        (flit--with-parsed (host path) filename
          (let ((dir (file-name-directory path)))
            (cond
             ;; Path is exactly ~ - it IS a directory, return ~/
             ((equal path "~") (flit--format-path host "~/"))
             ;; Path has a directory component (contains /)
             (dir (flit--format-path host dir))
             ;; No directory component - return root
             (t (flit--format-path host "/"))))))

       ((eq operation 'file-name-nondirectory)
        (flit--with-parsed (_host path) filename
          (file-name-nondirectory path)))

       ((eq operation 'directory-file-name)
        (flit--with-parsed (host path) filename
          (flit--format-path host (directory-file-name path))))

       ((eq operation 'file-name-as-directory)
        (flit--with-parsed (host path) filename
          (flit--format-path host (file-name-as-directory path))))

       ((eq operation 'get-file-buffer)
        ;; Find buffer visiting this file
        ;; Only check flit buffers - non-flit buffers can't be visiting the same file
        (let ((truename (flit--file-name-handler 'file-truename filename)))
          (cl-find-if (lambda (buf)
                        (with-current-buffer buf
                          (and buffer-file-name
                               (flit--file-name-p buffer-file-name)
                               (string= (flit--file-name-handler 'file-truename buffer-file-name)
                                        truename))))
                      (buffer-list))))

       ((eq operation 'file-remote-p)
        (let ((id (nth 1 args)))
          (cond
           ((eq id 'method) "flit")
           ((eq id 'host) (flit--host filename))
           ((eq id 'localname) (flit--path filename))
           (t (flit--format-path (flit--host filename) nil)))))

       ((eq operation 'unhandled-file-name-directory)
        ;; Return nil for flit paths - we handle everything including process execution
        nil)

       ((eq operation 'expand-file-name)
        ;; expand-file-name can be called with (NAME DEFAULT-DIRECTORY)
        ;; where either or both might be flit paths
        (let* ((name (car args))
               (default-dir (nth 1 args)))
          (cond
           ;; NAME is a flit path - expand it
           ((and (stringp name) (flit--file-name-p name))
            (flit--with-parsed (host path) name
              ;; Handle ~ specially - expand to remote home
              (let ((expanded (cond
                               ((string-prefix-p "~/" path)
                                (concat (flit--get-home-directory host)
                                        (substring path 1)))
                               ((equal path "~")
                                (flit--get-home-directory host))
                               (t
                                (expand-file-name path "/")))))
                (flit--format-path host expanded))))
           ;; NAME is absolute (starts with / or ~) - expand locally, like TRAMP does.
           ;; This ensures ~/foo always means local home, even when default-dir is remote.
           ;; To get remote home, use an explicit flit path like /flit!host/~/foo
           ((and (stringp name)
                 (file-name-absolute-p name))
            (flit--call-default 'expand-file-name (list name "/")))
           ;; NAME is relative, DEFAULT-DIR is flit - expand on remote
           ((and (stringp name) (stringp default-dir) (flit--file-name-p default-dir))
            (flit--with-parsed (host dir-path) default-dir
              ;; Handle ~ in dir-path
              (let* ((expanded-dir (cond
                                    ((string-prefix-p "~/" dir-path)
                                     (concat (flit--get-home-directory host)
                                             (substring dir-path 1)))
                                    ((equal dir-path "~")
                                     (flit--get-home-directory host))
                                    (t dir-path)))
                     (expanded (expand-file-name name expanded-dir))
                     (result (flit--format-path host expanded)))
                ;; DEBUG: Log suspicious expansions that should be local
                (when (string-match-p "\\.emacs\\.desktop\\|/\\.emacs\\.d/" name)
                  (flit--log-error "SUSPICIOUS EXPAND: name=%s default-dir=%s -> %s\nBacktrace:\n%s"
                                   name default-dir result
                                   (with-output-to-string (backtrace))))
                result)))
           ;; Fallback - expand with local default-directory
           (t
            (let ((default-directory "/"))
              (expand-file-name name (or default-dir "/")))))))

       ((eq operation 'file-truename)
        ;; Handle nil filename or non-flit paths
        (cond
         ;; Nil filename - just return nil
         ((not filename) nil)
         ;; Non-flit path - pass through to default handler
         ((not (flit--file-name-p filename))
          (flit--call-default operation args))
         ;; Handle flit path - use unified cache via flit--realpath
         (t
          (flit--with-parsed (host path) filename
            (condition-case nil
                (let ((resolved (flit--realpath filename)))
                  (flit--format-path host resolved))
              (flit-disconnected
               ;; Not connected - just return the path as-is
               filename)
              (error
               ;; If realpath fails, expand ~ if needed
               (let ((expanded (cond
                                ((string-prefix-p "~/" path)
                                 (concat (flit--get-home-directory host)
                                         (substring path 1)))
                                ((equal path "~")
                                 (flit--get-home-directory host))
                                (t
                                 (expand-file-name path "/")))))
                 (flit--format-path host expanded))))))))

       ((eq operation 'abbreviate-file-name)
        ;; Just return the filename as-is for remote files
        filename)

       ((eq operation 'substitute-in-file-name)
        ;; Expand ~ and environment variables in the path portion.
        ;; ~ must expand to the REMOTE home directory, not local.
        (flit--with-parsed (host path) filename
          (let* ((default-directory "/")
                 ;; Handle ~ specially - expand to remote home if connected,
                 ;; otherwise leave as-is (so shadowing doesn't occur prematurely)
                 (expanded-path
                  (cond
                   ((string-prefix-p "~" path)
                    ;; Try to get remote home, fall back to keeping ~ unexpanded
                    (condition-case nil
                        (let ((home (flit--get-home-directory host)))
                          (if (equal path "~")
                              home
                            (concat home (substring path 1))))
                      ;; Not connected - don't expand ~, keeps flit prefix intact
                      (error path)))
                   (t
                    ;; No ~, just substitute environment variables
                    (substitute-in-file-name path)))))
            (flit--format-path host expanded-path))))

       ((eq operation 'file-accessible-directory-p)
        (condition-case nil
            (let ((stat (flit--stat filename)))
              ;; Check isDir which is true for directories AND symlinks to directories
              (eq (plist-get stat :isDir) t))
          (flit-disconnected nil)))

       ((eq operation 'file-modes)
        (condition-case nil
            (let ((stat (flit--stat filename)))
              (plist-get stat :mode))
          (flit-disconnected nil)))

       ((eq operation 'file-executable-p)
        (condition-case nil
            (let* ((stat (flit--stat filename))
                   (mode (plist-get stat :mode)))
              ;; Check if any execute bit is set (owner, group, or other)
              (and mode (not (zerop (logand mode #o111)))))
          (flit-disconnected nil)))

       ((eq operation 'file-local-copy)
        ;; Copy remote file to local temp file and return the local path.
        ;; Used by operations that need local access (like vc-diff).
        (let ((local-file (make-temp-file "flit-local-copy-")))
          (condition-case err
              (let* ((result (flit--read filename))
                     (content (plist-get result :content)))
                (with-temp-file local-file
                  (set-buffer-multibyte nil)
                  (insert content))
                local-file)
            (error
             (delete-file local-file)
             (signal (car err) (cdr err))))))

       ((eq operation 'file-name-sans-versions)
        ;; Just return the filename as-is (no version stripping needed)
        filename)

       ((eq operation 'set-visited-file-modtime)
        ;; Update the buffer's visited file modtime from the remote file.
        ;; This is critical for undo to work correctly after revert.
        ;; When Emacs undoes past a (t . MODTIME) entry, it compares MODTIME
        ;; to the buffer's visited-file-modtime to decide if the buffer
        ;; should be marked modified.
        (let* ((time-arg (car args))
               (file buffer-file-name))
          (if time-arg
              ;; Explicit time provided - use it directly via default handler
              (let ((file-name-handler-alist nil))
                (set-visited-file-modtime time-arg))
            ;; No time provided - get modtime from remote file
            (when (and file (flit--file-name-p file))
              (condition-case nil
                  (let* ((stat (flit--stat file))
                         (mtime (plist-get stat :mtime)))
                    (when mtime
                      (let ((file-name-handler-alist nil))
                        (set-visited-file-modtime (seconds-to-time mtime)))))
                (error nil))))
          (flit--log-trace "set-visited-file-modtime: %s arg=%s result=%s"
                           (buffer-name) time-arg (visited-file-modtime)))
        ;; Clear the file-changed flag when we update modtime
        (setq-local flit--file-changed nil)
        nil)

       ((eq operation 'verify-visited-file-modtime)
        ;; Check if buffer's modtime matches file's current modtime.
        ;; This is called by save-buffer to detect external modifications.
        ;; Argument is optional buffer (defaults to current buffer).
        (let* ((buf (or (car args) (current-buffer)))
               (file (buffer-file-name buf)))
          (if (not (and file (flit--file-name-p file)))
              ;; Not a flit file, use default
              (flit--call-default operation args)
            ;; For flit files, compare modtimes
            (with-current-buffer buf
              (let ((mt (visited-file-modtime)))
                (cond
                 ;; Modtime of 0 means "don't check"
                 ((equal mt 0)
                  (flit--log-trace "verify-visited-file-modtime: %s visited=0 -> t"
                                   (buffer-name))
                  t)
                 ;; No connection or can't stat - assume unchanged
                 ((not (condition-case nil
                           (flit--stat file)
                         (error nil)))
                  (flit--log-trace "verify-visited-file-modtime: %s no-stat -> t"
                                   (buffer-name))
                  t)
                 ;; Compare modtimes (with 2 second tolerance like TRAMP)
                 (t
                  (condition-case nil
                      (let* ((stat (flit--stat file))
                             (file-mtime (plist-get stat :mtime))
                             (result (if file-mtime
                                         (< (abs (- file-mtime (float-time mt))) 2)
                                       t)))
                        (flit--log-trace "verify-visited-file-modtime: %s visited=%s file=%s -> %s"
                                         (buffer-name) (float-time mt) file-mtime result)
                        result)
                    (error
                     (flit--log-trace "verify-visited-file-modtime: %s error -> t"
                                      (buffer-name))
                     t)))))))))

       ((eq operation 'file-newer-than-file-p)
        ;; Compare two files - either could be flit or local
        (let* ((file1 (car args))
               (file2 (nth 1 args))
               (file1-flit (and (stringp file1) (flit--file-name-p file1)))
               (file2-flit (and (stringp file2) (flit--file-name-p file2))))
          ;; If neither is flit, delegate to default
          (if (not (or file1-flit file2-flit))
              (flit--call-default operation args)
            (condition-case nil
                (let* ((stat1 (if file1-flit
                                  (flit--stat file1)
                                (file-attributes file1)))
                       (stat2 (if file2-flit
                                  (flit--stat file2)
                                (file-attributes file2))))
                  ;; If either file doesn't exist, return nil
                  ;; (Note: float-time with nil arg returns current time, which is wrong)
                  (when (and stat1 stat2)
                    (let ((mtime1 (if file1-flit
                                      (plist-get stat1 :mtime)
                                    (let ((mt (nth 5 stat1)))
                                      (and mt (float-time mt)))))
                          (mtime2 (if file2-flit
                                      (plist-get stat2 :mtime)
                                    (let ((mt (nth 5 stat2)))
                                      (and mt (float-time mt))))))
                      (and mtime1 mtime2 (> mtime1 mtime2)))))
              (error nil)))))

       ;; VC integration - use VC's own backend root detection
       ;; This avoids running VCS commands (like `git ls-files`) on the remote.
       ;; We iterate over vc-handled-backends and call each backend's `root`
       ;; function, which just checks for marker directories via file-exists-p.
       ((eq operation 'vc-registered)
        (catch 'found
          (dolist (backend vc-handled-backends)
            ;; Load the backend if needed (to get its root function)
            (require (intern (format "vc-%s" (downcase (symbol-name backend)))) nil t)
            ;; Call the backend's root function - just does locate-dominating-file
            ;; Some backends (like RCS) signal vc-not-supported instead of returning nil
            (when (condition-case nil
                      (vc-call-backend backend 'root filename)
                    (vc-not-supported nil))
              (flit--log-debug "vc-registered for %s: backend=%s" filename backend)
              (vc-file-setprop filename 'vc-backend backend)
              (throw 'found backend)))
          nil))

       ((eq operation 'file-name-case-insensitive-p)
        ;; Assume case-sensitive (typical for Unix remote hosts)
        nil)

       ((eq operation 'make-backup-file-name-1)
        ;; Generate backup file name - append ~ to the remote path
        (flit--with-parsed (host path) filename
          (flit--format-path host (concat path "~"))))

       ((eq operation 'find-backup-file-name)
        ;; Find backup file name for remote files.
        ;; Use tramp-backup-directory-alist if available, otherwise use
        ;; backup-directory-alist, falling back to simple backup name.
        (let* ((localname (flit--path filename))
               (backup-dir
                (cl-loop for (regexp . dir) in (if (boundp 'tramp-backup-directory-alist)
                                                   tramp-backup-directory-alist
                                                 backup-directory-alist)
                         when (string-match-p regexp filename)
                         return dir)))
          (if backup-dir
              ;; Use the configured backup directory (local)
              (let* ((default-directory "/")
                     (expanded-dir (file-name-as-directory (expand-file-name backup-dir)))
                     ;; Flatten the remote path to create unique local backup name
                     (flat-name (replace-regexp-in-string
                                 "/" "!"
                                 (concat (flit--host filename) "!" localname))))
                (unless (file-directory-p expanded-dir)
                  (make-directory expanded-dir t))
                (list (expand-file-name (concat flat-name "~") expanded-dir)))
            ;; No backup directory configured - return nil to disable backups
            ;; (safer than writing backups to remote system)
            nil)))

       ((eq operation 'backup-file-name-p)
        ;; Check if this is a backup file
        (flit--with-parsed (_host path) filename
          (backup-file-name-p path)))

       ((eq operation 'file-selinux-context)
        '(nil nil nil nil))

       ((memq operation '(set-file-selinux-context file-acl set-file-acl))
        nil)

       ((eq operation 'set-file-modes)
        (flit--with-parsed (host path) filename
          (let ((mode (nth 1 args)))
            (flit--send-request host "fs/chmod" `(:path ,path :mode ,mode))
            ;; Invalidate cache since file attributes changed
            (flit--cache-invalidate host path)
            nil)))

       ((eq operation 'set-file-times)
        (flit--with-parsed (host path) filename
          (let* ((time (nth 1 args))
                 (mtime (if time (floor (float-time time)) 0)))
            (flit--send-request host "fs/touch" `(:path ,path :mtime ,mtime))
            ;; Invalidate cache since file attributes changed
            (flit--cache-invalidate host path)
            t)))

       ((eq operation 'file-equal-p)
        ;; Check if two files are the same
        (let* ((file2 (nth 1 args)))
          (and (stringp file2)
               (flit--file-name-p file2)
               (string= (flit--file-name-handler 'file-truename filename)
                        (flit--file-name-handler 'file-truename file2)))))

       ((eq operation 'file-in-directory-p)
        ;; Check if FILE is in DIR
        (let* ((dir (nth 1 args)))
          (if (and (stringp dir) (flit--file-name-p dir))
              (let* ((parsed-file (flit--parse-file-name filename))
                     (parsed-dir (flit--parse-file-name dir))
                     (file-host (car parsed-file))
                     (dir-host (car parsed-dir))
                     (file-path (cdr parsed-file))
                     (dir-path (file-name-as-directory (cdr parsed-dir))))
                (and (string= file-host dir-host)
                     (string-prefix-p dir-path file-path)))
            nil)))

       ((eq operation 'file-name-completion)
        ;; Return completion for FILE in DIRECTORY
        (let* ((file (car args))
               (dir (nth 1 args)))
          (flit--log "DEBUG file-name-completion: file=%S dir=%S flit-p=%S"
                     file dir (and (stringp dir) (flit--file-name-p dir)))
          (if (and (stringp dir) (flit--file-name-p dir))
              (condition-case err
                  ;; User has committed to a host — connect if needed.
                  (let ((flit--connection-tier 'connect))
                    (let* ((all (flit--file-name-handler 'file-name-all-completions file dir)))
                      (try-completion (or file "") all)))
                (error
                 (flit--log-info "Completion error: %s" (error-message-string err))
                 nil))
            ;; Not a flit directory - pass through
            (let ((inhibit-file-name-handlers
                   (cons 'flit--file-name-handler inhibit-file-name-handlers))
                  (inhibit-file-name-operation operation))
              (let ((result (apply operation args)))
                (flit--log "DEBUG file-name-completion: pass-through result=%S" result)
                result)))))

       ((eq operation 'file-name-all-completions)
        ;; Return completions for FILE in DIRECTORY
        ;; Standard behavior: return files that start with FILE prefix
        ;; Completion styles (like orderless) filter further on top of this
        (let* ((file (car args))
               (dir (nth 1 args))
               (is-flit (and (stringp dir) (flit--file-name-p dir))))
          (if is-flit
              (condition-case err
                  ;; User has committed to a host — connect if needed.
                  (let ((flit--connection-tier 'connect))
                    ;; Fire-and-forget fs/openDir to trigger high-limit async fetch
                    (flit--open-dir-async dir)
                    (let* ((entries (flit--list dir))
                           (names (mapcar (lambda (e)
                                            (let ((name (plist-get e :name))
                                                  (is-dir (eq (plist-get e :isDir) t)))
                                              (if is-dir
                                                  (file-name-as-directory name)
                                                name)))
                                          entries))
                           ;; Use all-completions for prefix matching (like TRAMP does)
                           (matches (all-completions (or file "") names)))
                      matches))
                (error
                 (flit--log-info "Completion error: %s" (error-message-string err))
                 nil))
            ;; Not a flit directory - pass through to default handler
            (flit--call-default operation args))))

       ((eq operation 'directory-files-and-attributes)
        ;; Return files with attributes
        (let* ((full (nth 1 args))
               (match (nth 2 args))
               (nosort (nth 3 args))
               (id-format (nth 4 args))
               (entries (flit--list filename))
               (result '()))
          ;; Add . and ..
          (push (cons ".." (flit--file-name-handler 'file-attributes
                                                    (flit--file-name-handler 'expand-file-name ".." filename)
                                                    id-format))
                result)
          (push (cons "." (flit--file-name-handler 'file-attributes filename id-format))
                result)
          ;; Add entries
          (dolist (e entries)
            (let* ((name (plist-get e :name)))
              (when (or (not match) (string-match-p match name))
                (push (cons (if full (concat (file-name-as-directory filename) name) name)
                            (flit--stat-to-attributes e id-format))
                      result))))
          (unless nosort
            (setq result (sort result (lambda (a b) (string< (car a) (car b))))))
          result))

       ((eq operation 'insert-directory)
        ;; Insert directory listing for dired
        ;; Fire-and-forget fs/openDir to trigger high-limit async fetch
        (flit--open-dir-async filename)
        (let* ((_switches (nth 1 args))
               (_wildcard (nth 2 args))
               (_full-directory-p (nth 3 args))
               (entries (flit--list filename)))
          ;; Insert "total" line that dired expects
          (insert "total 0\n")
          ;; Add . and .. entries
          (let ((dot-attrs (flit--file-name-handler 'file-attributes filename))
                (dotdot-attrs (flit--file-name-handler 'file-attributes
                                                       (flit--file-name-handler 'expand-file-name ".." filename))))
            (flit--insert-directory-entry "." dot-attrs)
            (flit--insert-directory-entry ".." dotdot-attrs))
          ;; Add regular entries
          (dolist (e (sort entries (lambda (a b)
                                     (string< (plist-get a :name) (plist-get b :name)))))
            (flit--insert-directory-entry
             (plist-get e :name)
             (flit--stat-to-attributes e)))))

       ((memq operation '(lock-buffer unlock-buffer file-locked-p))
        nil)

       ((eq operation 'make-auto-save-file-name)
        ;; Generate auto-save file name for remote files
        ;; Put auto-save files in a persistent local directory to avoid remote writes
        ;; and to survive system temp cleanup
        (if (and buffer-file-name (flit--file-name-p buffer-file-name))
            (flit--with-parsed (host path) buffer-file-name
              ;; Bind default-directory to local to avoid flit handler interference
              (let* ((default-directory "/")
                     (base-dir (expand-file-name "flit-auto-saves" user-emacs-directory))
                     (host-dir (expand-file-name host base-dir))
                     ;; Flatten the remote path to avoid directory structure issues
                     ;; Replace / with ! to create a unique flat filename
                     (flat-name (replace-regexp-in-string "/" "!" path))
                     (auto-save-name (format "#%s#" flat-name)))
                (unless (file-directory-p host-dir)
                  (make-directory host-dir t))
                (expand-file-name auto-save-name host-dir)))
          ;; Not a flit file - pass through
          (flit--call-default operation args)))

       ((memq operation '(lock-file unlock-file))
        (let ((file (car args)))
          (unless (and (stringp file) (flit--file-name-p file))
            (flit--call-default operation args))))

       ((eq operation 'make-directory)
        (flit--with-parsed (host path) filename
          (flit--send-request host "fs/mkdir" `(:path ,path))
          nil))

       ((eq operation 'delete-file)
        (flit--with-parsed (host path) filename
          (flit--send-request host "fs/delete" `(:path ,path))
          nil))

       ((eq operation 'delete-directory)
        (flit--with-parsed (host path) filename
          (let ((recursive (nth 1 args)))
            (flit--send-request host "fs/delete" `(:path ,path :recursive ,(if recursive t :json-false)))
            nil)))

       ((eq operation 'rename-file)
        (let* ((newname (nth 1 args)))
          (flit--with-parsed (host old-path) filename
            (let ((new-path (if (flit--file-name-p newname)
                                (cdr (flit--parse-file-name newname))
                              newname)))
              (flit--send-request host "fs/rename" `(:oldPath ,old-path :newPath ,new-path))
              nil))))

       ((eq operation 'copy-file)
        (let* ((newname (nth 1 args)))
          (flit--with-parsed (host src-path) filename
            (if (flit--file-name-p newname)
                ;; Both remote: server-side copy
                (let ((dest-path (cdr (flit--parse-file-name newname))))
                  (flit--send-request host "fs/copy" `(:src ,src-path :dest ,dest-path))
                  nil)
              ;; Remote src, local dest: read content and write directly
              (let* ((result (flit--read filename))
                     (content (plist-get result :content)))
                (with-temp-file newname
                  (set-buffer-multibyte nil)
                  (insert content)))))))

       ((eq operation 'copy-directory)
        ;; copy-directory args: (DIRECTORY NEWNAME &optional KEEP-TIME PARENTS COPY-CONTENTS)
        (let* ((newname (nth 1 args))
               (keep-time (nth 2 args))
               (_parents (nth 3 args))
               (copy-contents (nth 4 args)))
          (flit--with-parsed (host src-path) filename
            (let* ((dest-path (if (flit--file-name-p newname)
                                  (cdr (flit--parse-file-name newname))
                                newname))
                   ;; If copy-contents is nil, copy the whole directory including its name
                   (effective-dest (if copy-contents
                                       dest-path
                                     (concat (file-name-as-directory dest-path)
                                             (file-name-nondirectory
                                              (directory-file-name src-path))))))
              (flit--send-request host "fs/copy-dir"
                                  `(:src ,src-path
                                         :dest ,effective-dest
                                         :keepTime ,(if keep-time t :json-false)))
              nil))))

       ((eq operation 'dired-uncache)
        ;; Clear cached directory listing for this path
        (flit--with-parsed (host path) filename
          (flit--cache-invalidate host path)
          nil))

       ((eq operation 'temporary-file-directory)
        ;; Return remote temp directory
        (flit--with-parsed (host _path) filename
          (flit--format-path host "/tmp/")))

       ((eq operation 'access-file)
        ;; Signal error if file is not readable
        (let ((string (nth 1 args)))
          (unless (flit--exists filename)
            (signal 'file-error (list string "No such file or directory" filename)))
          nil))

       ((eq operation 'load)
        ;; Load elisp file from remote - copy locally first then load
        (let* ((noerror (nth 1 args))
               (nomessage (nth 2 args))
               (nosuffix (nth 3 args))
               (must-suffix (nth 4 args)))
          (condition-case err
              (let ((local-copy (flit--file-name-handler 'file-local-copy filename)))
                (unwind-protect
                    (load local-copy noerror nomessage nosuffix must-suffix)
                  (delete-file local-copy)))
            (error
             (if noerror nil (signal (car err) (cdr err)))))))

       ;; Async process execution (for compilation-mode, etc.)
       ;; Note: vterm and other make-process users go through the 'make-process handler instead
       ((eq operation 'start-file-process)
        ;; start-file-process args: (NAME BUFFER PROGRAM &rest PROGRAM-ARGS)
        (let* ((name (car args))
               (buffer (nth 1 args))
               (program (nth 2 args))
               (program-args (nthcdr 3 args))
               (buf (and buffer (get-buffer-create buffer))))
          (if (flit--file-name-p default-directory)
              (flit--with-parsed (host cwd) default-directory
                ;; Check if we need PTY mode (when process-connection-type is non-nil)
                (let* ((need-pty process-connection-type)
                       (rows (when need-pty
                               (or (and buf
                                        (window-live-p (get-buffer-window buf))
                                        (window-height (get-buffer-window buf)))
                                   24)))
                       (cols (when need-pty
                               (or (and buf
                                        (window-live-p (get-buffer-window buf))
                                        (window-width (get-buffer-window buf)))
                                   80)))
                       (env (flit--compute-env-delta))
                       (proc (flit--exec-start host name program program-args
                                               cwd env need-pty rows cols
                                               (when buf
                                                 (lambda (p) (set-process-buffer p buf))))))
                  proc))
            (flit--call-default operation args))))

       ;; make-process with :file-handler t
       ;; The C code calls (apply handler 'make-process contact) where contact is the plist
       ;; So args is the flattened plist: (:name "x" :buffer buf :command (...) ...)
       ((eq operation 'make-process)
        (flit--handle-make-process args))

       ;; Synchronous process execution
       ((eq operation 'process-file)
        ;; process-file args: (PROGRAM &optional INFILE DESTINATION DISPLAY &rest ARGS)
        (let* ((program (car args))
               (infile (nth 1 args))
               (destination (nth 2 args))
               (_display (nth 3 args))
               (program-args (nthcdr 4 args)))
          (if (flit--file-name-p default-directory)
              (flit--with-parsed (host cwd) default-directory
                (let* ((stdin (when (and infile (stringp infile))
                                (with-temp-buffer
                                  (insert-file-contents infile)
                                  (buffer-string))))
                       (result (flit--exec-run host program program-args cwd stdin))
                       (stdout (plist-get result :stdout))
                       (stderr (plist-get result :stderr))
                       (exit-code (plist-get result :exitCode)))
                  ;; Handle destination
                  (cond
                   ((null destination) nil)
                   ((eq destination t) (insert stdout))
                   ((or (bufferp destination) (stringp destination))
                    (with-current-buffer (get-buffer-create destination)
                      (insert stdout)))
                   ((consp destination)
                    (let ((real-dest (car destination))
                          (error-dest (cadr destination)))
                      (when real-dest
                        (if (eq real-dest t)
                            (insert stdout)
                          (with-current-buffer (get-buffer-create real-dest)
                            (insert stdout))))
                      (when error-dest
                        (cond
                         ((stringp error-dest)
                          (with-current-buffer (get-buffer-create error-dest)
                            (insert stderr)))
                         ((bufferp error-dest)
                          (with-current-buffer error-dest
                            (insert stderr))))))))
                  exit-code))
            (flit--call-default operation args))))

       ((eq operation 'shell-command)
        (let* ((command (car args))
               (output-buffer (nth 1 args))
               (error-buffer (nth 2 args))
               (async-pos (string-match "&[ \t]*\\'" command))
               (asynchronous async-pos)
               (command (if asynchronous
                            (substring command 0 async-pos)
                          command))
               (current-buffer-p nil)
               (output-buffer
                (cond
                 ((bufferp output-buffer)
                  (setq current-buffer-p (eq (current-buffer) output-buffer))
                  output-buffer)
                 ((stringp output-buffer)
                  (setq current-buffer-p
                        (eq (buffer-name (current-buffer)) output-buffer))
                  (get-buffer-create output-buffer))
                 (output-buffer
                  (setq current-buffer-p t)
                  (current-buffer))
                 (t (get-buffer-create
                     (if asynchronous
                         shell-command-buffer-name-async
                       shell-command-buffer-name)))))
               (error-buffer
                (cond
                 ((bufferp error-buffer) error-buffer)
                 ((stringp error-buffer) (get-buffer-create error-buffer))))
               (buffer (if error-buffer
                           ;; error-buffer not supported for remote error
                           ;; separation; just merge stderr into output.
                           output-buffer
                         output-buffer))
               (bname (buffer-name output-buffer))
               (proc (get-buffer-process output-buffer)))
          ;; Handle existing async process in output buffer.
          (when (and asynchronous proc)
            (cond
             ((eq async-shell-command-buffer 'confirm-kill-process)
              (if (yes-or-no-p
                   "A command is running in the default buffer.  Kill it? ")
                  (kill-process proc)
                (user-error "Shell command in progress")))
             ((eq async-shell-command-buffer 'confirm-new-buffer)
              (if (yes-or-no-p
                   "A command is running in the default buffer.  Use a new buffer? ")
                  (setq output-buffer (generate-new-buffer bname))
                (user-error "Shell command in progress")))
             ((eq async-shell-command-buffer 'new-buffer)
              (setq output-buffer (generate-new-buffer bname)))
             ((eq async-shell-command-buffer 'confirm-rename-buffer)
              (if (yes-or-no-p
                   "A command is running in the default buffer.  Rename it? ")
                  (progn
                    (with-current-buffer output-buffer
                      (rename-uniquely))
                    (setq output-buffer (get-buffer-create bname)))
                (user-error "Shell command in progress")))
             ((eq async-shell-command-buffer 'rename-buffer)
              (with-current-buffer output-buffer
                (rename-uniquely))
              (setq output-buffer (get-buffer-create bname)))))
          (with-current-buffer output-buffer
            (when current-buffer-p
              (barf-if-buffer-read-only)
              (push-mark nil t))
            (shell-command-save-pos-or-erase current-buffer-p))
          (if asynchronous
              (let* ((proc (start-file-process-shell-command
                            (buffer-name output-buffer) buffer command)))
                (when (process-live-p proc)
                  (process-put proc 'remote-command
                               (list shell-file-name shell-command-switch command))
                  (with-current-buffer output-buffer
                    (setq mode-line-process '(":%s"))
                    (require 'shell)
                    (unless (eq major-mode async-shell-command-mode)
                      (funcall async-shell-command-mode))
                    (set-process-filter proc #'comint-output-filter)
                    (set-process-sentinel proc #'shell-command-sentinel)
                    (if async-shell-command-display-buffer
                        (display-buffer output-buffer '(nil (allow-no-window . t)))
                      (let ((nonce (make-symbol "nonce")))
                        (add-function
                         :before (process-filter proc)
                         (lambda (proc _string)
                           (let ((buf (process-buffer proc)))
                             (when (buffer-live-p buf)
                               (remove-function (process-filter proc) nonce)
                               (display-buffer buf '(nil (allow-no-window . t))))))
                         `((name . ,nonce)))))))
                proc)
            ;; Synchronous.
            (prog1
                (process-file-shell-command command nil buffer)
              (if current-buffer-p
                  (progn
                    (goto-char (prog1 (mark t)
                                 (set-marker (mark-marker) (point)
                                             (current-buffer))))
                    (shell-command-set-point-after-cmd))
                (when (with-current-buffer output-buffer
                        (> (point-max) (point-min)))
                  (display-message-or-buffer output-buffer)))))))

       ;; exec-path - return remote PATH directories
       ;; Called with nil args, use default-directory to get host
       ((eq operation 'exec-path)
        (let* ((dir (or (car args) default-directory))
               (host (and (stringp dir) (flit--file-name-p dir)
                          (flit--host dir))))
          (when host
            (flit--get-remote-exec-path host))))

       ;; File notification: not supported via file-notify API
       ;; (flit uses its own watching mechanism via buffer-stale-function)
       ((eq operation 'file-notify-add-watch)
        (signal 'file-notify-error
                (list "file-notify not supported for flit files" (car args))))

       ;; Default: pass through to normal handlers for unhandled operations
       (t
        (flit--log-debug "Unhandled operation: %s %S" operation args)
        (flit--call-default operation args))))))

;;; Batch prefetching
;;
;; Used for session restore - fetches multiple files/directories in one RPC.

(defun flit--batch-prefetch (host paths callback)
  "Async batch fetch PATHS on HOST and populate caches.
CALLBACK is called with (RESULT ERROR-MSG) when done.
RESULT is the batch result on success, nil on error.
Async connection is established first; if auth is needed, fails immediately."
  (when (and host paths)
    (flit--log-info "Async batch prefetch %d paths on %s" (length paths) host)
    ;; First ensure we have a connection (async)
    (flit-connect
     host
     (lambda (_host success error-msg)
       (if (not success)
           ;; Connection failed (e.g., auth needed)
           (progn
             (flit--log-info "Batch prefetch connection failed for %s: %s"
                             host error-msg)
             (funcall callback nil error-msg))
         ;; Connected - now send the batch request
         (condition-case err
             (flit--send-request-async
              host "fs/batch" `(:paths ,(vconcat paths))
              (lambda (result)
                (flit--cache-from-response host result)
                (let ((errors (plist-get result :errors)))
                  (when errors
                    (cl-loop for (path msg) on errors by #'cddr
                             do (flit--log-debug "Batch error for %s: %s" path msg))))
                (funcall callback result nil))
              (lambda (err)
                (flit--log-error "Async batch prefetch failed for %s: %s" host err)
                (funcall callback nil (format "%s" err))))
           (error
            (flit--log-info "Batch prefetch request failed for %s: %s"
                            host (error-message-string err))
            (funcall callback nil (error-message-string err)))))))))

;;; Desktop/session restore integration
;;
;; Save flit buffers to desktop and restore them on startup.
;; Creates deferred buffers that load when user presses "g" or when
;; already connected and the buffer is displayed.

(defvar flit--deferred-buffers (make-hash-table :test 'equal)
  "Map of buffer-file-name to t for buffers that failed to load and are deferred.")

(defun flit--prefetch-deferred-buffers (host)
  "Prefetch content for deferred buffers on HOST.
Called via `flit-connection-hook' when a connection is established."
  (let ((paths nil))
    ;; Collect paths for this host
    (maphash (lambda (file-name _)
               (when (flit--file-name-p file-name)
                 (let* ((parsed (flit--parse-file-name file-name))
                        (buf-host (car parsed))
                        (path (cdr parsed)))
                   (when (equal buf-host host)
                     (push path paths)))))
             flit--deferred-buffers)
    (when paths
      (flit--log-info "Prefetching %d deferred buffers for %s" (length paths) host)
      (flit--batch-prefetch
       host paths
       (lambda (_result error-msg)
         (if error-msg
             (flit--log-info "Deferred buffer prefetch failed for %s: %s" host error-msg)
           (flit--log-info "Deferred buffer prefetch complete for %s" host)))))))

;; Register the deferred buffer prefetch
(flit-run-after-connect #'flit--prefetch-deferred-buffers)

;;; LSP workspace folder prefetch
;; Prefetch workspace folders for connected hosts when lsp-mode activates.
;; This prevents blocking f-dir? calls when lsp-mode checks workspace folders.

(with-eval-after-load 'lsp-mode
  (defvar flit--lsp-prefetched-hosts (make-hash-table :test 'equal)
    "Hosts for which LSP workspace folders have been prefetched.")

  (defun flit--prefetch-lsp-workspace-folders (host)
    "Prefetch LSP workspace folders for HOST.
This prevents blocking f-dir? calls when lsp-mode checks workspace folders."
    (let ((paths nil))
      (dolist (folder (lsp-session-folders (lsp-session)))
        (when (flit--file-name-p folder)
          (let* ((parsed (flit--parse-file-name folder))
                 (folder-host (car parsed))
                 (path (cdr parsed)))
            (when (equal folder-host host)
              (push path paths)))))
      (when paths
        (flit--log-info "Prefetching %d LSP workspace folders for %s" (length paths) host)
        (flit--batch-prefetch host paths
                              (lambda (_result error-msg)
                                (if error-msg
                                    (flit--log-info "LSP workspace prefetch failed for %s: %s" host error-msg)
                                  (flit--log-info "LSP workspace prefetch complete for %s" host)))))))

  (defun flit--maybe-prefetch-lsp-workspace-folders (host)
    "Prefetch LSP workspace folders for HOST if not already done."
    (unless (gethash host flit--lsp-prefetched-hosts)
      (puthash host t flit--lsp-prefetched-hosts)
      (flit--prefetch-lsp-workspace-folders host)))

  (defun flit--lsp-mode-hook ()
    "Prefetch LSP workspace folders for connected flit hosts.
Called from `lsp-mode-hook' when lsp-mode activates in a buffer."
    (maphash (lambda (host _conn)
               (when (flit--connection-alive-p host)
                 (flit--maybe-prefetch-lsp-workspace-folders host)))
             flit--connections))

  (add-hook 'lsp-mode-hook #'flit--lsp-mode-hook)

  ;; Also prefetch when new connections are established (if lsp session exists)
  (flit-run-after-connect #'flit--maybe-prefetch-lsp-workspace-folders))

(defvar-local flit--desktop-restore-fn nil
  "Saved function to call during deferred rehydration.
This is a lambda that captures the original desktop-create-buffer and its args.")

(defvar-local flit--desktop-saved-args nil
  "Original desktop-create-buffer args for this deferred buffer.
Used when re-saving so we preserve the original mode, not flit-deferred-mode.")

(defun flit--desktop-save-buffer (_desktop-dirname)
  "Return state to save for current flit buffer.
For deferred buffers, includes the original desktop args so we can
restore properly even if the buffer is saved as flit-deferred-mode."
  (if flit--desktop-saved-args
      ;; Deferred buffer - save original args so we can restore with correct mode
      (list :desktop-args flit--desktop-saved-args)
    ;; Normal flit buffer - just save the file name
    buffer-file-name))

;;; Deferred buffer mode

(defvar flit-deferred-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map "g" #'flit-deferred-reload)
    map)
  "Keymap for `flit-deferred-mode'.")

(define-derived-mode flit-deferred-mode special-mode "Flit-Deferred"
  "Major mode for flit deferred buffers awaiting connection.
These buffers represent remote files that couldn't be loaded because
the flit connection wasn't available. They will attempt to load
automatically when displayed (if connection state allows).

\\{flit-deferred-mode-map}"
  (setq-local revert-buffer-function #'flit-deferred-reload))

(defun flit-deferred-reload (&optional _ignore-auto _noconfirm)
  "Attempt to connect and load the deferred buffer.
Forces a sync connection attempt, then loads the file."
  (interactive)
  (unless buffer-file-name
    (user-error "No file associated with this buffer"))
  (let* ((parsed (flit--parse-file-name buffer-file-name))
         (host (car parsed)))
    ;; Force sync connection (will prompt if needed)
    (flit-connect host)
    ;; Now try to load the buffer
    (when (flit--connection-alive-p host)
      (flit--load-deferred-buffer))))

(defun flit--deferred-state-description (state)
  "Return a user-friendly description for connection STATE."
  (pcase state
    ('connected "Connected (loading...)")
    ('connecting "Connection in progress...")
    ('pending "Not connected")
    ('failed "Connection failed")
    ('disconnected "Disconnected by user")
    (_ (format "Unknown state: %s" state))))

(defun flit--deferred-action-hint (state)
  "Return action hint text for connection STATE."
  (pcase state
    ('connected "File should load automatically.")
    ('connecting "Please wait for connection to complete.")
    ('pending "Press 'g' or M-x flit-connect to connect.")
    ('failed "Press 'g' or M-x flit-connect to retry connection.")
    ('disconnected "Press 'g' or M-x flit-connect to reconnect.")
    (_ "")))

(defun flit--update-deferred-buffer-content ()
  "Update the deferred buffer content based on current connection state."
  (when (and buffer-file-name
             (derived-mode-p 'flit-deferred-mode))
    (let* ((parsed (flit--parse-file-name buffer-file-name))
           (host (car parsed))
           (state (flit--connection-state host))
           (inhibit-read-only t))
      (erase-buffer)
      (insert (format "Flit Deferred Buffer
====================

File: %s
Host: %s
Status: %s

%s

Keybindings:
  g     - Connect and reload file
  q     - Bury buffer
" buffer-file-name host
(flit--deferred-state-description state)
(flit--deferred-action-hint state)))
      (set-buffer-modified-p nil)
      (goto-char (point-min)))))

(defun flit--create-deferred-buffer (file-name buffer-name)
  "Create a deferred buffer for FILE-NAME with BUFFER-NAME.
The buffer will attempt to load the file when switched to."
  (flit--log-info "create-deferred-buffer: %s" file-name)
  (let ((buf (generate-new-buffer (or buffer-name (file-name-nondirectory file-name)))))
    (with-current-buffer buf
      (setq buffer-file-name file-name)
      ;; Set default-directory to the flit path's directory so that packages like
      ;; LSP/eglot recognize this as a remote buffer and don't start local servers.
      ;; The flit file-name-handler will handle operations on this path gracefully
      ;; (returning nil or signaling flit-disconnected as appropriate).
      (setq default-directory (file-name-directory file-name))
      (puthash file-name t flit--deferred-buffers)
      ;; Set up the mode first - major modes call kill-all-local-variables
      (flit-deferred-mode)
      ;; Set desktop-save-buffer so deferred buffers are saved correctly
      ;; (not with flit-deferred-mode as the major mode)
      (setq-local desktop-save-buffer #'flit--desktop-save-buffer)
      ;; Add hooks to auto-load when buffer is displayed (if already connected)
      (add-hook 'window-buffer-change-functions
                #'flit--maybe-load-deferred-buffer nil t)
      (add-hook 'window-selection-change-functions
                #'flit--maybe-load-deferred-buffer nil t)
      (flit--update-deferred-buffer-content))
    buf))

;; Declare desktop.el's internal variables as special for dynamic binding
(defvar desktop-first-buffer)
(defvar desktop-buffer-ok-count)
(defvar desktop-buffer-fail-count)
(defvar desktop-save)

(defun flit--load-deferred-buffer ()
  "Load the current deferred buffer's file content.
Assumes connection is already established. Returns t on success, nil on failure."
  (let ((file-name buffer-file-name)
        (restore-fn flit--desktop-restore-fn)
        (old-buffer (current-buffer)))
    (condition-case err
        (progn
          (remhash buffer-file-name flit--deferred-buffers)
          (remove-hook 'window-buffer-change-functions
                       #'flit--maybe-load-deferred-buffer t)
          (remove-hook 'window-selection-change-functions
                       #'flit--maybe-load-deferred-buffer t)
          (if restore-fn
              ;; Desktop restore - call saved restore function
              ;; Rename deferred buffer so desktop-create-buffer creates fresh,
              ;; then swap windows to avoid jarring buffer switch
              (let ((windows (get-buffer-window-list old-buffer nil t)))
                ;; Rename so find-buffer-visiting won't return it
                (with-current-buffer old-buffer
                  (setq buffer-file-name nil)
                  (rename-buffer (concat " *flit-rehydrating*" (buffer-name)) t))
                ;; Bind variables that desktop-create-buffer expects from desktop-read
                (let* ((desktop-first-buffer nil)
                       (desktop-buffer-ok-count 0)
                       (desktop-buffer-fail-count 0)
                       (desktop-save nil)
                       (new-buf (funcall restore-fn)))
                  (flit--log-info "Rehydrated %s via desktop-create-buffer" file-name)
                  ;; Replace old buffer with new in all windows
                  (dolist (win windows)
                    (when (window-live-p win)
                      (set-window-buffer win new-buf)))
                  ;; Now safe to kill the old buffer
                  (kill-buffer old-buffer)
                  (with-current-buffer new-buf
                    (flit--setup-file-watch)
                    (message "[flit] Loaded %s" file-name)
                    (redisplay t))))
            ;; Non-desktop case - load content directly
            (let ((inhibit-read-only t))
              (erase-buffer)
              (insert-file-contents file-name t)
              (kill-local-variable 'revert-buffer-function)
              (after-find-file nil nil t nil nil)
              (flit--setup-file-watch)
              (message "[flit] Loaded %s" file-name)
              (redisplay t)))
          t)
    (error
     ;; Turn into an empty buffer visiting the file path, like
     ;; find-file on a nonexistent file.  Use file-name since the
     ;; desktop restore path sets buffer-file-name to nil.
     (when restore-fn
       ;; Undo the desktop restore rename
       (rename-buffer (file-name-nondirectory file-name) t))
     (setq buffer-file-name file-name)
     (remhash file-name flit--deferred-buffers)
     (let ((inhibit-read-only t))
       (erase-buffer))
     (kill-local-variable 'revert-buffer-function)
     (fundamental-mode)
     (after-find-file nil nil t nil nil)
     (message "[flit] Failed to load %s: %s" file-name (error-message-string err))
     nil))))

(defun flit--maybe-load-deferred-buffer (_frame-or-window)
  "Load a deferred buffer if already connected, or ensure point is at start.
Does NOT attempt to connect - only loads if connection is already established."
  (when (and buffer-file-name
             (gethash buffer-file-name flit--deferred-buffers))
    (let* ((parsed (flit--parse-file-name buffer-file-name))
           (host (car parsed)))
      (if (and after-init-time (flit--connection-alive-p host))
          ;; Connected - load the buffer
          (flit--load-deferred-buffer)
        ;; Not connected or during startup - ensure point is at start
        ;; (desktop.el may have restored a saved point position)
        (goto-char (point-min))))))

(defvar flit--desktop-restoring nil
  "Non-nil while desktop is restoring buffers. Used to prevent auth prompts.")

(with-eval-after-load 'desktop
  ;; Advise desktop-create-buffer to defer entire restoration for flit paths
  (defun flit--desktop-create-buffer-advice (orig-fn &rest args)
    "Advice to defer flit buffer restoration entirely.
ARGS are the arguments to `desktop-create-buffer': file-version,
buffer-filename, buffer-name, major-mode, and other desktop state."
    (let* ((buffer-filename (nth 1 args))
           (buffer-name (nth 2 args))
           (major-mode-arg (nth 3 args))
           (misc-data (nth 8 args)))
      (if (and buffer-filename (flit--file-name-p buffer-filename))
          ;; Flit path - create deferred buffer and save restore function for later
          (progn
            (flit--log-info "desktop-create-buffer: deferring %s" buffer-filename)
            (let* ((buf (flit--create-deferred-buffer buffer-filename buffer-name))
                   ;; If saved mode was flit-deferred-mode, check if we have
                   ;; original args saved in misc-data from a previous save
                   (restored-args (and (eq major-mode-arg 'flit-deferred-mode)
                                       (listp misc-data)
                                       (plist-get misc-data :desktop-args)))
                   (fixed-args (or restored-args args)))
              (flit--log-info "desktop-create-buffer: mode=%s misc=%S restored=%S args-len=%d"
                              major-mode-arg misc-data (if restored-args t nil) (length args))
              (with-current-buffer buf
                ;; Save args for re-saving (preserves original mode if we never hydrate)
                (setq flit--desktop-saved-args fixed-args)
                (setq flit--desktop-restore-fn
                      (lambda () (apply orig-fn fixed-args)))
                ;; Ensure point is at start (desktop.el saves/restores point separately)
                (goto-char (point-min)))
              buf))
        ;; Non-flit path - use original handler
        (apply orig-fn args))))

  (advice-add 'desktop-create-buffer :around #'flit--desktop-create-buffer-advice)

  ;; Wrap entire desktop-read to set flit--desktop-restoring and fix deferred buffers after
  (defun flit--desktop-read-advice (orig-fn &rest args)
    "Advice to set flit--desktop-restoring during desktop-read.
Also resets point in all deferred buffers after restore completes,
since desktop.el restores window configurations with saved point positions."
    (let ((flit--desktop-restoring t))
      (prog1 (apply orig-fn args)
        ;; After desktop restore, reset point in all deferred buffers
        (maphash (lambda (file-name _)
                   (when-let ((buf (get-file-buffer file-name)))
                     (with-current-buffer buf
                       (goto-char (point-min)))))
                 flit--deferred-buffers))))

  (advice-add 'desktop-read :around #'flit--desktop-read-advice)

  ;; Make sure flit buffers get saved
  ;; desktop-save-buffer is buffer-local; set it in find-file-hook
  (defun flit--setup-desktop-save ()
    "Set up desktop saving for flit buffers."
    (when (and buffer-file-name (flit--file-name-p buffer-file-name))
      (setq-local desktop-save-buffer #'flit--desktop-save-buffer)))

  (add-hook 'find-file-hook #'flit--setup-desktop-save))

;;; Registration

(defconst flit--completion-regexp
  (concat "\\`/\\(?:flit\\)?" (regexp-quote (char-to-string flit--sep)))
  "Regexp matching incomplete flit paths for host completion.
Broader than `flit--file-name-regexp' — no path required.")

(defun flit--completion-handler (operation &rest args)
  "Handle completion for incomplete flit paths (host selection)."
  (cond
   ((eq operation 'file-name-all-completions)
    (let* ((file (car args))
           (hosts (flit--known-hosts))
           (candidates (mapcar (lambda (h) (concat h "/")) hosts)))
      (all-completions (or file "") candidates)))

   ((eq operation 'file-name-completion)
    (let* ((file (car args))
           (hosts (flit--known-hosts))
           (candidates (mapcar (lambda (h) (concat h "/")) hosts)))
      ;; Return nil for empty input — Emacs's completion--file-name-quoting
      ;; tries to put-text-property on the input string, which fails on "".
      ;; Vertico uses all-completions for display so candidates still show.
      (when (> (length file) 0)
        (try-completion file candidates))))

   ((eq operation 'file-name-directory)
    (let ((f (car args)))
      (cond
       ((string-prefix-p flit--prefix f) flit--prefix)
       ((string-prefix-p flit--short-prefix f) flit--short-prefix)
       (t nil))))

   ((eq operation 'file-name-nondirectory)
    (let ((f (car args)))
      (cond
       ((string-prefix-p flit--prefix f)
        (substring f flit--prefix-length))
       ((string-prefix-p flit--short-prefix f)
        (substring f (length flit--short-prefix)))
       (t ""))))

   ;; Return as-is for expand-file-name
   ((eq operation 'expand-file-name)
    (car args))

   ((eq operation 'substitute-in-file-name)
    (car args))

   ;; Delegate unknown operations to default handler
   (t (flit--call-default operation args))))

(defun flit--register ()
  "Register flit file name handlers."
  (setq file-name-handler-alist
        (cl-remove-if (lambda (x) (memq (cdr x)
                                        '(flit--file-name-handler
                                          flit--completion-handler)))
                      file-name-handler-alist))
  (push (cons flit--file-name-regexp #'flit--file-name-handler)
        file-name-handler-alist)
  (setq file-name-handler-alist
        (append file-name-handler-alist
                (list (cons flit--completion-regexp #'flit--completion-handler)))))

(defun flit--ensure-handler-priority ()
  "Ensure flit main handler is at front, completion handler at end.
Must run after TRAMP loads since TRAMP's regexp also matches flit paths."
  ;; Main handler to front
  (let ((entry (rassq 'flit--file-name-handler file-name-handler-alist)))
    (when entry
      (setq file-name-handler-alist
            (cons entry (delq entry file-name-handler-alist)))))
  ;; Completion handler to end
  (let ((entry (rassq 'flit--completion-handler file-name-handler-alist)))
    (when entry
      (setq file-name-handler-alist
            (append (delq entry file-name-handler-alist) (list entry))))))

(with-eval-after-load 'tramp
  (flit--ensure-handler-priority))


;;; Marginalia integration

(defun flit--marginalia-file-size (size)
  "Format SIZE for marginalia annotation."
  (propertize (file-size-human-readable size)
              'face 'marginalia-size))

(defun flit--marginalia-file-time (mtime)
  "Format MTIME for marginalia annotation."
  (let* ((time (seconds-to-time mtime))
         (age (float-time (time-since time))))
    (propertize
     (if (< age (* 60 60 24 14))  ; 14 days
         ;; Relative time
         (let ((secs age))
           (cond
            ((< secs 60) (format "%d sec ago" (round secs)))
            ((< secs 3600) (format "%d min ago" (round (/ secs 60))))
            ((< secs 86400) (format "%d hour ago" (round (/ secs 3600))))
            (t (format "%d day ago" (round (/ secs 86400))))))
       ;; Absolute time
       (format-time-string "%b %d %H:%M" time))
     'face 'marginalia-date)))

(defun flit--marginalia-file-owner (user group)
  "Format USER and GROUP for marginalia annotation."
  (when (or user group)
    (propertize (format "%s:%s" (or user "?") (or group "?"))
                'face 'marginalia-file-owner)))

(defun flit--marginalia-file-modes (mode is-dir)
  "Format MODE as a file mode string for marginalia."
  (let ((mode-str (flit--format-mode-string mode is-dir)))
    ;; Fontify like marginalia does
    (dotimes (i (length mode-str))
      (put-text-property
       i (1+ i) 'face
       (pcase (aref mode-str i)
         (?- 'marginalia-file-priv-no)
         (?d 'marginalia-file-priv-dir)
         (?l 'marginalia-file-priv-link)
         (?r 'marginalia-file-priv-read)
         (?w 'marginalia-file-priv-write)
         (?x 'marginalia-file-priv-exec)
         (_ 'marginalia-file-priv-other))
       mode-str))
    mode-str))

(defun flit--marginalia-annotate-file (cand)
  "Annotate flit file CAND with size, date, owner, and permissions."
  (when-let* ((full-path (flit--marginalia-full-candidate cand))
              ((flit--file-name-p full-path)))
    (condition-case err
        (flit--with-parsed (host path) full-path
          (let* ((expanded-path (if (string-prefix-p "~" path)
                                    (concat (flit--get-home-directory host)
                                            (substring path 1))
                                  path))
                 (info (or (flit--cache-get host expanded-path)
                           (flit--get-info full-path))))
            (when (and info (eq (plist-get info :exists) t))
              (let* ((size (or (plist-get info :size) 0))
                     (mtime (or (plist-get info :mtime) 0))
                     (mode (or (plist-get info :mode) #o644))
                     (is-dir (eq (plist-get info :isDir) t))
                     (user (plist-get info :user))
                     (group (plist-get info :group))
                     (sep "  "))
                (concat
                 (propertize " " 'display '(space :align-to center)
                             'marginalia--align t)
                 sep
                 (flit--marginalia-file-modes mode is-dir)
                 sep
                 (format "%7s" (flit--marginalia-file-size size))
                 sep
                 (format "%-12s" (flit--marginalia-file-time mtime))
                 sep
                 (or (flit--marginalia-file-owner user group) ""))))))
      (error
       (flit--log-debug "Marginalia annotation error for %s: %S" cand err)
       nil))))

(defun flit--marginalia-full-candidate (cand)
  "Get full path for CAND during minibuffer completion."
  (if (string-prefix-p "/" cand)
      cand
    (if-let* ((win (active-minibuffer-window)))
        (with-current-buffer (window-buffer win)
          (let* ((end (minibuffer-prompt-end))
                 (base (buffer-substring-no-properties end (point-max)))
                 (dir (file-name-directory base)))
            (expand-file-name cand (or dir default-directory))))
      (expand-file-name cand))))

(defun flit--marginalia-file-annotator (cand)
  "Annotate file CAND - use flit annotator for flit paths, else default."
  (let ((full-path (flit--marginalia-full-candidate cand)))
    (if (and full-path (flit--file-name-p full-path))
        (flit--marginalia-annotate-file cand)
      (marginalia-annotate-file cand))))

(with-eval-after-load 'marginalia
  ;; Prevent marginalia from looking up uid/gid locally for remote files
  (add-to-list 'marginalia-remote-file-regexps
               (concat "\\`" (regexp-quote flit--prefix)))
  (setf (alist-get 'file marginalia-annotators)
        (list #'flit--marginalia-file-annotator 'builtin 'none))
  (setf (alist-get 'project-file marginalia-annotators)
        (list #'flit--marginalia-file-annotator 'builtin 'none))
  (setf (alist-get 'buffer marginalia-annotators)
        '(flit--marginalia-buffer-annotator builtin none)))

;;; Buffer completion display

(defun flit--buffer-affixation (completions)
  "Add host suffix to flit buffer completions."
  (mapcar (lambda (buf)
            (let* ((name (if (consp buf) (car buf) buf))
                   (buffer (get-buffer name)))
              (if (and buffer
                       (buffer-file-name buffer)
                       (flit--file-name-p (buffer-file-name buffer)))
                  (let ((host (flit--host (buffer-file-name buffer))))
                    (list name "" (propertize (format " %c%s" flit--sep host)
                                              'face 'flit-host-annotation-face)))
                (list name "" ""))))
          completions))

(defun flit--setup-buffer-affixation ()
  "Set up buffer completion affixation to show host for flit buffers."
  (let ((existing (alist-get 'buffer completion-category-overrides)))
    (setf (alist-get 'buffer completion-category-overrides)
          (cons '(affixation-function . flit--buffer-affixation)
                (assq-delete-all 'affixation-function existing)))))

(with-eval-after-load 'minibuffer
  (flit--setup-buffer-affixation))

(defun flit--marginalia-annotate-buffer (cand)
  "Add host prefix annotation for flit buffer CAND."
  (when-let* ((buf (get-buffer cand))
              (file (buffer-file-name buf))
              ((flit--file-name-p file)))
    (propertize (format "@%s" (flit--host file))
                'face 'flit-host-annotation-face)))

(defun flit--marginalia-buffer-annotator (cand)
  "Annotate buffer CAND, adding host indicator for flit buffers."
  (let ((flit-part (flit--marginalia-annotate-buffer cand))
        (orig-part (marginalia-annotate-buffer cand)))
    (if flit-part
        (concat flit-part orig-part)
      orig-part)))

;;; vterm integration
;;
;; vterm checks file-remote-p and tries to use TRAMP's parsing macros for
;; remote paths. Since flit paths aren't TRAMP paths, we need to advise
;; vterm--get-shell to handle flit paths before TRAMP code runs.

(defun flit--vterm-get-shell-advice (orig-fun)
  "Advice for `vterm--get-shell' to handle flit paths.
For flit paths, return a shell that will work on the remote host.
ORIG-FUN is the original `vterm--get-shell' function."
  (if (and default-directory (flit--file-name-p default-directory))
      ;; For flit paths, use a sensible shell
      ;; We can't query the remote for shells, so use /bin/sh as fallback
      ;; Users can customize vterm-shell if needed
      (or (and (boundp 'vterm-shell) vterm-shell)
          "/bin/sh")
    ;; For non-flit paths (including TRAMP), use original function
    (funcall orig-fun)))

(with-eval-after-load 'vterm
  (advice-add 'vterm--get-shell :around #'flit--vterm-get-shell-advice)
  (advice-add 'vterm-send-stop :around #'flit--vterm-send-stop-advice)
  (advice-add 'vterm-send-start :around #'flit--vterm-send-start-advice))

(defun flit--vterm-send-stop-advice (orig-fun)
  "Suspend output on flit PTY processes.
Sends exec/ptyctl TCOOFF to the server so the remote PTY suspends output.
Without this, vterm-copy-mode scrolls to the bottom on new output
because the local tcflow call has no effect on remote PTYs."
  (funcall orig-fun)
  (when-let* ((proc (and (boundp 'vterm--process) vterm--process))
              (proc-id (flit--process-valid-for-rpc-p proc "ptyctl"))
              (host (process-get proc 'flit-host)))
    (condition-case err
        (flit--send-request-async host "exec/ptyctl"
                                  `(:procId ,proc-id :op "TCOOFF"))
      (error (flit--log-debug "TCOOFF failed: %s" (error-message-string err))))))

(defun flit--vterm-send-start-advice (orig-fun)
  "Resume output on flit PTY processes.
Sends exec/ptyctl TCOON to the server so the remote PTY resumes output."
  (funcall orig-fun)
  (when-let* ((proc (and (boundp 'vterm--process) vterm--process))
              (proc-id (flit--process-valid-for-rpc-p proc "ptyctl"))
              (host (process-get proc 'flit-host)))
    (condition-case err
        (flit--send-request-async host "exec/ptyctl"
                                  `(:procId ,proc-id :op "TCOON"))
      (error (flit--log-debug "TCOON failed: %s" (error-message-string err))))))

;;; lsp-mode compatibility
;;
;; lsp-mode checks `file-remote-p' and then calls TRAMP functions like
;; `tramp-dissect-file-name' to parse remote paths. Since flit paths aren't
;; TRAMP paths, these calls fail. We advise the specific problematic functions
;; to handle flit paths using `file-remote-p' which flit implements.

(defun flit--lsp-files-same-host-advice (orig-fun f1 f2)
  "Handle flit paths in `lsp--files-same-host'.
ORIG-FUN is the original function. F1 and F2 are file paths to compare."
  (let ((r1 (file-remote-p f1))
        (r2 (file-remote-p f2)))
    (cond
     ;; Both local
     ((not (or r1 r2)) t)
     ;; One local, one remote
     ((not (and r1 r2)) nil)
     ;; At least one flit path - compare hosts using file-remote-p
     ((or (flit--file-name-p f1) (flit--file-name-p f2))
      (equal (file-remote-p f1 'host)
             (file-remote-p f2 'host)))
     ;; Both non-flit remote paths - use original TRAMP-based comparison
     (t (funcall orig-fun f1 f2)))))

(with-eval-after-load 'lsp-mode
  (advice-add 'lsp--files-same-host :around #'flit--lsp-files-same-host-advice))

;;; make-process file handler operation
;;
;; Emacs's make-process (in C) checks for a 'make-process file handler,
;; NOT 'start-file-process. We need to handle make-process directly.

(defun flit--handle-make-process (args)
  "Handle make-process for flit remote directories.
ARGS is a plist of make-process arguments.
Delegates to our start-file-process handler."
  (flit--log-info "handle-make-process: dir=%s args=%S\n  callers: %s"
                  default-directory
                  (cl-loop for (k v) on args by #'cddr
                           collect k
                           collect (cond
                                    ((functionp v) '<function>)
                                    ((bufferp v) (format "<buffer %s>" (buffer-name v)))
                                    ((processp v) (format "<process %s>" (process-name v)))
                                    (t v)))
                  (flit--simple-backtrace))
  (let* ((name (plist-get args :name))
         (buffer (plist-get args :buffer))
         (command (plist-get args :command))
         (program (car command))
         (program-args (cdr command))
         (connection-type (or (plist-get args :connection-type)
                              ;; Fall back to process-connection-type variable.
                              ;; When t, it means use a PTY (e.g., eat sets this).
                              (and process-connection-type 'pty)))
         (coding (plist-get args :coding))
         (filter (plist-get args :filter))
         (sentinel (plist-get args :sentinel))
         (stderr (plist-get args :stderr))
         ;; Compute env delta like TRAMP - only send vars that differ from default
         (env (flit--compute-env-delta))
         (buf (and buffer (get-buffer-create buffer))))
    (flit--log-trace "handle-make-process: program=%s args=%s pty=%s"
                     program program-args connection-type)
    ;; Use our start-file-process handler logic
    (if (flit--file-name-p default-directory)
        (flit--with-parsed (host cwd) default-directory
          (flit--log-trace "handle-make-process: calling exec-start host=%s cwd=%s" host cwd)
          (let* ((need-pty (eq connection-type 'pty))
                 (rows (when need-pty
                         (or (and buf
                                  (window-live-p (get-buffer-window buf))
                                  (window-height (get-buffer-window buf)))
                             24)))
                 (cols (when need-pty
                         (or (and buf
                                  (window-live-p (get-buffer-window buf))
                                  (window-width (get-buffer-window buf)))
                             80)))
                 ;; Setup function runs before exec/start RPC to avoid race
                 ;; where exec/exit arrives before filter/sentinel are set
                 (setup-fn
                  (lambda (proc)
                    (when buf
                      (set-process-buffer proc buf))
                    (when coding
                      (set-process-coding-system proc coding coding))
                    (when filter
                      (set-process-filter proc filter))
                    (when sentinel
                      (set-process-sentinel proc sentinel))
                    (when stderr
                      (let* ((stderr-buf (if (bufferp stderr) stderr (get-buffer-create stderr)))
                             (stderr-proc (make-pipe-process
                                           :name (format "%s-stderr" name)
                                           :buffer stderr-buf
                                           :noquery t
                                           :filter #'flit--default-process-filter)))
                        (flit--log-trace "handle-make-process: created stderr proc=%s" stderr-proc)
                        (process-put proc 'flit-stderr-proc stderr-proc)))))
                 (proc (flit--exec-start host name program program-args
                                         cwd env need-pty rows cols
                                         setup-fn)))
            ;; Mark as flit PTY process so process-tty-name advice can return a name
            (when need-pty
              (process-put proc 'flit-pty t))
            proc))
      ;; Not a flit path - shouldn't happen, but fall back
      (flit--log-trace "handle-make-process: falling back to make-process (non-flit path)")
      (let ((inhibit-file-name-handlers
             (cons 'flit--file-name-handler
                   (and (eq inhibit-file-name-operation 'make-process)
                        inhibit-file-name-handlers)))
            (inhibit-file-name-operation 'make-process))
        (apply #'make-process args)))))

;; Advise process-tty-name to return a fake PTY name for flit PTY processes
;; vterm calls (process-tty-name proc) and expects a non-nil result for PTY processes
(defun flit--process-tty-name-advice (orig-fun process &optional stream)
  "Return a fake PTY name for flit PTY processes."
  (if (process-get process 'flit-pty)
      "/dev/pts/flit"
    (funcall orig-fun process stream)))

(advice-add 'process-tty-name :around #'flit--process-tty-name-advice)

;; Advise set-process-window-size to resize flit PTY processes
;; When Emacs detects a window size change, it calls this to resize the PTY
(defun flit--set-process-window-size-advice (orig-fun process height width)
  "Resize flit PTY processes via the server."
  (if (process-get process 'flit-pty)
      ;; Resize async and ignore errors, otherwise things can block and RPC errors can
      ;; propagate unfortunately.
      (when-let* ((proc-id (flit--process-valid-for-rpc-p process "resize"))
                  (host (process-get process 'flit-host)))
        (flit--send-request-async host "exec/ptyctl"
                                  `(:procId ,proc-id :op "resize" :rows ,height :cols ,width)
                                  nil
                                  (lambda (err)
                                    (flit--log-debug "Resize failed: %S" err)))
        t)  ; Return non-nil to indicate success
    (funcall orig-fun process height width)))

(advice-add 'set-process-window-size :around #'flit--set-process-window-size-advice)

(flit--register)

;;; Flit prefix styling in minibuffer

(defface flit-prefix-face
  '((t :inherit font-lock-comment-face))
  "Face for the flit prefix+host in the minibuffer."
  :group 'flit)

(defface flit-host-annotation-face
  '((t :inherit shadow))
  "Face for the host annotation in buffer lists and completions."
  :group 'flit)

(defvar-local flit--prefix-overlay nil
  "Overlay for styling the flit prefix in the minibuffer.")

(defun flit--prefix-setup-minibuffer ()
  "Set up minibuffer overlay for flit prefix styling."
  (when minibuffer-completing-file-name
    (setq flit--prefix-overlay
          (make-overlay (minibuffer-prompt-end) (minibuffer-prompt-end)))
    (overlay-put flit--prefix-overlay 'face 'flit-prefix-face)
    (overlay-put flit--prefix-overlay 'evaporate t)
    (add-hook 'post-command-hook #'flit--prefix-update-overlay nil t)))

(add-hook 'minibuffer-setup-hook #'flit--prefix-setup-minibuffer)

(defun flit--prefix-update-overlay ()
  "Update `flit--prefix-overlay' to cover the flit prefix+host."
  (when (and flit--prefix-overlay (overlayp flit--prefix-overlay))
    (let* ((input (minibuffer-contents-no-properties))
           (path-start (and (flit--file-name-p input)
                            (flit--find-path-start input))))
      (if path-start
          (move-overlay flit--prefix-overlay
                        (minibuffer-prompt-end)
                        (+ (minibuffer-prompt-end) path-start))
        (move-overlay flit--prefix-overlay
                      (minibuffer-prompt-end)
                      (minibuffer-prompt-end))))))

;;; Minibuffer file-name shadowing for flit paths
;; Like tramp-rfn-eshadow — // and /~ keep the flit prefix instead of
;; resetting to local.

(defvar-local flit--rfn-eshadow-overlay nil
  "Overlay for flit file-name shadowing in the minibuffer.")

(defun flit--rfn-eshadow-setup-minibuffer ()
  "Set up flit file-name shadowing overlay for the minibuffer."
  (when minibuffer-completing-file-name
    (setq flit--rfn-eshadow-overlay
          (make-overlay (minibuffer-prompt-end) (minibuffer-prompt-end)))
    ;; Copy rfn-eshadow-overlay properties (except 'field — breaks completion)
    (if (overlayp rfn-eshadow-overlay)
        (let ((props (overlay-properties rfn-eshadow-overlay)))
          (while props
            (if (not (eq (car props) 'field))
                (overlay-put flit--rfn-eshadow-overlay (pop props) (pop props))
              (pop props) (pop props))))
      ;; Fallback if rfn-eshadow-overlay not available
      (overlay-put flit--rfn-eshadow-overlay 'face 'file-name-shadow)
      (overlay-put flit--rfn-eshadow-overlay 'evaporate t))))

(add-hook 'rfn-eshadow-setup-minibuffer-hook #'flit--rfn-eshadow-setup-minibuffer)

(defun flit--rfn-eshadow-update-overlay ()
  "Update `flit--rfn-eshadow-overlay' — only shadow the local path part."
  (ignore-errors
    (let ((input (buffer-substring (minibuffer-prompt-end) (point-max))))
      (when (and (flit--file-name-p input)
                 (flit--find-path-start input))
        (let ((path-start (+ (minibuffer-prompt-end) (flit--find-path-start input))))
          (save-excursion
            (save-restriction
              (narrow-to-region path-start (point-max))
              (let ((rfn-eshadow-overlay flit--rfn-eshadow-overlay)
                    rfn-eshadow-update-overlay-hook
                    file-name-handler-alist)
                (move-overlay flit--rfn-eshadow-overlay (point-max) (point-max))
                (rfn-eshadow-update-overlay)))))))))

(add-hook 'rfn-eshadow-update-overlay-hook #'flit--rfn-eshadow-update-overlay)

;;; TCP Tunneling
;;
;; Supports both directions:
;; - Reverse: server listens, forwards connections to local port
;; - Forward: local listener, forwards connections to remote port
;;
;; Both directions share the same data/disconnect handling once a
;; connection is established.

(defvar flit--tunnels (make-hash-table :test 'equal)
  "Hash table mapping tunnel-id to tunnel state.
Each tunnel state is a plist with:
  :direction - `forward' or `reverse'
  :host - the flit host
  :local-port - local port
  :remote-port - remote port
  :listener - local server process (forward only)
  :connections - hash table of conn-id -> local network process")

(defvar flit--tunnel-conn-id-counter 0
  "Counter for generating tunnel connection IDs.")

(defun flit-reverse-tunnel-listen (host local-port &optional tunnel-id)
  "Start a tunnel on HOST that forwards to LOCAL-PORT on this machine.
TUNNEL-ID is an optional unique identifier for the tunnel.
Returns a plist with :tunnel-id and :remote-port."
  (let* ((tunnel-id (or tunnel-id (format "tunnel-%s-%d" host local-port)))
         (result (flit--send-request host "tunnel/listen"
                                     `(:tunnelId ,tunnel-id :port 0))))
    (let ((remote-port (plist-get result :port)))
      (puthash tunnel-id
               (list :direction 'reverse
                     :host host
                     :local-port local-port
                     :remote-port remote-port
                     :connections (make-hash-table :test 'equal))
               flit--tunnels)
      (flit--log-info "Reverse tunnel %s started: remote:%d -> local:%d"
                      tunnel-id remote-port local-port)
      (list :tunnel-id tunnel-id :remote-port remote-port))))

(defun flit-tunnel-listen (host local-port remote-port &optional tunnel-id)
  "Listen on LOCAL-PORT, forwarding to REMOTE-PORT on HOST.
TUNNEL-ID is an optional unique identifier for the tunnel.
Returns a plist with :tunnel-id and :local-port."
  (let* ((tunnel-id (or tunnel-id (format "tunnel-%s-%d-%d" host local-port remote-port)))
         (listener (make-network-process
                    :name (format "flit-tunnel-%s" tunnel-id)
                    :server t
                    :host "127.0.0.1"
                    :service local-port
                    :family 'ipv4
                    :coding 'binary
                    :filter #'ignore  ; replaced per-connection
                    :sentinel #'ignore
                    :filter-multibyte nil
                    :log (lambda (_server client _message)
                           (flit--tunnel-accept-connection tunnel-id client)))))
    (let ((actual-port (cadr (process-contact listener))))
      (puthash tunnel-id
               (list :direction 'forward
                     :host host
                     :local-port actual-port
                     :remote-port remote-port
                     :listener listener
                     :connections (make-hash-table :test 'equal))
               flit--tunnels)
      (flit--log-info "Forward tunnel %s started: local:%d -> remote:%d"
                      tunnel-id actual-port remote-port)
      (list :tunnel-id tunnel-id :local-port actual-port))))

(defun flit-tunnel-close (tunnel-id)
  "Close the tunnel with TUNNEL-ID (forward or reverse)."
  (when-let ((tunnel (gethash tunnel-id flit--tunnels)))
    (let ((host (plist-get tunnel :host))
          (conns (plist-get tunnel :connections)))
      ;; Close all local connections
      (maphash (lambda (conn-id proc)
                 (when (process-live-p proc)
                   (delete-process proc))
                 (when (flit--connection-alive-p host)
                   (condition-case nil
                       (flit--send-request-async host "tunnel/disconnect"
                                                 `(:connId ,conn-id))
                     (error nil))))
               conns)
      ;; Close local listener (forward tunnels only)
      (when-let ((listener (plist-get tunnel :listener)))
        (when (process-live-p listener)
          (delete-process listener)))
      ;; Close server listener (reverse tunnels only)
      (when (eq (plist-get tunnel :direction) 'reverse)
        (condition-case nil
            (flit--send-request host "tunnel/close" `(:tunnelId ,tunnel-id))
          (error nil)))
      (remhash tunnel-id flit--tunnels)
      (flit--log-info "Tunnel %s closed" tunnel-id))))

(defun flit-reverse-tunnel-close (tunnel-id)
  "Close the reverse tunnel with TUNNEL-ID."
  (flit-tunnel-close tunnel-id))

(defun flit--tunnel-accept-connection (tunnel-id client)
  "Handle a new connection CLIENT on forward tunnel TUNNEL-ID."
  (let ((tunnel (gethash tunnel-id flit--tunnels)))
    (when tunnel
      (let* ((host (plist-get tunnel :host))
             (remote-port (plist-get tunnel :remote-port))
             (conns (plist-get tunnel :connections))
             (conn-id (format "%s-%d" tunnel-id
                              (cl-incf flit--tunnel-conn-id-counter))))
        ;; Set up the client process
        (set-process-coding-system client 'binary 'binary)
        (set-process-filter client #'ignore) ; temporarily ignore until server connects
        (set-process-sentinel client
                              (lambda (_proc _event)
                                (flit--tunnel-local-disconnected tunnel-id conn-id)))
        ;; Store the connection
        (puthash conn-id client conns)
        ;; Tell server to connect to the remote port
        (condition-case err
            (progn
              (flit--send-request host "tunnel/connect"
                                  `(:connId ,conn-id :tunnelId ,tunnel-id :port ,remote-port))
              ;; Server connected - now set up data forwarding
              (set-process-filter client
                                  (lambda (_proc data)
                                    (flit--tunnel-send-data host conn-id data)))
              (flit--log-debug "Forward tunnel %s: new connection %s" tunnel-id conn-id))
          (error
           (flit--log-error "Forward tunnel %s: failed to connect %s: %s"
                            tunnel-id conn-id (error-message-string err))
           (remhash conn-id conns)
           (when (process-live-p client)
             (delete-process client))))))))

(defun flit--handle-tunnel-accept (_conn params)
  "Handle a tunnel/accept notification with PARAMS.
A new client has connected to a remote tunnel listener."
  (let* ((tunnel-id (plist-get params :tunnelId))
         (conn-id (plist-get params :connId))
         (tunnel (gethash tunnel-id flit--tunnels)))
    (if (not tunnel)
        (flit--log-error "Tunnel accept for unknown tunnel: %s" tunnel-id)
      (let* ((local-port (plist-get tunnel :local-port))
             (host (plist-get tunnel :host))
             (conns (plist-get tunnel :connections))
             (proc (condition-case err
                       (open-network-stream
                        (format "flit-tunnel-%s" conn-id)
                        nil
                        "127.0.0.1"
                        local-port
                        :coding 'binary)
                     (error
                      (flit--log-error "Failed to connect to local port %d: %s"
                                       local-port (error-message-string err))
                      nil))))
        (if (not proc)
            (flit--send-request-async host "tunnel/disconnect"
                                      `(:connId ,conn-id))
          (puthash conn-id proc conns)
          (set-process-filter proc
                              (lambda (_proc data)
                                (flit--tunnel-send-data host conn-id data)))
          (set-process-sentinel proc
                                (lambda (_proc _event)
                                  (flit--tunnel-local-disconnected tunnel-id conn-id)))
          (flit--log-debug "Tunnel %s: new connection %s" tunnel-id conn-id))))))

(defun flit--tunnel-send-data (host conn-id data)
  "Send DATA from local connection CONN-ID to the remote via HOST."
  (flit--send-notify host "tunnel/data"
                     `(:connId ,conn-id :data (:bin . ,data))))

(defun flit--tunnel-local-disconnected (tunnel-id conn-id)
  "Handle local connection CONN-ID in tunnel TUNNEL-ID being closed."
  (when-let ((tunnel (gethash tunnel-id flit--tunnels)))
    (let ((host (plist-get tunnel :host))
          (conns (plist-get tunnel :connections)))
      (remhash conn-id conns)
      (when (flit--connection-alive-p host)
        (condition-case nil
            (flit--send-request-async host "tunnel/disconnect"
                                      `(:connId ,conn-id))
          (error nil)))
      (flit--log-debug "Tunnel %s: local connection %s closed" tunnel-id conn-id))))

(defun flit--handle-tunnel-data (_conn params)
  "Handle a tunnel/data notification with PARAMS."
  (let* ((conn-id (plist-get params :connId))
         (data (or (plist-get params :payload) (plist-get params :data)))
         (tunnel-id (plist-get params :tunnelId))
         (tunnel (gethash tunnel-id flit--tunnels)))
    (when tunnel
      (let* ((conns (plist-get tunnel :connections))
             (proc (gethash conn-id conns)))
        (when (and proc (process-live-p proc))
          (process-send-string proc data))))))

(defun flit--handle-tunnel-disconnect (_conn params)
  "Handle a tunnel/disconnect notification with PARAMS."
  (let* ((conn-id (plist-get params :connId))
         (tunnel-id (plist-get params :tunnelId))
         (tunnel (gethash tunnel-id flit--tunnels)))
    (when tunnel
      (let* ((conns (plist-get tunnel :connections))
             (proc (gethash conn-id conns)))
        (when proc
          (remhash conn-id conns)
          (when (process-live-p proc)
            (delete-process proc))
          (flit--log-debug "Tunnel %s: remote connection %s closed"
                           tunnel-id conn-id))))))

(defun flit--cleanup-host-tunnels (host)
  "Clean up all tunnels associated with HOST."
  (let ((tunnels-to-remove nil))
    (maphash (lambda (tunnel-id tunnel)
               (when (equal (plist-get tunnel :host) host)
                 (push tunnel-id tunnels-to-remove)
                 ;; Close listener (forward tunnels only)
                 (when-let ((listener (plist-get tunnel :listener)))
                   (when (process-live-p listener)
                     (delete-process listener)))
                 ;; Close all local connections
                 (maphash (lambda (_conn-id proc)
                            (when (process-live-p proc)
                              (delete-process proc)))
                          (plist-get tunnel :connections))))
             flit--tunnels)
    (dolist (tunnel-id tunnels-to-remove)
      (remhash tunnel-id flit--tunnels)
      (flit--log-info "Cleaned up tunnel %s" tunnel-id))))

(defun flit--emacs-kill-hook ()
  "Send shutdown to all connected flit servers on Emacs exit."
  (maphash (lambda (host _conn)
             (flit--send-shutdown host))
           flit--connections))

(add-hook 'kill-emacs-hook #'flit--emacs-kill-hook)

(provide 'flit)
;;; flit.el ends here
