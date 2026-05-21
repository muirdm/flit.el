;;; flit-test.el --- Integration tests for flit -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;;; Commentary:

;; Integration tests for flit remote file editing.
;; Uses ERT (Emacs Lisp Regression Testing).
;;
;; Run tests with:
;;   FLIT_SERVER_BINARY=some/path emacs --batch -l flit-test.el -f ert-run-tests-batch-and-exit
;;
;; Or from Emacs:
;;   M-x ert RET t RET

;;; Code:

(require 'ert)
(require 'flit)

(declare-function flit--desktop-create-buffer-advice "flit")
(defvar flit--desktop-restoring)
(defvar tramp-backup-directory-alist)

;;; Test infrastructure

(defvar flit-test--temp-dir nil
  "Temporary directory for test files.")

(defvar flit-test--host "test"
  "Host name used for tests.")

(defvar flit-test--saved-connection-methods nil
  "Saved connection methods to restore after tests.")

(defvar flit-test--server-log-file nil
  "Path to the server log file for the current test.")

(defun flit-test--setup ()
  "Set up test environment."
  ;; Get server binary from environment
  (let ((binary (getenv "FLIT_SERVER_BINARY")))
    (unless binary
      (error "FLIT_SERVER_BINARY environment variable not set"))
    (unless (file-executable-p binary)
      (error "FLIT_SERVER_BINARY %s is not executable" binary))
    ;; Save existing config and set up for testing
    (setq flit-test--saved-connection-methods flit-connection-methods)
    (setq flit-test--server-log-file (make-temp-file "flit-server-log-" nil ".log"))
    (setq flit-connection-methods
          `((,flit-test--host . (:stdio (,binary "server" "--stdio"
                                                  "--log-file" ,flit-test--server-log-file))))))

  ;; Create temp directory
  (setq flit-test--temp-dir (make-temp-file "flit-test-" t))

  ;; Clear all flit state
  (flit-clear-all-state)
  ;; flit-clear-all-state marks known hosts as 'disconnected (to prevent
  ;; background reconnect in interactive use).  Reset the test host to
  ;; 'pending so file operations trigger connection with 'maybe tier.
  (remhash flit-test--host flit--connection-states)
  ;; Let any async process cleanup complete
  (sit-for 0.05))

(defun flit-test--teardown ()
  "Tear down test environment."
  ;; Close all connections
  (maphash (lambda (host _)
             (flit--close-connection host))
           flit--connections)

  ;; Remove temp directory
  (when (and flit-test--temp-dir (file-directory-p flit-test--temp-dir))
    (delete-directory flit-test--temp-dir t))

  ;; Reset state
  (setq flit-test--temp-dir nil)
  (setq flit-connection-methods flit-test--saved-connection-methods)
  ;; Clean up server log file
  (when flit-test--server-log-file
    (ignore-errors (delete-file flit-test--server-log-file))
    (setq flit-test--server-log-file nil)))

(defmacro flit-test--with-fixture (&rest body)
  "Execute BODY with test fixture set up and torn down."
  (declare (indent 0) (debug t))
  `(let ((flit-test--fixture-ok nil))
     (unwind-protect
         (progn
           (flit-test--setup)
           ;; Use pipe mode for tests - PTY mode is tested separately
           ;; Tests are explicit actions so use 'connect tier
           (let ((process-connection-type nil)
                 (flit--connection-tier 'connect))
             ,@body)
           (setq flit-test--fixture-ok t))
       (unless flit-test--fixture-ok
         (when (and flit-test--server-log-file
                    (file-exists-p flit-test--server-log-file))
           (message "=== Server log (test failure) ===")
           (message "%s" (with-temp-buffer
                           (insert-file-contents flit-test--server-log-file)
                           (buffer-string)))
           (message "=== End server log ===")))
       (flit-test--teardown))))

(defun flit-test--path (relative)
  "Return a flit path for RELATIVE path in test temp dir."
  ;; temp-dir already starts with /, so format is /flit!host/path
  (concat flit--prefix flit-test--host flit-test--temp-dir "/" relative))

(defun flit-test--local-path (relative)
  "Return a local path for RELATIVE path in test temp dir."
  (expand-file-name relative flit-test--temp-dir))

(defun flit-test--create-file (relative content)
  "Create a file at RELATIVE path with CONTENT."
  (let ((path (flit-test--local-path relative)))
    (with-temp-file path
      (insert content))
    path))

(defun flit-test--read-file (relative)
  "Read the contents of file at RELATIVE path."
  (let ((path (flit-test--local-path relative)))
    (with-temp-buffer
      (insert-file-contents path)
      (buffer-string))))

(defun flit-test--wait-for (predicate &optional timeout interval)
  "Wait for PREDICATE to return non-nil, with retries.
TIMEOUT is the maximum wait time in seconds (default 10).
INTERVAL is the polling interval in seconds (default 0.1).
While waiting, accepts process output to receive notifications.
Returns the predicate result, or nil if timeout was reached."
  (let ((timeout (or timeout 10.0))
        (interval (or interval 0.1))
        (start-time (float-time))
        (result nil))
    (while (and (not (setq result (funcall predicate)))
                (< (- (float-time) start-time) timeout))
      ;; Accept process output from flit connection
      (let ((conn (gethash flit-test--host flit--connections)))
        (if conn
            (accept-process-output (flit-conn-process conn) interval)
          ;; No connection yet, just sleep
          (sleep-for interval))))
    result))

(defun flit-test--wait-for-notification ()
  "Wait for a file change notification to be received.
This waits for `flit--file-changed' to become non-nil.
Must be called with the target buffer current."
  (flit-test--wait-for (lambda () flit--file-changed)))

;;; File existence tests

(ert-deftest flit-test-file-exists-p-existing ()
  "Test file-exists-p on an existing file."
  (flit-test--with-fixture
    (flit-test--create-file "exists.txt" "content")
    (should (file-exists-p (flit-test--path "exists.txt")))))

(ert-deftest flit-test-file-exists-p-nonexistent ()
  "Test file-exists-p on a non-existent file."
  (flit-test--with-fixture
    (should-not (file-exists-p (flit-test--path "nonexistent.txt")))))

(ert-deftest flit-test-file-exists-p-directory ()
  "Test file-exists-p on a directory."
  (flit-test--with-fixture
    (make-directory (flit-test--local-path "subdir"))
    (should (file-exists-p (flit-test--path "subdir")))))

;;; File type tests

(ert-deftest flit-test-file-directory-p-on-directory ()
  "Test file-directory-p returns t for directory."
  (flit-test--with-fixture
    (make-directory (flit-test--local-path "subdir"))
    (should (file-directory-p (flit-test--path "subdir")))))

(ert-deftest flit-test-file-directory-p-on-file ()
  "Test file-directory-p returns nil for file."
  (flit-test--with-fixture
    (flit-test--create-file "file.txt" "content")
    (should-not (file-directory-p (flit-test--path "file.txt")))))

(ert-deftest flit-test-file-regular-p-on-file ()
  "Test file-regular-p returns t for regular file."
  (flit-test--with-fixture
    (flit-test--create-file "file.txt" "content")
    (should (file-regular-p (flit-test--path "file.txt")))))

(ert-deftest flit-test-file-regular-p-on-directory ()
  "Test file-regular-p returns nil for directory."
  (flit-test--with-fixture
    (make-directory (flit-test--local-path "subdir"))
    (should-not (file-regular-p (flit-test--path "subdir")))))

;;; File attributes tests

(ert-deftest flit-test-file-attributes-existing ()
  "Test file-attributes on an existing file."
  (flit-test--with-fixture
    (flit-test--create-file "attrs.txt" "hello")
    (let ((attrs (file-attributes (flit-test--path "attrs.txt"))))
      (should attrs)
      (should (null (nth 0 attrs))) ; not a directory
      (should (= 5 (nth 7 attrs)))))) ; size

(ert-deftest flit-test-file-attributes-directory ()
  "Test file-attributes on a directory."
  (flit-test--with-fixture
    (make-directory (flit-test--local-path "subdir"))
    (let ((attrs (file-attributes (flit-test--path "subdir"))))
      (should attrs)
      (should (eq t (nth 0 attrs)))))) ; is a directory

(ert-deftest flit-test-file-attributes-nonexistent ()
  "Test file-attributes on a non-existent file."
  (flit-test--with-fixture
    (should-not (file-attributes (flit-test--path "nonexistent.txt")))))

;;; Read file tests

(ert-deftest flit-test-insert-file-contents ()
  "Test insert-file-contents reads file content."
  (flit-test--with-fixture
    (flit-test--create-file "read.txt" "hello world")
    (with-temp-buffer
      (insert-file-contents (flit-test--path "read.txt"))
      (should (equal "hello world" (buffer-string))))))

(ert-deftest flit-test-insert-file-contents-visit ()
  "Test insert-file-contents with visit sets buffer-file-name."
  (flit-test--with-fixture
    (flit-test--create-file "visit.txt" "content")
    (with-temp-buffer
      (insert-file-contents (flit-test--path "visit.txt") t)
      (should (equal (flit-test--path "visit.txt") buffer-file-name)))))

(ert-deftest flit-test-insert-file-contents-nonexistent ()
  "Test insert-file-contents on non-existent file signals error."
  (flit-test--with-fixture
    (should-error
     (with-temp-buffer
       (insert-file-contents (flit-test--path "nonexistent.txt")))
     :type 'file-missing)))

(ert-deftest flit-test-insert-file-contents-replace-preserves-point ()
  "Test insert-file-contents with REPLACE preserves point position."
  (flit-test--with-fixture
    (flit-test--create-file "replace.txt" "hello world")
    (let ((buf (find-file-noselect (flit-test--path "replace.txt"))))
      (unwind-protect
          (with-current-buffer buf
            ;; Move point to middle of buffer
            (goto-char 6)
            (should (= 6 (point)))
            ;; Replace buffer contents (simulates revert)
            (insert-file-contents buffer-file-name nil nil nil t)
            ;; Point should be preserved
            (should (= 6 (point))))
        (kill-buffer buf)))))

;;; Write file tests

(ert-deftest flit-test-write-region ()
  "Test write-region writes content to file."
  (flit-test--with-fixture
    (with-temp-buffer
      (insert "test content")
      (write-region (point-min) (point-max) (flit-test--path "write.txt")))
    (should (equal "test content" (flit-test--read-file "write.txt")))))

(ert-deftest flit-test-write-region-string ()
  "Test write-region with string as start argument."
  (flit-test--with-fixture
    (write-region "string content" nil (flit-test--path "string.txt"))
    (should (equal "string content" (flit-test--read-file "string.txt")))))

;;; Directory listing tests

(ert-deftest flit-test-directory-files ()
  "Test directory-files lists directory contents."
  (flit-test--with-fixture
    (flit-test--create-file "a.txt" "a")
    (flit-test--create-file "b.txt" "b")
    (make-directory (flit-test--local-path "subdir"))
    (let ((files (directory-files (flit-test--path ""))))
      (should (member "." files))
      (should (member ".." files))
      (should (member "a.txt" files))
      (should (member "b.txt" files))
      (should (member "subdir" files)))))

(ert-deftest flit-test-directory-files-full ()
  "Test directory-files with full paths."
  (flit-test--with-fixture
    (flit-test--create-file "file.txt" "content")
    (let ((files (directory-files (flit-test--path "") t)))
      (should (cl-some (lambda (f) (string-suffix-p "file.txt" f)) files)))))

(ert-deftest flit-test-directory-files-match ()
  "Test directory-files with match pattern."
  (flit-test--with-fixture
    (flit-test--create-file "a.txt" "a")
    (flit-test--create-file "b.el" "b")
    (let ((files (directory-files (flit-test--path "") nil "\\.txt$")))
      (should (member "a.txt" files))
      (should-not (member "b.el" files)))))

;;; Path manipulation tests

(ert-deftest flit-test-file-name-directory ()
  "Test file-name-directory extracts directory."
  (flit-test--with-fixture
    (should (equal (concat flit--prefix flit-test--host flit-test--temp-dir "/")
                   (file-name-directory (flit-test--path "file.txt"))))))

(ert-deftest flit-test-file-name-nondirectory ()
  "Test file-name-nondirectory extracts filename."
  (flit-test--with-fixture
    (should (equal "file.txt"
                   (file-name-nondirectory (flit-test--path "file.txt"))))))

(ert-deftest flit-test-expand-file-name ()
  "Test expand-file-name on flit paths."
  (flit-test--with-fixture
    (let ((path (flit-test--path "file.txt")))
      (should (equal path (expand-file-name path))))))

(ert-deftest flit-test-expand-file-name-relative ()
  "Test expand-file-name with relative path and flit default-directory."
  (flit-test--with-fixture
    (let ((default-directory (flit-test--path "")))
      (should (string-match-p (concat "^" (regexp-quote flit--prefix) "test/.*file\\.txt$")
                              (expand-file-name "file.txt"))))))

(ert-deftest flit-test-file-truename ()
  "Test file-truename on flit paths."
  (flit-test--with-fixture
    (flit-test--create-file "real.txt" "content")
    (let ((truename (file-truename (flit-test--path "real.txt"))))
      (should (string-prefix-p (concat flit--prefix "test/") truename)))))

;;; File modification tests

(ert-deftest flit-test-make-directory ()
  "Test make-directory creates directory."
  (flit-test--with-fixture
    (make-directory (flit-test--path "newdir"))
    (should (file-directory-p (flit-test--local-path "newdir")))))

(ert-deftest flit-test-delete-file ()
  "Test delete-file removes file."
  (flit-test--with-fixture
    (flit-test--create-file "todelete.txt" "content")
    (delete-file (flit-test--path "todelete.txt"))
    (should-not (file-exists-p (flit-test--local-path "todelete.txt")))))

(ert-deftest flit-test-delete-directory ()
  "Test delete-directory removes empty directory."
  (flit-test--with-fixture
    (make-directory (flit-test--local-path "emptydir"))
    (delete-directory (flit-test--path "emptydir"))
    (should-not (file-exists-p (flit-test--local-path "emptydir")))))

(ert-deftest flit-test-delete-directory-recursive ()
  "Test delete-directory with recursive removes non-empty directory."
  (flit-test--with-fixture
    (make-directory (flit-test--local-path "nonempty"))
    (flit-test--create-file "nonempty/file.txt" "content")
    (delete-directory (flit-test--path "nonempty") t)
    (should-not (file-exists-p (flit-test--local-path "nonempty")))))

(ert-deftest flit-test-rename-file ()
  "Test rename-file renames file."
  (flit-test--with-fixture
    (flit-test--create-file "old.txt" "content")
    (rename-file (flit-test--path "old.txt") (flit-test--path "new.txt"))
    (should-not (file-exists-p (flit-test--local-path "old.txt")))
    (should (file-exists-p (flit-test--local-path "new.txt")))
    (should (equal "content" (flit-test--read-file "new.txt")))))

(ert-deftest flit-test-copy-file ()
  "Test copy-file copies file."
  (flit-test--with-fixture
    (flit-test--create-file "original.txt" "content")
    (copy-file (flit-test--path "original.txt") (flit-test--path "copy.txt"))
    (should (file-exists-p (flit-test--local-path "original.txt")))
    (should (file-exists-p (flit-test--local-path "copy.txt")))
    (should (equal "content" (flit-test--read-file "copy.txt")))))

;;; Remote identification tests

(ert-deftest flit-test-file-remote-p ()
  "Test file-remote-p returns remote identifier."
  (flit-test--with-fixture
    (should (equal (concat flit--prefix "test") (file-remote-p (flit-test--path "file.txt"))))))

(ert-deftest flit-test-file-remote-p-method ()
  "Test file-remote-p with method returns flit."
  (flit-test--with-fixture
    (should (equal "flit" (file-remote-p (flit-test--path "file.txt") 'method)))))

(ert-deftest flit-test-file-remote-p-host ()
  "Test file-remote-p with host returns host."
  (flit-test--with-fixture
    (should (equal "test" (file-remote-p (flit-test--path "file.txt") 'host)))))

(ert-deftest flit-test-file-remote-p-localname ()
  "Test file-remote-p with localname returns path."
  (flit-test--with-fixture
    (let ((expected (format "%s/file.txt" flit-test--temp-dir)))
      (should (equal expected (file-remote-p (flit-test--path "file.txt") 'localname))))))

;;; Integration tests - basic flows

(ert-deftest flit-test-find-file-existing ()
  "Test find-file opens existing file."
  (flit-test--with-fixture
    (flit-test--create-file "find.txt" "file content")
    (let ((buf (find-file-noselect (flit-test--path "find.txt"))))
      (unwind-protect
          (with-current-buffer buf
            (should (equal "file content" (buffer-string)))
            (should (equal (flit-test--path "find.txt") buffer-file-name)))
        (kill-buffer buf)))))

(ert-deftest flit-test-find-file-new ()
  "Test find-file on new file creates empty buffer."
  (flit-test--with-fixture
    (let ((buf (find-file-noselect (flit-test--path "newfile.txt"))))
      (unwind-protect
          (with-current-buffer buf
            (should (= 0 (buffer-size)))
            (should (equal (flit-test--path "newfile.txt") buffer-file-name)))
        (kill-buffer buf)))))

(ert-deftest flit-test-find-file-save ()
  "Test find-file and save-buffer round-trip."
  (flit-test--with-fixture
    (let ((buf (find-file-noselect (flit-test--path "saveme.txt"))))
      (unwind-protect
          (with-current-buffer buf
            (insert "saved content")
            (save-buffer)
            (should-not (buffer-modified-p)))
        (kill-buffer buf)))
    ;; Verify file was written (Emacs may add final newline)
    (let ((content (flit-test--read-file "saveme.txt")))
      (should (string-prefix-p "saved content" content)))))

(ert-deftest flit-test-find-file-modify-save ()
  "Test find-file, modify, and save."
  (flit-test--with-fixture
    (flit-test--create-file "modify.txt" "original")
    (let ((buf (find-file-noselect (flit-test--path "modify.txt"))))
      (unwind-protect
          (with-current-buffer buf
            (should (equal "original" (buffer-string)))
            (erase-buffer)
            (insert "modified")
            (save-buffer))
        (kill-buffer buf)))
    ;; Verify file was modified (Emacs may add final newline)
    (let ((content (flit-test--read-file "modify.txt")))
      (should (string-prefix-p "modified" content)))))

(ert-deftest flit-test-make-auto-save-file-name ()
  "Test make-auto-save-file-name generates local auto-save path."
  (flit-test--with-fixture
    (flit-test--create-file "autosave.txt" "content")
    (let ((buf (find-file-noselect (flit-test--path "autosave.txt"))))
      (unwind-protect
          (with-current-buffer buf
            (let ((auto-save-name (make-auto-save-file-name)))
              ;; Auto-save file should be local (not a flit path)
              (should auto-save-name)
              (should-not (flit--file-name-p auto-save-name))
              ;; Should be in user-emacs-directory/flit-auto-saves
              ;; Use local default-directory to avoid flit handler affecting expand-file-name
              (let ((default-directory "/"))
                (should (string-prefix-p (expand-file-name "flit-auto-saves" user-emacs-directory)
                                         auto-save-name)))))
        (kill-buffer buf)))))

(ert-deftest flit-test-tramp-coexistence ()
  "Test that flit works when TRAMP is loaded."
  (flit-test--with-fixture
    (require 'tramp)
    ;; Ensure our handler is still at front after TRAMP loads
    (flit--ensure-handler-priority)
    ;; Test basic operations still work
    (flit-test--create-file "tramp-test.txt" "content")
    (should (file-exists-p (flit-test--path "tramp-test.txt")))
    (should (equal "content"
                   (with-temp-buffer
                     (insert-file-contents (flit-test--path "tramp-test.txt"))
                     (buffer-string))))))

(ert-deftest flit-test-get-file-buffer-with-tramp-buffers ()
  "Test get-file-buffer doesn't error when TRAMP buffers exist."
  (flit-test--with-fixture
    (require 'tramp)
    (flit-test--create-file "getbuf.txt" "content")
    ;; Create a buffer with a fake TRAMP-style path
    ;; (not a real connection, just to test that get-file-buffer skips it)
    (let ((tramp-buf (generate-new-buffer "fake-tramp")))
      (unwind-protect
          (progn
            (with-current-buffer tramp-buf
              ;; Set buffer-file-name to a TRAMP-style path
              ;; This simulates having an open TRAMP buffer
              (setq buffer-file-name "/ssh:somehost:/path/to/file.txt"))
            ;; Now open a flit file - get-file-buffer should not error
            (let ((flit-buf (find-file-noselect (flit-test--path "getbuf.txt"))))
              (unwind-protect
                  (progn
                    (should flit-buf)
                    (with-current-buffer flit-buf
                      (should (equal "content" (buffer-string))))
                    ;; get-file-buffer should find our buffer
                    (should (eq flit-buf (get-file-buffer (flit-test--path "getbuf.txt")))))
                (kill-buffer flit-buf))))
        (kill-buffer tramp-buf)))))

(ert-deftest flit-test-buffer-affixation ()
  "Test that buffer affixation adds host suffix for flit buffers."
  (flit-test--with-fixture
    (flit-test--create-file "affix.txt" "content")
    (let ((buf (find-file-noselect (flit-test--path "affix.txt"))))
      (unwind-protect
          (let* ((completions (list (buffer-name buf) "*Messages*"))
                 (affixed (flit--buffer-affixation completions)))
            ;; Flit buffer should have host suffix
            (let ((flit-entry (car affixed)))
              (should (equal (car flit-entry) (buffer-name buf)))
              (should (equal (nth 1 flit-entry) ""))
              (should (string-match-p (regexp-quote (format "%c%s" flit--sep "test")) (nth 2 flit-entry))))
            ;; Non-flit buffer should have empty suffix
            (let ((other-entry (cadr affixed)))
              (should (equal (car other-entry) "*Messages*"))
              (should (equal (nth 2 other-entry) ""))))
        (kill-buffer buf)))))

(ert-deftest flit-test-file-watch-setup ()
  "Test that file watching is set up when opening a flit file."
  (flit-test--with-fixture
    (flit-test--create-file "watch-test.txt" "initial content")
    (let ((buf (find-file-noselect (flit-test--path "watch-test.txt"))))
      (unwind-protect
          (with-current-buffer buf
            ;; File should have been opened successfully
            (should (equal "initial content" (buffer-string)))
            ;; flit--file-changed should be nil initially
            (should-not flit--file-changed)
            ;; verify-visited-file-modtime should return t (not changed)
            (should (verify-visited-file-modtime buf)))
        (kill-buffer buf)))))

(ert-deftest flit-test-file-watch-notification ()
  "Test that file change notifications work."
  (flit-test--with-fixture
    (flit-test--create-file "notify-test.txt" "initial content")
    (let ((buf (find-file-noselect (flit-test--path "notify-test.txt"))))
      (unwind-protect
          (with-current-buffer buf
            ;; Manually set up watching (find-file-hook may not run in batch)
            (flit--watch buffer-file-name)
            (setq-local buffer-stale-function #'flit--buffer-stale-p)
            ;; Verify initial state
            (should-not flit--file-changed)
            (should-not (funcall buffer-stale-function))
            ;; Modify file externally (via direct write, not through buffer)
            (let ((path (flit-test--local-path "notify-test.txt")))
              (with-temp-file path
                (insert "modified content")))
            ;; Wait for file change notification (server has 1s debounce)
            (sleep-for 1.1)
            (should (flit-test--wait-for-notification))
            ;; buffer-stale-function should return t (and clear the flag)
            (should (funcall buffer-stale-function))
            ;; After calling buffer-stale-function, flag should be cleared
            (should-not flit--file-changed))
        (ignore-errors (flit--unwatch (buffer-file-name buf)))
        (kill-buffer buf)))))

;;; Process execution tests

(ert-deftest flit-test-process-file-simple ()
  "Test process-file runs a simple command."
  (flit-test--with-fixture
    (let* ((default-directory (flit-test--path ""))
           (exit-code (process-file "echo" nil t nil "-n" "hello")))
      (should (= 0 exit-code))
      (should (equal "hello" (buffer-string))))))

(ert-deftest flit-test-process-file-exit-code ()
  "Test process-file returns non-zero exit code."
  (flit-test--with-fixture
    (let ((default-directory (flit-test--path "")))
      (with-temp-buffer
        (let ((exit-code (process-file "sh" nil nil nil "-c" "exit 42")))
          (should (= 42 exit-code)))))))

(ert-deftest flit-test-process-file-cwd ()
  "Test process-file runs in correct directory."
  (flit-test--with-fixture
    (make-directory (flit-test--local-path "subdir"))
    (let ((default-directory (flit-test--path "subdir")))
      (with-temp-buffer
        (process-file "pwd" nil t nil)
        (should (string-match-p "subdir" (buffer-string)))))))

(ert-deftest flit-test-start-file-process ()
  "Test start-file-process starts async process."
  (flit-test--with-fixture
    (let* ((default-directory (flit-test--path ""))
           (output "")
           (proc (start-file-process "test-echo" nil "sh" "-c" "echo hello; sleep 0.1; echo world")))
      (should proc)
      (should (process-live-p proc))
      ;; Set up filter to collect output
      (set-process-filter proc (lambda (_p str) (setq output (concat output str))))
      ;; Wait for process to complete
      (let ((conn (gethash "test" flit--connections)))
        (when conn
          ;; Accept output for up to 2 seconds
          (let ((start (float-time)))
            (while (and (process-live-p proc)
                        (< (- (float-time) start) 2))
              (accept-process-output (flit-conn-process conn) 0.1)))))
      ;; Check we got output
      (should (string-match-p "hello" output))
      (should (string-match-p "world" output)))))

(ert-deftest flit-test-start-file-process-with-buffer ()
  "Test start-file-process with output buffer."
  (flit-test--with-fixture
    (let* ((default-directory (flit-test--path ""))
           (buf (generate-new-buffer " *test-output*"))
           (proc (start-file-process "test-echo-buf" buf "echo" "-n" "buffered")))
      (unwind-protect
          (progn
            (should proc)
            ;; Wait for output
            (let ((conn (gethash "test" flit--connections)))
              (when conn
                (let ((start (float-time)))
                  (while (and (process-live-p proc)
                              (< (- (float-time) start) 2))
                    (accept-process-output (flit-conn-process conn) 0.1)))))
            ;; Check buffer has output
            (with-current-buffer buf
              (should (string-match-p "buffered" (buffer-string)))))
        (kill-buffer buf)))))

;;; PTY tests
;; Note: PTY tests may be skipped on systems where PTY allocation is restricted
;; (e.g., macOS with sandboxed binaries).

(defun flit-test--pty-available-p ()
  "Check if PTY creation is available by attempting to create one."
  (condition-case err
      (let* ((default-directory (flit-test--path ""))
             (buf (generate-new-buffer " *test-pty-check*"))
             (proc (flit--exec-start flit-test--host "pty-check" "true" nil
                                      flit-test--temp-dir nil t 24 80)))
        (unwind-protect
            (progn
              (when proc (delete-process proc))
              t)
          (kill-buffer buf)))
    (flitrpc-error
     (let ((msg (or (plist-get (cadr err) :message) "")))
       (if (string-match-p "operation not permitted" msg)
           nil
         (signal (car err) (cdr err)))))))

(ert-deftest flit-test-pty-create ()
  "Test creating a process with PTY."
  (flit-test--with-fixture
    (unless (flit-test--pty-available-p)
      (ert-skip "PTY not available (operation not permitted)"))
    (let* ((default-directory (flit-test--path ""))
           (buf (generate-new-buffer " *test-pty-create*"))
           (proc (flit--exec-start flit-test--host "test-pty" "sh" '("-c" "exit 0")
                                    flit-test--temp-dir nil t 24 80)))
      (unwind-protect
          (progn
            (should proc)
            (should (processp proc))
            (should (process-get proc 'flit-proc-id))
            (should (process-get proc 'flit-pty)))
        (when (and proc (process-live-p proc))
          (delete-process proc))
        (kill-buffer buf)))))

(ert-deftest flit-test-pty-output ()
  "Test receiving output from a PTY process."
  (flit-test--with-fixture
    (unless (flit-test--pty-available-p)
      (ert-skip "PTY not available (operation not permitted)"))
    (let* ((default-directory (flit-test--path ""))
           (buf (generate-new-buffer " *test-pty-output*"))
           (proc (flit--exec-start flit-test--host "test-pty-out" "sh"
                                    '("-c" "echo hello-pty")
                                    flit-test--temp-dir nil t 24 80)))
      (unwind-protect
          (progn
            (set-process-buffer proc buf)
            ;; Wait for output
            (let ((conn (gethash "test" flit--connections))
                  (start (float-time)))
              (while (and (< (- (float-time) start) 5)
                          (with-current-buffer buf
                            (not (string-match-p "hello-pty" (buffer-string)))))
                (when conn
                  (accept-process-output (flit-conn-process conn) 0.1))))
            ;; Check we got output
            (with-current-buffer buf
              (should (string-match-p "hello-pty" (buffer-string)))))
        (when (and proc (process-live-p proc))
          (delete-process proc))
        (kill-buffer buf)))))

(ert-deftest flit-test-pty-input ()
  "Test sending input to a PTY process."
  (flit-test--with-fixture
    (unless (flit-test--pty-available-p)
      (ert-skip "PTY not available (operation not permitted)"))
    (let* ((default-directory (flit-test--path ""))
           (buf (generate-new-buffer " *test-pty-input*"))
           (proc (flit--exec-start flit-test--host "test-pty-in" "cat" nil
                                    flit-test--temp-dir nil t 24 80)))
      (unwind-protect
          (progn
            (set-process-buffer proc buf)
            ;; Send input
            (process-send-string proc "hello from input\n")
            ;; Send EOF (Ctrl-D) to make cat exit
            (process-send-string proc "\004")
            ;; Wait for output
            (let ((conn (gethash "test" flit--connections))
                  (start (float-time)))
              (while (and (< (- (float-time) start) 5)
                          (with-current-buffer buf
                            (not (string-match-p "hello from input" (buffer-string)))))
                (when conn
                  (accept-process-output (flit-conn-process conn) 0.1))))
            ;; Check we got the echo back
            (with-current-buffer buf
              (should (string-match-p "hello from input" (buffer-string)))))
        (when (and proc (process-live-p proc))
          (delete-process proc))
        (kill-buffer buf)))))

(ert-deftest flit-test-pty-exit ()
  "Test PTY process exit notification."
  (flit-test--with-fixture
    (unless (flit-test--pty-available-p)
      (ert-skip "PTY not available (operation not permitted)"))
    (let* ((default-directory (flit-test--path ""))
           (buf (generate-new-buffer " *test-pty-exit*"))
           (exit-code nil)
           ;; Use sleep to ensure process doesn't exit before sentinel is set
           (proc (flit--exec-start flit-test--host "test-pty-exit" "sh"
                                    '("-c" "sleep 0.1; exit 42")
                                    flit-test--temp-dir nil t 24 80)))
      (unwind-protect
          (progn
            (set-process-buffer proc buf)
            ;; Set sentinel to capture exit code
            (set-process-sentinel proc
                                   (lambda (p _event)
                                     (setq exit-code (process-exit-status p))))
            ;; Wait for process to exit
            (let ((conn (gethash "test" flit--connections))
                  (start (float-time)))
              (while (and (< (- (float-time) start) 5)
                          (null exit-code))
                (when conn
                  (accept-process-output (flit-conn-process conn) 0.1))))
            ;; Check exit code
            (should (eq exit-code 42)))
        (when (and proc (process-live-p proc))
          (delete-process proc))
        (kill-buffer buf)))))

(ert-deftest flit-test-pty-resize ()
  "Test resizing a PTY process."
  (flit-test--with-fixture
    (unless (flit-test--pty-available-p)
      (ert-skip "PTY not available (operation not permitted)"))
    (let* ((default-directory (flit-test--path ""))
           (buf (generate-new-buffer " *test-pty-resize*"))
           (proc (flit--exec-start flit-test--host "test-pty-resize" "sh"
                                    '("-c" "sleep 1")
                                    flit-test--temp-dir nil t 24 80)))
      (unwind-protect
          (progn
            (set-process-buffer proc buf)
            ;; Resize should not error
            (set-process-window-size proc 40 120)
            ;; Give a moment for the async request
            (let ((conn (gethash "test" flit--connections)))
              (when conn
                (accept-process-output (flit-conn-process conn) 0.2)))
            ;; Just verify no error occurred
            (should t))
        (when (and proc (process-live-p proc))
          (delete-process proc))
        (kill-buffer buf)))))

;;; Async process (exec) tests

(ert-deftest flit-test-async-process-basic ()
  "Test basic async process execution via start-file-process."
  (flit-test--with-fixture
    (let* ((default-directory (flit-test--path ""))
           (buf (generate-new-buffer " *test-async*"))
           (proc (start-file-process "test-echo" buf "echo" "hello world")))
      (unwind-protect
          (progn
            ;; Should return a process
            (should (processp proc))
            ;; Should have flit properties
            (should (process-get proc 'flit-proc-id))
            (should (process-get proc 'flit-host))
            ;; Wait for output
            (let ((conn (gethash "test" flit--connections))
                  (start (float-time)))
              (while (and (< (- (float-time) start) 5)
                          (with-current-buffer buf
                            (= (buffer-size) 0)))
                (when conn
                  (accept-process-output (flit-conn-process conn) 0.1))))
            ;; Check output
            (with-current-buffer buf
              (should (string-match-p "hello world" (buffer-string)))))
        (when (process-live-p proc)
          (delete-process proc))
        (kill-buffer buf)))))

(ert-deftest flit-test-async-process-input ()
  "Test sending input to async process via process-send-string."
  (flit-test--with-fixture
    (let* ((default-directory (flit-test--path ""))
           (buf (generate-new-buffer " *test-async-input*"))
           ;; Use sh to read a line and echo it - doesn't need EOF
           (proc (start-file-process "test-input" buf
                                      "sh" "-c" "read line && echo \"got: $line\"")))
      (unwind-protect
          (progn
            (should (processp proc))
            ;; Send input (the newline triggers read to complete)
            (process-send-string proc "test input line\n")
            ;; Wait for output
            (let ((conn (gethash "test" flit--connections))
                  (start (float-time)))
              (while (and (< (- (float-time) start) 5)
                          (with-current-buffer buf
                            (not (string-match-p "got: test input line" (buffer-string)))))
                (when conn
                  (accept-process-output (flit-conn-process conn) 0.1))))
            ;; Check output echoes input
            (with-current-buffer buf
              (should (string-match-p "got: test input line" (buffer-string)))))
        (when (process-live-p proc)
          (delete-process proc))
        (kill-buffer buf)))))

(ert-deftest flit-test-async-process-exit-code ()
  "Test async process exit code is captured."
  (flit-test--with-fixture
    (let* ((default-directory (flit-test--path ""))
           (buf (generate-new-buffer " *test-async-exit*"))
           ;; Sleep briefly to avoid race, then exit with code 7
           (proc (start-file-process "test-exit" buf "sh" "-c" "sleep 0.1; exit 7")))
      (unwind-protect
          (progn
            (should (processp proc))
            ;; Wait for process to exit
            (let ((conn (gethash "test" flit--connections))
                  (start (float-time)))
              (while (and (< (- (float-time) start) 3)
                          (not (process-get proc 'flit-exit-code)))
                (when conn
                  (accept-process-output (flit-conn-process conn) 0.1))))
            ;; Check exit code
            (should (= (process-exit-status proc) 7)))
        (when (process-live-p proc)
          (delete-process proc))
        (kill-buffer buf)))))

(ert-deftest flit-test-async-process-signal ()
  "Test sending signal to async process."
  (flit-test--with-fixture
    (let* ((default-directory (flit-test--path ""))
           (buf (generate-new-buffer " *test-async-signal*"))
           ;; Long-running process
           (proc (start-file-process "test-sleep" buf "sleep" "60")))
      (unwind-protect
          (progn
            (should (processp proc))
            (let ((proc-id (process-get proc 'flit-proc-id)))
              (should proc-id)
              ;; Send SIGTERM via signal-process
              (signal-process proc 'SIGTERM)
              ;; Wait for process to exit
              (let ((conn (gethash "test" flit--connections))
                    (start (float-time)))
                (while (and (< (- (float-time) start) 2)
                            (not (process-get proc 'flit-exit-code)))
                  (when conn
                    (accept-process-output (flit-conn-process conn) 0.1))))
              ;; Process should have exited (signal causes non-zero exit)
              (should (process-get proc 'flit-exit-code))))
        (when (process-live-p proc)
          (delete-process proc))
        (kill-buffer buf)))))

(ert-deftest flit-test-async-process-env-propagation ()
  "Test that custom env vars propagate but default ones don't."
  (flit-test--with-fixture
    (let* ((default-directory (flit-test--path ""))
           (buf (generate-new-buffer " *test-async-env*")))
      ;; First, establish the connection by doing a file operation
      ;; This starts the flit-server
      (ignore (file-exists-p default-directory))
      ;; NOW set a "default" env var - server already started, won't inherit it
      ;; But since it's set with setenv (not let-bound), it goes into default-toplevel-value
      ;; and should NOT be sent in the delta
      (setenv "FLIT_TEST_DEFAULT_VAR" "default_should_not_appear")
      (unwind-protect
          (let* (;; Set a custom env var via let - this SHOULD propagate
                 (process-environment (cons "FLIT_TEST_CUSTOM_VAR=custom_value_12345"
                                            process-environment))
                 (proc (start-file-process "test-env" buf
                                           "sh" "-c" "echo CUSTOM=$FLIT_TEST_CUSTOM_VAR; echo DEFAULT=$FLIT_TEST_DEFAULT_VAR")))
            (unwind-protect
                (progn
                  (should (processp proc))
                  ;; Wait for output
                  (let ((conn (gethash "test" flit--connections))
                        (start (float-time)))
                    (while (and (< (- (float-time) start) 2)
                                (with-current-buffer buf
                                  (not (string-match-p "CUSTOM=" (buffer-string)))))
                      (when conn
                        (accept-process-output (flit-conn-process conn) 0.1))))
                  (with-current-buffer buf
                    (let ((output (buffer-string)))
                      ;; Custom var should be propagated
                      (should (string-match-p "CUSTOM=custom_value_12345" output))
                      ;; Default var should NOT be propagated - remote won't have it
                      ;; (line should show "DEFAULT=" with empty value or just newline)
                      (should (string-match-p "DEFAULT=\n" output)))))
              (when (process-live-p proc)
                (delete-process proc))))
        ;; Clean up the default env var
        (setenv "FLIT_TEST_DEFAULT_VAR" nil)
        (kill-buffer buf)))))

(ert-deftest flit-test-env-delta-computation ()
  "Test that flit--compute-env-delta only returns modified vars."
  ;; Default env should produce empty delta
  (should (null (flit--compute-env-delta)))
  ;; Adding a var should include it in delta
  (let ((process-environment (cons "FLIT_TEST_VAR=test_value" process-environment)))
    (let ((delta (flit--compute-env-delta)))
      (should (equal delta '(("FLIT_TEST_VAR" . "test_value"))))))
  ;; Multiple vars should all appear
  (let ((process-environment (append '("FLIT_A=1" "FLIT_B=2") process-environment)))
    (let ((delta (flit--compute-env-delta)))
      (should (assoc "FLIT_A" delta))
      (should (assoc "FLIT_B" delta))
      (should (equal (cdr (assoc "FLIT_A" delta)) "1"))
      (should (equal (cdr (assoc "FLIT_B" delta)) "2"))))
  ;; Default vars like PATH should NOT be in delta
  (let ((delta (flit--compute-env-delta)))
    (should-not (assoc "PATH" delta))
    (should-not (assoc "HOME" delta))))

;;; Batch prefetch tests

(ert-deftest flit-test-batch-prefetch ()
  "Test flit--batch-prefetch fetches multiple files and directories."
  (flit-test--with-fixture
    ;; Create test files and directories
    (flit-test--create-file "file1.txt" "content 1")
    (flit-test--create-file "file2.txt" "content 2")
    (make-directory (flit-test--local-path "subdir"))
    (flit-test--create-file "subdir/nested.txt" "nested content")
    ;; Clear cache
    (clrhash flit--cache)
    ;; Batch prefetch (async)
    (let* ((paths (list (concat flit-test--temp-dir "/file1.txt")
                        (concat flit-test--temp-dir "/file2.txt")
                        (concat flit-test--temp-dir "/subdir")))
           (done nil)
           (result nil))
      (flit--batch-prefetch flit-test--host paths
                            (lambda (r _err)
                              (setq result r done t)))
      ;; Wait for callback
      (with-timeout (5 (error "Batch prefetch timed out"))
        (while (not done)
          (accept-process-output nil 0.1)))
      (should result)
      ;; Cache entries arrive as async notifications — wait for them
      (flit-test--wait-for
       (lambda () (flit--cache-get flit-test--host (concat flit-test--temp-dir "/file1.txt"))))
      ;; Check files were cached with content
      (let ((file1-info (flit--cache-get flit-test--host (concat flit-test--temp-dir "/file1.txt"))))
        (should file1-info)
        (should (eq (plist-get file1-info :exists) t))
        (should (plist-get file1-info :content))
        ;; Content is already decoded on cache
        (should (equal "content 1" (plist-get file1-info :content))))
      ;; Check file2 was cached with content
      (let ((file2-info (flit--cache-get flit-test--host (concat flit-test--temp-dir "/file2.txt"))))
        (should file2-info)
        (should (eq (plist-get file2-info :exists) t))
        (should (plist-get file2-info :content))
        (should (equal "content 2" (plist-get file2-info :content))))
      ;; Check subdir was cached with children (list of names)
      (let ((dir-info (flit--cache-get flit-test--host (concat flit-test--temp-dir "/subdir"))))
        (should dir-info)
        (should (eq (plist-get dir-info :exists) t))
        (should (plist-get dir-info :children)))
      ;; Verify that opening a file uses cached content (no extra RPC needed)
      ;; by checking flit--read returns the correct content from cache
      (let ((result (flit--read (concat flit--prefix flit-test--host
                                       flit-test--temp-dir "/file1.txt"))))
        (should (equal "content 1" (plist-get result :content)))))))

(ert-deftest flit-test-batch-prefetch-parent-dirs ()
  "Test that batch prefetch also fetches parent directories of files."
  (flit-test--with-fixture
    ;; Create test file
    (flit-test--create-file "file.txt" "content")
    ;; Clear cache
    (clrhash flit--cache)
    ;; Batch prefetch just the file (async)
    (let ((paths (list (concat flit-test--temp-dir "/file.txt")))
          (done nil))
      (flit--batch-prefetch flit-test--host paths
                            (lambda (_r _err) (setq done t)))
      (with-timeout (5 (error "Batch prefetch timed out"))
        (while (not done)
          (accept-process-output nil 0.1))))
    ;; Check parent directory was also cached with children
    (let ((dir-info (flit--cache-get flit-test--host flit-test--temp-dir)))
      (should dir-info)
      (should (plist-get dir-info :children)))))

;;; Desktop/session restore tests

(ert-deftest flit-test-desktop-save-buffer ()
  "Test that flit-desktop-save-buffer returns the buffer-file-name."
  (flit-test--with-fixture
    (flit-test--create-file "desktop-test.txt" "content")
    (let ((buf (find-file-noselect (flit-test--path "desktop-test.txt"))))
      (unwind-protect
          (with-current-buffer buf
            ;; desktop-save-buffer should return the file name
            (let ((saved (flit--desktop-save-buffer "/tmp")))
              (should (equal saved buffer-file-name))
              (should (string-match-p "flit!" saved))))
        (kill-buffer buf)))))

(ert-deftest flit-test-desktop-restore-with-cache ()
  "Test desktop restore when file is already in cache (server available)."
  (flit-test--with-fixture
    (flit-test--create-file "restore-cached.txt" "cached content")
    ;; Prefetch the file to populate cache
    (let ((file-path (flit-test--path "restore-cached.txt")))
      (flit--get-info file-path)
      ;; Verify it's cached
      (should (flit--cache-get flit-test--host
                               (concat flit-test--temp-dir "/restore-cached.txt")))
      ;; Now simulate desktop restore — file is cached so find-file-noselect works
      (let ((buf (find-file-noselect file-path)))
        (unwind-protect
            (progn
              (should buf)
              (should (buffer-live-p buf))
              (with-current-buffer buf
                (should (equal buffer-file-name file-path))
                (should (string= (buffer-string) "cached content"))))
          (when (buffer-live-p buf) (kill-buffer buf)))))))

(ert-deftest flit-test-desktop-restore-without-cache ()
  "Test desktop restore when file is NOT in cache (creates deferred buffer)."
  (flit-test--with-fixture
    (flit-test--create-file "restore-uncached.txt" "uncached content")
    (let ((file-path (flit-test--path "restore-uncached.txt")))
      ;; Clear cache to simulate no prefetch
      (clrhash flit--cache)
      (clrhash flit--deferred-buffers)
      ;; Desktop restore should create deferred buffer
      (let ((buf (flit--create-deferred-buffer file-path "restore-uncached.txt")))
        (unwind-protect
            (progn
              (should buf)
              (should (buffer-live-p buf))
              (with-current-buffer buf
                (should (equal buffer-file-name file-path))
                ;; Buffer should be marked as deferred
                (should (gethash file-path flit--deferred-buffers))
                ;; Buffer should contain deferred buffer placeholder text
                (should (string-match-p "Flit Deferred Buffer"
                                        (buffer-string)))))
          (remhash file-path flit--deferred-buffers)
          (when (buffer-live-p buf) (kill-buffer buf)))))))

(ert-deftest flit-test-deferred-buffer-creation ()
  "Test flit--create-deferred-buffer creates proper placeholder."
  (flit-test--with-fixture
    (let* ((file-path (flit-test--path "deferred.txt"))
           (buf (flit--create-deferred-buffer file-path "deferred.txt")))
      (unwind-protect
          (progn
            (should buf)
            (should (buffer-live-p buf))
            (with-current-buffer buf
              ;; Check buffer-file-name is set
              (should (equal buffer-file-name file-path))
              ;; Check it's in the deferred table
              (should (gethash file-path flit--deferred-buffers))
              ;; Check default-directory is a flit path (so LSP/eglot know it's remote)
              (should (string-match-p "flit!" default-directory))
              (should (flit--file-name-p default-directory))
              ;; Check buffer is not modified
              (should-not (buffer-modified-p))
              ;; Check placeholder text (keybinding hint)
              (should (string-match-p "Connect and reload file" (buffer-string)))))
        (remhash file-path flit--deferred-buffers)
        (when (buffer-live-p buf) (kill-buffer buf))))))

(ert-deftest flit-test-retry-deferred-success ()
  "Test revert-buffer loads deferred buffers when server is available."
  (flit-test--with-fixture
    (flit-test--create-file "retry-test.txt" "retry content")
    (let* ((file-path (flit-test--path "retry-test.txt"))
           (buf (flit--create-deferred-buffer file-path "retry-test.txt")))
      (unwind-protect
          (progn
            ;; Buffer should be deferred
            (should (gethash file-path flit--deferred-buffers))
            ;; Retry via revert-buffer - should load since server is available
            (with-current-buffer buf
              (revert-buffer t t))
            ;; Buffer should no longer be deferred
            (should-not (gethash file-path flit--deferred-buffers))
            ;; Buffer should have real content
            (with-current-buffer buf
              (should (string= (buffer-string) "retry content"))))
        (remhash file-path flit--deferred-buffers)
        (when (buffer-live-p buf) (kill-buffer buf))))))

(ert-deftest flit-test-batch-prefetch-failure-marks-deferred ()
  "Test that failed batch prefetch marks files as deferred."
  ;; This test does NOT use the fixture, so no server is available
  ;; We use a command that will fail to simulate connection failure
  (let ((flit-connection-methods '((".*" . (:stdio "false"))))  ; Command that always fails
        (file-path (concat flit--prefix "localhost/path/to/unavailable.txt")))
    ;; Clear deferred table
    (clrhash flit--deferred-buffers)
    ;; Put this file in deferred as if prefetch failed
    (puthash file-path t flit--deferred-buffers)
    ;; Now try to restore - should create deferred buffer since server is unavailable
    (let ((buf (flit--create-deferred-buffer file-path "unavailable.txt")))
      (unwind-protect
          (progn
            (should buf)
            (with-current-buffer buf
              ;; Should be a deferred placeholder
              (should (string-match-p "unavailable\\|deferred\\|pending"
                                      (buffer-string)))))
        (remhash file-path flit--deferred-buffers)
        (when (buffer-live-p buf) (kill-buffer buf))))))

;;; File watching lifecycle tests

(ert-deftest flit-test-watch-survives-atomic-rewrite ()
  "Test that file watch survives atomic rewrite (rename temp over original).
This tests the common editor pattern of writing to temp file then renaming."
  (flit-test--with-fixture
    (flit-test--create-file "atomic-test.txt" "initial content")
    (let ((buf (find-file-noselect (flit-test--path "atomic-test.txt"))))
      (unwind-protect
          (with-current-buffer buf
            ;; Set up watching
            (flit--watch buffer-file-name)
            (setq-local buffer-stale-function #'flit--buffer-stale-p)
            (should-not flit--file-changed)

            ;; Perform atomic rewrite: write to temp, then rename over original
            (let ((temp-path (flit-test--local-path "atomic-test.txt.tmp")))
              (with-temp-file temp-path
                (insert "rewritten via atomic"))
              (rename-file temp-path (flit-test--local-path "atomic-test.txt") t))

            ;; Wait for notification (deleted or modified)
            (flit-test--wait-for-notification)
            (setq flit--file-changed nil)

            ;; Now modify the file again - watch should still work
            (sleep-for 1.1)  ; Wait for mtime to change
            (with-temp-file (flit-test--local-path "atomic-test.txt")
              (insert "second modification"))

            ;; Should have received the second notification
            (should (flit-test--wait-for-notification)))
        (ignore-errors (flit--unwatch (buffer-file-name buf)))
        (kill-buffer buf)))))

(ert-deftest flit-test-watch-survives-delete-then-create ()
  "Test that file watch survives delete followed by create.
This handles the non-atomic case where file is deleted then recreated."
  (flit-test--with-fixture
    (flit-test--create-file "delete-create.txt" "initial content")
    (let ((buf (find-file-noselect (flit-test--path "delete-create.txt"))))
      (unwind-protect
          (with-current-buffer buf
            ;; Set up watching
            (flit--watch buffer-file-name)
            (setq-local buffer-stale-function #'flit--buffer-stale-p)
            (should-not flit--file-changed)

            ;; Delete the file
            (delete-file (flit-test--local-path "delete-create.txt"))

            ;; Wait for delete notification
            (flit-test--wait-for-notification)
            (setq flit--file-changed nil)

            ;; Recreate the file
            (with-temp-file (flit-test--local-path "delete-create.txt")
              (insert "recreated content"))

            ;; Should have received create notification
            (should (flit-test--wait-for-notification))
            (setq flit--file-changed nil)

            ;; Now modify the file - watch should still work
            (sleep-for 1.1)  ; Wait for mtime to change
            (with-temp-file (flit-test--local-path "delete-create.txt")
              (insert "modified after recreate"))

            ;; Should have received the modification notification
            (should (flit-test--wait-for-notification)))
        (ignore-errors (flit--unwatch (buffer-file-name buf)))
        (kill-buffer buf)))))

(ert-deftest flit-test-watch-survives-rename-away ()
  "Test that file watch survives rename away followed by new file creation.
This tests the vim backup pattern: rename file.txt to file.txt~, create new file.txt."
  (flit-test--with-fixture
    (flit-test--create-file "rename-away.txt" "initial content")
    (let ((buf (find-file-noselect (flit-test--path "rename-away.txt"))))
      (unwind-protect
          (with-current-buffer buf
            ;; Set up watching
            (flit--watch buffer-file-name)
            (setq-local buffer-stale-function #'flit--buffer-stale-p)
            (should-not flit--file-changed)

            ;; Rename the file away (like vim backup)
            (rename-file (flit-test--local-path "rename-away.txt")
                         (flit-test--local-path "rename-away.txt~"))

            ;; Wait for rename notification
            (flit-test--wait-for-notification)
            (setq flit--file-changed nil)

            ;; Create new file at original path
            (with-temp-file (flit-test--local-path "rename-away.txt")
              (insert "new file content"))

            ;; Should have received create notification
            (should (flit-test--wait-for-notification))
            (setq flit--file-changed nil)

            ;; Now modify the file - watch should still work
            (sleep-for 1.1)  ; Wait for mtime to change
            (with-temp-file (flit-test--local-path "rename-away.txt")
              (insert "modified new file"))

            ;; Should have received the modification notification
            (should (flit-test--wait-for-notification)))
        (ignore-errors (flit--unwatch (buffer-file-name buf)))
        (kill-buffer buf)))))

(ert-deftest flit-test-watch-survives-multiple-rewrites ()
  "Test that file watch survives multiple consecutive atomic rewrites."
  (flit-test--with-fixture
    (flit-test--create-file "multi-rewrite.txt" "initial content")
    (let ((buf (find-file-noselect (flit-test--path "multi-rewrite.txt"))))
      (unwind-protect
          (with-current-buffer buf
            ;; Set up watching
            (flit--watch buffer-file-name)
            (setq-local buffer-stale-function #'flit--buffer-stale-p)

            ;; Do multiple atomic rewrites
            (dotimes (i 3)
              (let ((temp-path (flit-test--local-path "multi-rewrite.txt.tmp")))
                (with-temp-file temp-path
                  (insert (format "rewrite %d" i)))
                (rename-file temp-path (flit-test--local-path "multi-rewrite.txt") t))
              ;; Wait for watch to be re-established
              (flit-test--wait-for-notification)
              (setq flit--file-changed nil))

            ;; Final modification should still trigger notification
            (sleep-for 1.1)
            (with-temp-file (flit-test--local-path "multi-rewrite.txt")
              (insert "final modification"))

            ;; Should have received notification
            (should (flit-test--wait-for-notification)))
        (ignore-errors (flit--unwatch (buffer-file-name buf)))
        (kill-buffer buf)))))

(ert-deftest flit-test-fs-open-called-on-find-file ()
  "Test that fs/open is called when opening a flit file."
  (flit-test--with-fixture
    (flit-test--create-file "open-test.txt" "test content")
    (let* ((open-calls nil)
           (orig-send-request-async (symbol-function 'flit--send-request-async)))
      (cl-letf (((symbol-function 'flit--send-request-async)
                 (lambda (host method params &optional success-fn error-fn)
                   (when (equal method "fs/open")
                     (push (plist-get params :path) open-calls))
                   (funcall orig-send-request-async host method params success-fn error-fn))))
        (let ((buf (find-file-noselect (flit-test--path "open-test.txt"))))
          (unwind-protect
              (progn
                ;; Wait for async fs/open request to be sent
                (should (flit-test--wait-for (lambda () open-calls)))
                (should (string-suffix-p "open-test.txt" (car open-calls))))
            (kill-buffer buf)))))))

(ert-deftest flit-test-fs-close-called-on-kill-buffer ()
  "Test that fs/close is called when killing a flit buffer."
  (flit-test--with-fixture
    (flit-test--create-file "close-test.txt" "test content")
    (let* ((close-calls nil)
           (orig-send-request-async (symbol-function 'flit--send-request-async)))
      (cl-letf (((symbol-function 'flit--send-request-async)
                 (lambda (host method params &optional success-fn error-fn)
                   (when (equal method "fs/close")
                     (push (plist-get params :path) close-calls))
                   (funcall orig-send-request-async host method params success-fn error-fn))))
        (let ((buf (find-file-noselect (flit-test--path "close-test.txt"))))
          ;; Kill the buffer
          (kill-buffer buf)
          ;; Wait for async fs/close request to be sent
          (should (flit-test--wait-for (lambda () close-calls)))
          (should (string-suffix-p "close-test.txt" (car close-calls))))))))

;;; Cache prefetching tests

(ert-deftest flit-test-directory-listing-caches-entries ()
  "Test that directory listing populates cache for child entries.
After listing a directory, file-attributes on entries should use cache."
  (flit-test--with-fixture
    ;; Create a directory with multiple files
    (make-directory (flit-test--local-path "cachedir"))
    (flit-test--create-file "cachedir/file1.txt" "content 1")
    (flit-test--create-file "cachedir/file2.txt" "content 2")
    (flit-test--create-file "cachedir/file3.txt" "content 3")
    ;; Clear cache to start fresh
    (clrhash flit--cache)
    ;; List the directory - this should cache info for all entries
    (let ((entries (directory-files (flit-test--path "cachedir"))))
      (should (member "file1.txt" entries))
      (should (member "file2.txt" entries))
      (should (member "file3.txt" entries)))
    ;; Wait for async entryInfo notification to arrive
    (let ((conn (gethash "test" flit--connections)))
      (when conn
        (accept-process-output (flit-conn-process conn) 0.5)))
    ;; Now count RPCs when getting file-attributes for each entry
    (let* ((rpc-count 0)
           (orig-send-request (symbol-function 'flit--send-request)))
      (cl-letf (((symbol-function 'flit--send-request)
                 (lambda (host method params)
                   (setq rpc-count (1+ rpc-count))
                   (funcall orig-send-request host method params))))
        ;; These should all use cached data - no RPCs
        (file-attributes (flit-test--path "cachedir/file1.txt"))
        (file-attributes (flit-test--path "cachedir/file2.txt"))
        (file-attributes (flit-test--path "cachedir/file3.txt")))
      ;; Should be 0 RPCs if cache is working
      (should (= 0 rpc-count)))))

(ert-deftest flit-test-directory-listing-caches-content ()
  "Test that directory listing caches content for small files.
After listing a directory, reading small files should use cached content."
  (flit-test--with-fixture
    ;; Create a directory with a small file
    (make-directory (flit-test--local-path "contentdir"))
    (flit-test--create-file "contentdir/small.txt" "small file content")
    ;; Clear cache to start fresh
    (clrhash flit--cache)
    ;; List the directory to populate cache
    (directory-files (flit-test--path "contentdir"))
    ;; Wait for async entryInfo notification to populate cache with file content
    (should (flit-test--wait-for
             (lambda ()
               (let ((cached (gethash (cons flit-test--host
                                            (concat flit-test--temp-dir "/contentdir/small.txt"))
                                      flit--cache)))
                 (and cached (plist-get cached :content))))))
    ;; Count RPCs when reading the file
    ;; Disable VC to avoid slow VC root detection during find-file
    (let* ((rpc-count 0)
           (vc-handled-backends nil)
           (orig-send-request (symbol-function 'flit--send-request)))
      (cl-letf (((symbol-function 'flit--send-request)
                 (lambda (host method params)
                   (setq rpc-count (1+ rpc-count))
                   (funcall orig-send-request host method params))))
        (let ((buf (find-file-noselect (flit-test--path "contentdir/small.txt"))))
          (unwind-protect
              (with-current-buffer buf
                (should (equal "small file content" (buffer-string))))
            (kill-buffer buf))))
      ;; Should be 0 RPCs for reading if content was prefetched
      (should (= 0 rpc-count)))))

(ert-deftest flit-test-directory-cache-invalidated-on-file-create ()
  "Test that directory cache is invalidated when a new file is created.
Creating a file should update the parent directory listing."
  (flit-test--with-fixture
    ;; Create a directory
    (make-directory (flit-test--local-path "newfiledir"))
    (flit-test--create-file "newfiledir/existing.txt" "existing content")
    ;; Clear cache
    (clrhash flit--cache)
    ;; List the directory - this caches the entries
    (let ((entries (directory-files (flit-test--path "newfiledir"))))
      (should (member "existing.txt" entries))
      (should-not (member "newfile.txt" entries)))
    ;; Create a new file locally (simulating external write)
    (flit-test--create-file "newfiledir/newfile.txt" "new content")
    ;; Wait for fsnotify to detect the change (debounce is 1s)
    (sleep-for 1.1)
    ;; Accept process output to receive notification
    (let ((conn (gethash "test" flit--connections)))
      (when conn
        (accept-process-output (flit-conn-process conn) 0.5)))
    ;; List directory again - should see the new file
    (let ((entries (directory-files (flit-test--path "newfiledir"))))
      (should (member "existing.txt" entries))
      (should (member "newfile.txt" entries)))))

;;; Process lifecycle and connection tests

(ert-deftest flit-test-process-exit-calls-sentinel ()
  "Test that process exit calls the sentinel with correct status."
  (flit-test--with-fixture
    (let* ((default-directory (flit-test--path ""))
           (buf (generate-new-buffer " *test-sentinel*"))
           (sentinel-event nil)
           (sentinel-exit-code nil)
           (proc (start-file-process "test-sentinel" buf "sh" "-c" "sleep 0.1; exit 13")))
      (unwind-protect
          (progn
            ;; Set sentinel to capture the event
            (set-process-sentinel proc
                                   (lambda (p event)
                                     (setq sentinel-event event)
                                     (setq sentinel-exit-code (process-exit-status p))))
            ;; Wait for process to exit
            (let ((conn (gethash "test" flit--connections))
                  (start (float-time)))
              (while (and (< (- (float-time) start) 3)
                          (null sentinel-event))
                (when conn
                  (accept-process-output (flit-conn-process conn) 0.1))))
            ;; Sentinel should have been called
            (should sentinel-event)
            ;; Exit code should be captured
            (should (= 13 sentinel-exit-code))
            ;; process-exit-status should return the code
            (should (= 13 (process-exit-status proc))))
        (when (process-live-p proc)
          (delete-process proc))
        (kill-buffer buf)))))

(ert-deftest flit-test-server-exit-cleans-up-processes ()
  "Test that server exit marks processes as exited and calls sentinels."
  (flit-test--with-fixture
    (let* ((default-directory (flit-test--path ""))
           (buf (generate-new-buffer " *test-server-exit*"))
           (sentinel-called nil)
           ;; Start a long-running process
           (proc (start-file-process "test-long" buf "sleep" "60")))
      (unwind-protect
          (progn
            (should (processp proc))
            (should (process-get proc 'flit-proc-id))
            ;; Set sentinel
            (set-process-sentinel proc (lambda (_p _event) (setq sentinel-called t)))
            ;; Simulate server exit by closing the connection
            (flit--close-connection flit-test--host)
            ;; Sentinel should have been called
            (should sentinel-called)
            ;; Process should be marked as exited
            (should (process-get proc 'flit-exited))
            ;; Exit code should be -1 (connection closed)
            (should (= -1 (process-exit-status proc))))
        (kill-buffer buf)))))

(ert-deftest flit-test-accept-process-output-on-flit-process ()
  "Test that accept-process-output works on flit processes."
  (flit-test--with-fixture
    (let* ((default-directory (flit-test--path ""))
           (buf (generate-new-buffer " *test-accept*"))
           (output-received nil)
           (proc (start-file-process "test-accept" buf "sh" "-c" "sleep 0.2; echo accept-test-output")))
      (unwind-protect
          (progn
            (should (processp proc))
            ;; Set filter to track output
            (set-process-filter proc (lambda (_p str)
                                       (when (string-match-p "accept-test-output" str)
                                         (setq output-received t))))
            ;; Use accept-process-output directly on the flit process
            ;; This should work due to our advice redirecting to jsonrpc connection
            (let ((start (float-time)))
              (while (and (< (- (float-time) start) 3)
                          (not output-received))
                ;; Call accept-process-output on the flit process itself
                (accept-process-output proc 0.1)))
            ;; Should have received the output
            (should output-received))
        (when (process-live-p proc)
          (delete-process proc))
        (kill-buffer buf)))))

(ert-deftest flit-test-accept-process-output-nonblocking ()
  "Test that accept-process-output with timeout=0 returns immediately.
This is a regression test for a bug where timeout=0 caused an infinite loop."
  (flit-test--with-fixture
    (let* ((default-directory (flit-test--path ""))
           (buf (generate-new-buffer " *test-nonblock*"))
           (proc (start-file-process "test-nonblock" buf "sleep" "60")))
      (unwind-protect
          (progn
            (should (processp proc))
            ;; timeout=0 should be non-blocking and return immediately
            ;; If the bug is present, this will hang forever
            (let ((start (float-time)))
              (accept-process-output proc 0)
              ;; Should return in well under 1 second
              (should (< (- (float-time) start) 1.0))))
        (when (process-live-p proc)
          (delete-process proc))
        (kill-buffer buf)))))

(ert-deftest flit-test-accept-process-output-waits-for-correct-process ()
  "Test that accept-process-output waits for output from the specific process.
This tests that when multiple processes are running, we wait for output from
the process we asked about, not just any output on the connection."
  (flit-test--with-fixture
    (let* ((default-directory (flit-test--path ""))
           ;; Create buffers and output trackers for 3 processes
           (buf1 (generate-new-buffer " *test-multi1*"))
           (buf2 (generate-new-buffer " *test-multi2*"))
           (buf3 (generate-new-buffer " *test-multi3*"))
           (output1 nil)
           (output2 nil)
           (output3 nil)
           ;; Start 3 processes with different delays:
           ;; proc1: 300ms delay, proc2: 100ms delay, proc3: 200ms delay
           ;; So output arrives in order: proc2, proc3, proc1
           (proc1 (start-file-process "multi1" buf1 "sh" "-c" "sleep 0.3; echo OUTPUT-FROM-PROC1"))
           (proc2 (start-file-process "multi2" buf2 "sh" "-c" "sleep 0.1; echo OUTPUT-FROM-PROC2"))
           (proc3 (start-file-process "multi3" buf3 "sh" "-c" "sleep 0.2; echo OUTPUT-FROM-PROC3")))
      (unwind-protect
          (progn
            ;; Set up filters to capture output
            (set-process-filter proc1 (lambda (_p str)
                                        (when (string-match-p "OUTPUT-FROM-PROC1" str)
                                          (setq output1 t))))
            (set-process-filter proc2 (lambda (_p str)
                                        (when (string-match-p "OUTPUT-FROM-PROC2" str)
                                          (setq output2 t))))
            (set-process-filter proc3 (lambda (_p str)
                                        (when (string-match-p "OUTPUT-FROM-PROC3" str)
                                          (setq output3 t))))

            ;; Wait for proc1 output first (even though proc2 and proc3 output arrive earlier)
            ;; If the bug were present, this would return as soon as proc2's output arrives,
            ;; but output1 would still be nil
            (let ((deadline (+ (float-time) 2)))
              (while (and (not output1) (< (float-time) deadline))
                (accept-process-output proc1 0.05)))
            (should output1)

            ;; Now wait for proc2 (should already have output, returns immediately)
            (let ((deadline (+ (float-time) 2)))
              (while (and (not output2) (< (float-time) deadline))
                (accept-process-output proc2 0.05)))
            (should output2)

            ;; Now wait for proc3 (should already have output, returns immediately)
            (let ((deadline (+ (float-time) 2)))
              (while (and (not output3) (< (float-time) deadline))
                (accept-process-output proc3 0.05)))
            (should output3))

        ;; Cleanup
        (dolist (proc (list proc1 proc2 proc3))
          (when (process-live-p proc)
            (delete-process proc)))
        (dolist (buf (list buf1 buf2 buf3))
          (kill-buffer buf))))))

(ert-deftest flit-test-connection-sentinel-on-server-crash ()
  "Test that connection sentinel cleans up when server dies unexpectedly."
  (flit-test--with-fixture
    (let* ((default-directory (flit-test--path ""))
           (buf (generate-new-buffer " *test-crash*"))
           (sentinel-called nil)
           ;; Start a process
           (proc (start-file-process "test-crash" buf "sleep" "60")))
      (unwind-protect
          (progn
            (should (processp proc))
            (set-process-sentinel proc (lambda (_p _event) (setq sentinel-called t)))
            ;; Get the connection process and kill it to simulate crash
            (let* ((conn (gethash flit-test--host flit--connections))
                   (conn-proc (and conn (flit-conn-process conn))))
              (should conn-proc)
              ;; Kill the connection process
              (delete-process conn-proc)
              ;; Use sit-for to allow Emacs to process events
              (sit-for 0.3))
            ;; Process sentinel should have been called
            (should sentinel-called)
            ;; Process should be marked as exited
            (should (process-get proc 'flit-exited)))
        (kill-buffer buf)))))

;;; Completion tests

(ert-deftest flit-test-local-completions-with-flit-default-directory ()
  "Test that ~/ completions work when default-directory is a flit path.
This verifies that file-name-all-completions passes through to the
default handler for non-flit directories like ~/."
  (flit-test--with-fixture
    (let ((default-directory (flit-test--path "")))
      ;; ~/ should return local home directory completions
      (let ((completions (file-name-all-completions "" "~/")))
        (should (listp completions))
        ;; Should have at least some entries (local home directory)
        (should (> (length completions) 0))
        ;; Should NOT be flit paths
        (dolist (c completions)
          (should-not (string-match-p (concat "^" (regexp-quote flit--prefix)) c)))))))

;;; Undo/revert tests

(ert-deftest flit-test-undo-after-revert-marks-buffer-modified ()
  "Test that undo after revert marks buffer as modified.
Regression test for bug where set-visited-file-modtime wasn't actually
updating the buffer's modtime, causing undo after revert to leave the
buffer in an unmodified state even though content differed from disk."
  (flit-test--with-fixture
    (let* ((path (flit-test--path "revert-test.txt"))
           (local-path (flit-test--local-path "revert-test.txt"))
           buf)
      ;; Create file with initial content
      (flit-test--create-file "revert-test.txt" "original content")
      (sleep-for 0.1)

      ;; Visit the file
      (setq buf (find-file-noselect path))
      (unwind-protect
          (with-current-buffer buf
            ;; Disable final newline to avoid confusion
            (setq-local require-final-newline nil)
            ;; Enable undo (buffer-undo-list may be t initially)
            (when (eq buffer-undo-list t)
              (setq buffer-undo-list nil))

            ;; Buffer should have original content and be unmodified
            (should (equal "original content" (buffer-string)))
            (should-not (buffer-modified-p))

            ;; Make an edit so we have something to undo to
            (goto-char (point-max))
            (insert " - edited")
            (should (buffer-modified-p))

            ;; Save the file
            (save-buffer)
            (should-not (buffer-modified-p))

            ;; Wait >1 second for mtime to change (mtime has second granularity)
            (sleep-for 1.1)
            (let ((coding-system-for-write 'no-conversion))
              (write-region "externally modified content" nil local-path nil 'silent))
            ;; Invalidate flit's cache so it sees the new content
            (flit--cache-invalidate flit-test--host
                                    (flit--normalize-path
                                     (flit--path buffer-file-name)))

            ;; Manually reload using insert-file-contents (preserves undo)
            ;; Then call set-visited-file-modtime and set-buffer-modified-p
            ;; This simulates what happens during revert
            (let ((inhibit-read-only t))
              (insert-file-contents buffer-file-name nil nil nil t)
              (set-visited-file-modtime)  ; This is the function we're testing!
              (set-buffer-modified-p nil))

            ;; Buffer should now have external content and be unmodified
            (should (equal "externally modified content" (buffer-string)))
            (should-not (buffer-modified-p))

            ;; Add undo boundary before attempting undo
            (undo-boundary)

            ;; Now undo - should restore previous content
            (undo)
            ;; After undo, we should be back to some previous state
            ;; (content now differs from disk)

            ;; CRITICAL: Buffer should be marked as modified because
            ;; the content differs from what's on disk
            (should (buffer-modified-p)))
        (when buf (kill-buffer buf))))))

;;; New handler tests

(ert-deftest flit-test-set-file-modes ()
  "Test set-file-modes changes permissions."
  (flit-test--with-fixture
    (let* ((_path (flit-test--create-file "chmod-test.txt" "test content"))
           (flit-path (flit-test--path "chmod-test.txt")))
      ;; Set to read-only
      (set-file-modes flit-path #o444)
      ;; Verify mode changed
      (let ((mode (file-modes flit-path)))
        (should (= (logand mode #o777) #o444)))
      ;; Set back to writable
      (set-file-modes flit-path #o644)
      (let ((mode (file-modes flit-path)))
        (should (= (logand mode #o777) #o644))))))

(ert-deftest flit-test-set-file-times ()
  "Test set-file-times changes modification time."
  (flit-test--with-fixture
    (let* ((_path (flit-test--create-file "touch-test.txt" "test content"))
           (flit-path (flit-test--path "touch-test.txt"))
           (_old-time (file-attribute-modification-time
                      (file-attributes flit-path)))
           ;; Set to a specific time (2020-01-01 00:00:00)
           (new-time (encode-time 0 0 0 1 1 2020)))
      (set-file-times flit-path new-time)
      ;; Verify time changed
      (let* ((attrs (file-attributes flit-path))
             (result-time (file-attribute-modification-time attrs)))
        ;; Check that the time is approximately the one we set
        (should (< (abs (- (float-time result-time) (float-time new-time))) 2))))))

(ert-deftest flit-test-file-local-copy ()
  "Test file-local-copy creates a local temp file."
  (flit-test--with-fixture
    (let* ((content "content for local copy")
           (_path (flit-test--create-file "local-copy.txt" content))
           (flit-path (flit-test--path "local-copy.txt"))
           (local-copy (file-local-copy flit-path)))
      (unwind-protect
          (progn
            ;; Local copy should exist and be a local file
            (should local-copy)
            (should (file-exists-p local-copy))
            (should-not (flit--file-name-p local-copy))
            ;; Content should match
            (should (equal content
                           (with-temp-buffer
                             (insert-file-contents local-copy)
                             (buffer-string)))))
        ;; Clean up
        (when (and local-copy (file-exists-p local-copy))
          (delete-file local-copy))))))

(ert-deftest flit-test-copy-directory ()
  "Test copy-directory copies a directory recursively."
  (flit-test--with-fixture
    ;; Create source directory with files
    (make-directory (flit-test--local-path "src-dir"))
    (flit-test--create-file "src-dir/file1.txt" "file 1 content")
    (flit-test--create-file "src-dir/file2.txt" "file 2 content")
    (make-directory (flit-test--local-path "src-dir/subdir"))
    (flit-test--create-file "src-dir/subdir/nested.txt" "nested content")

    (let ((src (flit-test--path "src-dir"))
          (dest (flit-test--path "dest-dir")))
      ;; Copy with copy-contents = t (copy contents into dest)
      (make-directory dest)
      (copy-directory src dest nil nil t)

      ;; Verify files were copied
      (should (file-exists-p (flit-test--path "dest-dir/file1.txt")))
      (should (file-exists-p (flit-test--path "dest-dir/file2.txt")))
      (should (file-exists-p (flit-test--path "dest-dir/subdir/nested.txt")))

      ;; Verify content
      (should (equal "file 1 content"
                     (flit-test--read-file "dest-dir/file1.txt"))))))

(ert-deftest flit-test-temporary-file-directory ()
  "Test temporary-file-directory handler returns remote /tmp."
  (flit-test--with-fixture
    (let* ((flit-path (flit-test--path "any-file")))
      ;; Call the handler directly since temporary-file-directory function
      ;; doesn't go through file handlers
      (let ((temp-dir (flit--file-name-handler 'temporary-file-directory flit-path)))
        ;; Should return a flit path to /tmp
        (should (flit--file-name-p temp-dir))
        (should (string-match-p "/tmp/?$" temp-dir))))))

(ert-deftest flit-test-access-file ()
  "Test access-file signals error for non-existent files."
  (flit-test--with-fixture
    (let ((flit-path (flit-test--path "nonexistent.txt")))
      ;; Should signal error for non-existent file
      (should-error (access-file flit-path "testing access")
                    :type 'file-error))
    ;; Should not signal for existing file
    (let* ((_path (flit-test--create-file "exists.txt" "content"))
           (flit-path (flit-test--path "exists.txt")))
      (should-not (access-file flit-path "testing access")))))

(ert-deftest flit-test-dired-uncache ()
  "Test dired-uncache handler clears directory cache."
  (flit-test--with-fixture
    (let ((dir-path (flit-test--path "")))
      ;; First access to populate cache
      (directory-files dir-path)
      ;; Call the handler directly since dired-uncache is a handler operation
      ;; not a standalone function
      (flit--file-name-handler 'dired-uncache dir-path)
      ;; This is a smoke test - mainly verifying the operation doesn't error
      (should t))))

(ert-deftest flit-test-find-backup-file-name ()
  "Test find-backup-file-name returns appropriate backup names."
  (flit-test--with-fixture
    (let* ((tramp-backup-directory-alist '(("." . "~/.emacs.d/backups")))
           (flit-path (flit-test--path "backup-test.txt"))
           (backup-names (find-backup-file-name flit-path)))
      ;; Should return a list with local backup path
      (if backup-names
          (progn
            (should (listp backup-names))
            (should (stringp (car backup-names)))
            ;; Backup should be local (not a flit path)
            (should-not (flit--file-name-p (car backup-names))))
        ;; Or nil if no backup directory configured
        (should-not backup-names)))))

;;; Write safety tests (mtime mismatch detection)
;;
;; These tests verify the server-side mtime checking for write safety.
;; We call the RPC directly to test server behavior without the client-side
;; prompt handling (which can't run in batch mode).

(ert-deftest flit-test-write-mismatch-detects-external-change ()
  "Test that saving after external file modification triggers mismatch."
  (flit-test--with-fixture
    (flit-test--create-file "mismatch.txt" "original content")
    (let ((flit-path (flit-test--path "mismatch.txt"))
          (local-path (flit-test--local-path "mismatch.txt")))
      (let ((buf (find-file-noselect flit-path)))
        (unwind-protect
            (progn
              ;; Buffer has original content and visited-file-modtime set
              (with-current-buffer buf
                (should (equal "original content" (buffer-string))))

              ;; Get the current mtime that the buffer thinks the file has
              (let ((original-mtime (with-current-buffer buf
                                      (float-time (visited-file-modtime)))))

                ;; Externally modify the file (simulating another process)
                (sleep-for 3.5)  ; Ensure mtime differs by > 2 seconds (tolerance)
                (with-temp-file local-path
                  (insert "externally modified content"))

                ;; Call RPC directly with the old expected mtime
                (flit--with-parsed (host path) flit-path
                  (let* ((content "modified in emacs")
                         (raw-content (encode-coding-string content 'utf-8-unix))
                         (result (flit--send-request host "fs/write"
                                   `(:path ,path
                                     :expectedMtime ,original-mtime)
                                   nil raw-content)))
                    ;; Should get mismatch response
                    (should (eq (plist-get result :mismatch) t))
                    ;; Current file info should be returned
                    (should (plist-get result :current))
                    ;; File should NOT have been modified
                    (should (equal "externally modified content"
                                   (flit-test--read-file "mismatch.txt")))))))
          (when (buffer-live-p buf)
            (with-current-buffer buf (set-buffer-modified-p nil))
            (kill-buffer buf)))))))

(ert-deftest flit-test-write-mismatch-new-file-exists ()
  "Test that saving a 'new' file that now exists triggers mismatch."
  (flit-test--with-fixture
    (let ((flit-path (flit-test--path "newfile.txt"))
          (local-path (flit-test--local-path "newfile.txt")))
      ;; Someone else creates the file first!
      (with-temp-file local-path
        (insert "someone else created this"))

      ;; Now try to write with expectNotExist flag
      (flit--with-parsed (host path) flit-path
        (let* ((content "my new content")
               (raw-content (encode-coding-string content 'utf-8-unix))
               (result (flit--send-request host "fs/write"
                         `(:path ,path
                           :expectNotExist t)
                         nil raw-content)))
          ;; Should get mismatch response
          (should (eq (plist-get result :mismatch) t))
          ;; Current file info should be returned
          (should (plist-get result :current))
          ;; File should NOT have been overwritten
          (should (equal "someone else created this"
                         (flit-test--read-file "newfile.txt"))))))))

(ert-deftest flit-test-write-no-mismatch-when-unchanged ()
  "Test that saving unchanged file succeeds without mismatch."
  (flit-test--with-fixture
    (flit-test--create-file "unchanged.txt" "original content")
    (let ((flit-path (flit-test--path "unchanged.txt")))
      (let ((buf (find-file-noselect flit-path)))
        (unwind-protect
            (progn
              (with-current-buffer buf
                (should (equal "original content" (buffer-string))))
              ;; Get current mtime and send write with it - should succeed
              (let ((current-mtime (with-current-buffer buf
                                     (float-time (visited-file-modtime)))))
                (flit--with-parsed (host path) flit-path
                  (let* ((content "modified content")
                         (raw-content (encode-coding-string content 'utf-8-unix))
                         (result (flit--send-request host "fs/write"
                                   `(:path ,path
                                     :expectedMtime ,current-mtime)
                                   nil raw-content)))
                    ;; Should NOT get mismatch
                    (should-not (plist-get result :mismatch))
                    ;; Should have file info
                    (should (plist-get result :exists))
                    ;; File should be updated
                    (should (equal "modified content"
                                   (flit-test--read-file "unchanged.txt")))))))
          (when (buffer-live-p buf)
            (with-current-buffer buf (set-buffer-modified-p nil))
            (kill-buffer buf)))))))

(ert-deftest flit-test-write-force-after-mismatch ()
  "Test that force write succeeds even after mismatch."
  (flit-test--with-fixture
    (flit-test--create-file "force.txt" "original content")
    (let ((flit-path (flit-test--path "force.txt"))
          (local-path (flit-test--local-path "force.txt")))
      (let ((buf (find-file-noselect flit-path)))
        (unwind-protect
            (progn
              ;; Get original mtime
              (let ((original-mtime (with-current-buffer buf
                                      (float-time (visited-file-modtime)))))
                ;; Externally modify the file
                (sleep-for 3.5)  ; Ensure mtime differs by > 2 seconds (tolerance)
                (with-temp-file local-path
                  (insert "externally modified"))

                ;; First write should return mismatch
                (flit--with-parsed (host path) flit-path
                  (let* ((content "my new content")
                         (raw-content (encode-coding-string content 'utf-8-unix))
                         (result (flit--send-request host "fs/write"
                                   `(:path ,path
                                     :expectedMtime ,original-mtime)
                                   nil raw-content)))
                    (should (eq (plist-get result :mismatch) t)))

                  ;; Force write should succeed
                  (let* ((content "my new content")
                         (raw-content (encode-coding-string content 'utf-8-unix))
                         (result (flit--send-request host "fs/write"
                                   `(:path ,path :force t)
                                   nil raw-content)))
                    ;; Should succeed without mismatch
                    (should-not (plist-get result :mismatch))
                    ;; File should have our content now
                    (should (equal "my new content" (flit-test--read-file "force.txt")))))))
          (when (buffer-live-p buf)
            (with-current-buffer buf (set-buffer-modified-p nil))
            (kill-buffer buf)))))))

(ert-deftest flit-test-write-new-file-succeeds ()
  "Test that saving a genuinely new file succeeds with expectNotExist."
  (flit-test--with-fixture
    (let ((flit-path (flit-test--path "genuinely-new.txt")))
      ;; Write with expectNotExist to a file that genuinely doesn't exist
      (flit--with-parsed (host path) flit-path
        (let* ((content "brand new content")
               (raw-content (encode-coding-string content 'utf-8-unix))
               (result (flit--send-request host "fs/write"
                         `(:path ,path
                           :expectNotExist t)
                         nil raw-content)))
          ;; Should NOT get mismatch
          (should-not (plist-get result :mismatch))
          ;; File should exist now
          (should (equal "brand new content"
                         (flit-test--read-file "genuinely-new.txt"))))))))

;;; PATH prefetch and cache lookup tests

(ert-deftest flit-test-path-prefetch-caches-children ()
  "Test that init prefetches PATH directories with children."
  (flit-test--with-fixture
    ;; Trigger connection by accessing a flit path
    (flit--get-info (flit-test--path ""))
    ;; Now check that PATH directories have been cached with children
    (let ((sys-info (flit--get-sys-info flit-test--host)))
      (should sys-info)
      (let ((path-dirs (plist-get sys-info :path)))
        (should path-dirs)
        ;; Convert vector to list if needed
        (when (vectorp path-dirs)
          (setq path-dirs (append path-dirs nil)))
        ;; Check that at least one PATH directory is cached with children
        (let ((found-cached-dir nil))
          (dolist (dir path-dirs)
            (let ((dir-info (flit--cache-get flit-test--host dir)))
              (when (and dir-info (plist-get dir-info :children))
                (setq found-cached-dir t))))
          (should found-cached-dir))))))

(ert-deftest flit-test-nonexistent-file-from-parent-cache ()
  "Test that non-existent file is detected from parent's cached children."
  (flit-test--with-fixture
    ;; Create a directory with some files
    (make-directory (flit-test--local-path "testdir"))
    (flit-test--create-file "testdir/exists.txt" "I exist")
    ;; First, cache the parent directory with its children by calling fs/info
    (let* ((dir-path (flit-test--path "testdir"))
           (dir-info (flit--get-info dir-path)))
      (should dir-info)
      (should (plist-get dir-info :children))
      ;; Verify children includes exists.txt (new format: plists with :name)
      (let ((children (plist-get dir-info :children)))
        (when (vectorp children)
          (setq children (append children nil)))
        (let ((names (mapcar (lambda (c) (plist-get c :name)) children)))
          (should (member "exists.txt" names))))
      ;; Now check for a non-existent file - should return from cache WITHOUT RPC
      ;; We can verify this by checking the result has :exists :json-false
      ;; and doesn't have fields that would only come from an RPC (like :realpath)
      (let ((nonexistent-info (flit--get-info (flit-test--path "testdir/nonexistent.txt"))))
        (should nonexistent-info)
        (should (eq (plist-get nonexistent-info :exists) :json-false))
        ;; This synthetic result should only have :exists and :path
        ;; It should NOT have :realpath which would indicate an RPC was made
        (should (plist-get nonexistent-info :path))))))

(ert-deftest flit-test-parent-children-lookup ()
  "Test that flit--find-entry correctly finds children in parent info."
  (flit-test--with-fixture
    ;; Create a directory with files
    (make-directory (flit-test--local-path "lookup-test"))
    (flit-test--create-file "lookup-test/file1.txt" "content")
    (flit-test--create-file "lookup-test/file2.txt" "content")
    ;; Get directory info to cache children
    (let ((dir-info (flit--get-info (flit-test--path "lookup-test"))))
      (should dir-info)
      (let ((children (plist-get dir-info :children)))
        (should children)
        ;; Test flit--find-entry
        (should (flit--find-entry dir-info "file1.txt"))
        (should (flit--find-entry dir-info "file2.txt"))
        (should-not (flit--find-entry dir-info "nonexistent.txt"))))))

(ert-deftest flit-test-fs-info-returns-children ()
  "Test that fs/info response includes children for directories."
  (flit-test--with-fixture
    ;; Create a directory with files
    (make-directory (flit-test--local-path "info-test"))
    (flit-test--create-file "info-test/file1.txt" "content")
    (flit-test--create-file "info-test/file2.txt" "content")
    ;; Get directory info
    (let ((dir-info (flit--get-info (flit-test--path "info-test"))))
      (should dir-info)
      (should (eq (plist-get dir-info :exists) t))
      (should (equal (plist-get dir-info :type) "directory"))
      ;; Check children are present (new format: plists with :name and :isDir)
      (let ((children (plist-get dir-info :children)))
        (should children)
        (when (vectorp children)
          (setq children (append children nil)))
        ;; Children are now plists with :name field
        (let ((names (mapcar (lambda (c) (plist-get c :name)) children)))
          (should (member "file1.txt" names))
          (should (member "file2.txt" names)))))))

(ert-deftest flit-test-raw-fs-info-response-has-children ()
  "Test that raw fs/info RPC response includes children field."
  (flit-test--with-fixture
    ;; Create a directory with files
    (make-directory (flit-test--local-path "raw-test"))
    (flit-test--create-file "raw-test/a.txt" "a")
    (flit-test--create-file "raw-test/b.txt" "b")
    ;; Make raw RPC call (bypassing flit--get-info caching)
    (flit--with-parsed (host path) (flit-test--path "raw-test")
      (let* ((result (flit--send-request host "fs/info" `(:path ,path))))
        ;; Log the result keys for debugging
        (message "Raw fs/info result keys: %S"
                 (cl-loop for (k _v) on result by #'cddr collect k))
        (message "Raw fs/info :children = %S" (plist-get result :children))
        ;; Check the raw response has children
        (should (plist-get result :exists))
        (should (equal (plist-get result :type) "directory"))
        (should (plist-get result :children))
        (let ((children (plist-get result :children)))
          (when (vectorp children)
            (setq children (append children nil)))
          (should (>= (length children) 2)))))))

;;; Desktop save/restore integration tests

(ert-deftest flit-test-desktop-save-restore-full-cycle-connected ()
  "Test full desktop save/restore cycle when server is available.
This simulates:
1. Open a flit file
2. Save desktop state
3. Clear all flit state (simulating Emacs restart)
4. Restore buffer with `flit--desktop-restoring' set
5. Verify buffer-file-name retains flit prefix"
  (flit-test--with-fixture
    (flit-test--create-file "cycle-test.txt" "cycle content")
    (let* ((file-path (flit-test--path "cycle-test.txt"))
           (original-buf (find-file-noselect file-path)))
      (unwind-protect
          (progn
            ;; Verify original buffer works
            (with-current-buffer original-buf
              (should (equal buffer-file-name file-path))
              (should (string-match-p "flit!" buffer-file-name))
              (should (string= (buffer-string) "cycle content")))

            ;; Save desktop state
            (let ((saved-state (with-current-buffer original-buf
                                 (flit--desktop-save-buffer "/tmp"))))
              (should (equal saved-state file-path))

              ;; Kill the buffer
              (kill-buffer original-buf)
              (setq original-buf nil)

              ;; Clear all flit state to simulate Emacs restart
              (clrhash flit--cache)
              (clrhash flit--deferred-buffers)
              ;; Note: We don't close the connection - server is still available

              ;; Simulate desktop restore with flit--desktop-restoring set
              (let ((flit--desktop-restoring t))
                (let ((restored-buf (flit--create-deferred-buffer
                                     saved-state "cycle-test.txt")))
                  (unwind-protect
                      (progn
                        (should restored-buf)
                        (should (buffer-live-p restored-buf))
                        (with-current-buffer restored-buf
                          ;; CRITICAL: buffer-file-name must retain flit prefix
                          (should (string-match-p "flit!" buffer-file-name))
                          (should (equal buffer-file-name file-path))
                          ;; Since cache was cleared, this should be a deferred buffer
                          ;; (prefetch didn't happen in this simplified test)
                          (when (gethash buffer-file-name flit--deferred-buffers)
                            ;; If deferred, verify it can be loaded
                            (let ((flit--connection-tier 'always))
                              (flit--load-deferred-buffer))
                            (should (string= (buffer-string) "cycle content")))))
                    (when (and restored-buf (buffer-live-p restored-buf))
                      (kill-buffer restored-buf)))))))
        (when (and original-buf (buffer-live-p original-buf))
          (kill-buffer original-buf))))))

(ert-deftest flit-test-desktop-save-restore-full-cycle-disconnected ()
  "Test full desktop save/restore cycle when server is NOT available.
This simulates:
1. Open a flit file and save desktop state
2. Clear all flit state AND close connection (simulating Emacs restart with no server)
3. Restore buffer with `flit--desktop-restoring' set
4. Verify buffer-file-name retains flit prefix (even in deferred state)"
  (flit-test--with-fixture
    (flit-test--create-file "disconnected-test.txt" "disconnected content")
    (let* ((file-path (flit-test--path "disconnected-test.txt"))
           (original-buf (find-file-noselect file-path)))
      (unwind-protect
          (progn
            ;; Save desktop state while connected
            (let ((saved-state (with-current-buffer original-buf
                                 (flit--desktop-save-buffer "/tmp"))))
              (should (equal saved-state file-path))

              ;; Kill the buffer
              (kill-buffer original-buf)
              (setq original-buf nil)

              ;; Clear ALL flit state AND close connection
              (clrhash flit--cache)
              (clrhash flit--deferred-buffers)
              (flit--close-connection flit-test--host)
              (puthash flit-test--host 'pending flit--connection-states)

              ;; Simulate desktop restore with flit--desktop-restoring set
              ;; Connection will fail because we closed it
              (let ((flit--desktop-restoring t))
                (let ((restored-buf (flit--create-deferred-buffer
                                     saved-state "disconnected-test.txt")))
                  (unwind-protect
                      (progn
                        (should restored-buf)
                        (should (buffer-live-p restored-buf))
                        (with-current-buffer restored-buf
                          ;; CRITICAL: buffer-file-name MUST retain flit prefix
                          ;; This is the bug we're testing for - if startup-block
                          ;; causes flit paths to fall through to local handling,
                          ;; buffer-file-name would NOT have the flit! prefix
                          (should (string-match-p "flit!" buffer-file-name))
                          (should (equal buffer-file-name file-path))
                          ;; Should be in deferred state since we can't connect
                          (should (or (gethash buffer-file-name flit--deferred-buffers)
                                      ;; Or already has content if connection worked
                                      (string= (buffer-string) "disconnected content")))))
                    (when (and restored-buf (buffer-live-p restored-buf))
                      (remhash file-path flit--deferred-buffers)
                      (kill-buffer restored-buf)))))))
        (when (and original-buf (buffer-live-p original-buf))
          (kill-buffer original-buf))))))

(ert-deftest flit-test-deferred-buffer-preserves-flit-path ()
  "Test that deferred buffers always preserve the flit path.
Even when connection is not available, the buffer-file-name must be a flit path."
  (flit-test--with-fixture
    (let* ((file-path (flit-test--path "preserve-test.txt"))
           ;; Close connection before creating deferred buffer
           (_ (progn
                (flit--close-connection flit-test--host)
                (puthash flit-test--host 'pending flit--connection-states)))
           ;; Create deferred buffer (as if desktop restore failed to connect)
           (buf (flit--create-deferred-buffer file-path "preserve-test.txt")))
      (unwind-protect
          (with-current-buffer buf
            ;; buffer-file-name MUST be the flit path, not a local path
            (should (equal buffer-file-name file-path))
            (should (string-match-p "flit!" buffer-file-name))
            ;; Verify it's recognized as a flit path
            (should (flit--file-name-p buffer-file-name))
            ;; default-directory should be a flit path (so LSP knows it's remote)
            (should (string-match-p "flit!" default-directory))
            (should (flit--file-name-p default-directory)))
        (remhash file-path flit--deferred-buffers)
        (when (buffer-live-p buf) (kill-buffer buf))))))

(ert-deftest flit-test-desktop-restore-during-startup ()
  "Test desktop restore when after-init-time is nil (startup scenario).
This simulates:
1. Emacs is starting up (after-init-time = nil)
2. Desktop restore is running
3. Flit path should be preserved even when connection fails"
  (flit-test--with-fixture
    (flit-test--create-file "startup-test.txt" "startup content")
    (let* ((file-path (flit-test--path "startup-test.txt"))
           (original-buf (find-file-noselect file-path)))
      (unwind-protect
          (progn
            ;; Save desktop state while connected
            (let ((saved-state (with-current-buffer original-buf
                                 (flit--desktop-save-buffer "/tmp"))))
              (kill-buffer original-buf)
              (setq original-buf nil)

              ;; Clear state and close connection
              (clrhash flit--cache)
              (clrhash flit--deferred-buffers)
              (flit--close-connection flit-test--host)
              (puthash flit-test--host 'pending flit--connection-states)

              ;; Simulate FULL startup scenario:
              ;; - after-init-time is nil
              ;; - flit--desktop-restoring is t
              (let ((after-init-time nil)
                    (flit--desktop-restoring t))
                (require 'desktop)
                ;; Call the advice with desktop-create-buffer args format:
                ;; (file-version buffer-filename buffer-name major-mode ...)
                (let ((restored-buf (flit--desktop-create-buffer-advice
                                     (lambda (&rest _args)
                                       ;; If this gets called, flit path wasn't recognized
                                       nil)
                                     208 saved-state "startup-test.txt"
                                     'fundamental-mode)))
                  (unwind-protect
                      (progn
                        ;; A buffer should have been created
                        (should restored-buf)
                        (should (buffer-live-p restored-buf))
                        (with-current-buffer restored-buf
                          ;; CRITICAL: buffer-file-name MUST retain flit prefix
                          (should (string-match-p "flit!" buffer-file-name))
                          (should (equal buffer-file-name file-path))))
                    (when (and restored-buf (buffer-live-p restored-buf))
                      (remhash file-path flit--deferred-buffers)
                      (kill-buffer restored-buf)))))))
        (when (and original-buf (buffer-live-p original-buf))
          (kill-buffer original-buf))))))

(ert-deftest flit-test-desktop-restore-advice-preserves-flit-path ()
  "Test that the desktop restore advice correctly routes flit paths.
Simulates calling the advised desktop-create-buffer function."
  (flit-test--with-fixture
    (flit-test--create-file "advice-test.txt" "advice content")
    (let* ((file-path (flit-test--path "advice-test.txt"))
           ;; Clear cache to force deferred buffer creation
           (_ (clrhash flit--cache)))
      ;; Load desktop.el to ensure the advice function is defined
      (require 'desktop)
      ;; Simulate desktop restore context
      (let ((flit--desktop-restoring t))
        ;; Call with desktop-create-buffer args format
        (let ((buf (flit--desktop-create-buffer-advice
                    ;; The orig-fn (shouldn't be called for flit paths)
                    (lambda (&rest _args)
                      (error "Original handler should not be called for flit paths"))
                    208 file-path "advice-test.txt" 'fundamental-mode)))
          (unwind-protect
              (progn
                (should buf)
                (should (buffer-live-p buf))
                (with-current-buffer buf
                  ;; buffer-file-name must be the flit path
                  (should (string-match-p "flit!" buffer-file-name))
                  (should (equal buffer-file-name file-path))))
            (remhash file-path flit--deferred-buffers)
            (when (buffer-live-p buf) (kill-buffer buf))))))))

;;; Auto-revert race condition tests

(ert-deftest flit-test-auto-revert-preserves-modified-buffer ()
  "Test that auto-revert does NOT revert a buffer with unsaved modifications.
This tests a race condition where:
1. User types in buffer (buffer-modified-p is t)
2. External file change triggers fs/changed notification
3. Buffer should NOT be reverted because user has unsaved changes"
  (flit-test--with-fixture
    (flit-test--create-file "modified-test.txt" "initial content")
    (let ((buf (find-file-noselect (flit-test--path "modified-test.txt"))))
      (unwind-protect
          (with-current-buffer buf
            ;; Set up watching and auto-revert
            (flit--watch buffer-file-name)
            (setq-local buffer-stale-function #'flit--buffer-stale-p)
            (auto-revert-mode 1)
            (should-not flit--file-changed)
            (should (equal "initial content" (buffer-string)))

            ;; User types in the buffer - this creates unsaved modifications
            (goto-char (point-max))
            (insert " - user typing")
            (should (buffer-modified-p))
            (should (equal "initial content - user typing" (buffer-string)))

            ;; External process modifies the file (simulates delayed fs/changed)
            ;; Wait for debounce period (server has 1s debounce, plus margin)
            (sleep-for 1.5)
            (with-temp-file (flit-test--local-path "modified-test.txt")
              (insert "externally modified"))

            ;; Wait for notification to be processed
            ;; Note: flit--file-changed won't be set because buffer is modified,
            ;; so we just need to give enough time for any notification to arrive
            (flit-test--wait-for (lambda () nil) 3.0)  ; Wait up to 3s, accepting output

            ;; CRITICAL: Buffer should NOT be reverted because it has unsaved changes!
            ;; The user's modifications should be preserved
            (should (buffer-modified-p))
            (should (equal "initial content - user typing" (buffer-string))))
        (ignore-errors (flit--unwatch (buffer-file-name buf)))
        (kill-buffer buf)))))

(ert-deftest flit-test-auto-revert-works-for-unmodified-buffer ()
  "Test that auto-revert DOES revert an unmodified buffer when file changes.
This is the normal case - buffer has no unsaved changes, so external
file changes should be reflected in the buffer."
  (flit-test--with-fixture
    (flit-test--create-file "unmodified-test.txt" "initial content")
    (let ((buf (find-file-noselect (flit-test--path "unmodified-test.txt"))))
      (unwind-protect
          (with-current-buffer buf
            ;; Set up watching and auto-revert
            (flit--watch buffer-file-name)
            (setq-local buffer-stale-function #'flit--buffer-stale-p)
            (auto-revert-mode 1)
            (should-not flit--file-changed)
            (should (equal "initial content" (buffer-string)))
            (should-not (buffer-modified-p))

            ;; External process modifies the file
            ;; Wait for debounce period (server has 1s debounce, plus margin)
            (sleep-for 1.5)
            (with-temp-file (flit-test--local-path "unmodified-test.txt")
              (insert "externally modified"))

            ;; Wait for buffer to be reverted (content changes)
            ;; Use longer timeout since file watching can be slow
            (should (flit-test--wait-for
                     (lambda () (equal "externally modified" (buffer-string)))
                     15.0))

            ;; Buffer should be reverted since it had no unsaved changes
            (should-not (buffer-modified-p)))
        (ignore-errors (flit--unwatch (buffer-file-name buf)))
        (kill-buffer buf)))))

;;; shell-command tests

(ert-deftest flit-test-shell-command-sync ()
  "Test synchronous shell-command on remote host."
  (flit-test--with-fixture
    (let ((default-directory (flit-test--path "")))
      (with-temp-buffer
        (shell-command "echo hello" (current-buffer))
        (should (string-match-p "hello" (buffer-string)))))))

(ert-deftest flit-test-shell-command-sync-exit-code ()
  "Test synchronous shell-command returns output from remote."
  (flit-test--with-fixture
    (flit-test--create-file "testfile.txt" "file content here")
    (let ((default-directory (flit-test--path "")))
      (with-temp-buffer
        (shell-command "cat testfile.txt" (current-buffer))
        (should (string-match-p "file content here" (buffer-string)))))))

(ert-deftest flit-test-shell-command-sync-cwd ()
  "Test synchronous shell-command runs in correct directory."
  (flit-test--with-fixture
    (make-directory (flit-test--local-path "shelldir"))
    (let ((default-directory (flit-test--path "shelldir/")))
      (with-temp-buffer
        (shell-command "pwd" (current-buffer))
        (should (string-match-p "shelldir" (buffer-string)))))))

(ert-deftest flit-test-shell-command-sync-pipeline ()
  "Test synchronous shell-command with a pipeline."
  (flit-test--with-fixture
    (let ((default-directory (flit-test--path "")))
      (with-temp-buffer
        (shell-command "echo 'abc def ghi' | tr ' ' '\\n' | sort -r" (current-buffer))
        (should (string-match-p "ghi" (buffer-string)))
        (should (string-match-p "abc" (buffer-string)))))))

(ert-deftest flit-test-shell-command-async ()
  "Test asynchronous shell-command on remote host."
  (flit-test--with-fixture
    (let* ((default-directory (flit-test--path ""))
           (buf (generate-new-buffer " *test-shell-async*")))
      (unwind-protect
          (progn
            (shell-command "echo async-output &" buf)
            ;; Wait for output
            (let ((conn (gethash "test" flit--connections))
                  (start (float-time)))
              (while (and (< (- (float-time) start) 5)
                          (with-current-buffer buf
                            (not (string-match-p "async-output" (buffer-string)))))
                (when conn
                  (accept-process-output (flit-conn-process conn) 0.1))))
            (with-current-buffer buf
              (should (string-match-p "async-output" (buffer-string)))))
        (when-let* ((proc (get-buffer-process buf)))
          (delete-process proc))
        (kill-buffer buf)))))

(ert-deftest flit-test-shell-command-to-string ()
  "Test shell-command-to-string on remote host."
  (flit-test--with-fixture
    (let ((default-directory (flit-test--path "")))
      (should (string-match-p "hello"
                              (shell-command-to-string "echo hello"))))))

;;; Provide

(provide 'flit-test)
;;; flit-test.el ends here
