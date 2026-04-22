;;; flitrpc-test.el --- Tests for flitrpc -*- lexical-binding: t; -*-

(require 'ert)
(require 'flitrpc)

;;; Msgpack roundtrip tests

(defun flitrpc-test--roundtrip (value)
  "Encode VALUE to msgpack and decode it back."
  (let* ((encoded (flitrpc--write-msgpack value))
         (buf (generate-new-buffer " *test*")))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (set-buffer-multibyte nil)
            (insert encoded))
          (car (flitrpc--read-msgpack buf 1)))
      (kill-buffer buf))))

(ert-deftest flitrpc-test-msgpack-nil ()
  (should (eq nil (flitrpc-test--roundtrip nil))))

(ert-deftest flitrpc-test-msgpack-true ()
  (should (eq t (flitrpc-test--roundtrip t))))

(ert-deftest flitrpc-test-msgpack-false ()
  (should (eq :json-false (flitrpc-test--roundtrip :json-false))))

(ert-deftest flitrpc-test-msgpack-positive-fixint ()
  (should (= 0 (flitrpc-test--roundtrip 0)))
  (should (= 1 (flitrpc-test--roundtrip 1)))
  (should (= 127 (flitrpc-test--roundtrip 127))))

(ert-deftest flitrpc-test-msgpack-negative-fixint ()
  (should (= -1 (flitrpc-test--roundtrip -1)))
  (should (= -32 (flitrpc-test--roundtrip -32))))

(ert-deftest flitrpc-test-msgpack-uint8 ()
  (should (= 128 (flitrpc-test--roundtrip 128)))
  (should (= 255 (flitrpc-test--roundtrip 255))))

(ert-deftest flitrpc-test-msgpack-uint16 ()
  (should (= 256 (flitrpc-test--roundtrip 256)))
  (should (= 65535 (flitrpc-test--roundtrip 65535))))

(ert-deftest flitrpc-test-msgpack-uint32 ()
  (should (= 65536 (flitrpc-test--roundtrip 65536)))
  (should (= #xffffffff (flitrpc-test--roundtrip #xffffffff))))

(ert-deftest flitrpc-test-msgpack-int8 ()
  (should (= -33 (flitrpc-test--roundtrip -33)))
  (should (= -128 (flitrpc-test--roundtrip -128))))

(ert-deftest flitrpc-test-msgpack-int16 ()
  (should (= -129 (flitrpc-test--roundtrip -129)))
  (should (= -32768 (flitrpc-test--roundtrip -32768))))

(ert-deftest flitrpc-test-msgpack-int32 ()
  (should (= -32769 (flitrpc-test--roundtrip -32769))))

(ert-deftest flitrpc-test-msgpack-fixstr ()
  (should (equal "" (flitrpc-test--roundtrip "")))
  (should (equal "hello" (flitrpc-test--roundtrip "hello")))
  (should (equal (make-string 31 ?x) (flitrpc-test--roundtrip (make-string 31 ?x)))))

(ert-deftest flitrpc-test-msgpack-str8 ()
  (should (equal (make-string 32 ?x) (flitrpc-test--roundtrip (make-string 32 ?x))))
  (should (equal (make-string 255 ?x) (flitrpc-test--roundtrip (make-string 255 ?x)))))

(ert-deftest flitrpc-test-msgpack-str16 ()
  (should (equal (make-string 256 ?x) (flitrpc-test--roundtrip (make-string 256 ?x)))))

(ert-deftest flitrpc-test-msgpack-utf8 ()
  (should (equal "héllo" (flitrpc-test--roundtrip "héllo")))
  (should (equal "日本語" (flitrpc-test--roundtrip "日本語"))))

(ert-deftest flitrpc-test-msgpack-fixarray ()
  (should (equal [] (flitrpc-test--roundtrip [])))
  (should (equal [1 2 3] (flitrpc-test--roundtrip [1 2 3])))
  (should (equal ["a" "b"] (flitrpc-test--roundtrip ["a" "b"]))))

(ert-deftest flitrpc-test-msgpack-fixmap ()
  (let ((result (flitrpc-test--roundtrip '(:name "test" :count 42))))
    (should (equal "test" (plist-get result :name)))
    (should (= 42 (plist-get result :count)))))

(ert-deftest flitrpc-test-msgpack-nested ()
  (let* ((input '(:files [(:name "a.txt" :size 100)
                           (:name "b.txt" :size 200)]
                  :total 2))
         (result (flitrpc-test--roundtrip input)))
    (should (= 2 (plist-get result :total)))
    (let ((files (plist-get result :files)))
      (should (= 2 (length files)))
      (should (equal "a.txt" (plist-get (aref files 0) :name)))
      (should (= 200 (plist-get (aref files 1) :size))))))

(ert-deftest flitrpc-test-msgpack-float64 ()
  (should (= 3.14 (flitrpc-test--roundtrip 3.14)))
  (should (= -1.5 (flitrpc-test--roundtrip -1.5)))
  (should (= 0.0 (flitrpc-test--roundtrip 0.0))))

;;; Frame tests

(defun flitrpc-test--make-frame (msg-type id meta &optional payload)
  "Build a raw frame as a unibyte string."
  (let* ((full-meta (plist-put (plist-put (copy-sequence meta) :t msg-type) :id id))
         (meta-bytes (flitrpc--write-msgpack full-meta))
         (meta-len (length meta-bytes))
         (payload-len (if payload (length payload) 0)))
    (concat (unibyte-string
             #x46 #x4C #x54 #x52
             (logand (ash meta-len -24) #xff)
             (logand (ash meta-len -16) #xff)
             (logand (ash meta-len -8) #xff)
             (logand meta-len #xff)
             (logand (ash payload-len -24) #xff)
             (logand (ash payload-len -16) #xff)
             (logand (ash payload-len -8) #xff)
             (logand payload-len #xff))
            meta-bytes
            (or payload ""))))

(ert-deftest flitrpc-test-frame-parse-response ()
  "Parse a response frame."
  (let* ((dispatched nil)
         (frame (flitrpc-test--make-frame
                 flitrpc--type-response 42
                 '(:result (:exists t :path "/foo"))))
         (buf (generate-new-buffer " *test*"))
         (conn (make-flitrpc-conn
                :buffer buf
                :pending (let ((ht (make-hash-table :test 'eql)))
                           (puthash 42 (cons (lambda (meta _ps _pe)
                                               (setq dispatched meta))
                                             #'ignore)
                                    ht)
                           ht)
                :next-id 0
                :chunks (make-hash-table :test 'eql))))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (set-buffer-multibyte nil)
            (insert frame))
          (flitrpc--parse-frames conn)
          (should dispatched)
          (let ((result (plist-get dispatched :result)))
            (should (equal t (plist-get result :exists)))
            (should (equal "/foo" (plist-get result :path)))))
      (kill-buffer buf))))

(ert-deftest flitrpc-test-frame-parse-notification ()
  "Parse a notification frame."
  (let* ((got-method nil)
         (got-params nil)
         (frame (flitrpc-test--make-frame
                 flitrpc--type-notification 0
                 '(:method "fs/changed" :params (:path "/bar" :type "modified"))))
         (buf (generate-new-buffer " *test*"))
         (conn (make-flitrpc-conn
                :buffer buf
                :pending (make-hash-table :test 'eql)
                :next-id 0
                :notification-fn (lambda (_conn method params _ps _pe)
                                   (setq got-method method got-params params))
                :chunks (make-hash-table :test 'eql))))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (set-buffer-multibyte nil)
            (insert frame))
          (flitrpc--parse-frames conn)
          (should (equal "fs/changed" got-method))
          (should (equal "/bar" (plist-get got-params :path))))
      (kill-buffer buf))))

(ert-deftest flitrpc-test-frame-with-payload ()
  "Parse a response frame with binary payload."
  (let* ((got-payload nil)
         (payload-data (unibyte-string 0 1 2 3 255 254 253))
         (frame (flitrpc-test--make-frame
                 flitrpc--type-response 7
                 '(:result (:path "/test"))
                 payload-data))
         (buf (generate-new-buffer " *test*"))
         (conn (make-flitrpc-conn
                :buffer buf
                :pending (let ((ht (make-hash-table :test 'eql)))
                           (puthash 7 (cons (lambda (_meta ps pe)
                                              (setq got-payload
                                                    (with-current-buffer buf
                                                      (buffer-substring-no-properties ps pe))))
                                            #'ignore)
                                    ht)
                           ht)
                :next-id 0
                :chunks (make-hash-table :test 'eql))))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (set-buffer-multibyte nil)
            (insert frame))
          (flitrpc--parse-frames conn)
          (should got-payload)
          (should (equal payload-data got-payload)))
      (kill-buffer buf))))

(ert-deftest flitrpc-test-frame-partial ()
  "Partial frames should wait for more data."
  (let* ((dispatched nil)
         (frame (flitrpc-test--make-frame
                 flitrpc--type-response 1
                 '(:result (:ok t))))
         (buf (generate-new-buffer " *test*"))
         (conn (make-flitrpc-conn
                :buffer buf
                :pending (let ((ht (make-hash-table :test 'eql)))
                           (puthash 1 (cons (lambda (_meta _ps _pe)
                                              (setq dispatched t))
                                            #'ignore)
                                    ht)
                           ht)
                :next-id 0
                :chunks (make-hash-table :test 'eql))))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (set-buffer-multibyte nil)
            ;; Insert only first half of the frame
            (insert (substring frame 0 (/ (length frame) 2))))
          (flitrpc--parse-frames conn)
          (should-not dispatched)
          ;; Insert the rest
          (with-current-buffer buf
            (goto-char (point-max))
            (insert (substring frame (/ (length frame) 2))))
          (flitrpc--parse-frames conn)
          (should dispatched))
      (kill-buffer buf))))

(ert-deftest flitrpc-test-frame-multiple ()
  "Parse multiple frames from a single buffer."
  (let* ((results nil)
         (frame1 (flitrpc-test--make-frame
                  flitrpc--type-response 1 '(:result (:v 1))))
         (frame2 (flitrpc-test--make-frame
                  flitrpc--type-response 2 '(:result (:v 2))))
         (buf (generate-new-buffer " *test*"))
         (ht (make-hash-table :test 'eql))
         (conn (make-flitrpc-conn
                :buffer buf
                :pending ht
                :next-id 0
                :chunks (make-hash-table :test 'eql))))
    (puthash 1 (cons (lambda (meta _ps _pe)
                       (push (plist-get (plist-get meta :result) :v) results))
                     #'ignore) ht)
    (puthash 2 (cons (lambda (meta _ps _pe)
                       (push (plist-get (plist-get meta :result) :v) results))
                     #'ignore) ht)
    (unwind-protect
        (progn
          (with-current-buffer buf
            (set-buffer-multibyte nil)
            (insert frame1 frame2))
          (flitrpc--parse-frames conn)
          (should (= 2 (length results)))
          (should (memq 1 results))
          (should (memq 2 results)))
      (kill-buffer buf))))

(provide 'flitrpc-test)

;;; flitrpc-test.el ends here
