;;; launcher-icons-tests.el --- App icon cache checks -*- lexical-binding: t; -*-

;; Deterministic: a temporary cache, a fake clock, and a pipe process in
;; place of the worker, whose output each check writes.  Needs neither
;; macOS nor a display.  test/launcher-icons-worker-tests.el checks the
;; real worker on macOS.

(require 'ert)
(require 'cl-lib)
(require 'launcher)
(require 'launcher-icons)

(defvar launcher-icons-test--clock 1000.0 "The fake time, in seconds.")
(defvar launcher-icons-test--spawned nil "Arguments of the fake worker processes.")
(defvar launcher-icons-test--flushed nil "Images flushed from Emacs's cache.")
(defvar launcher-icons-test--fingerprints nil
  "Alist of app paths and the fingerprints the fake worker reports.")

(defconst launcher-icons-test--png "\211PNG\r\n\032\nfake image"
  "Contents of the fake worker's PNGs.")

(defun launcher-icons-test--make-process (&rest args)
  "Record ARGS of `make-process' and return a pipe process for them.
Add the manifest the worker would read, parsed, as :manifest."
  (let ((process (make-pipe-process :name "launcher-icons-test" :noquery t))
        (manifest (with-temp-buffer
                    (insert-file-contents (car (last (plist-get args :command))))
                    (json-parse-buffer :object-type 'alist :array-type 'list))))
    (push (cons process (append args (list :manifest manifest)))
          launcher-icons-test--spawned)
    process))

(defun launcher-icons-test--requests (&optional spawned)
  "Return the requests of SPAWNED, the latest fake worker by default."
  (alist-get 'requests (plist-get (cdr (or spawned (car launcher-icons-test--spawned)))
                                  :manifest)))

(defun launcher-icons-test--create-image (file &optional _type _data-p &rest props)
  "Return a fake image specification of FILE with PROPS."
  (append (list 'image :file file) props))

(defmacro launcher-icons-test--with-cache (&rest body)
  "Run BODY with an empty temporary icon cache, icons enabled, a fake
clock at 1000, a 2x display, and the fake worker."
  (declare (indent 0))
  `(let* ((root (file-name-as-directory (make-temp-file "launcher-icons-test" t)))
          (launcher-icon-cache-directory root)
          (launcher-show-icons t)
          (launcher-icon-size 20)
          (launcher-icon-check-interval 300)
          (launcher-icons--records (make-hash-table :test #'equal))
          (launcher-icons--epoch 0)
          (launcher-icons--urgent nil)
          (launcher-icons--queue nil)
          (launcher-icons--job nil)
          (launcher-icons--kick-timer nil)
          (launcher-icons--swept t)
          (launcher-icons--available t)
          (launcher-icon-updated-hook nil)
          (launcher-icons-test--clock 1000.0)
          (launcher-icons-test--spawned nil)
          (launcher-icons-test--flushed nil)
          (launcher-icons-test--fingerprints nil))
     (cl-letf (((symbol-function 'launcher-icons--now)
                (lambda () launcher-icons-test--clock))
               ((symbol-function 'launcher-icons--display-p) (lambda (_frame) t))
               ((symbol-function 'launcher-icons--scale) (lambda (_frame) 2))
               ((symbol-function 'make-process) #'launcher-icons-test--make-process)
               ((symbol-function 'create-image) #'launcher-icons-test--create-image)
               ((symbol-function 'image-flush)
                (lambda (image &rest _) (push image launcher-icons-test--flushed))))
       (unwind-protect
           (progn ,@body)
         (when launcher-icons--job
           (launcher-icons--finish launcher-icons--job))
         (when launcher-icons--kick-timer
           (cancel-timer launcher-icons--kick-timer))
         (dolist (spawned launcher-icons-test--spawned)
           (when (process-live-p (car spawned))
             (delete-process (car spawned))))
         (delete-directory root t)))))

(defun launcher-icons-test--fingerprint (path)
  "Return the fingerprint the fake worker reports for PATH."
  (or (cdr (assoc path launcher-icons-test--fingerprints))
      '(("launcher-icons-fingerprint" "1") ("bundle" "1" "2" "3"))))

(defun launcher-icons-test--ok (request &optional written)
  "Return a successful result of REQUEST, writing PNGs of WRITTEN sizes.
WRITTEN defaults to all sizes requested; `none' writes none."
  (let ((sizes (if (eq written 'none) nil (or written (alist-get 'sizes request)))))
    (dolist (size sizes)
      (let ((coding-system-for-write 'no-conversion))
        (write-region launcher-icons-test--png nil
                      (alist-get (intern (number-to-string size)) (alist-get 'outputs request))
                      nil 'silent)))
    `((id . ,(alist-get 'id request))
      (fingerprint . ,(vconcat (mapcar #'vconcat (launcher-icons-test--fingerprint
                                                  (alist-get 'app request)))))
      (written . ,(vconcat sizes)))))

(defun launcher-icons-test--line (result)
  "Return RESULT, an alist or a string, as a line of worker output."
  (concat (if (stringp result) result (json-serialize result)) "\n"))

(defun launcher-icons-test--job (&optional respond finish)
  "Start the next worker job, answer it with RESPOND, and end it.
RESPOND is called with the manifest's requests, as alists, and returns
results: alists or raw lines.  It defaults to answering all requests
successfully.  Unless FINISH is `keep', the job ends after the output.
Return the requests."
  (when launcher-icons--kick-timer
    (cancel-timer launcher-icons--kick-timer)
    (setq launcher-icons--kick-timer nil))
  (launcher-icons--start)
  (let* ((job launcher-icons--job)
         (args (cdr (car launcher-icons-test--spawned)))
         (command (plist-get args :command))
         (manifest (plist-get args :manifest))
         (requests (alist-get 'requests manifest))
         (results (funcall (or respond (lambda (requests)
                                         (mapcar #'launcher-icons-test--ok requests)))
                           requests)))
    (should job)
    (should (equal (seq-take command 4)
                   (list launcher-icons--osascript "-l" "JavaScript" launcher-icons--script)))
    (should (equal (alist-get 'version manifest) 1))
    (funcall (plist-get args :filter) (launcher-icons--job-process job)
             (mapconcat #'launcher-icons-test--line results ""))
    (unless (eq finish 'keep)
      (launcher-icons--finish job))
    requests))

(defun launcher-icons-test--record (path)
  "Return the record of PATH, without loading one."
  (gethash path launcher-icons--records))

(defun launcher-icons-test--queued ()
  "Return the work queued, as (PATH . WORK) lists, urgent first."
  (mapcar (lambda (record)
            (cons (launcher-icons--record-path record)
                  (sort (mapcar (lambda (what) (if (eq what 'check) 0 what))
                                (launcher-icons--record-queued record))
                        #'<)))
          (append launcher-icons--urgent launcher-icons--queue)))

(defun launcher-icons-test--files ()
  "Return the files of the cache, relative to its root, sorted."
  (sort (mapcar (lambda (file) (file-relative-name file launcher-icon-cache-directory))
                (directory-files-recursively launcher-icon-cache-directory ""))
        #'string<))

(defconst launcher-icons-test--elpa
  (expand-file-name "../.cache/elpa" (file-name-directory (or load-file-name buffer-file-name)))
  "Where `sh test/elpa.sh' puts the pinned packages.")

(dolist (package '("marginalia-2.13" "nerd-icons.el-17faac7977242b470732efd417d3bcc8eb5a830e"
                   "nerd-icons-completion-f924dd490c8c4c1066fd97a76e0dc31e303fca30"))
  (let ((directory (expand-file-name package launcher-icons-test--elpa)))
    (when (file-directory-p directory)
      (add-to-list 'load-path directory))))

(defconst launcher-icons-test--calc "/Applications/Calculator.app")
(defconst launcher-icons-test--notes "/Applications/Notes.app")

;;; Sizes

(ert-deftest launcher-icons-bucket-edges ()
  (dolist (case '((20 2 64) (32 2 64) (33 2 256) (128 2 256) (129 2 512)
                  (256 2 512) (400 2 512) (20 1 64) (64 1 64) (65 1 256)
                  (128 1 256) (256 1 256) (257 1 512) (20 1.5 64) (60 1.5 256)))
    (should (equal (launcher-icons--bucket (nth 0 case) (nth 1 case)) (nth 2 case)))))

(ert-deftest launcher-icons-scale-defaults-to-retina ()
  (cl-letf (((symbol-function 'frame-scale-factor) (lambda (&rest _) nil)))
    (should (= (launcher-icons--scale nil) 2)))
  (cl-letf (((symbol-function 'frame-scale-factor) (lambda (&rest _) 1.0)))
    (should (= (launcher-icons--scale nil) 1.0))))

;;; Fingerprints

(ert-deftest launcher-icons-fingerprint-hash-is-stable-and-ordered ()
  (let ((a '(("launcher-icons-fingerprint" "1") ("bundle" "1" "2" "3")
             ("plist" "4" "5" "225" "12.0") ("icon" "AppIcon.icns" "6" "7")
             ("assets" "missing")))
        (b '(("launcher-icons-fingerprint" "1") ("bundle" "1" "2" "3")
             ("plist" "4" "5" "225" "12.0") ("icon" "AppIcon.icns" "6" "7")
             ("assets" "8" "9"))))
    (should (equal (launcher-icons--fingerprint-hash a)
                   (launcher-icons--fingerprint-hash (copy-tree a))))
    (should (string-match-p "\\`[0-9a-f]\\{64\\}\\'" (launcher-icons--fingerprint-hash a)))
    (should-not (equal (launcher-icons--fingerprint-hash a) (launcher-icons--fingerprint-hash b)))
    (should-not (equal (launcher-icons--fingerprint-hash a)
                       (launcher-icons--fingerprint-hash (reverse a))))
    ;; No part can run into the next.
    (should-not (equal (launcher-icons--fingerprint-hash '(("a" "bc")))
                       (launcher-icons--fingerprint-hash '(("ab" "c")))))
    (should (launcher-icons--fingerprint-hash '(("名前" "é"))))))

(ert-deftest launcher-icons-fingerprint-validation ()
  (should (launcher-icons--valid-fingerprint '(("a" "b"))))
  (dolist (bad '(nil "abc" (("a" 1)) ("a") (("a" . "b")) ((("a")))))
    (should-not (launcher-icons--valid-fingerprint bad))))

;;; Warming, previews and the disk cache

(ert-deftest launcher-icons-warm-up-makes-only-list-icons ()
  (launcher-icons-test--with-cache
    (launcher-icons-prepare (list launcher-icons-test--calc launcher-icons-test--notes))
    (should (equal (launcher-icons-test--queued)
                   `((,launcher-icons-test--calc 0 64) (,launcher-icons-test--notes 0 64))))
    (let ((requests (launcher-icons-test--job)))
      (should (equal (mapcar (lambda (request) (alist-get 'sizes request)) requests)
                     '((64) (64)))))
    (should (equal (launcher-icons-test--files)
                   (let* ((calc (launcher-icons-test--record launcher-icons-test--calc))
                          (notes (launcher-icons-test--record launcher-icons-test--notes)))
                     (sort (list (format "v1/%s/%s/64.png" (launcher-icons--record-key calc)
                                         (launcher-icons--record-generation calc))
                                 (format "v1/%s/metadata.json" (launcher-icons--record-key calc))
                                 (format "v1/%s/%s/64.png" (launcher-icons--record-key notes)
                                         (launcher-icons--record-generation notes))
                                 (format "v1/%s/metadata.json" (launcher-icons--record-key notes)))
                           #'string<))))
    ;; The job is gone, and nothing waits or polls.
    (should-not launcher-icons--job)
    (should-not launcher-icons--kick-timer)
    (should-not (launcher-icons-test--queued))
    (let ((image (launcher--icon launcher-icons-test--calc 20)))
      (should (string-suffix-p "/64.png" (plist-get (cdr image) :file)))
      (should (equal (plist-get (cdr image) :width) 20))
      (should (equal (plist-get (cdr image) :scale) 1))
      ;; The same specification each time, for Emacs's image cache.
      (should (eq image (launcher--icon launcher-icons-test--calc 20))))))

(ert-deftest launcher-icons-preview-is-lazy-urgent-and-provisional ()
  (launcher-icons-test--with-cache
    (launcher-icons-prepare (list launcher-icons-test--calc launcher-icons-test--notes))
    (launcher-icons-test--job)
    ;; Notes needs a check, as warming; Calculator's preview comes first.
    (setq launcher-icons-test--clock 2000.0)
    (launcher-icons-prepare (list launcher-icons-test--notes))
    (let ((provisional (launcher--icon launcher-icons-test--calc 128)))
      (should (string-suffix-p "/64.png" (plist-get (cdr provisional) :file)))
      (should (equal (plist-get (cdr provisional) :width) 128)))
    (should (equal (launcher-icons-test--queued)
                   `((,launcher-icons-test--calc 0 256) (,launcher-icons-test--notes 0))))
    (let ((requests (launcher-icons-test--job)))
      (should (equal (mapcar (lambda (request) (alist-get 'sizes request)) requests)
                     '((256) ()))))
    (should (string-suffix-p "/256.png" (plist-get (cdr (launcher--icon launcher-icons-test--calc 128))
                                                   :file)))
    (should (equal (launcher-icons--record-rasters
                    (launcher-icons-test--record launcher-icons-test--calc))
                   '(64 256)))
    ;; A larger preview: 512, rendered from the source, not enlarged.
    (launcher--icon launcher-icons-test--calc 256)
    (should (equal (launcher-icons-test--queued) `((,launcher-icons-test--calc 512))))))

(ert-deftest launcher-icons-disk-hit-survives-a-new-process ()
  (launcher-icons-test--with-cache
    (launcher-icons-prepare (list launcher-icons-test--calc))
    (launcher-icons-test--job)
    (launcher--icon launcher-icons-test--calc 128)
    (launcher-icons-test--job)
    (let ((generation (launcher-icons--record-generation
                       (launcher-icons-test--record launcher-icons-test--calc))))
      ;; A new process: nothing in memory.
      (clrhash launcher-icons--records)
      (setq launcher-icons-test--spawned nil)
      (let ((image (launcher--icon launcher-icons-test--calc 20)))
        (should (equal (plist-get (cdr image) :file)
                       (expand-file-name
                        (format "v1/%s/%s/64.png"
                                (launcher-icons--record-key
                                 (launcher-icons-test--record launcher-icons-test--calc))
                                generation)
                        launcher-icon-cache-directory))))
      (should (launcher--icon launcher-icons-test--calc 128))
      ;; Only the check the new process owes; no image is made again.
      (should (equal (launcher-icons-test--queued) `((,launcher-icons-test--calc 0))))
      (launcher-icons-test--job)
      (should (equal (launcher-icons--record-generation
                      (launcher-icons-test--record launcher-icons-test--calc))
                     generation))
      ;; Fresh: repeated lookups start no worker.
      (setq launcher-icons-test--spawned nil)
      (dotimes (_ 3)
        (launcher--icon launcher-icons-test--calc 20)
        (launcher--icon launcher-icons-test--calc 128)
        (launcher-icons-prepare (list launcher-icons-test--calc)))
      (should-not (launcher-icons-test--queued))
      (should-not launcher-icons--kick-timer)
      (launcher-icons--start)
      (should-not launcher-icons-test--spawned))))

(ert-deftest launcher-icons-invalid-metadata-is-a-miss ()
  (launcher-icons-test--with-cache
    (launcher-icons-prepare (list launcher-icons-test--calc))
    (launcher-icons-test--job)
    (let* ((record (launcher-icons-test--record launcher-icons-test--calc))
           (file (expand-file-name "metadata.json" (launcher-icons--app-directory record)))
           (valid (with-temp-buffer (insert-file-contents file) (buffer-string))))
      (dolist (contents (list "{not json" "[]" "(eval (kill-emacs))" ""
                              (replace-regexp-in-string "Calculator" "Notes" valid)
                              (replace-regexp-in-string "\"v1\"" "\"v2\"" valid)
                              (replace-regexp-in-string "\\[64\\]" "[64,1000]" valid)
                              (replace-regexp-in-string "\"generation\":\"[^\"]*\""
                                                        "\"generation\":\"../../x\"" valid)))
        (with-temp-file file (insert contents))
        (clrhash launcher-icons--records)
        (should-not (launcher--icon launcher-icons-test--calc 20))
        (should (equal (launcher-icons-test--queued) `((,launcher-icons-test--calc 0 64))))
        (setq launcher-icons--queue nil)))))

(ert-deftest launcher-icons-missing-png-makes-a-new-generation ()
  (launcher-icons-test--with-cache
    (launcher-icons-prepare (list launcher-icons-test--calc))
    (launcher-icons-test--job)
    (let* ((record (launcher-icons-test--record launcher-icons-test--calc))
           (old (launcher-icons--record-generation record)))
      (delete-file (launcher-icons--png record 64))
      (should-not (launcher--icon launcher-icons-test--calc 20))
      (should-not (equal (launcher-icons--record-generation record) old))
      (should-not (launcher-icons--record-rasters record))
      (should (equal (launcher-icons-test--queued) `((,launcher-icons-test--calc 64))))
      (launcher-icons-test--job)
      (should (launcher--icon launcher-icons-test--calc 20)))))

;;; Invalidation

(ert-deftest launcher-icons-fingerprint-change-invalidates-all-sizes ()
  (launcher-icons-test--with-cache
    (let (notified)
      (launcher-icons-prepare (list launcher-icons-test--calc))
      (launcher-icons-test--job)
      (launcher--icon launcher-icons-test--calc 128)
      (launcher-icons-test--job)
      (let* ((record (launcher-icons-test--record launcher-icons-test--calc))
             (old (launcher-icons--record-generation record))
             (images (list (launcher--icon launcher-icons-test--calc 20)
                           (launcher--icon launcher-icons-test--calc 128))))
        (launcher-icons--fail record 512)
        (add-hook 'launcher-icon-updated-hook (lambda (path) (push path notified)))
        (setq launcher-icons-test--clock 2000.0)
        (push (cons launcher-icons-test--calc
                    '(("launcher-icons-fingerprint" "1") ("bundle" "1" "2" "3")
                      ("plist" "x" "y" "226" "12.1")))
              launcher-icons-test--fingerprints)
        (launcher-icons-prepare (list launcher-icons-test--calc))
        (launcher-icons-test--job)
        (should (equal notified (list launcher-icons-test--calc)))
        (should-not (equal (launcher-icons--record-generation record) old))
        (should-not (launcher-icons--record-rasters record))
        (should-not (launcher-icons--record-failures record))
        (should (cl-subsetp images launcher-icons-test--flushed))
        (should-not (file-exists-p (expand-file-name old (launcher-icons--app-directory record))))
        ;; Until the new generation has icons, a placeholder, not old icons.
        (should-not (launcher--icon launcher-icons-test--calc 20))
        (should-not (launcher--icon launcher-icons-test--calc 128))
        ;; List icons as warming, the preview's size urgently.
        (should (equal (launcher-icons-test--queued)
                       `((,launcher-icons-test--calc 64 256))))
        (launcher-icons-test--job)
        (should (equal (launcher-icons--record-rasters record) '(64 256)))
        (should (equal (length notified) 2))))))

(ert-deftest launcher-icons-unchanged-fingerprint-keeps-assets ()
  (launcher-icons-test--with-cache
    (launcher-icons-prepare (list launcher-icons-test--calc))
    (launcher-icons-test--job)
    (let* ((record (launcher-icons-test--record launcher-icons-test--calc))
           (generation (launcher-icons--record-generation record))
           (files (launcher-icons-test--files)))
      (setq launcher-icons-test--clock 2000.0)
      (launcher-icons-prepare (list launcher-icons-test--calc))
      (let ((requests (launcher-icons-test--job)))
        (should (equal (alist-get 'sizes (car requests)) nil)))
      (should (equal (launcher-icons--record-generation record) generation))
      (should (equal (launcher-icons-test--files) files))
      (should (= (launcher-icons--record-checked record) 2000.0)))))

(ert-deftest launcher-icons-checks-are-throttled ()
  (launcher-icons-test--with-cache
    (launcher-icons-prepare (list launcher-icons-test--calc))
    (launcher-icons-test--job)
    (setq launcher-icons-test--clock 1299.0)
    (launcher-icons-prepare (list launcher-icons-test--calc))
    (launcher--icon launcher-icons-test--calc 20)
    (should-not (launcher-icons-test--queued))
    (setq launcher-icons-test--clock 1300.0)
    (launcher-icons-prepare (list launcher-icons-test--calc))
    (should (equal (launcher-icons-test--queued) `((,launcher-icons-test--calc 0))))
    (launcher-icons-test--job)
    ;; A refresh of the index checks again at once.
    (setq launcher-icons-test--clock 1301.0)
    (launcher-icons-index-refreshed (list launcher-icons-test--calc))
    (should (equal (launcher-icons-test--queued) `((,launcher-icons-test--calc 0))))))

(ert-deftest launcher-icons-changed-during-drawing-checks-again ()
  (launcher-icons-test--with-cache
    (launcher-icons-prepare (list launcher-icons-test--calc))
    (launcher-icons-test--job
     (lambda (requests)
       (list `((id . ,(alist-get 'id (car requests)))
               (error . ((code . "changed") (message . "The application changed")))))))
    (let ((record (launcher-icons-test--record launcher-icons-test--calc)))
      (should-not (launcher-icons--record-rasters record))
      (should (equal (launcher-icons-test--queued) `((,launcher-icons-test--calc 0))))
      (launcher-icons-test--job)
      ;; The check finds the new fingerprint, so a generation needing icons.
      (should (equal (launcher-icons-test--queued) `((,launcher-icons-test--calc 64))))
      (launcher-icons-test--job)
      (should (equal (launcher-icons--record-rasters record) '(64))))))

;;; Stale and bad results

(ert-deftest launcher-icons-late-results-are-discarded ()
  (dolist (event '(reset invalidate retire))
    (launcher-icons-test--with-cache
      (let (notified)
        (launcher-icons-prepare (list launcher-icons-test--calc launcher-icons-test--notes))
        (launcher-icons--start)
        (let* ((job launcher-icons--job)
               (record (launcher-icons-test--record launcher-icons-test--calc)))
          (pcase event
            ('reset (launcher-clear-icon-cache))
            ('invalidate (launcher-icons--invalidate record))
            ('retire (launcher-icons-index-refreshed (list launcher-icons-test--notes))))
          (add-hook 'launcher-icon-updated-hook (lambda (path) (push path notified)))
          ;; The job was started before; its output arrives now.
          (let* ((args (cdr (car (last launcher-icons-test--spawned))))
                 (calc (car (launcher-icons-test--requests
                             (car (last launcher-icons-test--spawned))))))
            (when (eq event 'reset)
              ;; The reset stopped the job, so its outputs went with it.
              (should (launcher-icons--job-done job))
              (should-not (file-exists-p (launcher-icons--job-directory job)))
              (make-directory (launcher-icons--job-directory job) t))
            (funcall (plist-get args :filter) (launcher-icons--job-process job)
                     (launcher-icons-test--line (launcher-icons-test--ok calc)))
            (launcher-icons--finish job))
          (should-not notified)
          (let ((current (gethash launcher-icons-test--calc launcher-icons--records)))
            (when current
              (should-not (launcher-icons--record-rasters current))))
          (should-not (seq-some (lambda (file)
                                  (string-prefix-p
                                   (format "v1/%s/" (launcher-icons--record-key record)) file))
                                (seq-filter (lambda (file) (string-suffix-p ".png" file))
                                            (launcher-icons-test--files)))))))))

(ert-deftest launcher-icons-bad-output-is-rejected ()
  (launcher-icons-test--with-cache
    (let ((apps (mapcar (lambda (n) (format "/Applications/App %d.app" n)) (number-sequence 1 9))))
      (launcher-icons-prepare apps)
      (launcher-icons-test--job
       (lambda (requests)
         (let ((r (lambda (n) (nth (1- n) requests))))
           (list
            ;; 1: split across chunks and still parsed: see below.
            "not json"
            "[1,2,3]"
            "{\"id\":\"unknown\",\"written\":[]}"
            ;; 2: a size that was not requested.
            (let ((result (launcher-icons-test--ok (funcall r 2))))
              (setf (alist-get 'written result) [64 512])
              result)
            ;; 3: claims a PNG it did not write.
            (let ((result (launcher-icons-test--ok (funcall r 3) 'none)))
              (setf (alist-get 'written result) [64])
              result)
            ;; 4: a file that is no PNG.
            (let ((result (launcher-icons-test--ok (funcall r 4))))
              (with-temp-file (alist-get (intern "64") (alist-get 'outputs (funcall r 4)))
                (insert "GIF89a"))
              result)
            ;; 5: an invalid fingerprint.
            (let ((result (launcher-icons-test--ok (funcall r 5))))
              (setf (alist-get 'fingerprint result) [[1 2]])
              result)
            ;; 6: a per-app error.
            `((id . ,(alist-get 'id (funcall r 6)))
              (error . ((code . "failed") (message . "No icon"))))
            ;; 7: fine; 7 again: a duplicate, ignored.
            (launcher-icons-test--ok (funcall r 7))
            (let ((result (launcher-icons-test--ok (funcall r 7))))
              (setf (alist-get 'fingerprint result) [["other"]])
              result)
            ;; 8: fine.  9: never answered.
            (launcher-icons-test--ok (funcall r 8))))))
      (let ((rasters (mapcar (lambda (app)
                               (launcher-icons--record-rasters (launcher-icons-test--record app)))
                             apps)))
        (should (equal rasters '(nil nil nil nil nil nil (64) (64) nil))))
      ;; Failures wait for the next interval, without errors.
      (dolist (n '(2 3 4 5 6 9))
        (let ((record (launcher-icons-test--record (nth (1- n) apps))))
          (should (launcher-icons--failed-p record 64))
          (should (launcher-icons--failed-p record 'check))))
      (should (equal (launcher-icons--record-fingerprint
                      (launcher-icons-test--record (nth 6 apps)))
                     (launcher-icons--fingerprint-hash
                      (launcher-icons-test--fingerprint (nth 6 apps)))))
      (should-not (launcher-icons-test--queued))
      (launcher--icon (nth 1 apps) 20)
      (should-not (launcher-icons-test--queued))
      (setq launcher-icons-test--clock 1300.0)
      (launcher--icon (nth 1 apps) 20)
      (should (equal (launcher-icons-test--queued) `((,(nth 1 apps) 0 64)))))))

(ert-deftest launcher-icons-partial-lines-are-joined ()
  (launcher-icons-test--with-cache
    (launcher-icons-prepare (list launcher-icons-test--calc))
    (launcher-icons--start)
    (let* ((job launcher-icons--job)
           (args (cdr (car launcher-icons-test--spawned)))
           (line (launcher-icons-test--line
                  (launcher-icons-test--ok (car (launcher-icons-test--requests))))))
      (dotimes (i (length line))
        (funcall (plist-get args :filter) (launcher-icons--job-process job)
                 (substring line i (1+ i))))
      (launcher-icons--finish job)
      (should (equal (launcher-icons--record-rasters
                      (launcher-icons-test--record launcher-icons-test--calc))
                     '(64))))))

(ert-deftest launcher-icons-oversized-output-stops-the-worker ()
  (launcher-icons-test--with-cache
    (launcher-icons-prepare (list launcher-icons-test--calc launcher-icons-test--notes))
    (launcher-icons--start)
    (let* ((job launcher-icons--job)
           (args (cdr (car launcher-icons-test--spawned))))
      (funcall (plist-get args :filter) (launcher-icons--job-process job)
               (make-string (1+ launcher-icons--max-record) ?x))
      (should (launcher-icons--job-done job))
      (should-not launcher-icons--job)
      (should-not (process-live-p (launcher-icons--job-process job)))
      (should-not (file-exists-p (launcher-icons--job-directory job)))
      (should (launcher-icons--failed-p
               (launcher-icons-test--record launcher-icons-test--calc) 64)))))

(ert-deftest launcher-icons-timeout-and-exit-clean-up ()
  (launcher-icons-test--with-cache
    (let ((launcher-icons--batch-limit 1))
      (launcher-icons-prepare (list launcher-icons-test--calc launcher-icons-test--notes))
      (launcher-icons--start)
      (let* ((job launcher-icons--job)
             (timer (launcher-icons--job-timer job))
             (stderr (launcher-icons--job-stderr job)))
        (should (memq timer timer-list))
        ;; The timeout fires.
        (funcall (timer--function timer))
        (should-not (memq timer timer-list))
        (should-not (buffer-live-p stderr))
        (should-not (file-exists-p (launcher-icons--job-directory job)))
        (should (launcher-icons--failed-p
                 (launcher-icons-test--record launcher-icons-test--calc) 64))
        ;; The rest of the queue goes on.
        (should launcher-icons--kick-timer)
        (launcher-icons-test--job)
        (should (equal (launcher-icons--record-rasters
                        (launcher-icons-test--record launcher-icons-test--notes))
                       '(64)))
        (should-not launcher-icons--kick-timer)
        (should-not launcher-icons--job)))))

(ert-deftest launcher-icons-unwritable-cache-fails-quietly ()
  (launcher-icons-test--with-cache
    (let ((launcher-icon-cache-directory (expand-file-name "file" root)))
      (with-temp-file launcher-icon-cache-directory (insert "not a directory"))
      (launcher-icons-prepare (list launcher-icons-test--calc))
      (launcher-icons--start)
      (should-not launcher-icons--job)
      (should-not launcher-icons-test--spawned)
      (should-not (launcher--icon launcher-icons-test--calc 20))
      (should (launcher-icons--failed-p
               (launcher-icons-test--record launcher-icons-test--calc) 64))
      (should-not (launcher-icons-test--queued))
      (with-temp-buffer
        (insert "x")
        (should (equal (launcher-icons-prefix launcher-icons-test--calc)
                       (concat (propertize " " 'display '(space :width (20) :height (20))) " ")))))))

(ert-deftest launcher-icons-publication-failure-keeps-going ()
  (launcher-icons-test--with-cache
    (launcher-icons-prepare (list launcher-icons-test--calc launcher-icons-test--notes))
    (let ((calc-key (launcher-icons--record-key
                     (launcher-icons-test--record launcher-icons-test--calc))))
      ;; Calculator's directory is taken by a file.
      (make-directory (expand-file-name "v1" root) t)
      (with-temp-file (expand-file-name (concat "v1/" calc-key) root) (insert "x"))
      (launcher-icons-test--job)
      (should-not (launcher-icons--record-rasters
                   (launcher-icons-test--record launcher-icons-test--calc)))
      (should (launcher-icons--failed-p
               (launcher-icons-test--record launcher-icons-test--calc) 64))
      (should (equal (launcher-icons--record-rasters
                      (launcher-icons-test--record launcher-icons-test--notes))
                     '(64))))))

;;; Hooks

(ert-deftest launcher-icons-hook-errors-are-isolated ()
  (launcher-icons-test--with-cache
    (let (seen)
      (add-hook 'launcher-icon-updated-hook (lambda (_path) (error "Broken listener")))
      (add-hook 'launcher-icon-updated-hook (lambda (path) (push path seen)) t)
      (launcher-icons-prepare (list launcher-icons-test--calc launcher-icons-test--notes))
      (launcher-icons-test--job)
      (should (equal (sort seen #'string<)
                     (list launcher-icons-test--calc launcher-icons-test--notes)))
      (should (equal (launcher-icons--record-rasters
                      (launcher-icons-test--record launcher-icons-test--notes))
                     '(64))))))

;;; Refreshes, resets and cleanup

(ert-deftest launcher-icons-refresh-retires-removed-apps ()
  (launcher-icons-test--with-cache
    (launcher-icons-prepare (list launcher-icons-test--calc launcher-icons-test--notes))
    (launcher-icons-test--job)
    (let ((notes-dir (launcher-icons--app-directory
                      (launcher-icons-test--record launcher-icons-test--notes)))
          (calc-dir (launcher-icons--app-directory
                     (launcher-icons-test--record launcher-icons-test--calc)))
          (foreign (expand-file-name "notes.txt" root))
          (other-version (expand-file-name "v0/keep.png" root))
          (odd (expand-file-name "v1/not-a-hash/keep" root))
          (fresh-orphan (expand-file-name (concat "v1/" (make-string 64 ?a)) root))
          (old-orphan (expand-file-name (concat "v1/" (make-string 64 ?b)) root)))
      (with-temp-file foreign (insert "mine"))
      (make-directory (file-name-directory other-version) t)
      (with-temp-file other-version (insert "x"))
      (make-directory (file-name-directory odd) t)
      (with-temp-file odd (insert "x"))
      (make-directory fresh-orphan t)
      (make-directory old-orphan t)
      (set-file-times old-orphan (time-subtract nil (* 2 launcher-icons--stale-age)))
      (cl-letf (((symbol-function 'launcher-icons--now) #'float-time))
        ;; A failed discovery, or none, changes nothing.
        (launcher-icons-index-refreshed nil)
        (should (file-exists-p notes-dir))
        (launcher-icons-index-refreshed (list launcher-icons-test--calc)))
      (should-not (launcher-icons-test--record launcher-icons-test--notes))
      (should-not (file-exists-p notes-dir))
      (should-not (file-exists-p old-orphan))
      (dolist (kept (list calc-dir foreign other-version odd fresh-orphan))
        (should (file-exists-p kept))))))

(ert-deftest launcher-icons-reset-starts-over ()
  (launcher-icons-test--with-cache
    (let* ((launcher--apps (list (cons "Calculator" launcher-icons-test--calc)))
           (foreign (expand-file-name "v1/README" root))
           (live-job (expand-file-name "v1/jobs/20261007T120000-1-abcdef/manifest.json" root))
           (image nil)
           (notified nil))
      (launcher-icons-prepare (list launcher-icons-test--calc))
      (launcher-icons-test--job)
      (setq image (launcher--icon launcher-icons-test--calc 20))
      (let ((old (launcher-icons-test--record launcher-icons-test--calc)))
        (with-temp-file foreign (insert "x"))
        (make-directory (file-name-directory live-job) t)
        (with-temp-file live-job (insert "{}"))
        (add-hook 'launcher-icon-updated-hook (lambda (path) (push path notified)))
        (launcher-clear-icon-cache)
        (should (= launcher-icons--epoch 1))
        (should (memq image launcher-icons-test--flushed))
        (should (equal notified (list launcher-icons-test--calc)))
        (should-not (file-exists-p (launcher-icons--app-directory old)))
        ;; Another process's live job and unknown files stay.
        (should (file-exists-p foreign))
        (should (file-exists-p live-job))
        ;; Made again although the app is unchanged.
        (should-not (launcher--icon launcher-icons-test--calc 20))
        (should (equal (launcher-icons-test--queued) `((,launcher-icons-test--calc 0 64))))
        (launcher-icons-test--job)
        (let ((new (launcher-icons-test--record launcher-icons-test--calc)))
          (should-not (eq new old))
          (should (equal (launcher-icons--record-fingerprint new)
                         (launcher-icons--record-fingerprint old)))
          (should-not (equal (launcher-icons--record-generation new)
                             (launcher-icons--record-generation old)))
          (should (launcher--icon launcher-icons-test--calc 20)))))))

(ert-deftest launcher-icons-sweep-keeps-live-jobs ()
  (launcher-icons-test--with-cache
    (let ((old (expand-file-name "v1/jobs/20200101T000000-1-000001" root))
          (live (expand-file-name "v1/jobs/20261007T120000-2-000002" root))
          (odd (expand-file-name "v1/jobs/keep-me" root)))
      (dolist (directory (list old live odd))
        (make-directory directory t)
        (with-temp-file (expand-file-name "manifest.json" directory) (insert "{}")))
      (set-file-times old (time-subtract nil (* 2 launcher-icons--stale-age)))
      (set-file-times odd (time-subtract nil (* 2 launcher-icons--stale-age)))
      (cl-letf (((symbol-function 'launcher-icons--now) #'float-time))
        (launcher-icons--sweep-jobs))
      (should-not (file-exists-p old))
      (should (file-exists-p live))
      (should (file-exists-p odd)))))

;;; Display and completion

(ert-deftest launcher-icons-disabled-or-unsupported-does-nothing ()
  (launcher-icons-test--with-cache
    (dolist (setup (list (lambda () (setq launcher-show-icons nil))
                         (lambda () (setq launcher-icons--available nil))
                         (lambda () (advice-add 'launcher-icons--display-p :override #'ignore))))
      (let ((launcher-show-icons t)
            (launcher-icons--available t))
        (unwind-protect
            (progn
              (funcall setup)
              (should-not (launcher-icons-enabled-p))
              (should-not (launcher--icon launcher-icons-test--calc 20))
              (should (equal (launcher--completion-properties
                              (list (cons "Calculator" launcher-icons-test--calc)))
                             '(:annotation-function launcher--annotation)))
              (let ((launcher--apps (list (cons "Calculator" launcher-icons-test--calc)))
                    (completing-read-function
                     (lambda (_prompt table &rest _)
                       (should (equal (completion-metadata "" table nil) '(metadata)))
                       (should (equal completion-extra-properties
                                      '(:annotation-function launcher--annotation)))
                       "Calculator")))
                (cl-letf (((symbol-function 'launcher--launch) #'ignore))
                  (launcher)))
              (should-not (launcher-icons-test--queued))
              (should-not launcher-icons--kick-timer)
              (should (= (hash-table-count launcher-icons--records) 0)))
          (advice-remove 'launcher-icons--display-p #'ignore))))))

(ert-deftest launcher-icons-load-starts-nothing ()
  (should-not (and (boundp 'launcher-icons--job) launcher-icons--job))
  (dolist (option '(launcher-show-icons launcher-icon-size launcher-icon-cache-directory
                                        launcher-icon-check-interval))
    (should (custom-variable-p option))
    (should (assq option (get 'launcher 'custom-group))))
  (should (commandp 'launcher-clear-icon-cache))
  (should (file-readable-p launcher-icons--script)))

(ert-deftest launcher-icons-affixation-keeps-candidates ()
  (launcher-icons-test--with-cache
    (let* ((launcher-bangs '(("!g" "Google" "https://www.google.com/search?q=%s")))
           (entries (list (cons "Calculator" launcher-icons-test--calc)
                          (cons "Notes  (/Applications/Notes.app)" "/Applications/Notes.app")
                          (cons "Notes  (~/Applications/Notes.app)"
                                (expand-file-name "~/Applications/Notes.app"))
                          (cons "It's $(rm -rf) ; `x` 名前" "/Applications/It's $(rm -rf) ; `x` 名前.app")))
           (launcher--current-entries entries)
           (candidates (append '("!g") (mapcar #'car entries))))
      (should (equal (launcher--completion-properties entries)
                     '(:affixation-function launcher--affixation)))
      (launcher-icons-test--job)
      (let ((affixed (launcher--affixation candidates)))
        (should (equal (mapcar #'car affixed) candidates))
        (should (eq (car (car affixed)) (car candidates)))
        (should (equal (nth 2 (car affixed)) "  → Google search"))
        (should (equal (nth 2 (nth 1 affixed)) "  /Applications/Calculator.app"))
        ;; The bang: blank, as wide as an icon, and no request for it.
        (should (equal (get-text-property 0 'display (nth 1 (car affixed)))
                       '(space :width (20) :height (20))))
        (should-not (seq-find (lambda (file) (string-search "!g" file))
                              (launcher-icons-test--files)))
        ;; Each app, duplicates included, has its own icon.
        (let ((files (mapcar (lambda (entry)
                               (plist-get (cdr (get-text-property 0 'display (nth 1 entry)))
                                          :file))
                             (cdr affixed))))
          (should (seq-every-p #'stringp files))
          (should (= (length (delete-dups (copy-sequence files))) 4)))
        (dolist (entry affixed)
          (should (= (length (nth 1 entry)) 2))
          (should (equal (substring-no-properties (nth 1 entry)) "  ")))))
    ;; The worker got each path exactly, as data.
    (should (member "/Applications/It's $(rm -rf) ; `x` 名前.app"
                    (mapcar (lambda (request) (alist-get 'app request))
                            (launcher-icons-test--requests))))))

(ert-deftest launcher-icons-reader-gets-affixation ()
  ;; Through `launcher', with icons enabled, with stubbed effects.
  (launcher-icons-test--with-cache
    (let* ((launcher--apps (list (cons "Calculator" launcher-icons-test--calc)))
           (launcher--current-entries nil)
           (launched nil)
           (properties nil)
           (collection nil)
           (completing-read-function
            (lambda (_prompt table &rest _)
              (setq properties completion-extra-properties
                    collection table)
              (funcall (plist-get completion-extra-properties :affixation-function)
                       '("Calculator"))
              "Calculator")))
      (cl-letf (((symbol-function 'launcher--launch) (lambda (path) (push path launched))))
        (launcher))
      (should (equal properties '(:affixation-function launcher--affixation)))
      (should (equal (completion-metadata "" collection nil)
                     '(metadata (category . launcher-app))))
      (should (equal launched (list launcher-icons-test--calc)))
      (should-not launcher--current-entries)
      (should (equal (launcher-icons-test--queued) `((,launcher-icons-test--calc 0 64)))))))

(ert-deftest launcher-icons-refresh-checks-icons ()
  (launcher-icons-test--with-cache
    (let ((launcher--apps nil))
      (cl-letf (((symbol-function 'launcher--collect-paths)
                 (lambda () (list launcher-icons-test--calc))))
        (launcher-refresh))
      (should (equal (launcher-icons-test--queued) `((,launcher-icons-test--calc 0 64))))
      ;; An icon failure never fails the refresh.
      (cl-letf (((symbol-function 'launcher--collect-paths)
                 (lambda () (list launcher-icons-test--calc)))
                ((symbol-function 'launcher-icons-index-refreshed)
                 (lambda (_) (error "Broken"))))
        (launcher-refresh))
      (should (equal launcher--apps (list (cons "Calculator" launcher-icons-test--calc)))))))

;;; Other completion add-ons

(declare-function marginalia-mode "marginalia" (&optional arg))
(declare-function nerd-icons-completion-mode "nerd-icons-completion" (&optional arg))
(defvar marginalia-mode)
(defvar nerd-icons-completion-mode)

(ert-deftest launcher-icons-coexist-with-marginalia-and-nerd-icons ()
  ;; Both advise `completion-metadata-get', as completion UIs ask it for
  ;; the affixation function: the launcher's icon stays the only one.
  (skip-unless (and (require 'marginalia nil t) (require 'nerd-icons-completion nil t)))
  (launcher-icons-test--with-cache
    (let* ((entries (list (cons "Calculator" launcher-icons-test--calc)))
           (launcher--current-entries entries)
           (launcher-bangs '(("!g" "Google" "https://www.google.com/search?q=%s")))
           (completion-extra-properties (launcher--completion-properties entries))
           (metadata (completion-metadata
                      "" (launcher--make-collection entries nil 'launcher-app) nil)))
      (launcher-icons-test--job)
      (unwind-protect
          (progn
            (marginalia-mode 1)
            (nerd-icons-completion-mode 1)
            (let* ((affixation (completion-metadata-get metadata 'affixation-function))
                   (affixed (funcall affixation '("!g" "Calculator"))))
              (should (equal (mapcar #'car affixed) '("!g" "Calculator")))
              ;; One prefix of two characters: the icon or its space, and a gap.
              (dolist (entry affixed)
                (should (equal (substring-no-properties (nth 1 entry)) "  ")))
              (should (eq (car (get-text-property 0 'display (nth 1 (nth 1 affixed)))) 'image))
              (should (equal (nth 2 (nth 1 affixed)) "  /Applications/Calculator.app"))
              (should (equal (nth 2 (car affixed)) "  → Google search"))))
        (nerd-icons-completion-mode -1)
        (marginalia-mode -1)))))

;;; launcher-icons-tests.el ends here
