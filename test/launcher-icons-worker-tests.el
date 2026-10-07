;;; launcher-icons-worker-tests.el --- Real icon worker checks -*- lexical-binding: t; -*-

;; MACOS ONLY: runs assets/launcher-icons.js with /usr/bin/osascript on
;; real and fake application bundles in temporary directories, and skips
;; elsewhere.  Needs no display: run in batch, as the other checks.
;; Writes nothing outside temporary directories, and opens no app.

(require 'ert)
(require 'cl-lib)
(require 'launcher-icons)

(defconst launcher-icons-worker-test--stats
  (expand-file-name "png-stats.js" (file-name-directory (or load-file-name buffer-file-name)))
  "Script measuring a PNG.")

(defconst launcher-icons-worker-test--calculator "/System/Applications/Calculator.app")
(defconst launcher-icons-worker-test--notes "/System/Applications/Notes.app")

(defmacro launcher-icons-worker-test--with-directory (var &rest body)
  "Run BODY with VAR bound to a new temporary directory, deleted after."
  (declare (indent 1))
  `(let ((,var (file-name-as-directory (make-temp-file "launcher-icons-worker" t))))
     (skip-unless (and (eq system-type 'darwin)
                       (file-executable-p launcher-icons--osascript)))
     (unwind-protect (progn ,@body)
       (delete-directory ,var t))))

(defun launcher-icons-worker-test--run (directory requests)
  "Run the worker on REQUESTS in DIRECTORY; return its results, as alists.
Each request is (APP SIZES); outputs are named after the request number.
Signal an error if the worker fails or writes to standard error."
  (let ((manifest (expand-file-name "manifest.json" directory))
        (number 0))
    (let ((coding-system-for-write 'utf-8-unix))
      (with-temp-file manifest
        (json-insert
         `((version . 1)
           (requests
            . ,(vconcat
                (mapcar (pcase-lambda (`(,app ,sizes))
                          (let ((id (number-to-string (cl-incf number))))
                            `((id . ,id) (app . ,app) (sizes . ,(vconcat sizes))
                              (outputs
                               . ,(mapcar (lambda (size)
                                            (cons (intern (number-to-string size))
                                                  (expand-file-name
                                                   (format "%s-%d.png" id size) directory)))
                                          sizes)))))
                        requests)))))))
    (let* ((stderr (make-temp-file "launcher-icons-stderr"))
           (output (with-temp-buffer
                     (let* ((coding-system-for-read 'utf-8-unix)
                            (status (call-process launcher-icons--osascript nil
                                                  (list t stderr) nil
                                                  "-l" "JavaScript" launcher-icons--script
                                                  manifest)))
                       (unless (eql status 0)
                         (error "Worker exited with %s: %s" status
                                (with-temp-buffer (insert-file-contents stderr) (buffer-string)))))
                     (buffer-string))))
      (unwind-protect
          (should (equal (with-temp-buffer (insert-file-contents stderr) (buffer-string)) ""))
        (delete-file stderr))
      (mapcar (lambda (line)
                (json-parse-string line :object-type 'alist :array-type 'list))
              (split-string output "\n" t)))))

(defun launcher-icons-worker-test--fingerprint (directory app)
  "Return the worker's fingerprint of APP, checked from DIRECTORY."
  (let ((result (car (launcher-icons-worker-test--run directory (list (list app nil))))))
    (should-not (alist-get 'error result))
    (alist-get 'fingerprint result)))

(defun launcher-icons-worker-test--plist (version short icon)
  "Return an Info.plist with VERSION, SHORT version and ICON file."
  (concat "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
          "<!DOCTYPE plist PUBLIC \"-//Apple//DTD PLIST 1.0//EN\" "
          "\"http://www.apple.com/DTDs/PropertyList-1.0.dtd\">\n"
          "<plist version=\"1.0\"><dict>"
          "<key>CFBundleIdentifier</key><string>dev.launcher.icon-test</string>"
          "<key>CFBundlePackageType</key><string>APPL</string>"
          "<key>CFBundleVersion</key><string>" version "</string>"
          "<key>CFBundleShortVersionString</key><string>" short "</string>"
          (if icon (concat "<key>CFBundleIconFile</key><string>" icon "</string>") "")
          "</dict></plist>\n"))

(defun launcher-icons-worker-test--bundle (directory name &optional version)
  "Make a fake app NAME in DIRECTORY, with Calculator's icon file.
VERSION defaults to 1.  Return the bundle's path."
  (let* ((app (expand-file-name name directory))
         (resources (expand-file-name "Contents/Resources" app)))
    (make-directory resources t)
    (with-temp-file (expand-file-name "Contents/Info.plist" app)
      (insert (launcher-icons-worker-test--plist (or version "1") "1.0" "AppIcon")))
    (copy-file (expand-file-name "Contents/Resources/AppIcon.icns"
                                 launcher-icons-worker-test--calculator)
               (expand-file-name "AppIcon.icns" resources))
    (directory-file-name app)))

(defun launcher-icons-worker-test--set-time (file seconds)
  "Set FILE's modification time to SECONDS since the epoch."
  (set-file-times file (seconds-to-time seconds)))

(defun launcher-icons-worker-test--png-size (file)
  "Return FILE's width and height, from its PNG header."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file nil 0 24)
    (should (equal (buffer-substring 1 9) "\211PNG\r\n\032\n"))
    (cl-flet ((u32 (start) (let ((value 0))
                             (dotimes (i 4) (setq value (+ (* value 256) (char-after (+ start i)))))
                             value)))
      (list (u32 17) (u32 21)))))

(defun launcher-icons-worker-test--stats (file)
  "Return FILE's measurements by test/png-stats.js, as an alist."
  (json-parse-string
   (car (process-lines launcher-icons--osascript "-l" "JavaScript"
                       launcher-icons-worker-test--stats file))
   :object-type 'alist))

;;; Rendering

(ert-deftest launcher-icons-worker-renders-native-icons ()
  (launcher-icons-worker-test--with-directory directory
    (let* ((start (float-time))
           (results (launcher-icons-worker-test--run
                     directory
                     `((,launcher-icons-worker-test--calculator (64 256 512))
                       (,launcher-icons-worker-test--notes (64)))))
           (elapsed (- (float-time) start)))
      (message "Worker: Calculator at 64, 256 and 512 and Notes at 64 in %.3fs" elapsed)
      (should (equal (mapcar (lambda (result) (alist-get 'written result)) results)
                     '((64 256 512) (64))))
      (dolist (case '(("1-64.png" 64) ("1-256.png" 256) ("1-512.png" 512) ("2-64.png" 64)))
        (let* ((file (expand-file-name (car case) directory))
               (stats (launcher-icons-worker-test--stats file)))
          (message "Worker: %s %S" (car case) stats)
          (should (equal (launcher-icons-worker-test--png-size file)
                         (list (cadr case) (cadr case))))
          ;; Drawn, not blank: most of the square, in many colors.
          (should (> (alist-get 'opaque stats) 0.5))
          (should (> (alist-get 'colors stats) 20))))
      ;; Each app's own icon.
      (should-not (equal (with-temp-buffer
                           (set-buffer-multibyte nil)
                           (insert-file-contents-literally (expand-file-name "1-64.png" directory))
                           (buffer-string))
                         (with-temp-buffer
                           (set-buffer-multibyte nil)
                           (insert-file-contents-literally (expand-file-name "2-64.png" directory))
                           (buffer-string))))
      ;; Larger rasters hold more detail, drawn from the source.
      (should (> (file-attribute-size (file-attributes (expand-file-name "1-512.png" directory)))
                 (* 4 (file-attribute-size
                       (file-attributes (expand-file-name "1-64.png" directory)))))))))

(ert-deftest launcher-icons-worker-isolates-failures ()
  (launcher-icons-worker-test--with-directory directory
    (let ((results (launcher-icons-worker-test--run
                    directory
                    `(("/Applications/Launcher Icon Test Missing.app" (64))
                      (,launcher-icons-worker-test--calculator (64))
                      ("relative/Path.app" (64))))))
      (should (equal (mapcar (lambda (result) (alist-get 'id result)) results) '("1" "2" "3")))
      (should (equal (alist-get 'code (alist-get 'error (nth 0 results))) "missing"))
      (should (equal (alist-get 'written (nth 1 results)) '(64)))
      (should (equal (alist-get 'code (alist-get 'error (nth 2 results))) "failed"))
      (should-not (file-exists-p (expand-file-name "1-64.png" directory)))
      (should (file-exists-p (expand-file-name "2-64.png" directory))))))

(ert-deftest launcher-icons-worker-paths-are-data ()
  (launcher-icons-worker-test--with-directory directory
    (let* ((name "It's $(touch pwned) `touch pwned2` ; \"q\" 名前 é.app")
           (app (launcher-icons-worker-test--bundle directory name))
           (results (launcher-icons-worker-test--run directory `((,app (64))))))
      (should (equal (alist-get 'written (car results)) '(64)))
      (should (> (alist-get 'opaque (launcher-icons-worker-test--stats
                                     (expand-file-name "1-64.png" directory)))
                 0.5))
      (should-not (file-exists-p (expand-file-name "pwned" directory)))
      (should-not (file-exists-p (expand-file-name "pwned2" directory)))
      (should-not (file-exists-p "pwned")))))

;;; Fingerprints

(ert-deftest launcher-icons-worker-fingerprint-follows-metadata ()
  (launcher-icons-worker-test--with-directory directory
    (let* ((app (launcher-icons-worker-test--bundle directory "Fingerprint.app"))
           (plist (expand-file-name "Contents/Info.plist" app))
           (icon (expand-file-name "Contents/Resources/AppIcon.icns" app))
           (assets (expand-file-name "Contents/Resources/Assets.car" app))
           (fingerprint (lambda () (launcher-icons-worker-test--fingerprint directory app)))
           (base (funcall fingerprint)))
      (message "Worker: fake app fingerprint %S" base)
      (should (equal (assoc "assets" base) '("assets" "missing")))
      (should (equal (car (cdr (assoc "icon" base))) "AppIcon.icns"))
      ;; Unchanged metadata, unchanged fingerprint.
      (should (equal (funcall fingerprint) base))
      (let (previous)
        (cl-flet ((changes (description)
                    (let ((now (funcall fingerprint)))
                      (should-not (equal now (or previous base)))
                      (message "Worker: fingerprint changes after %s" description)
                      (setq previous now))))
          ;; A new version, with the plist's time kept.
          (let ((time (file-attribute-modification-time (file-attributes plist))))
            (with-temp-file plist
              (insert (launcher-icons-worker-test--plist "2" "1.0" "AppIcon")))
            (set-file-times plist time))
          (changes "a version change")
          ;; The icon file's time, then its size.
          (launcher-icons-worker-test--set-time icon 1700000000)
          (changes "an icon time change")
          (let ((time (file-attribute-modification-time (file-attributes icon))))
            (write-region "x" nil icon t 'silent)
            (set-file-times icon time))
          (changes "an icon size change")
          ;; An asset catalog appears, then changes.
          (with-temp-file assets (insert "catalog"))
          (changes "an asset catalog appearing")
          (launcher-icons-worker-test--set-time assets 1700000000)
          (changes "an asset catalog time change")
          ;; A nested change that leaves the bundle's own time alone.
          (let ((bundle-time (file-attribute-modification-time (file-attributes app)))
                (resources-time (file-attribute-modification-time
                                 (file-attributes (file-name-directory icon)))))
            (write-region "y" nil icon t 'silent)
            (set-file-times (file-name-directory icon) resources-time)
            (set-file-times app bundle-time)
            (should (equal (file-attribute-modification-time (file-attributes app)) bundle-time)))
          (changes "a nested icon change, the bundle's time kept"))))))

(ert-deftest launcher-icons-worker-replaced-bundle-changes-fingerprint ()
  (launcher-icons-worker-test--with-directory directory
    (let* ((app (launcher-icons-worker-test--bundle directory "Replaced.app"))
           (base (launcher-icons-worker-test--fingerprint directory app))
           (times (mapcar (lambda (file)
                            (cons (file-relative-name file app)
                                  (file-attribute-modification-time (file-attributes file))))
                          (cons app (directory-files-recursively app "" t))))
           (staged (expand-file-name "Staged.app" directory)))
      ;; An update that installs a whole new bundle, metadata and times equal.
      (copy-directory app staged nil t t)
      (pcase-dolist (`(,file . ,time) times)
        (set-file-times (expand-file-name file staged) time))
      (delete-directory app t)
      (rename-file staged app)
      (let ((now (launcher-icons-worker-test--fingerprint directory app)))
        (should-not (equal now base))
        ;; Only the bundle's file identity tells them apart.
        (should (equal (cl-remove "bundle" now :key #'car :test #'equal)
                       (cl-remove "bundle" base :key #'car :test #'equal)))))))

;;; Through the cache

(ert-deftest launcher-icons-worker-cache-end-to-end ()
  (launcher-icons-worker-test--with-directory directory
    (let* ((launcher-icon-cache-directory directory)
           (launcher-icons--records (make-hash-table :test #'equal))
           (launcher-icons--epoch 0)
           (launcher-icons--urgent nil)
           (launcher-icons--queue nil)
           (launcher-icons--job nil)
           (launcher-icons--kick-timer nil)
           (launcher-icons--swept nil)
           (launcher-icons--available t)
           (launcher-icon-updated-hook nil)
           (apps (list launcher-icons-worker-test--calculator launcher-icons-worker-test--notes
                       "/System/Applications/Chess.app" "/Applications/Launcher Missing.app"))
           (spawned 0)
           (wait (lambda ()
                   (with-timeout (20 (error "Icon worker did not finish"))
                     (while (or launcher-icons--job launcher-icons--kick-timer)
                       (accept-process-output nil 0.05))))))
      (cl-letf* ((make-process (symbol-function 'make-process))
                 ((symbol-function 'make-process)
                  (lambda (&rest args) (cl-incf spawned) (apply make-process args)))
                 ((symbol-function 'launcher-icons--display-p) (lambda (_) t))
                 ((symbol-function 'launcher-icons--scale) (lambda (_) 2)))
        (let ((start (float-time)))
          (launcher-icons-prepare apps)
          (funcall wait)
          (message "Cache: first warm-up of %d apps in %.3fs, %d worker(s)"
                   (length apps) (- (float-time) start) spawned))
        (dolist (app (butlast apps))
          (should (string-suffix-p "/64.png" (plist-get (cdr (launcher--icon app 20)) :file))))
        (should-not (launcher--icon (car (last apps)) 20))
        ;; A preview, later.
        (let ((start (float-time)))
          (launcher--icon launcher-icons-worker-test--calculator 128)
          (funcall wait)
          (message "Cache: a 256-pixel preview in %.3fs" (- (float-time) start)))
        (should (string-suffix-p "/256.png" (plist-get (cdr (launcher--icon
                                                             launcher-icons-worker-test--calculator
                                                             128))
                                                       :file)))
        ;; A new process: icons from disk, a check, and nothing drawn.
        (clrhash launcher-icons--records)
        (setq spawned 0)
        (let ((start (float-time)))
          (dolist (app (butlast apps))
            (should (launcher--icon app 20)))
          (message "Cache: disk hits of %d apps in %.4fs" (1- (length apps))
                   (- (float-time) start)))
        (let ((generations (mapcar (lambda (app)
                                     (launcher-icons--record-generation
                                      (launcher-icons--record app)))
                                   (butlast apps)))
              (start (float-time)))
          (funcall wait)
          (message "Cache: checks of a new process in %.3fs, %d worker(s)"
                   (- (float-time) start) spawned)
          (should (equal (mapcar (lambda (app)
                                   (launcher-icons--record-generation (launcher-icons--record app)))
                                 (butlast apps))
                         generations)))
        ;; Fresh lookups start nothing.
        (setq spawned 0)
        (dotimes (_ 100)
          (dolist (app (butlast apps)) (launcher--icon app 20)))
        (should-not launcher-icons--kick-timer)
        (should (= spawned 0))
        (should-not (directory-files (expand-file-name "v1/jobs" directory) nil
                                     directory-files-no-dot-files-regexp))))))

;;; launcher-icons-worker-tests.el ends here
