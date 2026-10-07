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

(defun launcher-icons-worker-test--run (directory apps)
  "Run the worker on APPS, writing N.png in DIRECTORY for the Nth app.
Return what it printed, and the PNG files."
  (let* ((pngs (cl-loop for i from 1 to (length apps)
                        collect (expand-file-name (format "%d.png" i) directory)))
         (output (with-temp-buffer
                   (should (eql (apply #'call-process launcher-icons--osascript nil t nil
                                       "-l" "JavaScript" launcher-icons--script "64"
                                       (cl-mapcan #'list apps pngs))
                                0))
                   (string-trim (buffer-string)))))
    (cons output pngs)))

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

(defun launcher-icons-worker-test--bytes (file)
  "Return FILE's contents, unibyte."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file)
    (buffer-string)))

(ert-deftest launcher-icons-worker-renders-native-icons ()
  (launcher-icons-worker-test--with-directory directory
    (let* ((start (float-time))
           (result (launcher-icons-worker-test--run
                    directory (list launcher-icons-worker-test--calculator
                                    launcher-icons-worker-test--notes))))
      (message "Worker: Calculator and Notes in %.3fs" (- (float-time) start))
      (should (equal (car result) ""))
      (dolist (file (cdr result))
        (let ((stats (launcher-icons-worker-test--stats file)))
          (message "Worker: %s %S" (file-name-nondirectory file) stats)
          (should (equal (launcher-icons-worker-test--png-size file) '(64 64)))
          ;; Drawn, not blank: most of the square, in many colors.
          (should (> (alist-get 'opaque stats) 0.5))
          (should (> (alist-get 'colors stats) 20))))
      ;; Each app's own icon.
      (should-not (equal (launcher-icons-worker-test--bytes (nth 1 result))
                         (launcher-icons-worker-test--bytes (nth 2 result)))))))

(ert-deftest launcher-icons-worker-isolates-failures ()
  (launcher-icons-worker-test--with-directory directory
    (let* ((missing "/Applications/Launcher Icon Test Missing.app")
           (result (launcher-icons-worker-test--run
                    directory (list missing launcher-icons-worker-test--calculator))))
      (should (equal (car result) (concat missing ": no such application")))
      (should-not (file-exists-p (nth 1 result)))
      (should (file-exists-p (nth 2 result))))))

(ert-deftest launcher-icons-worker-paths-are-data ()
  (launcher-icons-worker-test--with-directory directory
    (let* ((app (expand-file-name "It's $(touch pwned) `touch pwned2` ; \"q\" 名前 é.app"
                                  directory))
           (resources (expand-file-name "Contents/Resources/" app)))
      (make-directory resources t)
      (with-temp-file (expand-file-name "Contents/Info.plist" app)
        (insert "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<plist version=\"1.0\"><dict>"
                "<key>CFBundlePackageType</key><string>APPL</string>"
                "<key>CFBundleIconFile</key><string>AppIcon</string></dict></plist>\n"))
      (copy-file (expand-file-name "Contents/Resources/AppIcon.icns"
                                   launcher-icons-worker-test--calculator)
                 resources)
      (let ((result (launcher-icons-worker-test--run directory (list app))))
        (should (equal (car result) ""))
        (should (> (alist-get 'opaque (launcher-icons-worker-test--stats (nth 1 result))) 0.5)))
      (should-not (file-exists-p (expand-file-name "pwned" directory)))
      (should-not (file-exists-p (expand-file-name "pwned2" directory)))
      (should-not (file-exists-p "/pwned")))))

(ert-deftest launcher-icons-worker-cache-end-to-end ()
  ;; `launcher-icons-prepare' with the real worker, as a launcher starts.
  (launcher-icons-worker-test--with-directory directory
    (let ((launcher-icon-cache-directory (expand-file-name "cache/" directory))
          (launcher-icons--files (make-hash-table :test #'equal))
          (launcher-icons--requested (make-hash-table :test #'equal))
          (launcher-icons--process nil)
          (apps (list launcher-icons-worker-test--calculator launcher-icons-worker-test--notes))
          (start (float-time)))
      (launcher-icons-prepare apps)
      (while (process-live-p launcher-icons--process)
        (accept-process-output launcher-icons--process 0.05))
      (message "Worker: cache warmed with 2 apps in %.3fs" (- (float-time) start))
      (dolist (app apps)
        (should (eq (car (get-text-property 0 'display (launcher-icons-prefix app))) 'image)))
      ;; A new launcher, with the icons on disk, starts no worker.
      (let ((worker launcher-icons--process))
        (clrhash launcher-icons--requested)
        (launcher-icons-prepare apps)
        (should (eq launcher-icons--process worker))))))

;;; launcher-icons-worker-tests.el ends here
