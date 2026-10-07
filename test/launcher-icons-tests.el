;;; launcher-icons-tests.el --- App icon cache checks -*- lexical-binding: t; -*-

;; Deterministic: fake apps and a cache in a temporary directory, and a
;; pipe process in place of the worker.  Needs neither macOS nor a
;; display.  test/launcher-icons-worker-tests.el checks the real worker
;; on macOS.

(require 'ert)
(require 'cl-lib)
(require 'launcher)
(require 'launcher-icons)

(defvar launcher-icons-test--root nil
  "The temporary directory of a check's fake apps and cache.")

(defvar launcher-icons-test--spawned nil
  "Commands of the fake worker processes, latest first.")

(defun launcher-icons-test--make-process (&rest args)
  "Record the command in ARGS, and return a live pipe process.
It keeps the sentinel of ARGS, which `launcher-icons-test--exit' runs."
  (push (plist-get args :command) launcher-icons-test--spawned)
  (let ((process (make-pipe-process :name "launcher-icons-test" :noquery t)))
    (process-put process 'sentinel (plist-get args :sentinel))
    process))

(defun launcher-icons-test--exit ()
  "End the running fake worker, and run its sentinel as Emacs would."
  (let ((process launcher-icons--process))
    (delete-process process)
    (funcall (process-get process 'sentinel) process "finished\n")))

(defmacro launcher-icons-test--with-cache (&rest body)
  "Run BODY with icons enabled, a fake worker, and a temporary
`launcher-icons-test--root', whose \"cache/\" is the icon cache."
  (declare (indent 0))
  `(let* ((launcher-icons-test--root (file-name-as-directory (make-temp-file "launcher-icons-test" t)))
          (launcher-icon-cache-directory
           (expand-file-name "cache/" launcher-icons-test--root))
          (launcher-show-icons t)
          (launcher-icon-size 20)
          (launcher-icons--files (make-hash-table :test #'equal))
          (launcher-icons--requested (make-hash-table :test #'equal))
          (launcher-icons--process nil)
          (launcher-icons-test--spawned nil))
     (cl-letf (((symbol-function 'launcher-icons-enabled-p) (lambda (&rest _) t))
               ((symbol-function 'make-process) #'launcher-icons-test--make-process))
       (unwind-protect
           (progn ,@body)
         (when (process-live-p launcher-icons--process)
           (delete-process launcher-icons--process))
         (delete-directory launcher-icons-test--root t)))))

(defun launcher-icons-test--app (name &optional time)
  "Make a fake app NAME under `launcher-icons-test--root', with an Info.plist modified at TIME.
Return its path."
  (let* ((app (expand-file-name name launcher-icons-test--root))
         (plist (expand-file-name "Contents/Info.plist" app)))
    (make-directory (file-name-directory plist) t)
    (write-region "<plist/>" nil plist nil 'silent)
    (when time
      (set-file-times plist (seconds-to-time time)))
    app))

(defun launcher-icons-test--draw (&optional command)
  "Do what the worker COMMAND, the latest by default, does: write its PNGs."
  (let ((pairs (nthcdr 5 (or command (car launcher-icons-test--spawned)))))
    (while pairs
      (write-region "png" nil (cadr pairs) nil 'silent)
      (setq pairs (cddr pairs)))))

(defun launcher-icons-test--foreign-png ()
  "Return a PNG name of another program's, in the configured directory.
It is named like the launcher's own, as a shared directory may hold."
  (expand-file-name (concat (make-string 40 ?a) ".png") launcher-icon-cache-directory))

(defun launcher-icons-test--apps-of (command)
  "Return the app paths COMMAND, a worker's, asks icons of."
  (seq-filter (lambda (arg) (string-suffix-p ".app" arg)) (nthcdr 5 command)))

(defun launcher-icons-test--display (path)
  "Return the display property of the prefix of the app at PATH."
  (get-text-property 0 'display (launcher-icons-prefix path)))

(declare-function marginalia-mode "marginalia" (&optional arg))
(declare-function nerd-icons-completion-mode "nerd-icons-completion" (&optional arg))
(defvar marginalia-mode)
(defvar nerd-icons-completion-mode)

(defconst launcher-icons-test--elpa
  (expand-file-name "../.cache/elpa" (file-name-directory (or load-file-name buffer-file-name)))
  "Where `sh test/elpa.sh' puts the pinned packages.")

(dolist (package '("marginalia-2.13" "nerd-icons.el-17faac7977242b470732efd417d3bcc8eb5a830e"
                   "nerd-icons-completion-f924dd490c8c4c1066fd97a76e0dc31e303fca30"))
  (let ((directory (expand-file-name package launcher-icons-test--elpa)))
    (when (file-directory-p directory)
      (add-to-list 'load-path directory))))

;;; Cache

(ert-deftest launcher-icons-file-follows-the-app ()
  (launcher-icons-test--with-cache
    (let* ((calc (launcher-icons-test--app "Calc.app" 1000))
           (notes (launcher-icons-test--app "Notes.app" 1000))
           (file (launcher-icons--file calc)))
      (should (string-match-p launcher-icons--png-regexp (file-name-nondirectory file)))
      (should (equal (file-name-directory file) (launcher-icons--directory)))
      (should (string-prefix-p launcher-icon-cache-directory file))
      (should (equal (launcher-icons--file calc) file))
      (should-not (equal (launcher-icons--file notes) file))
      ;; An updated app gets a new icon.
      (set-file-times (expand-file-name "Contents/Info.plist" calc) (seconds-to-time 2000))
      (should-not (equal (launcher-icons--file calc) file))
      ;; Without an Info.plist, the bundle's time; without an app, none.
      (delete-file (expand-file-name "Contents/Info.plist" calc))
      (should (launcher-icons--file calc))
      (should-not (launcher-icons--file (expand-file-name "Gone.app" launcher-icons-test--root))))))

(ert-deftest launcher-icons-prepare-makes-only-missing-icons ()
  (launcher-icons-test--with-cache
    (let ((calc (launcher-icons-test--app "Calc.app"))
          (notes (launcher-icons-test--app "Notes.app"))
          (gone (expand-file-name "Gone.app" launcher-icons-test--root)))
      (launcher-icons-prepare (list calc notes gone))
      (should (= (length launcher-icons-test--spawned) 1))
      (let ((command (car launcher-icons-test--spawned)))
        (should (equal (seq-take command 5)
                       (list launcher-icons--osascript "-l" "JavaScript"
                             launcher-icons--script "64")))
        (should (equal (nthcdr 5 command)
                       (list calc (launcher-icons--file calc)
                             notes (launcher-icons--file notes)))))
      ;; While the worker runs, nothing starts another.
      (launcher-icons-prepare (list calc notes))
      (should (= (length launcher-icons-test--spawned) 1))
      ;; The worker makes Calc's icon, and fails on Notes'.
      (write-region "png" nil (launcher-icons--file calc) nil 'silent)
      (launcher-icons-test--exit)
      ;; Made icons, and those that failed, are not asked for again.
      (launcher-icons-prepare (list calc notes))
      (should (= (length launcher-icons-test--spawned) 1))
      ;; An updated app is.
      (set-file-times (expand-file-name "Contents/Info.plist" calc) (seconds-to-time 5000))
      (launcher-icons-prepare (list calc notes))
      (should (equal (launcher-icons-test--apps-of (car launcher-icons-test--spawned))
                     (list calc))))))

(ert-deftest launcher-icons-a-failed-start-is-retried ()
  (launcher-icons-test--with-cache
    (let ((calc (launcher-icons-test--app "Calc.app")))
      (cl-letf (((symbol-function 'make-process)
                 (lambda (&rest _) (error "Too many processes"))))
        (should-error (launcher-icons-prepare (list calc))))
      (should-not launcher-icons-test--spawned)
      (launcher-icons-prepare (list calc))
      (should (equal (launcher-icons-test--apps-of (car launcher-icons-test--spawned))
                     (list calc))))))

(ert-deftest launcher-icons-a-deleted-icon-is-made-again ()
  ;; As when another Emacs, sharing the cache, deletes it in a refresh.
  (launcher-icons-test--with-cache
    (let ((calc (launcher-icons-test--app "Calc.app"))
          (broken (launcher-icons-test--app "Broken.app")))
      (launcher-icons-prepare (list calc broken))
      ;; The worker makes Calc's icon, and fails on Broken's.
      (write-region "png" nil (launcher-icons--file calc) nil 'silent)
      (launcher-icons-test--exit)
      (delete-file (launcher-icons--file calc))
      (launcher-icons-prepare (list calc broken))
      (should (equal (launcher-icons-test--apps-of (car launcher-icons-test--spawned))
                     (list calc))))))

(ert-deftest launcher-icons-a-stuck-worker-is-stopped ()
  ;; Even when the next launcher finds no icon it has not asked for yet.
  (launcher-icons-test--with-cache
    (let ((calc (launcher-icons-test--app "Calc.app")))
      (launcher-icons-prepare (list calc))
      (let ((stuck launcher-icons--process))
        (launcher-icons-prepare (list calc))
        (should (eq launcher-icons--process stuck))
        (process-put stuck 'start (- (float-time) launcher-icons--timeout 1))
        (launcher-icons-prepare (list calc))
        (should-not (process-live-p stuck))
        (should (process-live-p launcher-icons--process))
        ;; Its icons are asked for again.
        (should (equal (launcher-icons-test--apps-of (car launcher-icons-test--spawned))
                       (list calc)))))))

(ert-deftest launcher-icons-refresh-deletes-only-icons-of-gone-apps ()
  (launcher-icons-test--with-cache
    (let* ((calc (launcher-icons-test--app "Calc.app"))
           (notes (launcher-icons-test--app "Notes.app"))
           (other (expand-file-name "notes.txt" launcher-icon-cache-directory))
           (foreign (launcher-icons-test--foreign-png)))
      (launcher-icons-prepare (list calc notes))
      (launcher-icons-test--draw)
      (delete-process launcher-icons--process)
      (write-region "mine" nil other nil 'silent)
      (write-region "theirs" nil foreign nil 'silent)
      ;; A failed Spotlight query deletes nothing.
      (launcher-icons-index-refreshed nil)
      (should (= (length (launcher-icons--pngs)) 2))
      (launcher-icons-index-refreshed (list calc))
      (should (equal (launcher-icons--pngs) (list (launcher-icons--file calc))))
      (should (file-exists-p other))
      (should (file-exists-p foreign))
      ;; Icons that failed are tried again.
      (should (= (hash-table-count launcher-icons--requested) 0)))))

(ert-deftest launcher-icons-clear-deletes-only-icons ()
  (launcher-icons-test--with-cache
    (let ((calc (launcher-icons-test--app "Calc.app"))
          (other (expand-file-name "notes.txt" launcher-icon-cache-directory))
          (foreign (launcher-icons-test--foreign-png)))
      (launcher-icons-prepare (list calc))
      (launcher-icons-test--draw)
      (write-region "mine" nil other nil 'silent)
      (write-region "theirs" nil foreign nil 'silent)
      (let ((worker launcher-icons--process))
        (launcher-clear-icon-cache)
        (should-not (process-live-p worker)))
      (should-not (launcher-icons--pngs))
      (should (file-exists-p other))
      (should (file-exists-p foreign))
      (launcher-icons-prepare (list calc))
      (should (= (length launcher-icons-test--spawned) 2)))))

;;; Display and completion

(ert-deftest launcher-icons-prefix-shows-the-icon-or-space ()
  (launcher-icons-test--with-cache
    (let ((calc (launcher-icons-test--app "Calc.app")))
      (launcher-icons-prepare (list calc))
      (should (equal (launcher-icons-test--display calc) '(space :width (20) :height (20))))
      (should (equal (launcher-icons-test--display nil) '(space :width (20) :height (20))))
      (launcher-icons-test--draw)
      (let ((image (launcher-icons-test--display calc)))
        (should (eq (car image) 'image))
        (should (equal (plist-get (cdr image) :file) (launcher-icons--file calc)))
        (should (equal (plist-get (cdr image) :width) 20))
        (should (equal (plist-get (cdr image) :height) 20)))
      (should (equal (substring-no-properties (launcher-icons-prefix calc)) "  ")))))

(ert-deftest launcher-icons-disabled-does-nothing ()
  (launcher-icons-test--with-cache
    (cl-letf (((symbol-function 'launcher-icons-enabled-p) #'ignore))
      (let* ((calc (launcher-icons-test--app "Calc.app"))
             (launcher--apps (list (cons "Calc" calc)))
             (completing-read-function
              (lambda (_prompt table &rest _)
                (should (equal (completion-metadata "" table nil) '(metadata)))
                (should (equal completion-extra-properties
                               '(:annotation-function launcher--annotation)))
                "Calc")))
        (cl-letf (((symbol-function 'launcher--launch) #'ignore))
          (launcher))
        (should-not launcher-icons-test--spawned)
        (should-not (file-exists-p launcher-icon-cache-directory))))))

(ert-deftest launcher-icons-load-starts-nothing ()
  (should-not (and (boundp 'launcher-icons--process) launcher-icons--process))
  (dolist (option '(launcher-show-icons launcher-icon-size launcher-icon-cache-directory))
    (should (custom-variable-p option))
    (should (assq option (get 'launcher 'custom-group))))
  (should (commandp 'launcher-clear-icon-cache))
  (should (file-readable-p launcher-icons--script)))

(ert-deftest launcher-icons-affixation-keeps-candidates ()
  (launcher-icons-test--with-cache
    (let* ((launcher-bangs '(("!g" "Google" "https://www.google.com/search?q=%s")))
           (odd (launcher-icons-test--app "It's $(rm -rf) ; `x` 名前.app"))
           (entries (list (cons "Calc" (launcher-icons-test--app "Calc.app"))
                          (cons "Notes  (a)" (launcher-icons-test--app "a/Notes.app"))
                          (cons "Notes  (b)" (launcher-icons-test--app "b/Notes.app"))
                          (cons "It's" odd)))
           (launcher--current-entries entries)
           (candidates (cons "!g" (mapcar #'car entries))))
      (should (equal (launcher--completion-properties entries)
                     '(:affixation-function launcher--affixation)))
      (launcher-icons-test--draw)
      (let ((affixed (launcher--affixation candidates)))
        (should (equal (mapcar #'car affixed) candidates))
        (should (equal (nth 2 (car affixed)) "  → Google search"))
        (should (equal (nth 2 (nth 1 affixed)) (concat "  " (abbreviate-file-name (cdar entries)))))
        ;; The bang: blank, as large as an icon.
        (should (equal (get-text-property 0 'display (nth 1 (car affixed)))
                       '(space :width (20) :height (20))))
        ;; Each app, duplicates included, has its own icon.
        (let ((files (mapcar (lambda (entry)
                               (plist-get (cdr (get-text-property 0 'display (nth 1 entry)))
                                          :file))
                             (cdr affixed))))
          (should (seq-every-p #'stringp files))
          (should (= (length (delete-dups (copy-sequence files))) 4)))
        (dolist (entry affixed)
          (should (equal (substring-no-properties (nth 1 entry)) "  ")))))))

(ert-deftest launcher-icons-reader-gets-affixation ()
  ;; Through `launcher', with icons enabled, with stubbed effects.
  (launcher-icons-test--with-cache
    (let* ((calc (launcher-icons-test--app "Calc.app"))
           (launcher--apps (list (cons "Calc" calc)))
           (launcher--current-entries nil)
           (launched nil)
           (properties nil)
           (collection nil)
           (completing-read-function
            (lambda (_prompt table &rest _)
              (setq properties completion-extra-properties
                    collection table)
              "Calc")))
      (cl-letf (((symbol-function 'launcher--launch) (lambda (path) (push path launched))))
        (launcher))
      (should (equal properties '(:affixation-function launcher--affixation)))
      (should (equal (completion-metadata "" collection nil)
                     '(metadata (category . launcher-app))))
      (should (equal launched (list calc)))
      (should-not launcher--current-entries)
      (should (equal (launcher-icons-test--apps-of (car launcher-icons-test--spawned))
                     (list calc))))))

(ert-deftest launcher-icons-errors-never-fail-the-refresh ()
  (launcher-icons-test--with-cache
    (let ((calc (launcher-icons-test--app "Calc.app"))
          (launcher--apps nil))
      (cl-letf (((symbol-function 'launcher--collect-paths) (lambda () (list calc)))
                ((symbol-function 'launcher-icons-index-refreshed)
                 (lambda (_) (error "Broken"))))
        (launcher-refresh))
      (should (equal launcher--apps (list (cons "Calc" calc)))))))

(ert-deftest launcher-icons-coexist-with-marginalia-and-nerd-icons ()
  ;; Both advise `completion-metadata-get', as completion UIs ask it for
  ;; the affixation function: the launcher's icon stays the only one.
  (skip-unless (and (require 'marginalia nil t) (require 'nerd-icons-completion nil t)))
  (launcher-icons-test--with-cache
    (let* ((calc (launcher-icons-test--app "Calc.app"))
           (entries (list (cons "Calc" calc)))
           (launcher--current-entries entries)
           (launcher-bangs '(("!g" "Google" "https://www.google.com/search?q=%s")))
           (completion-extra-properties (launcher--completion-properties entries))
           (metadata (completion-metadata
                      "" (launcher--make-collection entries nil 'launcher-app) nil)))
      (launcher-icons-test--draw)
      (unwind-protect
          (progn
            (marginalia-mode 1)
            (nerd-icons-completion-mode 1)
            (let* ((affixation (completion-metadata-get metadata 'affixation-function))
                   (affixed (funcall affixation '("!g" "Calc"))))
              (should (equal (mapcar #'car affixed) '("!g" "Calc")))
              ;; One prefix of two characters: the icon or its space, and a gap.
              (dolist (entry affixed)
                (should (equal (substring-no-properties (nth 1 entry)) "  ")))
              (should (eq (car (get-text-property 0 'display (nth 1 (nth 1 affixed)))) 'image))
              (should (equal (nth 2 (car affixed)) "  → Google search"))))
        (nerd-icons-completion-mode -1)
        (marginalia-mode -1)))))

;;; launcher-icons-tests.el ends here
