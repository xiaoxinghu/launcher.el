;;; launcher-osx-dictionary-tests.el --- Apple Dictionary tool checks -*- lexical-binding: t; -*-

;; Batch checks of launcher-osx-dictionary.el with the pinned
;; osx-dictionary package, which `sh test/elpa.sh' fetches into
;; .cache/elpa; without it, the checks that need it are skipped.  The
;; package's own code renders each lookup, but a fake helper script
;; stands in for its native `osx-dictionary-cli': it logs its arguments
;; and answers deterministically.  These checks are no proof of a native
;; lookup, which launcher-osx-dictionary-gui-tests.el makes in the test VM.

(require 'ert)
(require 'cl-lib)
(require 'launcher-osx-dictionary)
(require 'launcher-tools-tests)

(defconst launcher-osx-dictionary-test--root
  (expand-file-name ".." (file-name-directory (or load-file-name buffer-file-name)))
  "The repository's root.")

(defconst launcher-osx-dictionary-test--package
  (expand-file-name ".cache/elpa/osx-dictionary.el-655bca5cea78440a1ac41f9cd78711b9c8aff8f3"
                    launcher-osx-dictionary-test--root)
  "The pinned osx-dictionary package.")

;; Before loading the package, which would restore the user's choice.
(defvar osx-dictionary-last-dictionary-file nil)
(when (file-directory-p launcher-osx-dictionary-test--package)
  (add-to-list 'load-path launcher-osx-dictionary-test--package))
(require 'osx-dictionary nil t)

(defvar vertico-count)
(defvar osx-dictionary-allowed-dictionaries)
(defvar osx-dictionary-search-log-file)
(defvar osx-dictionary-previous-window-configuration)
(defvar osx-dictionary-mode-map)
(defvar osx-dictionary-mode-header-line)

(defvar launcher-osx-dictionary-test--dir nil
  "Directory of the current check's fake helper, its log and answers.")

(defun launcher-osx-dictionary-test--write (name contents)
  "Write CONTENTS to file NAME of the fake helper's directory."
  (let ((coding-system-for-write 'utf-8-unix))
    (write-region contents nil (expand-file-name name launcher-osx-dictionary-test--dir)
                  nil 'silent)))

(defun launcher-osx-dictionary-test--write-helper ()
  "Write the fake helper: it logs its arguments, lists the dictionaries
of file \"dictionaries\" with the exit status of file \"status\", finds
nothing for \"qwxzv\" and a definition with a heading for other words."
  (let ((helper (expand-file-name "osx-dictionary-cli" launcher-osx-dictionary-test--dir)))
    (launcher-osx-dictionary-test--write
     "osx-dictionary-cli"
     (format "#!/bin/sh
dir=%s
for arg; do printf '%%s\\037' \"$arg\"; done >>\"$dir/log\"
printf '\\n' >>\"$dir/log\"
if [ \"$1\" = -l ]; then
    if [ -f \"$dir/dictionaries\" ]; then cat \"$dir/dictionaries\"; fi
    exit \"$(cat \"$dir/status\" 2>/dev/null || echo 0)\"
fi
for word; do :; done
case $word in
    qwxzv) ;;
    *) printf '\\001Fake Dictionary\\001\\n%%s | fake | noun the sense of %%s. \\n• a bullet sense of %%s. \\n' \"$word\" \"$word\" \"$word\" ;;
esac
"
             (shell-quote-argument launcher-osx-dictionary-test--dir)))
    (set-file-modes helper #o755)
    (launcher-osx-dictionary-test--write "dictionaries"
                                         "Fake Dictionary\nOther Dictionary\n")))

(defun launcher-osx-dictionary-test--calls ()
  "Return the argument lists the fake helper got, oldest first."
  (let ((log (expand-file-name "log" launcher-osx-dictionary-test--dir)))
    (when (file-exists-p log)
      (with-temp-buffer
        (let ((coding-system-for-read 'utf-8))
          (insert-file-contents log))
        (mapcar (lambda (line) (split-string line "\037" t))
                (split-string (buffer-string) "\n" t))))))

(defun launcher-osx-dictionary-test--error (function &rest args)
  "Return the message of the error FUNCTION signals with ARGS."
  (let ((err (should-error (apply function args))))
    (error-message-string err)))

(defmacro launcher-osx-dictionary-test--with (&rest body)
  "Run BODY with the pinned package, a fake helper and default settings.
Afterwards, kill the result buffers and delete the helper."
  (declare (indent 0))
  `(progn
     (skip-unless (featurep 'osx-dictionary))
     (let* ((launcher-osx-dictionary-test--dir (make-temp-file "launcher-osx-dictionary" t))
            (osx-dictionary--load-dir (file-name-as-directory
                                       launcher-osx-dictionary-test--dir))
            (osx-dictionary-current-dictionary nil)
            (osx-dictionary-allowed-dictionaries nil)
            (osx-dictionary-search-log-file nil)
            (osx-dictionary-last-dictionary-file nil)
            (osx-dictionary-previous-window-configuration nil)
            ;; The fake helper works anywhere.
            (system-type 'darwin))
       (launcher-osx-dictionary-test--write-helper)
       (unwind-protect
           (progn ,@body)
         (dolist (name (list launcher-osx-dictionary-buffer-name "*osx-dictionary*"))
           (when (get-buffer name)
             (kill-buffer name)))
         (delete-directory launcher-osx-dictionary-test--dir t)))))

(defun launcher-osx-dictionary-test--layout ()
  "Return the selected frame's windows, their buffers and the selection."
  (list (selected-window) (mapcar #'window-buffer (window-list nil 'never))
        (length (frame-list))))

;;; Loading and requirements

(defun launcher-osx-dictionary-test--emacs (form)
  "Return what FORM prints in a new batch Emacs without osx-dictionary."
  (with-temp-buffer
    (call-process (expand-file-name invocation-name invocation-directory) nil t nil
                  "-Q" "--batch" "-L" launcher-osx-dictionary-test--root
                  "--eval" (prin1-to-string form))
    (buffer-string)))

(ert-deftest launcher-osx-dictionary-loads-alone ()
  ;; Requiring the adapter loads no dictionary and registers no tool;
  ;; a lookup then says the package is missing.
  (should (string-suffix-p
           (prin1-to-string
            '(nil nil t "Needs the osx-dictionary package: M-x package-install RET osx-dictionary"))
           (launcher-osx-dictionary-test--emacs
            '(progn
               (require 'launcher-osx-dictionary)
               (prin1 (list (featurep 'osx-dictionary) launcher-tools
                            (fboundp 'launcher-osx-dictionary-lookup)
                            (let ((system-type 'darwin))
                              (condition-case err
                                  (launcher-osx-dictionary-lookup "hello")
                                (error (error-message-string err)))))))))))

(ert-deftest launcher-osx-dictionary-requirements-fail-clearly ()
  (let ((system-type 'gnu/linux))
    (should (equal "Apple dictionaries are only available on macOS"
                   (launcher-osx-dictionary-test--error
                    #'launcher-osx-dictionary-lookup "hello"))))
  (should (equal "Nothing to look up"
                 (launcher-osx-dictionary-test--error
                  #'launcher-osx-dictionary-lookup " \t "))))

(ert-deftest launcher-osx-dictionary-incompatible-package ()
  (launcher-osx-dictionary-test--with
    (cl-letf (((symbol-function 'osx-dictionary--insert-search-result) nil))
      (should (equal "This osx-dictionary lacks osx-dictionary--insert-search-result; \
launcher-osx-dictionary was tested with commit 655bca5"
                     (launcher-osx-dictionary-test--error
                      #'launcher-osx-dictionary-lookup "hello"))))
    (should-not (launcher-osx-dictionary-test--calls))))

;;; Lookups

(ert-deftest launcher-osx-dictionary-lookup-renders-one-query ()
  ;; One exact query, rendered by the package, displayed nowhere.
  (launcher-osx-dictionary-test--with
    (let* ((layout (launcher-osx-dictionary-test--layout))
           (buffer (launcher-osx-dictionary-lookup "  hello ")))
      (should (equal '(("hello")) (launcher-osx-dictionary-test--calls)))
      (should (buffer-live-p buffer))
      (should (equal launcher-osx-dictionary-buffer-name (buffer-name buffer)))
      (should (equal layout (launcher-osx-dictionary-test--layout)))
      (should-not (get-buffer-window buffer t))
      (should-not osx-dictionary-previous-window-configuration)
      (should-not (get-buffer "*osx-dictionary*"))
      (with-current-buffer buffer
        (should (eq 'osx-dictionary-mode major-mode))
        (should buffer-read-only)
        (should visual-line-mode)
        (should launcher-osx-dictionary-result-mode)
        (should (eq osx-dictionary-mode-header-line header-line-format))
        (should (equal "hello" osx-dictionary--current-word))
        (should (= (point) (point-min)))
        ;; The package's heading, bullet indent and whitespace cleanup.
        (should (equal "Fake Dictionary
hello | fake | noun the sense of hello.
• a bullet sense of hello.
"
                       (buffer-substring-no-properties (point-min) (point-max))))
        (should (eq 'osx-dictionary-dictionary-name
                    (get-text-property (point-min) 'font-lock-face)))
        (goto-char (point-min))
        (search-forward "•")
        (should (equal "  " (get-text-property (point) 'line-prefix)))
        (should (equal "  " (get-text-property (point) 'wrap-prefix)))))))

(ert-deftest launcher-osx-dictionary-input-and-settings-pass-through ()
  ;; Multiword and Unicode queries reach the helper as typed, inside
  ;; surrounding whitespace; the user's dictionary choice applies.
  (launcher-osx-dictionary-test--with
    (launcher-osx-dictionary-lookup "ice cream")
    (with-current-buffer (launcher-osx-dictionary-lookup " 中文 café ")
      (should (equal "中文 café" osx-dictionary--current-word))
      (should (string-match-p "the sense of 中文 café" (buffer-string))))
    (let ((osx-dictionary-current-dictionary "Other Dictionary"))
      (launcher-osx-dictionary-lookup "hello"))
    (should (equal '(("ice cream") ("中文 café") ("-d" "Other Dictionary" "hello"))
                   (launcher-osx-dictionary-test--calls)))))

(ert-deftest launcher-osx-dictionary-relative-search-log ()
  ;; The package expands its search log against `default-directory':
  ;; the caller's, not the package's, which may be read-only.
  (launcher-osx-dictionary-test--with
    (let* ((caller (file-name-as-directory (make-temp-file "launcher-osx-dictionary-caller" t)))
           (default-directory caller)
           (osx-dictionary-search-log-file "lookups.log"))
      (unwind-protect
          (progn
            (set-file-modes osx-dictionary--load-dir #o555)
            (launcher-osx-dictionary-lookup "hello")
            (should (equal "hello\n" (with-temp-buffer
                                       (insert-file-contents
                                        (expand-file-name "lookups.log" caller))
                                       (buffer-string))))
            (should-not (file-exists-p (expand-file-name "lookups.log"
                                                         osx-dictionary--load-dir)))
            (should (equal caller default-directory)))
        (set-file-modes osx-dictionary--load-dir #o755)
        (delete-directory caller t)))))

(ert-deftest launcher-osx-dictionary-results-are-independent ()
  ;; The package's own session and other buffers stay as they were; a
  ;; failed lookup leaves the previous result.
  (launcher-osx-dictionary-test--with
    (let ((own (get-buffer-create "*osx-dictionary*")))
      (with-current-buffer own
        (osx-dictionary-mode)
        (let ((inhibit-read-only t))
          (insert "The package's own session"))
        (setq osx-dictionary--current-word "own"))
      (let ((first (launcher-osx-dictionary-lookup "hello"))
            (second (launcher-osx-dictionary-lookup "world")))
        (should (eq first second))
        (should (string-prefix-p "Fake Dictionary\nworld |"
                                 (with-current-buffer second (buffer-string))))
        (should (string-match-p "No definition for \"qwxzv\""
                                (launcher-osx-dictionary-test--error
                                 #'launcher-osx-dictionary-lookup "qwxzv")))
        (with-current-buffer second
          (should (equal "world" osx-dictionary--current-word))
          (should (string-prefix-p "Fake Dictionary\nworld |" (buffer-string)))))
      (with-current-buffer own
        (should (equal "The package's own session" (buffer-string)))
        (should (equal "own" osx-dictionary--current-word))))))

(ert-deftest launcher-osx-dictionary-empty-results-say-why ()
  (launcher-osx-dictionary-test--with
    (cl-flet ((lookup-error (word)
                (prog1 (launcher-osx-dictionary-test--error
                        #'launcher-osx-dictionary-lookup word)
                  (should-not (get-buffer launcher-osx-dictionary-buffer-name)))))
      (should (equal "No definition for \"qwxzv\" (searched: All active dictionaries)"
                     (lookup-error "qwxzv")))
      (let ((osx-dictionary-current-dictionary "Gone Dictionary"))
        (should (equal "Dictionary \"Gone Dictionary\" is not installed; \
choose another with M-x osx-dictionary-select-dictionary"
                       (lookup-error "qwxzv"))))
      (launcher-osx-dictionary-test--write "status" "3")
      (should (equal "The osx-dictionary helper failed (3)" (lookup-error "qwxzv")))
      (launcher-osx-dictionary-test--write "status" "0")
      (launcher-osx-dictionary-test--write "dictionaries" "")
      (should (equal "Dictionary.app lists no installed dictionaries"
                     (lookup-error "qwxzv")))
      (should (equal '(("qwxzv") ("-l") ("-d" "Gone Dictionary" "qwxzv") ("-l")
                       ("qwxzv") ("-l") ("qwxzv") ("-l"))
                     (launcher-osx-dictionary-test--calls))))))

(ert-deftest launcher-osx-dictionary-helper-build-failures ()
  ;; A missing helper is built on submission; each failure says why.
  (launcher-osx-dictionary-test--with
    (let ((source (expand-file-name "osx-dictionary.m" osx-dictionary--load-dir)))
      (delete-file (expand-file-name "osx-dictionary-cli" osx-dictionary--load-dir))
      (let ((exec-path nil))
        (should (string-match-p "The osx-dictionary helper source .*osx-dictionary.m is missing; reinstall"
                                (launcher-osx-dictionary-test--error
                                 #'launcher-osx-dictionary-lookup "hello")))
        (launcher-osx-dictionary-test--write "osx-dictionary.m" "int main( {")
        (should (equal "Building the osx-dictionary helper needs the Xcode command line \
tools: run xcode-select --install"
                       (launcher-osx-dictionary-test--error
                        #'launcher-osx-dictionary-lookup "hello"))))
      (let ((exec-path '("/usr/bin")))
        (skip-unless (and (executable-find "clang")
                          (launcher-osx-dictionary--developer-tools-p)))
        (cl-letf (((symbol-function 'launcher-osx-dictionary--developer-tools-p) #'ignore))
          (should (string-match-p "needs the Xcode command line tools"
                                  (launcher-osx-dictionary-test--error
                                   #'launcher-osx-dictionary-lookup "hello"))))
        (set-file-modes osx-dictionary--load-dir #o555)
        (unwind-protect
            (should (string-match-p "Cannot build the osx-dictionary helper: .* is not writable"
                                    (launcher-osx-dictionary-test--error
                                     #'launcher-osx-dictionary-lookup "hello")))
          (set-file-modes osx-dictionary--load-dir #o755))
        ;; A real compiler, failing on the broken source.
        (should (string-match-p "\\`Building the osx-dictionary helper failed (1): .*error:"
                                (launcher-osx-dictionary-test--error
                                 #'launcher-osx-dictionary-lookup "hello")))
        (should (file-exists-p source))
        (should-not (get-buffer launcher-osx-dictionary-buffer-name))))))

;;; Result keys

(ert-deftest launcher-osx-dictionary-result-keys ()
  (launcher-osx-dictionary-test--with
    (let ((buffer (launcher-osx-dictionary-lookup "hello")))
      (with-current-buffer buffer
        (should (eq 'launcher-osx-dictionary-quit (key-binding "q")))
        (should (eq 'launcher-osx-dictionary-search (key-binding "s")))
        (should (eq 'launcher-osx-dictionary-select-dictionary (key-binding "S")))
        (should (eq 'osx-dictionary-open-dictionary.app (key-binding "o")))
        (should (eq 'osx-dictionary-read-word (key-binding "r"))))
      ;; Only in this buffer: the package's keymap is unchanged.
      (should (eq 'osx-dictionary-quit (keymap-lookup osx-dictionary-mode-map "q")))
      (should (eq 'osx-dictionary-search-input (keymap-lookup osx-dictionary-mode-map "s")))
      ;; In a launcher interaction, q and s go back to the query.
      (let* ((backs 0)
             (launcher--back-function (lambda () (cl-incf backs))))
        (with-current-buffer buffer
          (call-interactively (key-binding "q"))
          (call-interactively (key-binding "s")))
        (should (= 2 backs)))
      (should (equal '(("hello")) (launcher-osx-dictionary-test--calls))))))

(ert-deftest launcher-osx-dictionary-keys-outside-an-interaction ()
  ;; q quits the window it was shown in, not the package's saved
  ;; configuration; s and S look up again in this buffer, displaying
  ;; nothing else.
  (launcher-osx-dictionary-test--with
    (let ((origin (window-buffer))
          (buffer (launcher-osx-dictionary-lookup "hello")))
      (delete-other-windows)
      (save-window-excursion
        (split-window)
        (split-window)
        (setq osx-dictionary-previous-window-configuration
              (current-window-configuration)))
      (let ((saved osx-dictionary-previous-window-configuration))
        (pop-to-buffer buffer)
        (let ((layout (launcher-osx-dictionary-test--layout)))
          (cl-letf (((symbol-function 'read-string)
                     (lambda (prompt &rest _)
                       (should (equal "Word (default hello): " prompt))
                       "world"))
                    ((symbol-function 'completing-read)
                     (lambda (_prompt collection &rest _)
                       (should (equal '("Fake Dictionary" "Other Dictionary"
                                        "All active dictionaries")
                                      collection))
                       "Other Dictionary")))
            (execute-kbd-macro "s")
            (should (equal "world" (buffer-local-value 'osx-dictionary--current-word buffer)))
            (execute-kbd-macro "S"))
          (should (equal "Other Dictionary" osx-dictionary-current-dictionary))
          (should (equal "world" (buffer-local-value 'osx-dictionary--current-word buffer)))
          (should (equal layout (launcher-osx-dictionary-test--layout)))
          (should-not (get-buffer "*osx-dictionary*")))
        (with-current-buffer buffer
          (execute-kbd-macro "q"))
        (should (one-window-p))
        (should (eq origin (window-buffer)))
        (should (eq saved osx-dictionary-previous-window-configuration)))
      (should (equal '(("hello") ("world") ("-l") ("-d" "Other Dictionary" "world"))
                     (launcher-osx-dictionary-test--calls))))))

;;; Through the launcher

(ert-deftest launcher-osx-dictionary-through-both-commands ()
  ;; Under a configured prefix, the result shows in the launcher's
  ;; window, where q returns to the query, or by ordinary display.
  (launcher-osx-dictionary-test--with
    (let ((tools '(("w" :name "Dictionary" :prompt "Word: "
                    :function launcher-osx-dictionary-lookup)))
          shown)
      (launcher-tools-test--with
        (let ((launcher-tools tools))
          (should (eq 'quit (launcher-tools-test--run
                             #'launcher-buffer '(("w hello"))
                             (list '("hello" (:key "RET"))
                                   (list (lambda ()
                                           (setq shown (launcher-buffer--session-shown
                                                        launcher-buffer--session)))
                                         'quit))
                             "q")))
          (should (memq (get-buffer launcher-osx-dictionary-buffer-name) shown))
          (should (equal '(("" nil) ("hello" nil))
                         (mapcar (lambda (query) (list (nth 1 query) (nth 3 query)))
                                 (launcher-tools-test--events 'query))))
          (should-not (launcher-tools-test--events 'launch))
          (should-not (launcher-tools-test--events 'search))))
      (launcher-tools-test--with
        (let ((launcher-tools tools))
          (should (eq 'accepted (launcher-tools-test--run
                                 #'launcher '(("w hello")) '(("hello" (:key "RET"))))))
          (should (equal launcher-osx-dictionary-buffer-name
                         (buffer-name (window-buffer))))
          (should-not (launcher-tools-test--events 'launch))
          (should-not (launcher-tools-test--events 'search)))))
    (should (equal '(("hello") ("hello")) (launcher-osx-dictionary-test--calls)))))

(ert-deftest launcher-osx-dictionary-unknown-word-keeps-the-query ()
  (launcher-osx-dictionary-test--with
    (launcher-tools-test--with
      (let ((launcher-tools '(("d" :name "Dictionary" :prompt "Word: "
                               :function launcher-osx-dictionary-lookup))))
        (should (eq 'quit (launcher-tools-test--run
                           #'launcher-buffer '(("d qwxzv"))
                           '(("qwxzv" (:key "RET")) quit))))
        (should (equal '(("qwxzv" "Dictionary failed: No definition for \"qwxzv\" \
\(searched: All active dictionaries)"))
                       (cdr (mapcar (lambda (query) (list (nth 1 query) (nth 3 query)))
                                    (launcher-tools-test--events 'query)))))
        (should-not (launcher-tools-test--events 'search))))))

;;; launcher-osx-dictionary-tests.el ends here
