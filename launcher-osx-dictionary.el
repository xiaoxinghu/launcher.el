;;; launcher-osx-dictionary.el --- Apple Dictionary tool for launcher -*- lexical-binding: t; -*-

;;; Commentary:

;; `launcher-osx-dictionary-lookup' is a handler for `launcher-tools': it
;; looks a word up in Apple's dictionaries with the osx-dictionary package
;; (https://github.com/xuchunyang/osx-dictionary.el) and returns the
;; package's rendering of the result, without displaying it.  Register it
;; under a prefix of your choice:
;;
;;   (require 'launcher-osx-dictionary)
;;   (setq launcher-tools
;;         '(("d" :name "Dictionary" :prompt "Word: "
;;                 :function launcher-osx-dictionary-lookup)))
;;
;; Loading this file loads neither osx-dictionary nor its helper; the
;; first lookup does, and reports what is missing instead of looking up.
;; Needs macOS, osx-dictionary and Xcode's command line tools, with which
;; the first lookup builds the package's helper if it is missing.
;;
;; Tested with osx-dictionary commit 655bca5 (2026-09-06).  It has no
;; public function that looks a word up without prompting or displaying,
;; so this file uses its private `osx-dictionary--insert-search-result',
;; `osx-dictionary--current-dictionary-description',
;; `osx-dictionary--current-word' and `osx-dictionary--load-dir', and
;; reports a version that lacks them.

;;; Code:

(require 'launcher)
(require 'nadvice)

(defvar osx-dictionary-cli)
(defvar osx-dictionary--load-dir)
(defvar osx-dictionary--current-word)
(defvar osx-dictionary-current-dictionary)
(defvar osx-dictionary-search-log-file)
(declare-function osx-dictionary-mode "osx-dictionary" ())
(declare-function osx-dictionary--insert-search-result "osx-dictionary" (word))
(declare-function osx-dictionary--current-dictionary-description "osx-dictionary" ())
(declare-function osx-dictionary-select-dictionary "osx-dictionary" (&optional dictionary))

(defcustom launcher-osx-dictionary-buffer-name "*Launcher Dictionary*"
  "Name of the buffer `launcher-osx-dictionary-lookup' shows results in.
It differs from osx-dictionary's own buffer, so that lookups from the
launcher leave the package's own session alone."
  :type 'string
  :group 'launcher)

(defconst launcher-osx-dictionary--interface
  '(osx-dictionary-mode osx-dictionary--insert-search-result
    osx-dictionary--current-dictionary-description osx-dictionary-select-dictionary
    osx-dictionary-cli osx-dictionary--load-dir osx-dictionary--current-word
    osx-dictionary-current-dictionary)
  "Functions and variables of osx-dictionary this file uses.")

(defun launcher-osx-dictionary--require ()
  "Load osx-dictionary, or signal an error saying what is missing."
  (unless (eq system-type 'darwin)
    (error "Apple dictionaries are only available on macOS"))
  (unless (require 'osx-dictionary nil t)
    (error "Needs the osx-dictionary package: M-x package-install RET osx-dictionary"))
  (when-let* ((missing (seq-remove (lambda (symbol)
                                     (or (fboundp symbol) (boundp symbol)))
                                   launcher-osx-dictionary--interface)))
    (error "This osx-dictionary lacks %s; launcher-osx-dictionary was tested with commit 655bca5"
           (mapconcat #'symbol-name missing ", "))))

(defun launcher-osx-dictionary--find-helper ()
  "Return the osx-dictionary helper, where the package looks for it, or nil."
  (or (executable-find (expand-file-name osx-dictionary-cli osx-dictionary--load-dir))
      (executable-find osx-dictionary-cli)))

(defun launcher-osx-dictionary--developer-tools-p ()
  "Return non-nil if Xcode's command line tools are installed.
Without them, Apple's clang stub would offer to install them instead."
  (and (executable-find "xcode-select")
       (eql 0 (call-process "xcode-select" nil nil nil "-p"))))

(defun launcher-osx-dictionary--build-helper ()
  "Build the osx-dictionary helper where the package looks for it first.
Return its file name, or signal an error saying why it could not."
  (let ((source (expand-file-name "osx-dictionary.m" osx-dictionary--load-dir))
        (output (expand-file-name osx-dictionary-cli osx-dictionary--load-dir)))
    (unless (file-readable-p source)
      (error "The osx-dictionary helper source %s is missing; reinstall the package"
             (abbreviate-file-name source)))
    (unless (and (executable-find "clang") (launcher-osx-dictionary--developer-tools-p))
      (error "Building the osx-dictionary helper needs the Xcode command line tools: \
run xcode-select --install"))
    (unless (file-writable-p output)
      (error "Cannot build the osx-dictionary helper: %s is not writable"
             (abbreviate-file-name osx-dictionary--load-dir)))
    (message "Building the osx-dictionary helper...")
    (with-temp-buffer
      ;; The command of `osx-dictionary-recompile', which hides failures.
      (let ((status (call-process "clang" nil t nil "-O3"
                                  "-framework" "CoreServices" "-framework" "Foundation"
                                  source "-o" output)))
        (unless (and (eql status 0) (file-executable-p output))
          (error "Building the osx-dictionary helper failed (%s): %s" status
                 (string-trim (buffer-substring (point-min)
                                                (min (point-max) 1000)))))))
    (message "Building the osx-dictionary helper...done")
    output))

(defun launcher-osx-dictionary--dictionaries (helper)
  "Return the dictionaries HELPER lists, or signal an error if it fails."
  (with-temp-buffer
    (let ((status (call-process helper nil '(t nil) nil "-l")))
      (unless (eql status 0)
        (error "The osx-dictionary helper failed (%s)" status))
      (split-string (buffer-string) "\n" t))))

(defun launcher-osx-dictionary--no-result (word helper)
  "Signal an error saying why HELPER found nothing for WORD."
  (let ((installed (launcher-osx-dictionary--dictionaries helper)))
    (cond ((null installed)
           (error "Dictionary.app lists no installed dictionaries"))
          ((and osx-dictionary-current-dictionary
                (not (member osx-dictionary-current-dictionary installed)))
           (error "Dictionary %S is not installed; choose another with M-x \
osx-dictionary-select-dictionary" osx-dictionary-current-dictionary))
          (t
           (error "No definition for %S (searched: %s)" word
                  (osx-dictionary--current-dictionary-description))))))

(defun launcher-osx-dictionary--search (word)
  "Return osx-dictionary's rendering of WORD's definitions, as a string.
Signal an error if it finds none."
  (let ((helper (or (launcher-osx-dictionary--find-helper)
                    (launcher-osx-dictionary--build-helper))))
    (with-temp-buffer
      ;; Rendered as `osx-dictionary--view-result' does, but here.
      (osx-dictionary--insert-search-result word)
      (when (string-blank-p (buffer-string))
        (launcher-osx-dictionary--no-result word helper))
      (whitespace-cleanup)
      (buffer-string))))

(defvar-keymap launcher-osx-dictionary-result-mode-map
  :doc "Keys of launcher's dictionary results, over `osx-dictionary-mode-map'."
  "q" #'launcher-osx-dictionary-quit
  "s" #'launcher-osx-dictionary-search
  "S" #'launcher-osx-dictionary-select-dictionary)

(define-minor-mode launcher-osx-dictionary-result-mode
  "Minor mode of launcher's dictionary results.
It replaces the `osx-dictionary-mode' keys that would display the
package's own buffer, or restore the window configuration it saved,
in this buffer only:
\\{launcher-osx-dictionary-result-mode-map}"
  :lighter nil)

(defun launcher-osx-dictionary--render (word text)
  "Show TEXT, the definitions of WORD, in the result buffer and return it.
Display nothing."
  (let ((buffer (get-buffer-create launcher-osx-dictionary-buffer-name)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'osx-dictionary-mode)
        (osx-dictionary-mode))
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert text))
      ;; For the package's commands that act on the word, such as `o'.
      (setq osx-dictionary--current-word word)
      (launcher-osx-dictionary-result-mode 1)
      (goto-char (point-min))
      (dolist (window (get-buffer-window-list buffer nil t))
        (set-window-point window (point-min))
        (set-window-start window (point-min))))
    buffer))

;;;###autoload
(defun launcher-osx-dictionary-lookup (query)
  "Look QUERY up in Apple's dictionaries; return the result buffer.
A handler for `launcher-tools'.  Look up QUERY without surrounding
whitespace, in the dictionaries osx-dictionary is set to search, and
render the result as the package does, in the buffer named by
`launcher-osx-dictionary-buffer-name', without displaying it.  The
buffer is in `osx-dictionary-mode', with the keys of
`launcher-osx-dictionary-result-mode'.

Signal an error, leaving the buffer as it was, if osx-dictionary or
its helper is missing or fails, or if there is no definition.  Build
the helper first if it is missing, which blocks Emacs meanwhile, as
does the lookup."
  (let ((word (string-trim query)))
    (when (string-empty-p word)
      (error "Nothing to look up"))
    (launcher-osx-dictionary--require)
    ;; The package runs its helper through a shell in this directory,
    ;; which must be local.  A relative search log stays the caller's.
    (let* ((osx-dictionary-search-log-file
            (and osx-dictionary-search-log-file
                 (expand-file-name osx-dictionary-search-log-file)))
           (default-directory (file-name-as-directory osx-dictionary--load-dir)))
      (launcher-osx-dictionary--render word (launcher-osx-dictionary--search word)))))

(defun launcher-osx-dictionary-quit ()
  "Leave this dictionary result.
In a launcher interaction, return to its query, as `launcher-back'.
Otherwise quit the window as `quit-window' does, instead of restoring
the window configuration osx-dictionary saved for its own buffer."
  (interactive)
  (if launcher--back-function
      (launcher-back)
    (quit-window)))

(defun launcher-osx-dictionary-search ()
  "Look up another word.
In a launcher interaction, return to its query to edit the word, as
`launcher-back'.  Otherwise read a word and show it in this buffer."
  (interactive)
  (if launcher--back-function
      (launcher-back)
    (launcher-osx-dictionary-lookup
     (read-string (format-prompt "Word" osx-dictionary--current-word)
                  nil nil osx-dictionary--current-word))))

(defun launcher-osx-dictionary-select-dictionary ()
  "Choose the dictionaries to search, then look this word up again here.
Prompt as `osx-dictionary-select-dictionary', which saves the choice
for later lookups, but show the word in this buffer, not in the
package's own."
  (interactive)
  (let* ((word osx-dictionary--current-word)
         (default-directory (file-name-as-directory osx-dictionary--load-dir))
         (args (advice-eval-interactive-spec
                (cadr (interactive-form #'osx-dictionary-select-dictionary)))))
    ;; Outside its mode, the command does not show the word itself.
    (with-temp-buffer
      (apply #'osx-dictionary-select-dictionary args))
    (when word
      (launcher-osx-dictionary-lookup word))))

(provide 'launcher-osx-dictionary)
;;; launcher-osx-dictionary.el ends here
