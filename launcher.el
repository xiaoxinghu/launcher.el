;;; launcher.el --- Launch macOS apps from Emacs -*- lexical-binding: t; -*-

(require 'subr-x)
(require 'seq)
(require 'url-util)

(defgroup launcher nil
  "Launch macOS applications from Emacs."
  :group 'external)

(defcustom launcher-mdfind-query
  "kMDItemContentTypeTree == \"com.apple.application-bundle\""
  "Spotlight query used to discover application bundles."
  :type 'string
  :group 'launcher)

(defcustom launcher-app-directories
  '("/Applications"
    "/System/Applications"
    "/System/Library/CoreServices"
    "~/Applications")
  "Directories to search for applications.
These are the standard macOS application directories.
User-specific directories can be added here."
  :type '(repeat string)
  :group 'launcher)

(defcustom launcher-fallback-search-url
  "https://www.google.com/search?q=%s"
  "URL template for fallback web search when no app matches.
%s will be replaced with the URL-encoded search query."
  :type 'string
  :group 'launcher)

(defcustom launcher-bangs
  '(("!g"  "Google"  "https://www.google.com/search?q=%s")
    ("!yt" "YouTube" "https://www.youtube.com/results?search_query=%s")
    ("!gh" "GitHub"  "https://github.com/search?q=%s"))
  "Bang shortcuts for direct web searches.
Each entry is (BANG NAME URL-TEMPLATE).  BANG is the trigger prefix
\(e.g. \"!g\"), NAME is a human-readable label shown in annotations,
and URL-TEMPLATE has %s replaced with the URL-encoded search query.
Type BANG followed by a space in the launcher prompt to enter bang mode."
  :type '(repeat (list string string string))
  :group 'launcher)

(defvar launcher--apps nil
  "Cached app entries as an alist of (display-name . app-path).")

(defvar launcher--current-entries nil
  "Entries available to completion metadata annotators.")

(defun launcher--available-p (program)
  "Return non-nil when PROGRAM exists in PATH."
  (executable-find program))

(defun launcher--app-name-from-path (path)
  "Return app display name parsed from PATH."
  (file-name-base (directory-file-name path)))

(defun launcher--collect-paths ()
  "Return discovered .app paths from standard Application directories.
Searches only in `launcher-app-directories' using Spotlight's -onlyin flag."
  (unless (launcher--available-p "mdfind")
    (user-error "Cannot find `mdfind` in PATH"))
  (let (results)
    (dolist (dir launcher-app-directories)
      (let ((expanded-dir (expand-file-name dir)))
        (when (file-directory-p expanded-dir)
          (setq results
                (append results
                        (process-lines "mdfind"
                                       "-onlyin" expanded-dir
                                       launcher-mdfind-query))))))
    (seq-filter
     (lambda (path)
       (and (string-suffix-p ".app" path)
            (file-directory-p path)))
     results)))

(defun launcher--format-display-name (name path duplicate-p)
  "Return display label for NAME and PATH.
When DUPLICATE-P is non-nil, include path context."
  (if duplicate-p
      (format "%s  (%s)" name (abbreviate-file-name path))
    name))

(defun launcher--build-entries (paths)
  "Convert PATHS to an alist of (display-name . app-path)."
  (let ((counts (make-hash-table :test #'equal))
        entries)
    (dolist (path paths)
      (let ((name (launcher--app-name-from-path path)))
        (puthash name (1+ (gethash name counts 0)) counts)))
    (dolist (path paths)
      (let* ((name (launcher--app-name-from-path path))
             (duplicate-p (> (gethash name counts 0) 1))
             (display (launcher--format-display-name
                       name path duplicate-p)))
        (push (cons display path) entries)))
    (sort entries (lambda (a b) (string-lessp (car a) (car b))))))

(defun launcher-refresh ()
  "Refresh the cached list of launchable macOS apps."
  (interactive)
  (setq launcher--apps
        (launcher--build-entries (launcher--collect-paths)))
  (message "Indexed %d applications" (length launcher--apps)))

(defun launcher--ensure-index ()
  "Ensure app index is loaded and return it."
  (unless launcher--apps
    (launcher-refresh))
  launcher--apps)

(defun launcher--launch (app-path)
  "Launch APP-PATH with macOS `open`."
  (unless (launcher--available-p "open")
    (user-error "Cannot find `open` in PATH"))
  (unless (and app-path (file-directory-p app-path))
    (user-error "Invalid app path: %S" app-path))
  (let ((exit-code (call-process "open" nil nil nil "-a" app-path)))
    (unless (zerop exit-code)
      (user-error "Failed to launch app: %s" app-path))))

(defun launcher--bang-for-input (input)
  "Return the bang entry whose key prefixes INPUT (BANG SPACE ...), or nil.
INPUT must begin with a bang key from `launcher-bangs' followed by a space."
  (when-let* ((space-pos (string-match " " input))
              (bang-key (substring input 0 space-pos)))
    (seq-find (lambda (entry) (equal (car entry) bang-key))
              launcher-bangs)))

(defun launcher--annotation (candidate)
  "Return a completion annotation for CANDIDATE."
  (if-let* ((bang-entry (seq-find (lambda (e) (equal (car e) candidate))
                                  launcher-bangs)))
      (format "  → %s search" (cadr bang-entry))
    (when-let* ((path (cdr (assoc candidate launcher--current-entries))))
      (concat "  " (abbreviate-file-name path)))))

(defun launcher--make-collection (entries)
  "Return a dynamic completion collection for ENTRIES with bang support.
In normal mode completes against app names and bang shortcut keys.
Once the input is BANG SPACE (e.g. \"!g \"), enters bang mode: the
candidate list is cleared and any query typed after the space is passed
directly to the matching search engine on Enter."
  (let ((names (mapcar #'car entries))
        (bang-keys (mapcar #'car launcher-bangs)))
    (lambda (string pred action)
      ;; Read the real minibuffer contents rather than the `string' argument:
      ;; completion frameworks like vertico+orderless call the collection with
      ;; string="" to fetch all candidates and then filter themselves, so the
      ;; `string' argument never contains the full "!g ..." the user typed.
      (let* ((full-input (if (active-minibuffer-window)
                             (with-current-buffer
                                 (window-buffer (active-minibuffer-window))
                               (minibuffer-contents))
                           string))
             (in-bang-mode (launcher--bang-for-input full-input)))
        (if in-bang-mode
            ;; Bang mode: no candidates, accept any input as-is.
            (cond
             ((eq action 'metadata) '(metadata))
             ((consp action) nil)           ; (boundaries . suffix)
             ((eq action nil) nil)          ; try-completion: nothing to complete
             ((eq action t) '())            ; all-completions: empty list
             ((eq action 'lambda) t))       ; test-completion: always valid
          ;; Normal mode: complete against bang keys + app names.
          (complete-with-action action (append bang-keys names) string pred))))))

(defun launcher--read-choice (collection &optional initial setup)
  "Read from COLLECTION, allowing spaces and cancelling empty queries.
Keep the user's completion reader and any custom Space binding.
INITIAL is the initial input.  SETUP, if non-nil, is called with no
arguments in the new minibuffer, after the reader's own setup."
  (let* ((choice
          (minibuffer-with-setup-hook
              (lambda ()
                (when (eq (key-binding " ") #'minibuffer-complete-word)
                  (let ((map (make-sparse-keymap)))
                    (set-keymap-parent map (current-local-map))
                    (define-key map " " #'self-insert-command)
                    (use-local-map map)))
                (when setup
                  (funcall setup)))
            (completing-read "Launch: " collection nil nil initial)))
         (words (split-string choice)))
    (when (or (null words)
              (and (null (cdr words)) (assoc (car words) launcher-bangs)))
      (signal 'quit nil))
    choice))

(defun launcher--entries (refresh)
  "Return the app entries, rebuilding the index first when REFRESH."
  (when refresh
    (launcher-refresh))
  (or (launcher--ensure-index)
      (user-error "No apps discovered from Spotlight index")))

(defun launcher--read (entries &optional initial setup)
  "Read a launcher choice among ENTRIES, with app annotations.
INITIAL and SETUP are as for `launcher--read-choice'.  Signal `quit'
for empty input or a bang without a query."
  (let ((completion-extra-properties
         '(:annotation-function launcher--annotation)))
    (unwind-protect
        (progn
          (setq launcher--current-entries entries)
          (launcher--read-choice (launcher--make-collection entries)
                                 initial setup))
      (setq launcher--current-entries nil))))

(defun launcher--act (choice entries)
  "Launch the app CHOICE names among ENTRIES, or search the web for it.
A bang prefix searches its engine; other unmatched input falls back to
`launcher-fallback-search-url'."
  (let ((bang-entry (launcher--bang-for-input choice))
        (path (cdr (assoc choice entries))))
    (cond
     (bang-entry
      (let ((query (substring choice (1+ (length (car bang-entry))))))
        (browse-url (format (caddr bang-entry)
                            (url-hexify-string query)))
        (message "Searching %s for: %s" (cadr bang-entry) query)))
     (path
      (launcher--launch path)
      (message "Launching %s" choice))
     (t
      (browse-url (format launcher-fallback-search-url
                          (url-hexify-string choice)))
      (message "Searching Google for: %s" choice)))))

;;;###autoload
(defun launcher (&optional refresh)
  "Prompt for a macOS app from Spotlight index and launch it.
With prefix argument REFRESH, rebuild app index first.
Type a bang shortcut followed by a space to search the web directly:
  !g  → Google   !yt → YouTube   !gh → GitHub
If input does not match any app, search for it on Google.
Empty input or a bang without a query cancels without launching or searching.
Space inserts a space in stock completion; Tab still completes.
See `launcher-buffer' for the same choices at the top of a window."
  (interactive "P")
  (let ((entries (launcher--entries refresh)))
    (launcher--act (launcher--read entries) entries)))

(declare-function launcher-buffer--interact "launcher-buffer" (refresh))

;;;###autoload
(defun launcher-buffer (&optional refresh)
  "Launch an app or search the web from the top of the selected window.
Offer the same choices as `launcher', with Vertico showing the prompt
and candidates in the selected ordinary window instead of the
minibuffer.  With prefix argument REFRESH, rebuild the app index first.

Return only when a choice succeeds or the interaction ends.  Escape or
C-g quits from any view, signaling `quit'.  From a later view, C-c C-b
\(`launcher-back') returns to the previous one; see `launcher-buffer-map'.
On exit, the window shows its previous buffer again, unless it shows a
buffer the interaction did not put there.

The window keeps its size.  The picker shows up to `vertico-count'
candidates, as bound when this command starts, or as many as a shorter
window fits; moving the selection scrolls through the rest.

Needs Vertico 2.x, with `vertico-mode' enabled.  The interface is in
launcher-buffer.el, loaded on first use."
  (interactive "P")
  (require 'launcher-buffer)
  (launcher-buffer--interact refresh))

(provide 'launcher)
;;; launcher.el ends here
