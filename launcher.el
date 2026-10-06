;;; launcher.el --- Launch macOS apps from Emacs -*- lexical-binding: t; -*-

(require 'cl-lib)
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

(defcustom launcher-tools nil
  "Prefixes that route launcher input to free-text tools.
Each entry is (PREFIX :name NAME :prompt PROMPT :function FUNCTION),
optionally with :history HISTORY.  For example:

  (setq launcher-tools
        \\='((\"d\" :name \"Dictionary\" :prompt \"Word: \"
                :function my-dictionary-lookup)))

Input that starts with PREFIX and a space leaves the application list
for the tool's query, labelled \"NAME — PROMPT\": raw text, with no
candidates and no web search.  Return calls FUNCTION once with the
query, the text after \"PREFIX \" as typed, and shows the buffer it
returns.  PREFIX alone, or followed by anything but a space, is
ordinary input.

PREFIX is a nonempty string without whitespace, such as \"d\", \"dd\"
or \"!d\".  It matches case-sensitively and must differ from the other
tools' prefixes and from the keys of `launcher-bangs'.  NAME is a
nonempty string and PROMPT a string.

FUNCTION is a function, such as a closure, or a function's name, which
may be autoloaded.  It takes the query, a nonblank string, and returns
a live buffer showing the result; any normalization of the query is up
to it.  It should neither prompt nor display the buffer itself, and
may keep updating the buffer after returning.  If it signals an error
or returns anything else, the query reports that and keeps its text.

Each tool keeps its own query history for this session only.  To keep
it like other minibuffer history, for example with `savehist-mode',
give a variable as HISTORY; t keeps no history.

Commands validate this list when they start, and signal a `user-error'
for an invalid entry."
  :type '(repeat (cons :tag "Tool"
                       (string :tag "Prefix")
                       (plist :tag "Properties"
                              :options ((:name string)
                                        (:prompt string)
                                        (:function function)
                                        (:history symbol)))))
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

;;; Tools

(cl-defstruct (launcher--tool
               (:constructor launcher--tool-make)
               (:copier nil))
  "A validated entry of `launcher-tools'."
  prefix name prompt function history)

(defvar launcher--tool-histories (make-hash-table :test #'equal)
  "Query history variables of tools without :history, by prefix.
They are uninterned, so `savehist-mode' does not save them.")

(defun launcher--tool-history-default (prefix)
  "Return the session's query history variable for tools with PREFIX."
  (or (gethash prefix launcher--tool-histories)
      (let ((symbol (make-symbol (format "launcher-%s-history" prefix))))
        (set symbol nil)
        (puthash prefix symbol launcher--tool-histories))))

(defun launcher--tool-from-entry (entry)
  "Return the tool ENTRY of `launcher-tools' describes.
Signal a `user-error' explaining what is wrong with an invalid ENTRY."
  (cl-flet ((invalid (format &rest args)
              (user-error "Invalid `launcher-tools' entry %S: %s"
                          entry (apply #'format format args))))
    (unless (and (consp entry) (proper-list-p (cdr entry))
                 (cl-evenp (length (cdr entry))))
      (invalid "expected (PREFIX :name NAME :prompt PROMPT :function FUNCTION)"))
    (let ((prefix (car entry))
          (props (cdr entry)))
      (unless (and (stringp prefix)
                   (not (string-empty-p prefix))
                   (not (string-match-p "[[:blank:]\n\v\f\r]" prefix)))
        (invalid "PREFIX must be a nonempty string without whitespace"))
      (cl-loop for key in props by #'cddr
               unless (memq key '(:name :prompt :function :history))
               do (invalid "unknown property %S" key))
      (let ((name (plist-get props :name))
            (prompt (plist-get props :prompt))
            (function (plist-get props :function))
            (history (plist-get props :history)))
        (unless (and (stringp name) (not (string-empty-p name)))
          (invalid ":name must be a nonempty string"))
        (unless (stringp prompt)
          (invalid ":prompt must be a string"))
        (unless (or (functionp function)
                    (and function (symbolp function) (not (keywordp function))
                         (not (eq function t))))
          (invalid ":function must be a function or its name"))
        (unless (and (symbolp history) (not (keywordp history)))
          (invalid ":history must be a variable, or t for none"))
        (launcher--tool-make
         :prefix prefix :name name :prompt prompt :function function
         :history (or history (launcher--tool-history-default prefix)))))))

(defun launcher--tools ()
  "Return the tools of `launcher-tools', validated, for one interaction.
Signal a `user-error' for an invalid entry, a prefix of two tools, or a
prefix that is also a key of `launcher-bangs'."
  (unless (proper-list-p launcher-tools)
    (user-error "`launcher-tools' must be a list of tools: %S" launcher-tools))
  (let (tools)
    (dolist (entry launcher-tools)
      (let* ((tool (launcher--tool-from-entry entry))
             (prefix (launcher--tool-prefix tool)))
        (when (seq-find (lambda (other) (equal (launcher--tool-prefix other) prefix))
                        tools)
          (user-error "Two `launcher-tools' entries use the prefix %S" prefix))
        (when (assoc prefix launcher-bangs)
          (user-error "`launcher-tools' prefix %S is also a key of `launcher-bangs'"
                      prefix))
        (push tool tools)))
    (nreverse tools)))

(defun launcher--route (input tools)
  "Return (TOOL . QUERY) if INPUT starts with the prefix of one of TOOLS.
The prefix must be followed by an ASCII space, and matches exactly,
case included.  QUERY is the rest of INPUT, unchanged.  Return nil for
any other INPUT, which is ordinary app or web input."
  (when-let* (((stringp input))
              (space (string-search " " input))
              (prefix (substring input 0 space))
              (tool (seq-find (lambda (tool) (equal (launcher--tool-prefix tool) prefix))
                              tools)))
    (cons tool (substring input (1+ space)))))

(defvar-keymap launcher-query-map
  :doc "Keymap of a launcher tool's query, read as plain text.
Return submits a nonblank query.  \\[launcher-back], or deleting
backward in an empty query, returns to the application picker."
  :parent minibuffer-local-map
  "C-c C-b" #'launcher-back
  "<remap> <exit-minibuffer>" #'launcher-query-submit
  "<remap> <delete-backward-char>" #'launcher-query-delete-backward-char)

(defvar launcher--back-function nil
  "Function returning to the previous view of the active interaction.
Each launcher interaction binds it while it reads input or shows a view.")

(defun launcher-back ()
  "Return to the previous view of the active launcher interaction.
From a tool's result, return to its query, keeping the text; from a
query, return to the application picker."
  (interactive)
  (unless launcher--back-function
    (user-error "No launcher interaction is active"))
  (funcall launcher--back-function))

(defun launcher-query-submit ()
  "Submit the launcher tool's query, unless it is blank."
  (interactive)
  (if (string-blank-p (minibuffer-contents-no-properties))
      (minibuffer-message "Type a query first")
    (exit-minibuffer)))

(defun launcher-query-delete-backward-char ()
  "Delete the previous character, or leave an empty query.
In an empty query, return to the application picker, as `launcher-back'."
  (interactive)
  (if (string-empty-p (minibuffer-contents-no-properties))
      (launcher-back)
    (call-interactively #'delete-backward-char)))

(put 'launcher-query-delete-backward-char 'delete-selection 'supersede)

(defun launcher--read-query (tool &optional initial notice setup)
  "Read a query for TOOL as plain text, starting with INITIAL.
Show NOTICE in the minibuffer, if non-nil.  SETUP, if non-nil, is
called with no arguments in the new minibuffer.  Use TOOL's history."
  (minibuffer-with-setup-hook
      (lambda ()
        (when setup
          (funcall setup))
        (when notice
          ;; In an active minibuffer, shown after the input.
          (message "%s" notice)))
    (read-from-minibuffer (format "%s — %s" (launcher--tool-name tool)
                                  (launcher--tool-prompt tool))
                          initial launcher-query-map nil
                          (launcher--tool-history tool))))

(defun launcher--invoke (tool query)
  "Call TOOL's function with QUERY and return the buffer it returns.
Signal an error if the function is undefined, or if it returns anything
but a live buffer."
  (let ((function (launcher--tool-function tool))
        (name (launcher--tool-name tool)))
    (unless (functionp function)
      (error "%s needs `%s', which is not defined" name function))
    (let ((result (funcall function query)))
      (unless (and (buffer-live-p result) (not (minibufferp result)))
        (error "%s returned %S instead of a live buffer" name result))
      result)))

(defun launcher--query (tool &optional initial setup)
  "Read TOOL's query, starting with INITIAL, and return its result.
Call TOOL's function once per nonblank submission.  If it fails, read
the query again, keeping its text and reporting the error.  Return
\(BUFFER . QUERY), the buffer the function returned for QUERY.  SETUP
is as for `launcher--read-query'."
  (let ((query initial)
        notice result)
    (while (not result)
      (setq query (launcher--read-query tool query notice setup))
      (if (string-blank-p query)
          (setq notice "Type a query first")
        (condition-case err
            (setq result (launcher--invoke tool query))
          (error
           (setq notice (format "%s failed: %s" (launcher--tool-name tool)
                                (error-message-string err)))))))
    (cons result query)))

;;; Picker

(defun launcher--annotation (candidate)
  "Return a completion annotation for CANDIDATE."
  (if-let* ((bang-entry (seq-find (lambda (e) (equal (car e) candidate))
                                  launcher-bangs)))
      (format "  → %s search" (cadr bang-entry))
    (when-let* ((path (cdr (assoc candidate launcher--current-entries))))
      (concat "  " (abbreviate-file-name path)))))

(defun launcher--make-collection (entries &optional tools)
  "Return a dynamic completion collection for ENTRIES with bang support.
In normal mode completes against app names and bang shortcut keys.
Once the input is BANG SPACE (e.g. \"!g \"), enters bang mode: the
candidate list is cleared and any query typed after the space is passed
directly to the matching search engine on Enter.  Input routed to one
of TOOLS has no candidates either."
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
             (in-bang-mode (or (launcher--bang-for-input full-input)
                               (launcher--route full-input tools))))
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

(defun launcher--read-choice (prompt collection tools &optional initial setup)
  "Read with PROMPT from COLLECTION, allowing spaces and cancelling empty queries.
Keep the user's completion reader and any custom Space binding.
INITIAL is the initial input.  SETUP, if non-nil, is called with no
arguments in the new minibuffer, after the reader's own setup.

Return the input as soon as it is routed to one of TOOLS, whether
typed, pasted or recalled from history, without adding it to the
history.  Do not read at all if INITIAL is routed to a tool."
  (if (launcher--route initial tools)
      initial
    (let* ((tag (make-symbol "launcher--routed"))
           (choice
            (catch tag
              (minibuffer-with-setup-hook
                  (lambda ()
                    (when (eq (key-binding " ") #'minibuffer-complete-word)
                      (let ((map (make-sparse-keymap)))
                        (set-keymap-parent map (current-local-map))
                        (define-key map " " #'self-insert-command)
                        (use-local-map map)))
                    (when tools
                      (add-hook 'post-command-hook
                                (lambda ()
                                  (let ((input (minibuffer-contents-no-properties)))
                                    (when (launcher--route input tools)
                                      (throw tag input))))
                                nil t))
                    (when setup
                      (funcall setup)))
                (completing-read prompt collection nil nil initial))))
           (words (split-string choice)))
      (when (and (not (launcher--route choice tools))
                 (or (null words)
                     (and (null (cdr words)) (assoc (car words) launcher-bangs))))
        (signal 'quit nil))
      choice)))

(defun launcher--entries (refresh &optional tools)
  "Return the app entries, rebuilding the index first when REFRESH.
Signal an error if discovery fails or finds no apps, unless there are
TOOLS: then report the problem and return nil, keeping them reachable."
  (condition-case err
      (progn
        (when refresh
          (launcher-refresh))
        (or (launcher--ensure-index)
            (user-error "No apps discovered from Spotlight index")))
    (error
     (unless tools
       (signal (car err) (cdr err)))
     (message "Launcher apps unavailable: %s" (error-message-string err))
     nil)))

(defun launcher--read (entries tools &optional initial setup)
  "Read a launcher choice among ENTRIES and TOOLS, with app annotations.
INITIAL and SETUP are as for `launcher--read-choice'.  Signal `quit'
for empty input or a bang without a query.  Without ENTRIES, the
prompt says that apps are unavailable."
  (let ((completion-extra-properties
         '(:annotation-function launcher--annotation)))
    (unwind-protect
        (progn
          (setq launcher--current-entries entries)
          (launcher--read-choice (if entries "Launch: " "Launch (apps unavailable): ")
                                 (launcher--make-collection entries tools)
                                 tools initial setup))
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

(defun launcher--minibuffer-query (tool query)
  "Read TOOL's query in the minibuffer, starting with QUERY.
Return the buffer TOOL's function returns, or nil to go back."
  (let ((tag (make-symbol "launcher--back")))
    (catch tag
      (let ((launcher--back-function (lambda () (throw tag nil))))
        (car (launcher--query tool query))))))

;;;###autoload
(defun launcher (&optional refresh)
  "Prompt for a macOS app from Spotlight index and launch it.
With prefix argument REFRESH, rebuild app index first.
Type a bang shortcut followed by a space to search the web directly:
  !g  → Google   !yt → YouTube   !gh → GitHub
If input does not match any app, search for it on Google.
Empty input or a bang without a query cancels without launching or searching.
Space inserts a space in stock completion; Tab still completes.

\\<launcher-query-map>A prefix of `launcher-tools' followed by a space reads the
tool's query instead, as plain text.  Return calls the tool and shows
its result buffer with `pop-to-buffer', after the minibuffer closes.
In the query, \\[launcher-back], or deleting backward in an empty
query, returns to the picker.  With tools, the picker opens even if
app discovery fails.

See `launcher-buffer' for the same choices at the top of a window."
  (interactive "P")
  (let* ((tools (launcher--tools))
         (entries (launcher--entries refresh tools))
         (input nil)
         (done nil))
    (while (not done)
      (let* ((choice (launcher--read entries tools input))
             (route (launcher--route choice tools)))
        (if (not route)
            (progn (launcher--act choice entries)
                   (setq done t))
          (if-let* ((buffer (launcher--minibuffer-query (car route) (cdr route))))
              (progn (pop-to-buffer buffer)
                     (setq done t))
            ;; Back: the picker shows the prefix without its space.
            (setq input (launcher--tool-prefix (car route)))))))))

(declare-function launcher-buffer--interact "launcher-buffer" (refresh))

;;;###autoload
(defun launcher-buffer (&optional refresh)
  "Launch an app or search the web from the top of the selected window.
Offer the same choices as `launcher', with Vertico showing the prompt
and candidates in the selected ordinary window instead of the
minibuffer.  With prefix argument REFRESH, rebuild the app index first.

A prefix of `launcher-tools' followed by a space shows the tool's
query in the window instead, as plain text; Return shows the tool's
result buffer there, keeping its mode and keys.

Return only when a choice succeeds or the interaction ends.  Escape or
C-g quits from any view, signaling `quit'.  From a later view, C-c C-b
\(`launcher-back') returns to the previous one: from a result to its
query, keeping its text, and from a query to the picker, as does
deleting backward in an empty query; see `launcher-buffer-map'.
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
