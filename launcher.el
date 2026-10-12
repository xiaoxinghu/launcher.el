;;; launcher.el --- Launch macOS apps from Emacs -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'subr-x)
(require 'seq)
(require 'url-util)

(defgroup launcher nil
  "Launch macOS applications from Emacs."
  :group 'external)

;; After the group, which its options belong to.
(require 'launcher-icons)

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
  "URL template for unprefixed search when all tools return no candidates.
%s will be replaced with the URL-encoded search query.
Input starting with http:// or https:// (ignoring case) opens directly instead."
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
Each engine becomes a prefixed tool using shared entry completion and
routing."
  :type '(repeat (list string string string))
  :group 'launcher)

(defcustom launcher-tools nil
  "Custom tools, between the built-in app/calculator and generated search tools.
Each entry is (PREFIX :name NAME :query QUERY :debounce SECONDS), or
(PREFIX :name NAME :function FUNCTION) for a submit-only tool.
PREFIX is nil, an empty string, or a single case-sensitive token.
A nonempty prefix matches only when followed by an ASCII space.
Before a space, matching prefixes appear as entry candidates.  Return or
Tab on an entry fills its prefix and a space.  After a space, an exact
prefix queries only that tool; otherwise unprefixed tools run.
Explicit tool and bang queries never fall back to default web search.

QUERY receives the full input for an unprefixed tool, or the unchanged
text after PREFIX and its separating space.  It returns a list of
result plists: :id, :title, :function and optionally :subtitle and
:icon-path.  Each result's :function takes no arguments and returns nil
for a completed action, a live non-minibuffer buffer to display, or
(:input STRING) to replace the input and keep querying.  Results must
have distinct, stable ids within their tool.  QUERY must not display
buffers or execute the result actions.

An unprefixed tool must have QUERY.  A prefixed tool without QUERY
gets an execution candidate; selecting it calls FUNCTION with the
nonblank query.  Its result follows the same action return contract.
:prompt is accepted for compatibility but is no longer displayed.

:debounce defaults to 0 (immediate), and is a nonnegative delay in
seconds after the last input change.  Queries are synchronous: the
delay reduces calls but does not make a slow query nonblocking.

:history names a history variable, or t for no history.  Prefixed tools
otherwise keep separate session histories.  M-p/M-n restore the prefix
and submitted query in the unified input box.  Empty input queries no
tools.  Applications, calculator and bangs are added automatically."
  :type '(repeat (cons :tag "Tool"
                       (choice (const :tag "No prefix" nil) string)
                       (plist :tag "Properties"
                              :options ((:name string) (:prompt string)
                                        (:query function) (:debounce number)
                                        (:function function) (:history symbol)))))
  :group 'launcher)

(defvar launcher--apps nil
  "Cached app entries as an alist of (display-name . app-path).")

(defun launcher--collect-paths ()
  "Return discovered .app paths from standard Application directories.
Searches only in `launcher-app-directories' using Spotlight's -onlyin flag."
  (unless (executable-find "mdfind")
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

(defun launcher--build-entries (paths)
  "Convert PATHS to an alist of (display-name . app-path)."
  (let ((counts (make-hash-table :test #'equal))
        entries)
    (dolist (path paths)
      (let ((name (file-name-base (directory-file-name path))))
        (puthash name (1+ (gethash name counts 0)) counts)
        (push (cons name path) entries)))
    (dolist (entry entries)
      (when (> (gethash (car entry) counts) 1)
        (setcar entry (format "%s  (%s)" (car entry) (abbreviate-file-name (cdr entry))))))
    (sort entries (lambda (a b) (string-lessp (car a) (car b))))))

(defun launcher-refresh ()
  "Refresh the cached list of launchable macOS apps."
  (interactive)
  (setq launcher--apps
        (launcher--build-entries (launcher--collect-paths)))
  (condition-case err
      (launcher-icons-index-refreshed (mapcar #'cdr launcher--apps))
    (error (message "Launcher icons not refreshed: %s" (error-message-string err))))
  (message "Indexed %d applications" (length launcher--apps)))

(defun launcher--launch (app-path)
  "Launch APP-PATH with macOS `open`."
  (unless (executable-find "open")
    (user-error "Cannot find `open` in PATH"))
  (unless (and app-path (file-directory-p app-path))
    (user-error "Invalid app path: %S" app-path))
  (let ((exit-code (call-process "open" nil nil nil "-a" app-path)))
    (unless (zerop exit-code)
      (user-error "Failed to launch app: %s" app-path))))

;;; Tools

(cl-defstruct (launcher--tool
               (:constructor launcher--tool-make)
               (:copier nil))
  "A validated entry of `launcher-tools'."
  prefix name function query debounce history)

(defvar launcher--tool-histories (make-hash-table :test #'equal)
  "Query history variables of tools without :history, by prefix.
They are uninterned, so `savehist-mode' does not save them.")

(defun launcher--tool-history-default (prefix)
  "Return the session's query history variable for tools with PREFIX."
  (or (gethash prefix launcher--tool-histories)
      (let ((symbol (make-symbol (format "launcher-%s-history" prefix))))
        (set symbol nil)
        (puthash prefix symbol launcher--tool-histories))))

(defun launcher--callable-p (value)
  "Whether VALUE is a function or a nonempty function name."
  (or (functionp value)
      (and value (symbolp value) (not (keywordp value)) (not (eq value t)))))

(defun launcher--tool-from-entry (entry)
  "Validate ENTRY and return its tool."
  (cl-flet ((invalid (format &rest args)
              (user-error "Invalid `launcher-tools' entry %S: %s"
                          entry (apply #'format format args))))
    (unless (and (consp entry) (proper-list-p (cdr entry))
                 (cl-evenp (length (cdr entry))))
      (invalid "expected (PREFIX :name NAME ...)"))
    (let* ((prefix (car entry)) (props (cdr entry))
           (name (plist-get props :name))
           (prompt (or (plist-get props :prompt) ""))
           (function (plist-get props :function))
           (query (plist-get props :query))
           (debounce (or (plist-get props :debounce) 0))
           (history (plist-get props :history)))
      (unless (or (null prefix)
                  (and (stringp prefix)
                       (not (string-match-p "[[:space:]]" prefix))))
        (invalid "PREFIX must be nil or a single string without whitespace"))
      (when (equal prefix "") (setq prefix nil))
      (cl-loop for key in props by #'cddr
               unless (memq key '(:name :prompt :function :query :debounce :history))
               do (invalid "unknown property %S" key))
      (unless (and (stringp name) (not (string-blank-p name)))
        (invalid ":name must be a nonempty string"))
      (unless (stringp prompt) (invalid ":prompt must be a string"))
      (unless (and (numberp debounce) (>= debounce 0) (<= debounce most-positive-fixnum))
        (invalid ":debounce must be a finite nonnegative number"))
      (when (and query (not (launcher--callable-p query)))
        (invalid ":query must be a function or its name"))
      (when (and function (not (launcher--callable-p function)))
        (invalid ":function must be a function or its name"))
      (unless (or query (and prefix function))
        (invalid "an unprefixed tool needs :query; a prefixed tool needs :query or :function"))
      (unless (and (symbolp history) (not (keywordp history)))
        (invalid ":history must be a variable, or t for none"))
      (launcher--tool-make
       :prefix prefix :name name :function function
       :query query :debounce debounce
       :history (or history (and prefix (launcher--tool-history-default prefix)))))))

(defun launcher--tools ()
  "Return validated custom tools, in configuration order."
  (unless (proper-list-p launcher-tools)
    (user-error "`launcher-tools' must be a list of tools: %S" launcher-tools))
  (let (tools)
    (dolist (entry launcher-tools)
      (let* ((tool (launcher--tool-from-entry entry))
             (prefix (launcher--tool-prefix tool)))
        (when (and prefix
                   (seq-find (lambda (other) (equal (launcher--tool-prefix other) prefix)) tools))
          (user-error "Two `launcher-tools' entries use the prefix %S" prefix))
        (when (and prefix (assoc prefix launcher-bangs))
          (user-error "`launcher-tools' prefix %S is also a key of `launcher-bangs'" prefix))
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

(defvar launcher--back-function nil
  "Function returning to the previous view of the active interaction.")

(defun launcher-back ()
  "Return from a result to the launcher input, keeping its full text."
  (interactive)
  (unless launcher--back-function
    (user-error "No launcher interaction is active"))
  (funcall launcher--back-function))

(defun launcher--entries (refresh)
  "Return cached apps, refreshing if requested; keep other tools usable."
  (condition-case err
      (progn
        (when (or refresh (null launcher--apps)) (launcher-refresh))
        (or launcher--apps
            (user-error "No apps discovered from Spotlight index")))
    (error
     (message "Launcher apps unavailable: %s" (error-message-string err))
     nil)))

(require 'launcher-query)

;;;###autoload
(defun launcher (&optional refresh)
  "Query apps, calculator, custom tools and web searches as you type.
Empty input shows no candidates.  Return executes the selected result.
A tool prefix stays in the input box.  Results that return a buffer
are shown with `pop-to-buffer' after the minibuffer closes.
With prefix argument REFRESH, rebuild the application index first."
  (interactive "P")
  (let* ((tools (launcher--tools))
         (entries (launcher--entries refresh))
         (answer (launcher--read entries tools)))
    (when (bufferp (car answer)) (pop-to-buffer (car answer)))))

(declare-function launcher-buffer--interact "launcher-buffer" (refresh))

;;;###autoload
(defun launcher-buffer (&optional refresh)
  "Query launcher tools at the top of the selected window using Vertico.
Return executes a result; buffers are shown in the same window.
`launcher-back' returns from a result to the full input text.
With prefix argument REFRESH, rebuild the application index first."
  (interactive "P")
  (require 'launcher-buffer)
  (launcher-buffer--interact refresh))

(provide 'launcher)
;;; launcher.el ends here
