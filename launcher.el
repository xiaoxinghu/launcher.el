;;; launcher.el --- Launch macOS apps from Emacs -*- lexical-binding: t; -*-

(require 'subr-x)
(require 'seq)

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

(defun launcher--annotation (candidate)
  "Return a completion annotation for CANDIDATE."
  (when-let ((path (cdr (assoc candidate launcher--current-entries))))
    (concat "  " (abbreviate-file-name path))))

;;;###autoload
(defun launcher (&optional refresh)
  "Prompt for a macOS app from Spotlight index and launch it.
With prefix argument REFRESH, rebuild app index first.
If input does not match any app, search for it on Google."
  (interactive "P")
  (when refresh
    (launcher-refresh))
  (let* ((entries (launcher--ensure-index))
         (completion-extra-properties
          '(:annotation-function launcher--annotation))
         (names (mapcar #'car entries)))
    (unless entries
      (user-error "No apps discovered from Spotlight index"))
    (unwind-protect
        (progn
          (setq launcher--current-entries entries)
          (let* ((choice (completing-read "Launch app: " names nil nil))
                 (path (cdr (assoc choice entries))))
            (if path
                (progn
                  (launcher--launch path)
                  (message "Launching %s" choice))
              (browse-url (format launcher-fallback-search-url
                                  (url-hexify-string choice)))
              (message "Searching Google for: %s" choice))))
      (setq launcher--current-entries nil))))

(provide 'launcher)

;;; launcher.el ends here
