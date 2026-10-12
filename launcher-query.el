;;; launcher-query.el --- Dynamic launcher results -*- lexical-binding: t; -*-

;;; Commentary:
;; Shared synchronous query engine and completion adapter.  Loaded by
;; launcher after its tool and application definitions, without Vertico.

;;; Code:
(require 'cl-lib)
(require 'subr-x)
(require 'seq)

(defvar launcher-bangs)
(defvar launcher-fallback-search-url)
(declare-function launcher--launch "launcher" (app-path))
(declare-function launcher--callable-p "launcher" (value))
(declare-function launcher--tool-from-entry "launcher" (entry))
(declare-function launcher--route "launcher" (input tools))
(declare-function launcher-back "launcher" ())

(defvar vertico--input)
(defvar vertico--index)
(defvar vertico--candidates)
(defvar vertico--lock-candidate)
(declare-function vertico--update "vertico" (&optional interruptible))
(declare-function vertico--exhibit "vertico" ())
(declare-function vertico--candidate "vertico" ())

(cl-defstruct (launcher--query-session (:constructor launcher--query-session-create))
  tools entries styles overrides defaults buffer input (version 0)
  slots timers errors pending results keys candidates (serial 0) closed answer
  status-overlay history-kind selection route)

(defvar-local launcher--query-session nil "This minibuffer's launcher query session.")
(defvar-local launcher--input-history nil "Full input strings for contextual history.")

(defun launcher--apps-query (input)
  "Return application results for INPUT using the session's completion styles."
  (let* ((session launcher--query-session)
         (entries (launcher--query-session-entries session))
         (completion-styles (launcher--query-session-styles session))
         (completion-category-overrides (launcher--query-session-overrides session))
         (completion-category-defaults (launcher--query-session-defaults session))
         (completion-ignore-case t)
         (table (lambda (str pred action)
                  (if (eq action 'metadata) '(metadata (category . launcher-app))
                    (complete-with-action action entries str pred))))
         (matches (completion-all-completions input table nil (length input))))
    ;; completion-all-completions returns a list with an integer base-size tail.
    (cl-loop while (consp matches)
             for name = (pop matches)
             for path = (cdr (assoc name entries))
             collect (let ((app-path path) (app-name name))
                       (list :id app-path :title (substring-no-properties app-name)
                             :subtitle (abbreviate-file-name app-path) :icon-path app-path
                             :function (lambda ()
                                         (launcher--launch app-path)
                                         (message "Launching %s" app-name)
                                         nil))))))

(defun launcher--arithmetic (text)
  "Parse and calculate TEXT using only decimal numbers and + - * / ().
Return nil for incomplete or invalid syntax; signal an error for division
by zero.  No Lisp forms, variables, function calls or evaluation are used."
  (let ((pos 0) (len (length text)) division-by-zero)
    (cl-labels
        ((space () (while (and (< pos len) (memq (aref text pos) '(?\s ?\t ?\n)))
                     (cl-incf pos)))
         (peek () (space) (and (< pos len) (aref text pos)))
         (factor ()
           (pcase (peek)
             ((or ?+ ?-) (let ((negative (= (aref text pos) ?-)))
                           (cl-incf pos)
                           (* (if negative -1 1) (factor))))
             (?\( (cl-incf pos)
                  (let ((value (sum)))
                    (unless (eq (peek) ?\)) (throw 'invalid nil))
                    (cl-incf pos) value))
             (_ (space)
                (unless (and (string-match "\\(?:[0-9]+\\(?:\\.[0-9]*\\)?\\|\\.[0-9]+\\)" text pos)
                             (= (match-beginning 0) pos))
                  (throw 'invalid nil))
                (let ((number (match-string 0 text)))
                  (setq pos (match-end 0)) (string-to-number number)))))
         (product ()
           (let ((value (factor)))
             (while (memq (peek) '(?* ?/))
               (let ((op (aref text pos)))
                 (cl-incf pos)
                 (let ((right (factor)))
                   (setq value (cond ((= op ?*) (* value right))
                                     ((zerop right) (setq division-by-zero t) 0)
                                     (t (/ (float value) right)))))))
             value))
         (sum ()
           (let ((value (product)))
             (while (memq (peek) '(?+ ?-))
               (let ((op (aref text pos)))
                 (cl-incf pos)
                 (let ((right (product)))
                   (setq value (if (= op ?+) (+ value right) (- value right))))))
             value)))
      (catch 'invalid
        (let ((value (sum)))
          (space)
          (when (= pos len)
            (when division-by-zero (error "Division by zero"))
            value))))))

(defun launcher--calculator-query (input)
  "Return a calculation candidate, including INVALID for unfinished INPUT.
Invalid candidates retain the calculation identity and refuse execution."
  (let* ((explicit (string-prefix-p "=" input))
         (expression (if explicit (substring input 1) input)))
    (when (or explicit
              (and (string-match-p "\\`[0-9.() +*/\t\n-]+\\'" expression)
                   (string-match-p "[()+*/-]" expression)))
      (let ((number (condition-case nil (launcher--arithmetic expression)
                      (error nil))))
        (if (null number)
            (list (list :id 'calculation :title "= INVALID"
                        :subtitle "Incomplete or invalid expression"
                        :function (lambda () (user-error "Complete or correct the expression first"))))
          (let ((value (if (integerp number) (number-to-string number)
                         (format "%.12g" number))))
            (list (list :id 'calculation :title (concat "= " value)
                        :subtitle "Copy result"
                        :function (lambda () (kill-new value)
                                    (message "Copied %s" value) nil)))))))))

(defun launcher--search-tools ()
  "Generate ordinary prefixed search tools from `launcher-bangs'."
  (let ((prefixes (make-hash-table :test #'equal)))
    (mapcar
     (lambda (entry)
       (unless (and (proper-list-p entry) (= (length entry) 3)
                    (stringp (car entry)) (not (string-empty-p (car entry)))
                    (stringp (cadr entry)) (stringp (caddr entry)))
         (user-error "Invalid `launcher-bangs' entry: %S" entry))
       (let ((prefix (car entry)) (name (cadr entry)) (template (caddr entry)))
         (when (gethash prefix prefixes)
           (user-error "Two `launcher-bangs' entries use the prefix %S" prefix))
         (puthash prefix t prefixes)
         (launcher--tool-from-entry
          (list prefix :name name :query
                (lambda (query)
                  (unless (string-blank-p query)
                    (list (list :id prefix :title (format "Search %s: %s" name query)
                                :function (lambda ()
                                            (browse-url (format template (url-hexify-string query)))
                                            nil)))))))))
     launcher-bangs)))

(defun launcher--prefix-result (tool &optional input)
  "Return an entry that fills TOOL's prefix and a separating space.
With INPUT, preserve that full input when selecting the entry."
  (let ((input (or input (concat (launcher--tool-prefix tool) " "))))
    (list :id 'prefix-entry :title (launcher--tool-prefix tool)
          :subtitle (launcher--tool-name tool) :completion input
          :function (lambda () (list :input input)))))

(defun launcher--fallback-result (input)
  "Make a URL-opening or default search action for unmatched INPUT.
Open INPUT unchanged when it starts with http:// or https://, ignoring case."
  (let* ((case-fold-search t)
         (direct-url (string-match-p "\\`https?://" input))
         (url (if direct-url input
                (format launcher-fallback-search-url (url-hexify-string input)))))
    (list :id 'fallback :title (concat (if direct-url "Open URL: " "Search web: ") input)
          :function (lambda () (browse-url url) nil))))

(defun launcher--query-create (entries tools)
  "Create a query session for ENTRIES and custom TOOLS."
  (launcher--query-session-create
   :entries entries
   :tools (append (list (launcher--tool-make :name "Applications" :query #'launcher--apps-query :debounce 0)
                        (launcher--tool-make :name "Calculator" :query #'launcher--calculator-query :debounce 0))
                  tools (launcher--search-tools))
   :styles completion-styles :overrides completion-category-overrides
   :defaults completion-category-defaults
   :slots (make-hash-table :test #'eql) :timers (make-hash-table :test #'eql)
   :errors (make-hash-table :test #'eql) :pending (make-hash-table :test #'eql)
   :keys (make-hash-table :test #'equal)
   :candidates (make-hash-table :test #'equal)))

(defun launcher--query-close (session)
  "Cancel pending queries and invalidate SESSION."
  (setf (launcher--query-session-closed session) t)
  (maphash (lambda (_ timer) (cancel-timer timer)) (launcher--query-session-timers session))
  (clrhash (launcher--query-session-timers session))
  (clrhash (launcher--query-session-pending session))
  (when (overlayp (launcher--query-session-status-overlay session))
    (delete-overlay (launcher--query-session-status-overlay session))))

(defun launcher--query-valid-p (session version)
  "Whether VERSION can still publish into SESSION."
  (and (not (launcher--query-session-closed session))
       (= version (launcher--query-session-version session))
       (or (null (launcher--query-session-buffer session))
           (and (buffer-live-p (launcher--query-session-buffer session))
                (eq (buffer-local-value 'launcher--query-session
                                        (launcher--query-session-buffer session)) session)))))

(defun launcher--query-validate-results (results tool)
  "Validate RESULTS returned by TOOL, before publishing any of them."
  (unless (proper-list-p results) (error "Query must return a list of results"))
  (let ((ids (make-hash-table :test #'equal)))
    (dolist (result results)
      (unless (and (proper-list-p result) (cl-evenp (length result))
                   (plist-get result :id)
                   (stringp (plist-get result :title))
                   (not (string-blank-p (plist-get result :title)))
                   (launcher--callable-p (plist-get result :function))
                   (or (null (plist-get result :subtitle)) (stringp (plist-get result :subtitle)))
                   (or (null (plist-get result :icon-path)) (stringp (plist-get result :icon-path))))
        (error "%s returned an invalid result: %S" (launcher--tool-name tool) result))
      (let ((id (plist-get result :id)))
        (when (gethash id ids) (error "Duplicate result id: %S" id))
        (puthash id t ids)))))

(defun launcher--query-progress (session index tool query)
  "Mark TOOL's nonblank QUERY as pending at INDEX in SESSION."
  (when (and (launcher--tool-query tool) (not (string-blank-p query))
             (or (launcher--tool-prefix tool) (> (or (launcher--tool-debounce tool) 0) 0)))
    (puthash index (format "Querying %s..." (launcher--tool-name tool))
             (launcher--query-session-pending session))))

(defun launcher--query-run (session index tool query version &optional defer)
  "Run TOOL for QUERY in SESSION VERSION.
With DEFER, leave publishing to the caller."
  (when (launcher--query-valid-p session version)
    (remhash index (launcher--query-session-timers session))
    (unless defer (launcher--query-progress session index tool query))
    (let ((launcher--query-session session) results failure)
      (condition-case err
          (progn
            (setq results
                  (if (launcher--tool-query tool)
                      (funcall (launcher--tool-query tool) query)
                    (unless (string-blank-p query)
                      (list (list :id 'execute
                                  :title (format "%s: %s" (launcher--tool-name tool) query)
                                  :subtitle "Execute tool"
                                  :function (lambda () (funcall (launcher--tool-function tool) query)))))))
            (launcher--query-validate-results results tool))
        (error (setq results nil
                     failure (format "%s: %s" (launcher--tool-name tool) (error-message-string err)))))
      (when (launcher--query-valid-p session version)
        (remhash index (launcher--query-session-pending session))
        (puthash index results (launcher--query-session-slots session))
        (if failure (puthash index failure (launcher--query-session-errors session))
          (remhash index (launcher--query-session-errors session)))
        (unless defer (launcher--query-publish session))))))

(defun launcher--query-update (session input)
  "Schedule queries for SESSION if INPUT changed; clear obsolete results."
  (unless (equal input (launcher--query-session-input session))
    (when-let* ((buffer (launcher--query-session-buffer session))
                ((buffer-live-p buffer)))
      (with-current-buffer buffer
        (when (and (boundp 'vertico--input) vertico--input)
          (setf (launcher--query-session-selection session)
                (and (>= vertico--index 0) (nth vertico--index vertico--candidates))))))
    (setf (launcher--query-session-input session) input
          (launcher--query-session-route session)
          (launcher--route input (launcher--query-session-tools session)))
    (cl-incf (launcher--query-session-version session))
    (maphash (lambda (_ timer) (cancel-timer timer)) (launcher--query-session-timers session))
    (clrhash (launcher--query-session-timers session))
    (clrhash (launcher--query-session-slots session))
    (clrhash (launcher--query-session-errors session))
    (clrhash (launcher--query-session-pending session))
    ;; Invalidate old actions immediately, but publish only after the immediate
    ;; queries finish so the UI never displays their intermediate empty state.
    (clrhash (launcher--query-session-candidates session))
    (setf (launcher--query-session-results session) nil)
    (unless (string-blank-p input)
      (cl-loop for tool in (launcher--query-session-tools session) for index from 0
               for prefix = (launcher--tool-prefix tool)
               for route = (launcher--query-session-route session)
               do (cond
                   ((and route (eq tool (car route)) (string-blank-p (cdr route)))
                    (puthash index (list (launcher--prefix-result tool input))
                             (launcher--query-session-slots session)))
                   ((if route (eq tool (car route)) (null prefix))
                    (let* ((query (if route (cdr route) input))
                           (version (launcher--query-session-version session))
                           (delay (or (launcher--tool-debounce tool) 0)))
                      (if (zerop delay)
                          (launcher--query-run session index tool query version t)
                        (launcher--query-progress session index tool query)
                        (puthash index
                                 (run-at-time delay nil #'launcher--query-timer
                                              session index tool query version)
                                 (launcher--query-session-timers session)))))
                   ((and (null route) prefix (not (string-search " " input))
                         (string-prefix-p input prefix))
                    (puthash index (list (launcher--prefix-result tool))
                             (launcher--query-session-slots session))))))
    (launcher--query-publish session)))

(defun launcher--query-timer (session index tool query version)
  "Run a debounced query, retaining the minibuffer's dynamic bindings."
  (when (launcher--query-valid-p session version)
    (let ((buffer (launcher--query-session-buffer session)))
      (if buffer
          (with-current-buffer buffer
            ;; A key event may have changed text before its post-command hook.
            (when (equal (minibuffer-contents-no-properties) (launcher--query-session-input session))
              (launcher--query-run session index tool query version)))
        (launcher--query-run session index tool query version)))))

(defun launcher--query-item (session index tool item)
  "Give ITEM from TOOL at INDEX a stable completion identity in SESSION."
  (let* ((identity (cons index (plist-get item :id)))
         (key (or (gethash identity (launcher--query-session-keys session))
                  (puthash identity
                           (format "launcher:%d" (cl-incf (launcher--query-session-serial session)))
                           (launcher--query-session-keys session)))))
    ;; Titles may change independently of the completion identity.
    (setq key (propertize key 'display (plist-get item :title)))
    (let ((result (append (list :tool tool :candidate key) item)))
      (puthash key result (launcher--query-session-candidates session))
      result)))

(defun launcher--query-publish (session)
  "Merge tools in order; offer a URL or search fallback when results are empty."
  (let (results)
    (clrhash (launcher--query-session-candidates session))
    (cl-loop for tool in (launcher--query-session-tools session) for index from 0
             do (dolist (item (gethash index (launcher--query-session-slots session)))
                  (push (launcher--query-item session index tool item) results)))
    (when (and (null results)
               (null (launcher--query-session-route session))
               (launcher--query-session-input session)
               (not (string-blank-p (launcher--query-session-input session))))
      (push (launcher--query-item session -1 nil
                                  (launcher--fallback-result (launcher--query-session-input session)))
            results))
    (setf (launcher--query-session-results session) (nreverse results))
    (let ((buffer (launcher--query-session-buffer session)))
      (when (and (buffer-live-p buffer)
                 (eq (buffer-local-value 'launcher--query-session buffer) session))
        (with-current-buffer buffer
          (when (and (boundp 'vertico--input) vertico--input)
            (setq-local vertico--lock-candidate t vertico--input nil)
            (vertico--update)
            (setq-local vertico--index
                        (or (seq-position vertico--candidates (launcher--query-session-selection session))
                            (if vertico--candidates 0 -1)))
            (vertico--exhibit))
          (launcher--query-status session))))))

(defun launcher--query-status (session &optional notice)
  "Display SESSION's pending queries, errors or NOTICE without a candidate."
  (let ((overlay (launcher--query-session-status-overlay session)) messages)
    (unless (overlayp overlay)
      (setq overlay (make-overlay (point-max) (point-max) nil t t))
      (setf (launcher--query-session-status-overlay session) overlay))
    (cl-loop for index from 0 below (length (launcher--query-session-tools session))
             for status = (or (gethash index (launcher--query-session-errors session))
                              (gethash index (launcher--query-session-pending session)))
             when status do (push status messages))
    (move-overlay overlay (point-max) (point-max))
    (overlay-put overlay 'after-string
                 (when-let* ((text (or notice (and messages (string-join (nreverse messages) "; "))
                                      (when (and (launcher--query-session-route session)
                                                 (null (launcher--query-session-results session))
                                                 (zerop (hash-table-count (launcher--query-session-timers session))))
                                        "No results"))))
                   (propertize (concat "  " text) 'face
                               (if (or notice (> (hash-table-count (launcher--query-session-errors session)) 0))
                                   'warning 'shadow))))))

(defun launcher--results-all (_string table pred _point)
  "Completion style: return already matched results in their original order."
  (let ((items (all-completions "" table pred)))
    (when items (nconc items 0))))

(defun launcher--results-try (string _table _pred point)
  "Keep STRING and POINT unchanged; result actions do not complete text."
  (cons string point))

(defun launcher--results-collection (session)
  "Return SESSION's identity-preserving dynamic completion table."
  (lambda (string pred action)
    (cond
     ((eq action 'metadata)
      '(metadata (category . launcher-result)
                 (display-sort-function . identity) (cycle-sort-function . identity)))
     ((eq action t)
      (seq-filter (lambda (candidate) (or (null pred) (funcall pred candidate)))
                  (mapcar (lambda (result) (plist-get result :candidate))
                          (launcher--query-session-results session))))
     ((eq action 'lambda) (and (gethash string (launcher--query-session-candidates session)) t))
     (t nil))))

(defun launcher--result-for-candidate (candidate)
  "Find the current structured result for CANDIDATE."
  (and launcher--query-session
       (gethash candidate (launcher--query-session-candidates launcher--query-session))))

(defun launcher--results-annotation (candidate)
  "Annotate CANDIDATE using its structured result."
  (when-let* ((subtitle (plist-get (launcher--result-for-candidate candidate) :subtitle)))
    (concat "  " subtitle)))

(defun launcher--results-affixation (candidates)
  "Affix CANDIDATES with their tool-provided icon paths and subtitles."
  (mapcar (lambda (candidate)
            (list candidate
                  (launcher-icons-prefix (plist-get (launcher--result-for-candidate candidate) :icon-path))
                  (or (launcher--results-annotation candidate) ""))) candidates))

(defun launcher--results-history (session input)
  "Choose SESSION's contextual history from INPUT, retaining prefixes."
  (let* ((route (launcher--route input (launcher--query-session-tools session)))
         (tool (car route))
         (kind (or tool 'picker)))
    (unless (eq kind (launcher--query-session-history-kind session))
      (setf (launcher--query-session-history-kind session) kind)
      (setq-local minibuffer-history-variable 'launcher--input-history
                  minibuffer-history-position nil
                  launcher--input-history
                  (if tool
                      (let ((history (launcher--tool-history tool))
                            (prefix (launcher--tool-prefix tool)))
                        (when (and history (not (eq history t)) (boundp history))
                          (mapcar (lambda (query) (concat prefix " " query)) (symbol-value history))))
                    minibuffer-history)))))

(defun launcher--results-post-command ()
  "Update queries only when the actual minibuffer text changes."
  (when launcher--query-session
    (let* ((input (minibuffer-contents-no-properties))
           (inserted (launcher--result-for-candidate input)))
      ;; Stock completion can insert an opaque action key.  Keep the typed
      ;; query and remember the chosen identity instead of querying its label.
      (when inserted
        (delete-minibuffer-contents)
        (insert (launcher--query-session-input launcher--query-session))
        (setq input (minibuffer-contents-no-properties)))
      (when (and (equal input (launcher--query-session-input launcher--query-session))
                 (boundp 'vertico--input) vertico--input)
        (setf (launcher--query-session-selection launcher--query-session)
              (and (>= vertico--index 0) (nth vertico--index vertico--candidates))))
      (launcher--results-history launcher--query-session input)
      (launcher--query-update launcher--query-session input)
      (when inserted
        (setf (launcher--query-session-selection launcher--query-session)
              (plist-get inserted :candidate))))))

(defun launcher--results-current ()
  "Return the selected structured result, or the first for stock completion."
  (let ((session launcher--query-session))
    (when (and (boundp 'vertico--input) vertico--input)
      (vertico--update))
    (or (launcher--result-for-candidate (minibuffer-contents-no-properties))
        (when (and (boundp 'vertico--input) vertico--input (>= vertico--index 0))
          (launcher--result-for-candidate (vertico--candidate)))
        (when (not (and (boundp 'vertico--input) vertico--input))
          (launcher--result-for-candidate (launcher--query-session-selection session)))
        (car (launcher--query-session-results session)))))

(defun launcher--result-execute (result)
  "Execute RESULT and validate its action return value."
  (let ((value (funcall (plist-get result :function))))
    (unless (or (null value)
                (and (bufferp value) (buffer-live-p value) (not (minibufferp value)))
                (and (proper-list-p value) (= (length value) 2)
                     (eq (car value) :input) (stringp (cadr value))))
      (error "Result action must return nil, a live buffer, or (:input STRING)"))
    value))

(defun launcher--results-record-history (session result)
  "Record SESSION's submitted input in RESULT's tool or picker history."
  (let* ((input (launcher--query-session-input session))
         (tool (plist-get result :tool)) (prefix (and tool (launcher--tool-prefix tool)))
         (history (if prefix (launcher--tool-history tool) 'minibuffer-history)))
    (when (and history (not (eq history t)))
      (unless (boundp history) (set history nil))
      (add-to-history history (if prefix (substring input (1+ (length prefix))) input)))))

(defun launcher-results-submit ()
  "Execute only a current candidate; keep input when an action fails."
  (interactive)
  (let* ((session launcher--query-session)
         (input (minibuffer-contents-no-properties)))
    (when (string-blank-p input) (signal 'quit nil))
    (launcher--query-update session input)
    (if-let* ((result (launcher--results-current)))
        (condition-case err
            (let ((value (launcher--result-execute result)))
              (if (and (consp value) (eq (car value) :input))
                  (progn (delete-minibuffer-contents) (insert (cadr value))
                         (launcher--results-post-command))
                (launcher--results-record-history session result)
                (setf (launcher--query-session-answer session) (cons value input))
                (exit-minibuffer)))
          (error (launcher--query-status session (error-message-string err))))
      (launcher--query-status session
                              (unless (launcher--query-session-route session) "No result yet")))))

(defun launcher-results-insert ()
  "Insert the selected result's visible title, without its internal identity."
  (interactive)
  (when-let* ((result (launcher--results-current)))
    (delete-minibuffer-contents)
    (insert (or (plist-get result :completion) (plist-get result :title)))))

(defvar-keymap launcher-query-map
  :doc "Keys in the unified launcher input box."
  "RET" #'launcher-results-submit
  "<return>" #'launcher-results-submit
  "<remap> <exit-minibuffer>" #'launcher-results-submit
  "<remap> <minibuffer-complete-and-exit>" #'launcher-results-submit
  "<remap> <vertico-exit>" #'launcher-results-submit
  "<remap> <vertico-exit-input>" #'launcher-results-submit
  "<remap> <vertico-insert>" #'launcher-results-insert
  "C-c C-b" #'launcher-back)

(defun launcher--read (entries tools &optional initial setup)
  "Read and execute dynamic results for ENTRIES and TOOLS.
INITIAL is the full starting input.  SETUP runs after the completion UI
has initialized.  Return (ACTION-VALUE . FULL-INPUT), after closing the
minibuffer.  No raw input is executed."
  (let* ((session (launcher--query-create entries tools))
         (completion-styles-alist
          (cons '(launcher-result launcher--results-try launcher--results-all
                                  "Already matched launcher actions.") completion-styles-alist))
         (completion-category-overrides
          (cons '(launcher-result (styles launcher-result))
                (assq-delete-all 'launcher-result (copy-tree completion-category-overrides))))
         (icons (launcher-icons-enabled-p))
         choice
         (completion-extra-properties
          (if icons '(:affixation-function launcher--results-affixation)
            '(:annotation-function launcher--results-annotation))))
    (when (and icons entries)
      (condition-case err (launcher-icons-prepare (mapcar #'cdr entries))
        (error (message "Launcher icons unavailable: %s" (error-message-string err)))))
    (unwind-protect
        (minibuffer-with-setup-hook
            (lambda ()
              (setq-local launcher--query-session session)
              (setf (launcher--query-session-buffer session) (current-buffer))
              (when (eq (key-binding " ") #'minibuffer-complete-word)
                (let ((map (make-sparse-keymap)))
                  (set-keymap-parent map (current-local-map))
                  (define-key map " " #'self-insert-command)
                  (use-local-map map)))
              (use-local-map (make-composed-keymap launcher-query-map (current-local-map)))
              (when (boundp 'vertico--input)
                (setq-local vertico-sort-override-function #'identity
                            vertico-preselect 'first))
              (add-hook 'post-command-hook #'launcher--results-post-command -10 t)
              (add-hook 'minibuffer-exit-hook (lambda () (launcher--query-close session)) nil t)
              (when setup (funcall setup))
              (launcher--results-post-command))
          (setq choice
                (completing-read (if entries "Launch: " "Launch (apps unavailable): ")
                                 (launcher--results-collection session) nil nil initial t)))
      (launcher--query-close session))
    (or (launcher--query-session-answer session)
        ;; Stock mouse completion and alternative readers may exit directly
        ;; with an opaque candidate.  Resolve only a current identity, never
        ;; a visible label or arbitrary text, and keep actions single-shot.
        (when-let* ((result (gethash choice (launcher--query-session-candidates session))))
          (let ((value (condition-case err
                           (launcher--result-execute result)
                         (error
                          (message "%s" (error-message-string err))
                          (list :input (launcher--query-session-input session))))))
            (if (and (consp value) (eq (car value) :input))
                (launcher--read entries tools (cadr value) setup)
              (launcher--results-record-history session result)
              (cons value (launcher--query-session-input session)))))
        (signal 'quit nil))))

(provide 'launcher-query)
;;; launcher-query.el ends here
