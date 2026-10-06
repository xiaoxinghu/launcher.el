;;; launcher-tools-tests.el --- Tool routing checks -*- lexical-binding: t; -*-

;; Batch checks of `launcher-tools': validation, the shared router, and
;; the picker → query → result views of both `launcher' and
;; `launcher-buffer', with fake handlers returning real buffers.  Fake
;; readers stand in for the minibuffer, which batch Emacs cannot show:
;; they run the readers' setup and post-command hooks around typed,
;; pasted or recalled input and keys.  Vertico's display, native keys
;; and asynchronous results are checked in launcher-tools-gui-tests.el.
;; Nothing here launches an app, opens a browser or runs Spotlight.

(require 'ert)
(require 'cl-lib)
(require 'launcher-buffer)
(require 'url-util)

(defvar vertico-count)

(defvar launcher-tools-test--log nil "Events of the current check, newest first.")
(defvar launcher-tools-test--picker nil "Answers of the fake picker.")
(defvar launcher-tools-test--query nil "Answers of the fake query reader.")
(defvar launcher-tools-test-history nil "A user's history variable for a tool.")

(defconst launcher-tools-test--apps
  '(("Calculator" . "/Applications/Calculator.app")
    ("Dictionary" . "/System/Applications/Dictionary.app"))
  "Fake application index.")

(defun launcher-tools-test--log (&rest event)
  (push event launcher-tools-test--log))

(defun launcher-tools-test--events (kind)
  "Return the logged events of KIND, oldest first, without KIND."
  (mapcar #'cdr (seq-filter (lambda (event) (eq (car event) kind))
                            (reverse launcher-tools-test--log))))

(defun launcher-tools-test--result (query)
  "Return a read-only result buffer for QUERY, as a lookup would."
  (with-current-buffer (get-buffer-create "*launcher tools result*")
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert "Definition of " query "\n"))
    (special-mode)
    (goto-char (point-min))
    (current-buffer)))

(defun launcher-tools-test--lookup (query)
  "Fake dictionary handler: log QUERY and return its result buffer."
  (launcher-tools-test--log 'call query)
  (launcher-tools-test--result query))

(defconst launcher-tools-test--tools
  '(("d" :name "Dictionary" :prompt "Word: "
     :function launcher-tools-test--lookup)
    ("dd" :name "Thesaurus" :prompt "Word: "
     :function launcher-tools-test--lookup))
  "Tools of the checks: prefixes \"d\" and \"dd\".")

(defun launcher-tools-test--step (step)
  "Run STEP of a fake reader's answer in the current fake minibuffer.
A string is typed one character per command, a (:paste STRING) is
inserted by one command, a (:key KEY) runs KEY's binding, a function
is called and a symbol is signaled.  The reader's post-command hook
runs after each command, as in the command loop."
  (pcase step
    ((pred stringp)
     (dolist (char (string-to-list step))
       (insert char)
       (run-hooks 'post-command-hook)))
    (`(:paste ,string)
     (insert string)
     (run-hooks 'post-command-hook))
    (`(:key ,key)
     (let* ((keys (kbd key))
            (last-command-event (aref keys (1- (length keys)))))
       (call-interactively (key-binding keys)))
     (run-hooks 'post-command-hook))
    ((pred functionp) (funcall step))
    ((pred symbolp) (signal step nil))
    (_ (error "Unknown step %S" step))))

(defun launcher-tools-test--answer (answer)
  "Answer a fake prompt with ANSWER, in its fake minibuffer.
A string is returned at once, as a completion UI returns its selected
candidate, and a symbol is signaled.  A list of steps runs them, then
returns the input, unless a step exits first: by a non-local exit such
as the router's, or by exiting the minibuffer, which returns the input."
  (cond ((stringp answer) answer)
        ((and answer (symbolp answer)) (signal answer nil))
        (t (catch 'exit
             (mapc #'launcher-tools-test--step answer))
           (minibuffer-contents-no-properties))))

(defun launcher-tools-test--picker-reader (prompt _collection &optional _predicate
                                                  _require-match initial &rest _)
  "Fake `completing-read-function' answering from the picker's answers."
  (launcher-tools-test--log 'picker prompt initial)
  (with-temp-buffer
    (when initial (insert initial))
    ;; Emulate minibuffer setup while retaining the caller's reader.
    (run-hooks 'minibuffer-setup-hook)
    (launcher-tools-test--answer (pop launcher-tools-test--picker))))

(defun launcher-tools-test--query-reader (prompt &optional initial keymap _read hist
                                                 &rest _)
  "Fake `read-from-minibuffer' answering from the query's answers.
Log the prompt, initial input, history variable, keymap and the notice
the setup shows, and add a nonempty value to the history."
  (with-temp-buffer
    (when initial (insert initial))
    (use-local-map keymap)
    (let (notices)
      (cl-letf (((symbol-function 'message)
                 (lambda (format &rest args)
                   (push (apply #'format format args) notices))))
        (run-hooks 'minibuffer-setup-hook))
      (launcher-tools-test--log 'query prompt initial hist (car notices) keymap))
    (let ((value (launcher-tools-test--answer (pop launcher-tools-test--query))))
      (when (and (stringp value) (not (string-empty-p value))
                 (symbolp hist) (not (eq hist t)))
        (add-to-history hist value))
      value)))

(defmacro launcher-tools-test--with (&rest body)
  "Run BODY with fake apps, browser, readers and tools, in a fresh window.
Afterwards, check that no interaction state remains."
  (declare (indent 0))
  `(let* ((launcher--apps launcher-tools-test--apps)
          (launcher--current-entries nil)
          (launcher--tool-histories (make-hash-table :test #'equal))
          (launcher-tools launcher-tools-test--tools)
          (launcher-bangs '(("!g" "Google" "https://www.google.com/search?q=%s")
                            ("!gh" "GitHub" "https://github.com/search?q=%s")))
          (launcher-fallback-search-url "https://example.com/?q=%s")
          (launcher-tools-test--log nil)
          (launcher-tools-test--picker nil)
          (launcher-tools-test--query nil)
          (completing-read-function #'launcher-tools-test--picker-reader)
          (minibuffer-history nil)
          (minibuffer-message-timeout 0)
          (minibuffer-setup-hook nil)
          (vertico-count 6)
          (emulation emulation-mode-map-alists)
          (origin (generate-new-buffer "*launcher tools origin*")))
     (delete-other-windows)
     (switch-to-buffer origin)
     (with-current-buffer origin
       (dotimes (i 50) (insert (format "Line %d\n" i)))
       (goto-char (point-min))
       (forward-line 20))
     (unwind-protect
         (cl-letf (((symbol-function 'launcher--launch)
                    (lambda (path) (launcher-tools-test--log 'launch path)))
                   ((symbol-function 'browse-url)
                    (lambda (url &rest _) (launcher-tools-test--log 'search url)))
                   ((symbol-function 'read-from-minibuffer)
                    #'launcher-tools-test--query-reader)
                   ;; Vertico's buffer display needs a graphical minibuffer.
                   ((symbol-function 'launcher-buffer--check) #'ignore)
                   ((symbol-function 'launcher-buffer--before-reader) #'ignore)
                   ((symbol-function 'launcher-buffer--after-reader) #'ignore)
                   ((symbol-function 'launcher-buffer--query-setup) #'ignore))
           ,@body)
       (setq unread-command-events nil)
       (delete-other-windows)
       (set-window-dedicated-p nil nil)
       (switch-to-buffer origin)
       (when (get-buffer "*launcher tools result*")
         (kill-buffer "*launcher tools result*"))
       (kill-buffer origin))
     (should-not launcher-buffer--session)
     (should-not launcher--back-function)
     (should (equal emulation emulation-mode-map-alists))
     (should-not (memq #'launcher-buffer--watch (default-value 'post-command-hook)))
     (should-not (memq #'launcher-buffer--follow
                       (default-value 'pre-redisplay-functions)))
     (should-not minibuffer-setup-hook)
     (should-not launcher--current-entries)
     (should (zerop (recursion-depth)))))

(defun launcher-tools-test--run (command picker &optional query keys)
  "Run COMMAND with fake PICKER and QUERY answers, then KEYS in views.
Return its outcome: `accepted', `quit', or the error's message."
  (setq launcher-tools-test--picker picker
        launcher-tools-test--query query
        unread-command-events (and keys (listify-key-sequence (kbd keys))))
  (prog1 (condition-case err
             (progn (funcall command) 'accepted)
           (quit 'quit)
           (error (error-message-string err)))
    (should-not launcher-tools-test--picker)
    (should-not launcher-tools-test--query)))

(defconst launcher-tools-test--commands '(launcher launcher-buffer)
  "Both entry points.")

;;; Configuration

(ert-deftest launcher-tools-validation ()
  (let ((launcher-bangs '(("!g" "Google" "https://www.google.com/search?q=%s")))
        (launcher--tool-histories (make-hash-table :test #'equal)))
    ;; Each invalid configuration names its problem.
    (dolist (case '(("d" . "must be a list")
                    ((("d" :name "D" :prompt "W: " :function ignore) . oops)
                     . "expected (PREFIX")
                    (("d") . ":name must")
                    (("d" :name "D" :prompt) . "expected (PREFIX")
                    ((d :name "D" :prompt "W: " :function ignore) . "PREFIX must")
                    (("" :name "D" :prompt "W: " :function ignore) . "PREFIX must")
                    (("d d" :name "D" :prompt "W: " :function ignore) . "PREFIX must")
                    ((" d" :name "D" :prompt "W: " :function ignore) . "PREFIX must")
                    (("d\t" :name "D" :prompt "W: " :function ignore) . "PREFIX must")
                    (("d " :name "D" :prompt "W: " :function ignore) . "PREFIX must")
                    (("d" :name "D" :prompt "W: " :fucntion ignore) . "unknown property")
                    (("d" :prompt "W: " :function ignore) . ":name must")
                    (("d" :name "" :prompt "W: " :function ignore) . ":name must")
                    (("d" :name "D" :function ignore) . ":prompt must")
                    (("d" :name "D" :prompt "W: ") . ":function must")
                    (("d" :name "D" :prompt "W: " :function :key) . ":function must")
                    (("d" :name "D" :prompt "W: " :function 42) . ":function must")
                    (("d" :name "D" :prompt "W: " :function ignore :history "h")
                     . ":history must")
                    (("d" :name "D" :prompt "W: " :function ignore :history :h)
                     . ":history must")))
      (let ((launcher-tools (if (stringp (car case)) (car case) (list (car case)))))
        (should (string-match-p (regexp-quote (cdr case))
                                (cadr (should-error (launcher--tools)
                                                    :type 'user-error))))))
    (let ((launcher-tools (cons '("d" :name "D" :prompt "W: " :function ignore) 'oops)))
      (should (string-match-p "must be a list"
                              (cadr (should-error (launcher--tools) :type 'user-error)))))
    ;; Two tools with one prefix, or a tool on a bang's key.
    (let ((launcher-tools '(("d" :name "A" :prompt "" :function ignore)
                            ("d" :name "B" :prompt "" :function ignore))))
      (should (equal (format-message "Two `launcher-tools' entries use the prefix \"d\"")
                     (cadr (should-error (launcher--tools) :type 'user-error)))))
    (let ((launcher-tools '(("!g" :name "G" :prompt "" :function ignore))))
      (should (equal (format-message
                      "`launcher-tools' prefix \"!g\" is also a key of `launcher-bangs'")
                     (cadr (should-error (launcher--tools) :type 'user-error)))))
    ;; Valid: prefixes that share a start, a bang-like prefix, named,
    ;; undefined, autoloaded and anonymous functions, and history choices.
    (let* ((closure (let ((n 0)) (lambda (_) (cl-incf n))))
           (launcher-tools `(("d" :name "D" :prompt "Word: " :function ignore)
                             ("dd" :name "DD" :prompt "" :function not-yet-defined)
                             ("!d" :name "Bang D" :prompt "Q: " :function ,closure
                              :history my-history)
                             ("Δ" :name "Delta" :prompt "Q: " :function ignore
                              :history t)))
           (tools (launcher--tools)))
      (should (equal '("d" "dd" "!d" "Δ") (mapcar #'launcher--tool-prefix tools)))
      (should (eq closure (launcher--tool-function (nth 2 tools))))
      (should (eq 'my-history (launcher--tool-history (nth 2 tools))))
      (should (eq t (launcher--tool-history (nth 3 tools))))
      ;; Default histories: one per prefix, kept across interactions,
      ;; uninterned so that savehist does not save them.
      (let ((history (launcher--tool-history (car tools))))
        (should (symbolp history))
        (should-not (eq history (intern-soft (symbol-name history))))
        (should (eq history (launcher--tool-history (car (launcher--tools)))))
        (should-not (eq history (launcher--tool-history (nth 1 tools))))))
    (let ((launcher-tools nil))
      (should-not (launcher--tools)))))

(ert-deftest launcher-tools-invalid-configuration-fails-before-action ()
  (launcher-tools-test--with
    (let ((launcher-tools '(("!g" :name "G" :prompt "" :function ignore)))
          refreshed)
      (cl-letf (((symbol-function 'launcher-refresh) (lambda () (setq refreshed t))))
        (dolist (command launcher-tools-test--commands)
          (should (string-match-p (format-message "also a key of `launcher-bangs'")
                                  (launcher-tools-test--run
                                   (lambda () (funcall command '(4)))
                                   nil))))
        (should-not refreshed)
        (should-not launcher-tools-test--log)))))

;;; Router

(ert-deftest launcher-tools-route ()
  (let* ((launcher--tool-histories (make-hash-table :test #'equal))
         (launcher-tools (append launcher-tools-test--tools
                                 '(("!d" :name "Bang" :prompt "" :function ignore))))
         (tools (launcher--tools)))
    (dolist (case '(("d serendipity" "d" "serendipity")
                    ("d " "d" "")
                    ("d  two spaces " "d" " two spaces ")
                    ("d 中文 café\tword" "d" "中文 café\tword")
                    ("dd word" "dd" "word")
                    ("!d word" "!d" "word")
                    ("d" nil) ("dd" nil) ("D word" nil) ("Dd word" nil)
                    (" d word" nil) ("d\tword" nil) ("d word" nil)
                    ("dx word" nil) ("x word" nil) ("!g word" nil) ("" nil)))
      (let ((route (launcher--route (car case) tools)))
        (should (equal (and (cadr case) (cdr case))
                       (and route (list (launcher--tool-prefix (car route))
                                        (cdr route)))))))
    (should-not (launcher--route nil tools))
    (should-not (launcher--route "d word" nil))))

(ert-deftest launcher-tools-collection-has-no-candidates-in-a-query ()
  (let* ((launcher--tool-histories (make-hash-table :test #'equal))
         (launcher-tools launcher-tools-test--tools)
         (collection (launcher--make-collection launcher-tools-test--apps
                                                (launcher--tools))))
    (should (equal '("Dictionary") (all-completions "D" collection)))
    (should (member "Dictionary" (all-completions "" collection)))
    (dolist (input '("d " "d word" "dd "))
      (should-not (all-completions input collection))
      (should (eq t (test-completion input collection))))))

;;; Both entry points

(ert-deftest launcher-tools-query-calls-handler-once ()
  ;; Typing "d SPC" leaves the picker for the query, with no app or
  ;; browser action; Return calls the handler once and shows its buffer.
  (dolist (command launcher-tools-test--commands)
    (launcher-tools-test--with
      (should (eq (if (eq command 'launcher) 'accepted 'quit)
                  (launcher-tools-test--run command
                                            '(("d serendipity"))
                                            '(("serendipity" (:key "RET")))
                                            "C-g")))
      (should (equal '(("serendipity")) (launcher-tools-test--events 'call)))
      (should-not (launcher-tools-test--events 'launch))
      (should-not (launcher-tools-test--events 'search))
      ;; The router took "d " from the picker, before "serendipity".
      (let ((query (car (launcher-tools-test--events 'query))))
        (should (equal "Dictionary — Word: " (nth 0 query)))
        (should (equal "" (nth 1 query)))
        (should (eq launcher-query-map (nth 4 query))))
      ;; The picker's history has no routed input; the tool's has the query.
      (should-not minibuffer-history)
      (let ((history (launcher--tool-history (car (launcher--tools)))))
        (should (equal '("serendipity") (symbol-value history))))
      (when (eq command 'launcher)
        ;; Shown by ordinary display after the minibuffer closed.
        (should (eq (get-buffer "*launcher tools result*")
                    (window-buffer (selected-window))))))))

(ert-deftest launcher-tools-paste-history-and-initial-input ()
  ;; However the input starts with "PREFIX SPC", the rest is the query.
  (dolist (command launcher-tools-test--commands)
    ;; Each case: the picker's answers, then the expected query.
    (dolist (case (list (list '(((:paste "d 中文 café  multi word")))
                              "中文 café  multi word")
                        (list '(("dd" (:paste " thesaurus"))) "thesaurus")
                        ;; Recalled history routes too, as M-p would insert it.
                        (list (list (list (lambda ()
                                            (insert "d recalled")
                                            (run-hooks 'post-command-hook))))
                              "recalled")))
      (launcher-tools-test--with
        (launcher-tools-test--run command (car case) '(()) "C-g")
        (should (equal (list (list (cadr case))) (launcher-tools-test--events 'call)))
        (should (equal (cadr case) (nth 1 (car (launcher-tools-test--events 'query)))))
        (should-not (launcher-tools-test--events 'search))))))

(ert-deftest launcher-tools-initial-input-routes-without-reading ()
  (launcher-tools-test--with
    (let ((tools (launcher--tools)))
      (should (equal "d word" (launcher--read launcher-tools-test--apps tools "d word")))
      (should-not (launcher-tools-test--events 'picker))
      (should-not minibuffer-history))))

(ert-deftest launcher-tools-other-input-is-ordinary ()
  ;; "d" alone, other case, other prefixes and leading or other spaces
  ;; are app or web input, as without tools.
  (dolist (command launcher-tools-test--commands)
    (dolist (case '(("d" search "d") ("D word" search "D word")
                    ("x word" search "x word") (" d word" search " d word")
                    ("Dictionary" launch "/System/Applications/Dictionary.app")
                    ("!g word" search "word")))
      (launcher-tools-test--with
        (should (eq 'accepted (launcher-tools-test--run command (list (car case)))))
        (should-not (launcher-tools-test--events 'call))
        (should-not (launcher-tools-test--events 'query))
        (should (equal (list (list (if (eq (nth 1 case) 'launch)
                                       (nth 2 case)
                                     (concat (if (string-prefix-p "!g" (car case))
                                                 "https://www.google.com/search?q="
                                               "https://example.com/?q=")
                                             (url-hexify-string (nth 2 case))))))
                       (launcher-tools-test--events (nth 1 case))))))))

(ert-deftest launcher-tools-blank-queries-call-nothing ()
  ;; Typing calls nothing; blank submissions stay in the query.
  (dolist (command launcher-tools-test--commands)
    (launcher-tools-test--with
      (launcher-tools-test--run command
                                '(("d "))
                                '(((:key "RET") "  " (:key "RET") "word" (:key "RET")))
                                "C-g")
      ;; Spaces typed before the word stay in the query.
      (should (equal '(("  word")) (launcher-tools-test--events 'call)))
      (should (= 1 (length (launcher-tools-test--events 'query)))))
    ;; A blank value returned otherwise is read again, with a notice.
    (launcher-tools-test--with
      (launcher-tools-test--run command '(("d ")) '("" "  " "word") "C-g")
      (should (equal '(("word")) (launcher-tools-test--events 'call)))
      (should (equal '(nil "Type a query first" "Type a query first")
                     (mapcar (lambda (query) (nth 3 query))
                             (launcher-tools-test--events 'query)))))))

(defun launcher-tools-test--failing (query)
  "Handler failing for \"bad\", returning nothing for \"none\"."
  (launcher-tools-test--log 'call query)
  (pcase query
    ("bad" (error "No definition for %s" query))
    ("none" nil)
    ("dead" (let ((buffer (generate-new-buffer "dead"))) (kill-buffer buffer) buffer))
    ("mini" (window-buffer (minibuffer-window)))
    (_ (launcher-tools-test--result query))))

(ert-deftest launcher-tools-failures-keep-the-query ()
  ;; Failures are reported in the query, which keeps its text; no web
  ;; search or app launch replaces them.
  (dolist (command launcher-tools-test--commands)
    (launcher-tools-test--with
      (let ((launcher-tools '(("d" :name "Dictionary" :prompt "Word: "
                               :function launcher-tools-test--failing))))
        (launcher-tools-test--run command '(("d ")) '("bad" "none" "dead" "mini" "good")
                                  "C-g")
        (should (equal '(("bad") ("none") ("dead") ("mini") ("good"))
                       (launcher-tools-test--events 'call)))
        (let ((queries (launcher-tools-test--events 'query)))
          (should (equal '("" "bad" "none" "dead" "mini")
                         (mapcar (lambda (query) (nth 1 query)) queries)))
          (should-not (nth 3 (nth 0 queries)))
          (should (equal "Dictionary failed: No definition for bad" (nth 3 (nth 1 queries))))
          (should (string-match-p "\\`Dictionary failed: Dictionary returned nil instead"
                                  (nth 3 (nth 2 queries))))
          (should (string-match-p "returned #<killed buffer> instead" (nth 3 (nth 3 queries))))
          (should (string-match-p "instead of a live buffer" (nth 3 (nth 4 queries)))))
        (should-not (launcher-tools-test--events 'search))
        (should-not (launcher-tools-test--events 'launch))))))

(ert-deftest launcher-tools-missing-or-autoloaded-handlers ()
  (let* ((directory (make-temp-file "launcher-tools" t))
         (file (expand-file-name "launcher-tools-test-autoloaded.el" directory)))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "(defun launcher-tools-test-autoloaded (query)
  (launcher-tools-test--log 'call query)
  (launcher-tools-test--result query))
(provide 'launcher-tools-test-autoloaded)\n"))
          (autoload 'launcher-tools-test-autoloaded file)
          (autoload 'launcher-tools-test-unloadable
            (expand-file-name "missing" directory))
          (dolist (command launcher-tools-test--commands)
            (launcher-tools-test--with
              (let ((launcher-tools
                     '(("m" :name "Missing" :prompt "Q: "
                        :function launcher-tools-test-undefined)
                       ("u" :name "Unloadable" :prompt "Q: "
                        :function launcher-tools-test-unloadable)
                       ("a" :name "Autoloaded" :prompt "Q: "
                        :function launcher-tools-test-autoloaded))))
                ;; Back from each failing query; the picker shows its prefix.
                (launcher-tools-test--run
                 command '(("m ") ((:key "DEL") "u ") ((:key "DEL") "a "))
                 `("word" ,(list #'launcher-back) "word" ,(list #'launcher-back) "word")
                 "C-g")
                (let ((notices (mapcar (lambda (q) (nth 3 q))
                                       (launcher-tools-test--events 'query))))
                  (should (equal (format-message "Missing failed: Missing needs \
`launcher-tools-test-undefined', which is not defined")
                                 (nth 1 notices)))
                  (should (string-match-p "\\`Unloadable failed: Cannot open load file"
                                          (nth 3 notices))))
                (should (equal '(("word")) (launcher-tools-test--events 'call)))
                (should-not (launcher-tools-test--events 'search))))))
      (fmakunbound 'launcher-tools-test-autoloaded)
      (fmakunbound 'launcher-tools-test-unloadable)
      (setq features (delq 'launcher-tools-test-autoloaded features))
      (delete-directory directory t))))

(ert-deftest launcher-tools-back-from-query-to-picker ()
  ;; C-c C-b, or DEL in an empty query, returns to the picker, which
  ;; shows the prefix without its space.  DEL in a nonempty query edits.
  (dolist (command launcher-tools-test--commands)
    (dolist (back `((,#'launcher-back) ((:key "C-c C-b")) ((:key "DEL"))
                    ("x" (:key "DEL") (:key "DEL"))))
      (launcher-tools-test--with
        (should (eq 'accepted
                    (launcher-tools-test--run command
                                              '(("d ") "Calculator")
                                              (list back))))
        (should (equal '(("Launch: " nil) ("Launch: " "d"))
                       (launcher-tools-test--events 'picker)))
        (should (equal '(("/Applications/Calculator.app"))
                       (launcher-tools-test--events 'launch)))
        (should-not (launcher-tools-test--events 'call))))))

(ert-deftest launcher-tools-quit-from-every-reading-view ()
  (dolist (command launcher-tools-test--commands)
    ;; Each case: the picker's answers, then the query's.
    (dolist (case '(((quit)) ((("d ")) (quit)) ((("d ")) (("word" quit)))))
      (launcher-tools-test--with
        (should (eq 'quit (launcher-tools-test--run command (car case) (cadr case))))
        (should-not (launcher-tools-test--events 'call))
        (should-not (launcher-tools-test--events 'search))))))

(ert-deftest launcher-tools-without-apps ()
  ;; Without mdfind, or with no apps found, tools stay reachable and the
  ;; picker says apps are unavailable.  Nothing runs Spotlight here.
  (dolist (command launcher-tools-test--commands)
    (dolist (setup (list (lambda () (setq exec-path nil))
                         (lambda () (fset 'launcher--collect-paths #'ignore))))
      (launcher-tools-test--with
        (let ((launcher--apps nil)
              (exec-path exec-path)
              (collect (symbol-function 'launcher--collect-paths))
              messages)
          (unwind-protect
              (cl-letf (((symbol-function 'message)
                         (lambda (format &rest args)
                           (push (apply #'format format args) messages))))
                (funcall setup)
                (launcher-tools-test--run command '(("d ")) '("word") "C-g"))
            (fset 'launcher--collect-paths collect))
          (should (equal '(("Launch (apps unavailable): " nil))
                         (launcher-tools-test--events 'picker)))
          (should (equal '(("word")) (launcher-tools-test--events 'call)))
          (should (seq-find (lambda (m) (string-prefix-p "Launcher apps unavailable: " m))
                            messages)))))
    ;; Without tools, discovery errors end the command as before.
    (launcher-tools-test--with
      (let ((launcher--apps nil)
            (launcher-tools nil)
            (exec-path nil))
        (should (equal (format-message "Cannot find `mdfind` in PATH")
                       (launcher-tools-test--run command nil)))
        (should-not launcher-tools-test--log)))))

(ert-deftest launcher-tools-are-snapshotted-per-interaction ()
  ;; Changing `launcher-tools' during an interaction affects the next one.
  (dolist (command launcher-tools-test--commands)
    (launcher-tools-test--with
      (launcher-tools-test--run
       command '(("d "))
       (list (list (lambda () (setq launcher-tools nil)) "word"))
       "C-g")
      (should (equal '(("word")) (launcher-tools-test--events 'call))))))

(ert-deftest launcher-tools-closures-and-user-history ()
  (dolist (command launcher-tools-test--commands)
    (launcher-tools-test--with
      (let* ((calls nil)
             (launcher-tools-test-history nil)
             (launcher-tools
              `(("!d" :name "Closure" :prompt "Q: "
                 :function ,(lambda (query)
                              (push query calls)
                              (launcher-tools-test--result query))
                 :history launcher-tools-test-history))))
        (launcher-tools-test--run command '(("!d ")) '("one") "C-g")
        (launcher-tools-test--run command '(((:paste "!d two"))) '(()) "C-g")
        (should (equal '("two" "one") calls))
        (should (equal '("two" "one") launcher-tools-test-history))
        (should (eq 'launcher-tools-test-history
                    (nth 2 (car (launcher-tools-test--events 'query)))))))))

(ert-deftest launcher-tools-custom-space-binding ()
  ;; A custom Space binding in the user's completion keys is kept: here
  ;; Space does not insert, so typing "d SPC" does not route, but pasting
  ;; "d word" does.
  (launcher-tools-test--with
    (let ((map (make-sparse-keymap))
          (reader (symbol-function 'launcher-tools-test--picker-reader)))
      (keymap-set map "SPC" #'ignore)
      (keymap-set map "d" #'self-insert-command)
      (cl-letf (((symbol-function 'launcher-tools-test--picker-reader)
                 (lambda (&rest args)
                   (let ((minibuffer-setup-hook
                          (cons (lambda () (use-local-map map)) minibuffer-setup-hook)))
                     (apply reader args)))))
        (should (eq 'accepted
                    (launcher-tools-test--run
                     #'launcher '(((:key "d") (:key "SPC") "word")))))
        (should (equal '(("https://example.com/?q=dword"))
                       (launcher-tools-test--events 'search)))
        (should-not (launcher-tools-test--events 'query))
        (setq launcher-tools-test--log nil)
        (launcher-tools-test--run #'launcher '(((:paste "d word"))) '(()))
        (should (equal '(("word")) (launcher-tools-test--events 'call)))))))

(ert-deftest launcher-tools-query-keys ()
  ;; The query reads plain text: Return exits only with a nonblank
  ;; query, and DEL leaves only an empty one.
  (with-temp-buffer
    (use-local-map launcher-query-map)
    (let ((launcher--back-function (lambda () (throw 'launcher-tools-test 'back))))
      (cl-letf (((symbol-function 'minibuffer-message) #'ignore))
        (dolist (key '("RET" "C-j"))
          (should (eq 'launcher-query-submit (key-binding (kbd key))))
          (erase-buffer)
          (insert " \t ")
          (should (eq 'stayed (catch 'exit (call-interactively #'launcher-query-submit)
                                     'stayed)))
          (erase-buffer)
          (insert "word")
          (should-not (catch 'exit (call-interactively #'launcher-query-submit)
                             'stayed)))
        (should (eq 'launcher-query-delete-backward-char (key-binding (kbd "DEL"))))
        (should (eq 'launcher-back (key-binding (kbd "C-c C-b"))))
        (should (eq 'self-insert-command (key-binding (kbd "SPC"))))
        (should (eq 'self-insert-command (key-binding (kbd "?"))))
        (erase-buffer)
        (insert "ab")
        (call-interactively #'launcher-query-delete-backward-char)
        (should (equal "a" (buffer-string)))
        (call-interactively #'launcher-query-delete-backward-char)
        (should (equal "" (buffer-string)))
        (should (eq 'back (catch 'launcher-tools-test
                            (call-interactively #'launcher-query-delete-backward-char))))))))

;;; launcher-buffer views

(ert-deftest launcher-tools-buffer-result-back-and-retry ()
  ;; Result → query keeps the submitted text without calling again;
  ;; query → picker shows the prefix.
  (launcher-tools-test--with
    (let ((window (selected-window))
          (before (list (window-buffer) (window-start) (window-point)))
          views)
      (should (eq 'accepted
                  (launcher-tools-test--run
                   #'launcher-buffer
                   '(("d serendipity") "Calculator")
                   (list "serendipity"
                         (list (lambda ()
                                 (push (launcher-buffer--session-view launcher-buffer--session)
                                       views)
                                 (push (launcher-buffer--session-history
                                        launcher-buffer--session)
                                       views))
                               #'launcher-back))
                   "C-c C-b")))
      (should (equal '(("serendipity")) (launcher-tools-test--events 'call)))
      (should (equal '("" "serendipity")
                     (mapcar (lambda (q) (nth 1 q)) (launcher-tools-test--events 'query))))
      (should (equal '(("Launch: " nil) ("Launch: " "d"))
                     (launcher-tools-test--events 'picker)))
      (pcase-let ((`(,history (query ,tool ,input)) views))
        (should (equal "d" (launcher--tool-prefix tool)))
        (should (equal "serendipity" input))
        (should (equal '((picker "d")) history)))
      (should (equal '(("/Applications/Calculator.app"))
                     (launcher-tools-test--events 'launch)))
      (should (equal before (list (window-buffer window) (window-start window)
                                  (window-point window))))
      ;; The result keeps its buffer, mode and contents.
      (let ((result (get-buffer "*launcher tools result*")))
        (should (buffer-live-p result))
        (should (eq 'special-mode (buffer-local-value 'major-mode result)))
        (should (equal "Definition of serendipity\n"
                       (with-current-buffer result (buffer-string))))))))

(ert-deftest launcher-tools-buffer-quit-and-kill-from-result ()
  (launcher-tools-test--with
    (let ((before (list (window-buffer) (window-start) (window-point))))
      ;; Escape and C-g quit a result view.
      (dolist (keys '("C-g" "<escape>"))
        (should (eq 'quit (launcher-tools-test--run #'launcher-buffer
                                                    '(("d ")) '("word") keys)))
        (should (equal before (list (window-buffer) (window-start) (window-point)))))
      ;; Killing the result returns to its query, which keeps its text.
      (setq launcher-tools-test--log nil)
      (keymap-set (current-global-map) "C-c k"
                  (lambda () (interactive) (kill-buffer (current-buffer))))
      (unwind-protect
          (should (eq 'quit (launcher-tools-test--run #'launcher-buffer
                                                      '(("d ")) '("word" (quit))
                                                      "C-c k")))
        (keymap-global-unset "C-c k"))
      (should (equal '("" "word")
                     (mapcar (lambda (q) (nth 1 q)) (launcher-tools-test--events 'query))))
      (should (equal '(("word")) (launcher-tools-test--events 'call)))
      (should (equal before (list (window-buffer) (window-start) (window-point)))))))

(ert-deftest launcher-tools-buffer-keeps-preexisting-results ()
  ;; A handler may return a buffer it already had, shown elsewhere:
  ;; Launcher shows it, then leaves it and the other window as they were.
  (launcher-tools-test--with
    (let* ((own (get-buffer-create "*launcher tools own*"))
           (other (split-window nil nil 'below))
           (launcher-tools `(("d" :name "D" :prompt "W: "
                              :function ,(lambda (_) own)))))
      (unwind-protect
          (progn
            (with-current-buffer own
              (dotimes (i 40) (insert (format "Own line %d\n" i)))
              (goto-char (point-min))
              (forward-line 10)
              (read-only-mode 1))
            (set-window-buffer other own)
            (set-window-point other (with-current-buffer own (point)))
            (let ((other-state (list (window-buffer other) (window-start other)
                                     (window-point other)))
                  (contents (with-current-buffer own (buffer-string))))
              (should (eq 'quit (launcher-tools-test--run #'launcher-buffer
                                                          '(("d ")) '("w" (quit))
                                                          "C-c C-b")))
              (should (buffer-live-p own))
              (should (equal contents (with-current-buffer own (buffer-string))))
              (should (buffer-local-value 'buffer-read-only own))
              (should (equal other-state (list (window-buffer other) (window-start other)
                                               (window-point other))))))
        (kill-buffer own)))))

(ert-deftest launcher-tools-buffer-follows-only-at-end ()
  ;; A result view follows text added at the end only while its point
  ;; is at the end of a nonempty buffer.
  (let* ((buffer (generate-new-buffer "*launcher tools follow*"))
         (window (selected-window))
         (session (launcher-buffer--session-make :window window
                                                 :view (list 'result buffer)))
         (launcher-buffer--session session)
         (old (window-buffer window)))
    (unwind-protect
        (cl-flet ((append-text (text)
                    (with-current-buffer buffer
                      (save-excursion (goto-char (point-max)) (insert text)))
                    (launcher-buffer--follow window)))
          (set-window-buffer window buffer)
          ;; Empty: not following, so its point stays at the start.
          (launcher-buffer--follow window)
          (append-text "first\n")
          (should (= 1 (window-point window)))
          ;; At the end: follows each addition.
          (set-window-point window (with-current-buffer buffer (point-max)))
          (launcher-buffer--follow window)
          (append-text "second\n")
          (append-text "third\n")
          (should (= (window-point window) (with-current-buffer buffer (point-max))))
          ;; Moved up: stays there.
          (set-window-point window 3)
          (launcher-buffer--follow window)
          (append-text "fourth\n")
          (should (= 3 (window-point window)))
          ;; Other views and windows are left alone.
          (set-window-point window (with-current-buffer buffer (point-max)))
          (launcher-buffer--follow window)
          (setf (launcher-buffer--session-view session) '(picker nil))
          (append-text "fifth\n")
          (should (< (window-point window) (with-current-buffer buffer (point-max)))))
      (set-window-buffer window old)
      (kill-buffer buffer))))

;;; launcher-tools-tests.el ends here
