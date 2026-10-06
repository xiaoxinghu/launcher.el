;;; launcher-tools-gui-tests.el --- Graphical tool routing checks -*- lexical-binding: t; -*-

;; GUI ONLY: run in the test VM with `bash test/vm.sh', which runs
;; test/gui.sh.  Keys go through AppKit's event queue into a disposable
;; graphical Emacs with neither Portal nor a user init, as in
;; launcher-buffer-gui-tests.el, whose fixture these checks share.
;; Handlers are fakes returning real buffers: no dictionary, network,
;; app launch or browser is involved.

(require 'launcher-buffer-gui-tests)

(defconst launcher-gui-tool--keys
  (append launcher-gui--keys
          `((s-v 9 1048576 "v") (M-p 35 524288 "p") (M-w 13 524288 "w")
            (C-SPC 49 262144 " ") (gt 47 131072 ">")))
  "Named keys of these checks: those of the shared checks, paste,
history, copy, mark and `end-of-buffer' in a special mode.")

(defvar launcher-gui-tool--calls nil "Queries the fake handlers got, newest first.")

(defvar-keymap launcher-gui-tool-result-mode-map
  "k" #'launcher-gui--kill-result)

(define-derived-mode launcher-gui-tool-result-mode special-mode "Definition"
  "Read-only synthetic definition with its own keys.")

(defun launcher-gui-tool--definition (query)
  "Fake dictionary handler: log QUERY, return a long read-only result."
  (push query launcher-gui-tool--calls)
  (pcase query
    ("bad" (error "No definition for %s" query))
    ("none" nil)
    (_ (with-current-buffer (get-buffer-create "*launcher GUI definition*")
         (let ((inhibit-read-only t))
           (erase-buffer)
           (insert (format "Definition of %s\n\n" query))
           (dotimes (i 60) (insert (format "Sense %d of %s\n" i query))))
         (launcher-gui-tool-result-mode)
         (goto-char (point-min))
         (current-buffer)))))

(defvar launcher-gui-tool--stream-timer nil "Timer updating the stream result.")

(defun launcher-gui-tool--stream (query)
  "Fake streaming handler: return a buffer a timer keeps appending to.
Its point is at the end of a header, so a view of it follows the end."
  (push query launcher-gui-tool--calls)
  (let ((buffer (get-buffer-create "*launcher GUI stream*"))
        (chunk 0))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "Answer to %s\n" query))
        (dotimes (i 30) (insert (format "Line %d\n" i))))
      (launcher-gui-tool-result-mode))
    (launcher-gui-tool--stop-stream)
    (setq launcher-gui-tool--stream-timer
          (run-with-timer
           0.1 0.1
           (lambda ()
             (when (buffer-live-p buffer)
               (with-current-buffer buffer
                 (let ((inhibit-read-only t))
                   (save-excursion
                     (goto-char (point-max))
                     (insert (format "Chunk %d\n" (cl-incf chunk))))))))))
    buffer))

(defun launcher-gui-tool--stop-stream ()
  (when launcher-gui-tool--stream-timer
    (cancel-timer launcher-gui-tool--stream-timer)
    (setq launcher-gui-tool--stream-timer nil)))

(defconst launcher-gui-tool--tools
  '(("d" :name "Dictionary" :prompt "Word: "
     :function launcher-gui-tool--definition)
    ("s" :name "Stream" :prompt "Question: "
     :function launcher-gui-tool--stream)
    ("m" :name "Missing" :prompt "Word: "
     :function launcher-gui-tool--undefined))
  "Tools of these checks.")

(defmacro launcher-gui-tool--with (&rest body)
  "Run BODY in the shared fixture, with this file's tools and keys."
  (declare (indent 0))
  `(launcher-gui--with-fixture
     (let ((launcher-tools launcher-gui-tool--tools)
           (launcher--tool-histories (make-hash-table :test #'equal))
           (launcher-gui--keys launcher-gui-tool--keys)
           (launcher-gui-tool--calls nil)
           (minibuffer-history nil)
           (kill-ring nil)
           (kill-ring-yank-pointer nil))
       (vertico-mode 1)
       (unwind-protect (progn ,@body)
         (vertico-mode -1)
         (launcher-gui-tool--stop-stream)
         (dolist (name '("*launcher GUI definition*" "*launcher GUI stream*"))
           (when (get-buffer name) (kill-buffer name)))))
     (should-not (memq #'launcher-buffer--follow
                       (default-value 'pre-redisplay-functions)))
     (should-not launcher--back-function)))

(defun launcher-gui-tool--after-strings (buffer)
  "Return the after-strings of BUFFER's overlays, as a minibuffer notice."
  (with-current-buffer buffer
    (mapconcat (lambda (overlay) (or (overlay-get overlay 'after-string) ""))
               (overlays-in (point-min) (point-max)) "")))

(defun launcher-gui-tool--minibuffer-state (buffer)
  "Return the prompt, input, notice and Vertico use of minibuffer BUFFER."
  (and (buffer-live-p buffer) (minibufferp buffer)
       (with-current-buffer buffer
         (list :prompt (buffer-substring-no-properties (point-min) (minibuffer-prompt-end))
               :input (minibuffer-contents-no-properties)
               :notice (launcher-gui-tool--after-strings buffer)
               :vertico (and (overlayp vertico--candidates-ov) t)))))

(defun launcher-gui-tool--snapshot (&optional name)
  "Return the observable state of the launcher and the minibuffer.
With NAME, also capture the frame's drawing."
  (redisplay t)
  (when name (launcher-gui--capture name))
  (let* ((session launcher-buffer--session)
         (window (if session (launcher-buffer--session-window session) (selected-window)))
         (shown (window-buffer window))
         (mini (active-minibuffer-window)))
    (append
     (list :view (and session (car (launcher-buffer--session-view session)))
           :window window
           :buffer shown
           :mode (buffer-local-value 'major-mode shown)
           :point (window-point window)
           :start (window-start window)
           :end (with-current-buffer shown (point-max))
           :top (= (window-start window) (with-current-buffer shown (point-min)))
           :selected (selected-window)
           :frames (length (frame-list))
           :windows (length (window-list (window-frame window) 'never))
           :depth (recursion-depth)
           :calls (reverse launcher-gui-tool--calls)
           :mini-hidden (and mini (> (window-vscroll mini t) 0))
           ;; Does an ordinary window show a minibuffer?
           :window-minibuffer (cl-some (lambda (w) (minibufferp (window-buffer w)))
                                       (window-list nil 'never))
           :mini (and mini (launcher-gui-tool--minibuffer-state (window-buffer mini))))
     (launcher-gui-tool--minibuffer-state shown))))

(defun launcher-gui-tool--run (command strokes)
  "Drive COMMAND with STROKES; return `returned', `quit' or (error MESSAGE)."
  (condition-case err
      (progn (launcher-gui--drive command strokes) 'returned)
    (quit 'quit)
    (error (list 'error (error-message-string err)))))

(defmacro launcher-gui-tool--snaps (&rest body)
  "Run BODY with `snap' making a stroke that records a snapshot.
Return the snapshots, oldest first."
  (declare (indent 0))
  `(let (states)
     (cl-flet ((snap (&optional name)
                 (lambda () (push (launcher-gui-tool--snapshot name) states))))
       ,@body)
     (nreverse states)))

(defun launcher-gui-tool--summary (states)
  "Return the gist of STATES, for the log."
  (mapcar (lambda (s) (list (plist-get s :view) (buffer-name (plist-get s :buffer))
                            (plist-get s :prompt) (plist-get s :input)
                            (plist-get s :notice) (plist-get s :calls)))
          states))

;;; Checks

(ert-deftest launcher-gui-tool-buffer-flow ()
  "`launcher-buffer': d SPC, a word, its result, Back twice and DEL."
  (launcher-gui-tool--with
    (let* ((window (selected-window))
           (before (launcher-gui--window-state window))
           outcome
           (states
            (launcher-gui-tool--snaps
              (setq outcome
                    (launcher-gui-tool--run
                     #'launcher-buffer
                     (list "d" (snap "20-tool-prefix-typed")
                           " " (snap "21-tool-query")
                           "serendipity" (snap "22-tool-query-typed")
                           'return (snap "23-tool-result")
                           ;; The result's keys: Space scrolls; mark and copy.
                           " " (snap) 'C-SPC 'down 'M-w (snap)
                           'C-c 'C-b (snap "24-tool-back-to-query")
                           'C-c 'C-b (snap "25-tool-back-to-picker")
                           " " (snap) 'backspace (snap)
                           'escape))))))
      (message "Tool buffer states: %S" (launcher-gui-tool--summary states))
      (should (eq 'quit outcome))
      (pcase-let ((`(,typed ,query ,word ,result ,scrolled ,copied ,back ,picker
                            ,again ,deleted)
                   states))
        ;; "d" alone is picker input; "d SPC" leaves the candidates at once.
        (should (eq 'picker (plist-get typed :view)))
        (should (equal "d" (plist-get typed :input)))
        (should (plist-get typed :vertico))
        (dolist (state (list query word back again))
          (should (eq 'query (plist-get state :view)))
          (should (eq window (plist-get state :window)))
          (should (equal "Dictionary — Word: " (plist-get state :prompt)))
          (should-not (plist-get state :vertico))
          (should (plist-get state :top))
          (should (plist-get state :mini-hidden))
          (should (= 1 (plist-get state :windows))))
        (should (equal "" (plist-get query :input)))
        (should (equal "serendipity" (plist-get word :input)))
        ;; The window's point follows the input, for its cursor.
        (should (= (plist-get word :point) (plist-get word :end)))
        ;; Typing called nothing; Return called the handler once.
        (should-not (plist-get word :calls))
        (should (equal '("serendipity") (plist-get result :calls)))
        (should (eq 'result (plist-get result :view)))
        (should (eq window (plist-get result :window)))
        (should (equal "*launcher GUI definition*" (buffer-name (plist-get result :buffer))))
        (should (eq 'launcher-gui-tool-result-mode (plist-get result :mode)))
        (should (= 1 (plist-get result :depth)))
        (should (> (plist-get scrolled :start) 1))
        (should (string-prefix-p "Sense " (car kill-ring)))
        (should (eq 'result (plist-get copied :view)))
        ;; Back: the query keeps its word, without another call.
        (should (equal "serendipity" (plist-get back :input)))
        (should (equal '("serendipity") (plist-get back :calls)))
        (should (eq 'picker (plist-get picker :view)))
        (should (equal "d" (plist-get picker :input)))
        (should (equal "" (plist-get again :input)))
        (should (eq 'picker (plist-get deleted :view)))
        (should (equal "d" (plist-get deleted :input))))
      (should (equal '("serendipity") launcher-gui-tool--calls))
      (should-not launcher-gui--launched)
      (should-not launcher-gui--searched)
      (should (equal before (launcher-gui--window-state window)))
      (let ((result (get-buffer "*launcher GUI definition*")))
        (should (eq 'launcher-gui-tool-result-mode (buffer-local-value 'major-mode result)))
        (should (buffer-local-value 'buffer-read-only result))
        (should (string-prefix-p "Definition of serendipity"
                                 (with-current-buffer result (buffer-string))))))))

(ert-deftest launcher-gui-tool-minibuffer-flow ()
  "`launcher': the query in the minibuffer, then ordinary display."
  (launcher-gui-tool--with
    (let ((window (selected-window))
          outcomes states)
      (dolist (vertico '(t nil))
        (vertico-mode (if vertico 1 -1))
        (setq launcher-gui-tool--calls nil)
        (let ((run (launcher-gui-tool--snaps
                     ;; Back by C-c C-b and by DEL, then a lookup.
                     (push (launcher-gui-tool--run
                            #'launcher
                            (list "d " (snap (and vertico "26-minibuffer-query"))
                                  'C-c 'C-b (snap) " " 'backspace (snap)
                                  " word" 'return))
                           outcomes)
                     (funcall (snap (and vertico "27-minibuffer-result")))
                     (delete-other-windows window)
                     (switch-to-buffer "*launcher GUI origin*")
                     ;; C-g quits a query.
                     (push (launcher-gui-tool--run #'launcher '("d " "word" quit))
                           outcomes))))
          (push (cons vertico run) states)))
      (setq outcomes (nreverse outcomes) states (nreverse states))
      (message "Minibuffer outcomes %S states %S" outcomes
               (mapcar (lambda (run) (cons (car run) (launcher-gui-tool--summary
                                                      (mapcar (lambda (s) (append (plist-get s :mini) s))
                                                              (cdr run)))))
                       states))
      (should (equal '(returned quit returned quit) outcomes))
      (dolist (run states)
        (pcase-let ((`(,query ,back ,deleted ,shown) (cdr run)))
          ;; The query reads plain text in the minibuffer, in no window.
          (should (equal "Dictionary — Word: " (plist-get (plist-get query :mini) :prompt)))
          (should (equal "" (plist-get (plist-get query :mini) :input)))
          (should-not (plist-get (plist-get query :mini) :vertico))
          (should-not (plist-get query :window-minibuffer))
          ;; Back returns to the picker, with the prefix.
          (dolist (state (list back deleted))
            (should (equal "Launch: " (plist-get (plist-get state :mini) :prompt)))
            (should (equal "d" (plist-get (plist-get state :mini) :input)))
            (should (eq (car run) (plist-get (plist-get state :mini) :vertico))))
          ;; After the minibuffer, ordinary display shows and selects it.
          (should (equal "*launcher GUI definition*" (buffer-name (plist-get shown :buffer))))
          (should-not (plist-get shown :mini))))
      (should (equal '("word") launcher-gui-tool--calls))
      (should-not launcher-gui--launched)
      (should-not launcher-gui--searched))))

(ert-deftest launcher-gui-tool-paste-history-and-blank ()
  "Pasted and recalled input routes; blank queries call nothing."
  (launcher-gui-tool--with
    (let ((query "中文 café  two words")
          outcomes)
      (kill-new (concat "d " query))
      (setq minibuffer-history '("d recalled"))
      (let ((states
             (launcher-gui-tool--snaps
               (push (launcher-gui-tool--run
                      #'launcher-buffer
                      (list 's-v (snap "28-tool-pasted") 'return (snap) 'escape))
                     outcomes)
               (push (launcher-gui-tool--run
                      #'launcher-buffer
                      (list 'M-p (snap) 'escape))
                     outcomes)
               (push (launcher-gui-tool--run
                      #'launcher-buffer
                      (list "d " 'return (snap "29-tool-blank") "   " 'return (snap)
                            'escape))
                     outcomes))))
        (message "Paste states: %S" (launcher-gui-tool--summary states))
        (should (equal '(quit quit quit) outcomes))
        (pcase-let ((`(,pasted ,result ,recalled ,blank ,spaces) states))
          (should (eq 'query (plist-get pasted :view)))
          (should (equal query (plist-get pasted :input)))
          (should (equal (list query) (plist-get result :calls)))
          (should (eq 'query (plist-get recalled :view)))
          (should (equal "recalled" (plist-get recalled :input)))
          (dolist (state (list blank spaces))
            (should (eq 'query (plist-get state :view)))
            (should (string-match-p "Type a query first" (plist-get state :notice))))
          (should (equal "   " (plist-get spaces :input))))
        (should (equal (list query) launcher-gui-tool--calls))
        ;; Routed input stays out of the picker's history; queries go to
        ;; the tool's own.
        (should (equal '("d recalled") minibuffer-history))
        (should (equal (list query)
                       (symbol-value (launcher--tool-history (car (launcher--tools))))))
        (should-not launcher-gui--searched)))))

(ert-deftest launcher-gui-tool-errors-kill-and-reentry ()
  "Failures keep the query; a killed result returns to it; reentry works."
  (launcher-gui-tool--with
    (let (outcomes)
      (let ((states
             (launcher-gui-tool--snaps
               (push (launcher-gui-tool--run
                      #'launcher-buffer
                      (list "d bad" 'return (snap "30-tool-error")
                            'backspace 'backspace 'backspace "none" 'return (snap)
                            'backspace 'backspace 'backspace 'backspace "good" 'return
                            (snap) "k" (snap "31-tool-killed-result") 'escape))
                     outcomes)
               (push (launcher-gui-tool--run
                      #'launcher-buffer
                      (list "m word" 'return (snap) 'quit))
                     outcomes)
               (push (launcher-gui-tool--run
                      #'launcher
                      (list "d bad" 'return (snap) 'quit))
                     outcomes)
               (push (launcher-gui-tool--run #'launcher-buffer '("Notes" return))
                     outcomes))))
        (message "Error states: %S" (launcher-gui-tool--summary states))
        (should (equal '(quit quit quit returned) (nreverse outcomes)))
        (pcase-let ((`(,bad ,none ,good ,killed ,missing ,minibuffer-bad) states))
          (should (eq 'query (plist-get bad :view)))
          (should (equal "bad" (plist-get bad :input)))
          (should (string-match-p "Dictionary failed: No definition for bad"
                                  (plist-get bad :notice)))
          (should (equal "none" (plist-get none :input)))
          (should (string-match-p "returned nil instead of a live buffer"
                                  (plist-get none :notice)))
          (should (eq 'result (plist-get good :view)))
          (should (eq 'query (plist-get killed :view)))
          (should (equal "good" (plist-get killed :input)))
          (should (string-match-p "Missing needs .launcher-gui-tool--undefined., which is not defined"
                                  (plist-get missing :notice)))
          (should (equal "word" (plist-get missing :input)))
          (should (string-match-p "No definition for bad"
                                  (plist-get (plist-get minibuffer-bad :mini) :notice)))
          (should (equal "bad" (plist-get (plist-get minibuffer-bad :mini) :input)))))
      (should (equal '("bad" "none" "good" "bad") (reverse launcher-gui-tool--calls)))
      (should (equal '("/Applications/Notes.app") launcher-gui--launched))
      (should-not launcher-gui--searched))))

(ert-deftest launcher-gui-tool-async-follow-and-ownership ()
  "Late updates follow only at the end, and never reopen a view."
  (launcher-gui-tool--with
    (let* ((window (selected-window))
           (before (launcher-gui--window-state window))
           ;; A stroke doing nothing while chunks arrive.
           (wait (lambda () nil))
           outcome)
      (let ((states
             (launcher-gui-tool--snaps
               (setq outcome
                     (launcher-gui-tool--run
                      #'launcher-buffer
                      (list "s why" 'return (snap) wait wait wait
                            (snap "32-tool-stream-following")
                            'up 'up (snap) wait wait wait (snap)
                            'gt (snap) wait wait wait (snap)
                            'C-c 'C-b (snap) wait wait wait (snap)
                            'escape))))))
        (message "Stream states: %S"
                 (mapcar (lambda (s) (list (plist-get s :view) (plist-get s :point)
                                           (plist-get s :end) (plist-get s :start)))
                         states))
        (should (eq 'quit outcome))
        (pcase-let ((`(,shown ,following ,up ,held ,ended ,followed ,back ,later) states))
          ;; At the end of the header, the view follows the chunks.
          (should (eq 'result (plist-get shown :view)))
          (should (> (plist-get following :end) (plist-get shown :end)))
          (should (= (plist-get following :point) (plist-get following :end)))
          (should (> (plist-get following :start) 1))
          ;; Moved up, the reader stays put while chunks arrive.
          (should (< (plist-get up :point) (plist-get up :end)))
          (should (> (plist-get held :end) (plist-get up :end)))
          (should (= (plist-get held :point) (plist-get up :point)))
          ;; Back at the end, following resumes.
          (should (= (plist-get ended :point) (plist-get ended :end)))
          (should (> (plist-get followed :end) (plist-get ended :end)))
          (should (= (plist-get followed :point) (plist-get followed :end)))
          ;; Updates to the result do not bring it back over the query.
          (dolist (state (list back later))
            (should (eq 'query (plist-get state :view)))
            (should (equal "why" (plist-get state :input)))
            (should (minibufferp (plist-get state :buffer))))))
      ;; After the interaction, updates change only their own buffer.
      (let* ((stream (get-buffer "*launcher GUI stream*"))
             (size (buffer-size stream)))
        (sit-for 0.5)
        (should (> (buffer-size stream) size))
        (should (equal before (launcher-gui--window-state window)))
        (should (eq window (selected-window)))
        (should-not (get-buffer-window stream t))
        (should-not launcher-buffer--session))
      (should (equal '("why") launcher-gui-tool--calls)))))

(ert-deftest launcher-gui-tool-preexisting-result-and-other-window ()
  "A handler's own buffer, shown elsewhere, stays as it was."
  (launcher-gui-tool--with
    (let* ((own (get-buffer-create "*launcher GUI own*"))
           (window (selected-window))
           (other (split-window window nil 'below))
           (launcher-tools `(("o" :name "Own" :prompt "Q: "
                              :function ,(lambda (query)
                                           (push query launcher-gui-tool--calls)
                                           own)))))
      (unwind-protect
          (progn
            (with-current-buffer own
              (dotimes (i 80) (insert (format "Own line %d\n" i)))
              (read-only-mode 1))
            (set-window-buffer other own)
            (with-selected-window other
              (goto-char (point-min))
              (forward-line 30)
              (recenter 0))
            (let ((other-state (launcher-gui--window-state other))
                  (contents (with-current-buffer own (buffer-string)))
                  (before (launcher-gui--window-state window))
                  outcome states)
              (setq states (launcher-gui-tool--snaps
                             (setq outcome
                                   (launcher-gui-tool--run
                                    #'launcher-buffer
                                    (list "o x" 'return (snap) 'C-c 'C-b (snap)
                                          'escape)))))
              (should (eq 'quit outcome))
              (should (eq own (plist-get (car states) :buffer)))
              (should (eq window (plist-get (car states) :window)))
              (should (eq 'query (plist-get (cadr states) :view)))
              (should (equal other-state (launcher-gui--window-state other)))
              (should (equal before (launcher-gui--window-state window)))
              (should (equal contents (with-current-buffer own (buffer-string))))
              (should (buffer-local-value 'buffer-read-only own))
              (should (eq 'fundamental-mode (buffer-local-value 'major-mode own)))))
        (kill-buffer own)))))

(ert-deftest launcher-gui-tool-without-apps ()
  "Without Spotlight's mdfind, both entry points still reach tools."
  (launcher-gui-tool--with
    (let ((launcher--apps nil)
          (exec-path nil)
          outcomes states)
      (setq states
            (launcher-gui-tool--snaps
              (push (launcher-gui-tool--run
                     #'launcher-buffer
                     (list (snap "33-tool-no-apps") "d word" 'return 'escape))
                    outcomes)
              (push (launcher-gui-tool--run #'launcher (list (snap) "d word" 'return))
                    outcomes)))
      (message "No-app states: %S"
               (mapcar (lambda (s) (list (plist-get s :prompt)
                                         (plist-get (plist-get s :mini) :prompt)))
                       states))
      (should (equal '(quit returned) (nreverse outcomes)))
      (should (equal "Launch (apps unavailable): " (plist-get (nth 0 states) :prompt)))
      (should (equal "Launch (apps unavailable): "
                     (plist-get (plist-get (nth 1 states) :mini) :prompt)))
      (should (equal '("word" "word") launcher-gui-tool--calls))
      (should (string-match-p "Launcher apps unavailable: Cannot find .mdfind. in PATH"
                              (with-current-buffer "*Messages*" (buffer-string)))))))

(defun launcher-gui-tool--insert-custom ()
  "A user's command in `launcher-query-map'."
  (interactive)
  (insert "custom"))

(ert-deftest launcher-gui-tool-custom-reader-and-keys ()
  "A custom reader, Orderless, Marginalia and multiform rules keep routing;
a user's key in `launcher-query-map' works in the query."
  (launcher-gui-tool--with
    (let* ((completion-styles '(orderless basic))
           (vertico-multiform-commands '((launcher-buffer grid)))
           (reader-prompts nil)
           (completing-read-function
            (lambda (prompt &rest args)
              (push prompt reader-prompts)
              (apply #'completing-read-default prompt args)))
           outcome states)
      (keymap-set launcher-query-map "C-c u" #'launcher-gui-tool--insert-custom)
      (vertico-multiform-mode 1)
      (marginalia-mode 1)
      (unwind-protect
          (let ((this-command 'launcher-buffer))
            (setq states (launcher-gui-tool--snaps
                           (setq outcome
                                 (launcher-gui-tool--run
                                  #'launcher-buffer
                                  (list "dic" (snap) 'backspace 'backspace " "
                                        'C-c "u" (snap) 'return 'escape))))))
        (keymap-unset launcher-query-map "C-c u" t)
        (vertico-multiform-mode -1)
        (marginalia-mode -1))
      (message "Custom states: %S reader %S" (launcher-gui-tool--summary states)
               reader-prompts)
      (should (eq 'quit outcome))
      (should (eq 'picker (plist-get (car states) :view)))
      (should (equal "dic" (plist-get (car states) :input)))
      (should (equal "custom" (plist-get (cadr states) :input)))
      (should (equal '("custom") launcher-gui-tool--calls))
      (should (member "Launch: " reader-prompts)))))

(provide 'launcher-tools-gui-tests)
;;; launcher-tools-gui-tests.el ends here
