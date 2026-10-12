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

(defun launcher-gui-query--state ()
  "Return query titles and selection from the active minibuffer."
  (let ((buffer (window-buffer (active-minibuffer-window))))
    (with-current-buffer buffer
      (list :input (minibuffer-contents-no-properties)
            :titles (mapcar (lambda (r) (plist-get r :title))
                            (launcher--query-session-results launcher--query-session))
            :selected (plist-get (launcher--results-current) :id)
            :notice (launcher-gui-tool--after-strings buffer)))))

(ert-deftest launcher-gui-tool-unified-input-and-result-back ()
  (launcher-gui-tool--with
    (let (states)
      (should (eq 'quit
                  (launcher-gui-tool--run
                   #'launcher-buffer
                   (list "d hello" (lambda () (push (launcher-gui-query--state) states))
                         'return (lambda () (should (eq 'result (car (launcher-buffer--session-view launcher-buffer--session)))))
                         'C-c 'C-b (lambda () (push (launcher-gui-query--state) states))
                         'escape))))
      (should (equal '("d hello" "d hello") (mapcar (lambda (s) (plist-get s :input)) states)))
      (should (equal '("hello") launcher-gui-tool--calls))
      (should (equal '("Dictionary: hello") (plist-get (car states) :titles)))
      (should-not launcher-gui--searched))))

(ert-deftest launcher-gui-tool-debounce-and-navigation ()
  (launcher-gui-tool--with
    (let* ((queries nil)
           (launcher-tools
            `(("d" :name "Dictionary" :debounce 0.2
                    :query ,(lambda (text)
                              (unless (string-blank-p text)
                                (push text queries)
                                (list (list :id text :title text :subtitle "Definition preview" :function #'ignore)))))))
           before after navigation changed)
      (should (eq 'quit
                  (launcher-gui-tool--run
                   #'launcher-buffer
                   (list "d hello"
                         (lambda () (setq before (launcher-gui-query--state)))
                         #'ignore
                         (lambda () (setq after (launcher-gui-query--state)))
                         'down 'up
                         (lambda () (setq navigation (length queries)))
                         "x" (lambda () (setq changed (launcher-gui-query--state)))
                         'escape))))
      (should-not (plist-get before :titles))
      (should (string-match-p "Querying Dictionary" (plist-get before :notice)))
      (should (equal '("hello") (plist-get after :titles)))
      (should (= 1 navigation))
      (should-not (member "hello" (plist-get changed :titles))))))

(ert-deftest launcher-gui-tool-duplicate-title-actions ()
  (launcher-gui-tool--with
    (let* ((executed nil)
           (launcher-tools
            `((nil :name "First" :query ,(lambda (_) (list (list :id 'same :title "Same" :function (lambda () (push 'first executed) nil)))))
              (nil :name "Second" :query ,(lambda (_) (list (list :id 'same :title "Same" :function (lambda () (push 'second executed) nil))))))))
      (should (eq 'returned (launcher-gui-tool--run #'launcher '("z" down return))))
      (should (equal '(second) executed)))))

(ert-deftest launcher-gui-tool-selection-survives-input-refresh ()
  (launcher-gui-tool--with
    (let* ((launcher-tools
            '((nil :name "Stable" :query
                   (lambda (text)
                     (list (list :id 'one :title (concat "One " text) :function #'ignore)
                           (list :id 'two :title (concat "Two " text) :function #'ignore))))))
           before after)
      (should (eq 'quit
                  (launcher-gui-tool--run
                   #'launcher
                   (list "z" 'down (lambda () (setq before (launcher-gui-query--state)))
                         "x" (lambda () (setq after (launcher-gui-query--state))) 'escape))))
      (should (eq 'two (plist-get before :selected)))
      (should (eq 'two (plist-get after :selected)))
      (should (equal '("One zx" "Two zx") (plist-get after :titles))))))

(ert-deftest launcher-gui-tool-action-failure-and-edit ()
  (launcher-gui-tool--with
    (let (state)
      (should (eq 'quit
                  (launcher-gui-tool--run
                   #'launcher-buffer
                   (list "d bad" 'return (lambda () (setq state (launcher-gui-query--state)))
                         'backspace 'backspace 'backspace "hello" 'return 'escape))))
      (should (equal "d bad" (plist-get state :input)))
      (should (string-match-p "No definition" (plist-get state :notice)))
      (should (equal '("bad" "hello") (reverse launcher-gui-tool--calls))))))

(ert-deftest launcher-gui-tool-history-restores-prefix ()
  (launcher-gui-tool--with
    (let (state)
      (should (eq 'quit
                  (launcher-gui-tool--run
                   #'launcher-buffer
                   (list "d hello" 'return 'C-c 'C-b
                         (lambda () (delete-minibuffer-contents) (insert "d "))
                         'M-p (lambda () (setq state (launcher-gui-query--state))) 'escape))))
      (should (equal "d hello" (plist-get state :input)))
      (should (equal '("hello") launcher-gui-tool--calls)))))

(ert-deftest launcher-gui-tool-calculator ()
  (launcher-gui-tool--with
    (dolist (command '(launcher launcher-buffer))
      (let (state)
        (should (eq 'returned
                    (launcher-gui-tool--run
                     command
                     (list (lambda () (insert "1 + 2 * 3"))
                           (lambda () (setq state (launcher-gui-query--state))) 'return))))
        (should (equal "= 7" (car (plist-get state :titles))))
        (should (equal "7" (car kill-ring)))))))

(provide 'launcher-tools-gui-tests)
;;; launcher-tools-gui-tests.el ends here
