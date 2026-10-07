;;; launcher-portal-gui-tests.el --- Launcher hosted in Portal's panel -*- lexical-binding: t; -*-

;; GUI ONLY: run with `bash test/portal-vm.sh', which builds Portal in the
;; test VM and runs test/portal-gui.sh there.  The user's proposed
;; `my/app-launcher' (test/portal-acceptance-config.el) presents
;; `launcher-buffer' in Portal's native panel.  Keys go through AppKit to
;; the panel; C-] runs a check's action between them, in order.  The app
;; index is a fixed list of real system apps with their real icons;
;; launching and browsing are stubbed.  Dictionary lookups are real.

(require 'portal-launcher-gui-tests)
(require 'portal-acceptance-config)
(require 'launcher-buffer)
(require 'launcher-osx-dictionary-gui-tests)
(require 'consult)
(require 'consult-imenu)

(declare-function portal--launcher-inspect "portal-launcher" ())
(declare-function portal-test-frontmost "test-terminal-input" (&optional app))
(declare-function portal-test-native-post-key "test-terminal-input" (code flags text &optional windowless))
(declare-function portal-test-native-target-frame "test-terminal-input" (title))
(declare-function portal-test-native-focus-target "test-terminal-input" ())

(defvar vertico-buffer-mode)
(defvar vertico-grid-mode)
(defvar vertico-indexed-mode)
(defvar osx-dictionary-current-dictionary)
(defvar osx-dictionary-previous-window-configuration)

(defconst launcher-portal--apps
  '(("Activity Monitor" . "/System/Applications/Utilities/Activity Monitor.app")
    ("Calculator" . "/System/Applications/Calculator.app")
    ("Calendar" . "/System/Applications/Calendar.app")
    ("Chess" . "/System/Applications/Chess.app")
    ("Contacts" . "/System/Applications/Contacts.app")
    ("Dictionary" . "/System/Applications/Dictionary.app")
    ("Mail" . "/System/Applications/Mail.app")
    ("Maps" . "/System/Applications/Maps.app")
    ("Notes" . "/System/Applications/Notes.app"))
  "App index: nine apps, so twelve candidates with the bangs.")

(defconst launcher-portal--keys
  `((return 36 0 "\r") (escape 53 0 "\e") (backspace 51 0 "\177")
    (C-c 8 262144 "\C-c") (C-b 11 262144 "\C-b") (C-v 9 262144 "\C-v"))
  "Named keys the checks press, as (NAME CODE FLAGS TEXT).")

(defvar launcher-portal--actions nil "Launches and searches, newest first.")
(defvar launcher-portal--icons nil "Directory of the checks' icon cache.")
(defvar launcher-portal--warm nil "How long the first real lookup took.")

(defun launcher-portal--long (_query)
  "A synthetic tool: a result buffer of five lines."
  (with-current-buffer (get-buffer-create "*Launcher Long*")
    (special-mode)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (portal-launcher-gui--lines 1 5)))
    (goto-char (point-min))
    (current-buffer)))

(defun launcher-portal--failing (query)
  "A synthetic tool that fails for QUERY."
  (error "Cannot look up %s" query))

(defconst launcher-portal--tools
  (append launcher-tools
          '(("l" :name "Long" :prompt "Text: " :function launcher-portal--long)
            ("e" :name "Failing" :prompt "Text: " :function launcher-portal--failing)))
  "The user's tools, and two synthetic ones.")

(defun launcher-portal--append (from to)
  "Append lines FROM to TO to the long result, from a timer, and wait."
  (run-at-time 0.05 nil
               (lambda ()
                 (with-current-buffer "*Launcher Long*"
                   (let ((inhibit-read-only t))
                     (save-excursion
                       (goto-char (point-max))
                       (insert "\n" (portal-launcher-gui--lines from to))))))))

(defun launcher-portal--wait ()
  "Let timers run and the panel refit, as idle time would."
  (sit-for 0.4)
  (portal-launcher-gui--settle))

(defun launcher-portal--state ()
  "Describe the launcher's view, in the panel."
  (let* ((session launcher-buffer--session)
         (view (and session (launcher-buffer--session-view session)))
         (mini (and session (launcher-buffer--session-minibuffer session)))
         (window (and session (launcher-buffer--session-window session))))
    (list :name nil
          :view (car view)
          :result (and (eq (car view) 'result) (buffer-name (cadr view)))
          :mode (and (eq (car view) 'result) (buffer-local-value 'major-mode (cadr view)))
          :label (and (buffer-live-p mini)
                      (with-current-buffer mini (minibuffer-prompt)))
          :input (and (buffer-live-p mini)
                      (with-current-buffer mini (minibuffer-contents-no-properties)))
          :notice (and (buffer-live-p mini) (launcher-gui-tool--after-strings mini))
          ;; Candidate rows drawn, without the picker's blank padding.
          :shown (and (buffer-live-p mini) (buffer-local-value 'vertico--input mini)
                      (with-current-buffer mini
                        (cl-count-if-not #'string-empty-p
                                         (cdr (split-string (or (overlay-get vertico--candidates-ov
                                                                             'before-string)
                                                                "")
                                                            "\n")))))
          :in-panel (and window (eq (window-frame window) portal-launcher--frame)
                         (eq window (frame-root-window portal-launcher--frame)))
          :selected (eq (selected-frame) portal-launcher--frame)
          :frontmost (portal-test-frontmost)
          :panel portal-launcher--frame
          :frames (frame-list))))

(defun launcher-portal--snap (&optional name)
  "An action recording the panel and the launcher's view, and capturing NAME."
  (lambda ()
    (portal-launcher-gui--record-buffer)
    (setcar portal-launcher-gui--layouts
            (append (plist-put (launcher-portal--state) :name name)
                    (car portal-launcher-gui--layouts)))
    (when name (portal-launcher-gui--capture name))))

(defun launcher-portal--drive (command strokes)
  "Run COMMAND with STROKES typed into Portal's panel; return how it ended.
A string types its characters, a symbol presses a named key, `quit'
quits, `wait' lets a stroke's time pass, a function is an action C-]
calls and (:now FUNCTION) calls FUNCTION from the driver's timer, as no
key would clear a minibuffer message first.  Strokes go to the panel
one by one while it reads input, as in `portal-launcher-gui--drive',
which this follows with a longer timeout for real lookups.  Return
`returned', `quit' or (error MESSAGE), and check that the session ended.
Portal hides its panel on Escape and C-g and returns normally."
  (let* ((actions nil)
         (queue (mapcan (lambda (stroke)
                          (cond ((stringp stroke) (portal-launcher-gui--text stroke))
                                ((memq stroke '(quit wait)) (list stroke))
                                ((eq (car-safe stroke) :now) (list stroke))
                                ((assq stroke launcher-portal--keys)
                                 (list (cdr (assq stroke launcher-portal--keys))))
                                ((functionp stroke)
                                 (setq actions (append actions (list stroke)))
                                 (list '(30 262144 "\C-]")))
                                (t (error "Unknown stroke %S" stroke))))
                        strokes))
         (inhibit-quit nil)
         (quit-flag nil)
         (map (make-sparse-keymap))
         (global (current-global-map))
         (depth (recursion-depth))
         (timer (run-with-timer
                 0.1 0.1
                 (lambda ()
                   (when (and queue (> (recursion-depth) depth))
                     (portal-view--with-frame-title portal-launcher--frame
                                                    #'portal-test-native-target-frame)
                     (pcase (pop queue)
                       ('quit (setq unread-command-events (append unread-command-events '(7))))
                       ('wait nil)
                       (`(:now ,function) (redisplay t) (funcall function))
                       (stroke (apply #'portal-test-native-post-key stroke))))))))
    (set-keymap-parent map global)
    (keymap-set map "C-]" (lambda () (interactive) (funcall (pop actions))))
    (use-global-map map)
    (unwind-protect
        (prog1 (condition-case err
                   (with-timeout (60 (error "Launcher check timed out; pending: %S" queue))
                     (call-interactively command)
                     'returned)
                 (quit 'quit)
                 (error (list 'error (error-message-string err))))
          (should-not queue)
          (should-not actions)
          (should (= depth (recursion-depth)))
          (should-not (active-minibuffer-window))
          (should-not portal-launcher--session)
          (should-not launcher-buffer--session)
          (should-not (plist-get (portal--launcher-inspect) :visible)))
      (cancel-timer timer)
      (portal-test-native-target-frame nil)
      (use-global-map global))))

(defun launcher-portal--leaks ()
  "Global state that a presentation must leave as it found it."
  (list emulation-mode-map-alists
        (default-value 'post-command-hook)
        (default-value 'pre-redisplay-functions)
        minibuffer-setup-hook
        (default-value 'minibuffer-exit-hook)
        window-buffer-change-functions
        display-buffer-alist
        display-buffer-overriding-action
        (default-value 'vertico-buffer-mode)
        (default-value 'vertico-count)
        (default-value 'mode-line-format)
        launcher-buffer--emulation
        (current-global-map)))

(defmacro launcher-portal--with (&rest body)
  "Run BODY with a Portal panel, the fixed app index and stubbed actions.
Check that the ordinary frame, window and global state are unchanged."
  (declare (indent 0))
  `(portal-launcher-gui--with-panel
     (let* ((launcher--apps launcher-portal--apps)
            (launcher--current-entries nil)
            (launcher-icon-cache-directory launcher-portal--icons)
            (launcher-tools launcher-portal--tools)
            (launcher-portal--actions nil)
            (osx-dictionary-current-dictionary nil)
            (osx-dictionary-previous-window-configuration nil)
            (frame (window-frame ordinary))
            (geometry (list (frame-position frame) (frame-pixel-width frame)
                            (frame-pixel-height frame)))
            (state (launcher-gui--window-state ordinary))
            (leaks (launcher-portal--leaks)))
       (cl-letf (((symbol-function 'launcher--launch)
                  (lambda (path) (push (list 'launch path) launcher-portal--actions)))
                 ((symbol-function 'browse-url)
                  (lambda (url &rest _) (push (list 'browse url) launcher-portal--actions))))
         ,@body)
       (should (equal leaks (launcher-portal--leaks)))
       (should (equal state (launcher-gui--window-state ordinary)))
       (should (equal geometry (list (frame-position frame) (frame-pixel-width frame)
                                     (frame-pixel-height frame))))
       (should-not (get-buffer "*osx-dictionary*"))
       (should-not osx-dictionary-previous-window-configuration)
       (dolist (name (list launcher-osx-dictionary-buffer-name "*Launcher Long*"))
         (when (get-buffer name) (kill-buffer name))))))

(defun launcher-portal--check-fitted (layouts)
  "Check that LAYOUTS fit their content, within the user's (content 480).
Each keeps one panel, session and keyboard focus, its top-left corner
and the user's width of 720 points, and Emacs's frame matches the native
height.  That height is what Portal's oracle,
`portal-launcher-gui--buffer-wanted', says the content wants, within
the limits, the legal height and the space below the top edge; below
the maximum, all the text drawn shows.  Unlike
`portal-launcher-gui--check-buffer-fitted', this does not require the
buffer's end to be visible: in the picker, it is on the empty line after
the last candidate, which a panel at its legal height leaves out."
  (let ((first (car layouts)))
    (dolist (layout layouts)
      (let ((wanted (portal-launcher-gui--buffer-wanted layout))
            (below (- (plist-get layout :top) (nth 1 (plist-get layout :area))))
            (height (plist-get layout :height)))
        (should (equal (list (plist-get layout :name) t t t t t t t t t 720 1)
                       (list (plist-get layout :name)
                             (eq (plist-get layout :session) (plist-get first :session))
                             (plist-get layout :key) (plist-get layout :native)
                             (plist-get layout :in-panel) (plist-get layout :selected)
                             (eq (plist-get layout :panel) (plist-get first :panel))
                             (equal (plist-get layout :frames) (plist-get first :frames))
                             (equal (plist-get layout :frontmost) (plist-get first :frontmost))
                             (equal (mapcar (lambda (key) (plist-get layout key)) '(:left :top))
                                    (mapcar (lambda (key) (plist-get first key)) '(:left :top)))
                             (plist-get layout :width) (plist-get layout :windows))))
        (should (equal (plist-get layout :frame) (list 720 height)))
        (should (= (plist-get layout :fitted) height))
        (should (= height (min below (max (plist-get layout :legal) (min 480 wanted)))))
        (when (< height (min 480 below))
          (should (<= (+ (plist-get layout :text) (plist-get layout :trailing))
                      (plist-get layout :body))))))))

(defun launcher-portal--summary (layouts)
  "Return the gist of LAYOUTS, for the log."
  (mapcar (lambda (layout)
            (list (plist-get layout :name) (plist-get layout :view)
                  (or (plist-get layout :result) (plist-get layout :input))
                  :shown (plist-get layout :shown) :height (plist-get layout :height)
                  :text (plist-get layout :text) :body (plist-get layout :body)
                  :start (plist-get layout :start)))
          layouts))

(defmacro launcher-portal--recording (&rest body)
  "Run BODY, then return the layouts its snaps recorded, oldest first."
  (declare (indent 0))
  `(let ((portal-launcher-gui--layouts nil)
         (timer-max-repeats 1))
     (message nil)
     ,@body
     (reverse portal-launcher-gui--layouts)))

;;; Checks

(ert-deftest launcher-portal-picker-and-dictionary ()
  ;; The daily flow in one presentation: the picker at six, two, none and
  ;; six candidates, the Dictionary query, a short and a long real
  ;; definition, scrolling and copying, Back, the dictionary's q, and
  ;; Back to the picker.  One panel and session throughout, fitted.
  (launcher-portal--with
    (let* (copied outcome
           (layouts
            (launcher-portal--recording
              (setq outcome
                    (launcher-portal--drive
                     'my/app-launcher
                     (list (launcher-portal--snap "60-panel-picker")
                           "cal" (launcher-portal--snap "61-panel-two")
                           "zz" (launcher-portal--snap "62-panel-none")
                           'backspace 'backspace 'backspace 'backspace 'backspace
                           (launcher-portal--snap "63-panel-six-again")
                           "d " (launcher-portal--snap "64-panel-query")
                           "hello" 'return (launcher-portal--snap "65-panel-hello")
                           'C-c 'C-b (launcher-portal--snap "66-panel-back")
                           'backspace 'backspace 'backspace 'backspace 'backspace
                           "set" 'return (launcher-portal--snap "67-panel-long")
                           'C-v 'C-v (launcher-portal--snap "68-panel-scrolled")
                           (lambda ()
                             (push-mark (point) t t)
                             (forward-line 2)
                             (call-interactively (key-binding (kbd "M-w")))
                             (setq copied (current-kill 0)))
                           "q" (launcher-portal--snap "69-panel-dictionary-q")
                           'backspace 'backspace 'backspace 'backspace
                           (launcher-portal--snap "70-panel-back-to-picker")
                           'escape))))))
      (message "Portal picker and dictionary: %S" outcome)
      (message "Portal picker and dictionary layouts: %S" (launcher-portal--summary layouts))
      (message "Portal copied: %S" copied)
      ;; Portal loads its native module with the first panel.
      (message "Portal screens: %S" (plist-get (portal--launcher-inspect) :screens))
      (should (eq outcome 'returned))
      (launcher-portal--check-fitted layouts)
      (pcase-let ((`(,six ,two ,none ,again ,query ,hello ,back ,long ,scrolled ,q ,picker)
                   layouts))
        (should (equal (mapcar (lambda (l) (plist-get l :view)) layouts)
                       '(picker picker picker picker query result query result result query picker)))
        (should (equal (mapcar (lambda (l) (plist-get l :shown)) (list six two none again))
                       '(6 2 0 6)))
        ;; Portal keeps a window at least `window-min-height' lines tall.
        (should (> (plist-get six :height) (plist-get two :height)))
        (should (>= (plist-get two :height) (plist-get none :height)))
        (should (= (plist-get six :height) (plist-get again :height)))
        (should (equal (plist-get query :label) "Dictionary — Word: "))
        (should (< (plist-get query :height) (plist-get six :height)))
        (dolist (result (list hello long scrolled))
          (should (equal (plist-get result :result) launcher-osx-dictionary-buffer-name))
          (should (eq (plist-get result :mode) 'osx-dictionary-mode)))
        ;; A short definition fits; a long one stops at 480, or at the
        ;; bottom of a smaller screen, and scrolls.
        (should (< (plist-get hello :height) 480))
        (should (plist-get hello :all))
        (should (= (min 480 (- (plist-get long :top) (nth 1 (plist-get long :area))))
                   (plist-get long :height) (plist-get scrolled :height)))
        (should (> (plist-get long :text) (plist-get long :body)))
        (should (= 1 (plist-get long :start)))
        (should (> (plist-get scrolled :start) 1))
        (should (equal (plist-get back :input) "hello"))
        (should (equal (plist-get q :input) "set"))
        (should (equal (plist-get picker :input) "d"))
        (should (string-match-p "\\`set\\b" (with-current-buffer launcher-osx-dictionary-buffer-name
                                               (buffer-string))))
        (should (> (length copied) 20)))
      (should-not launcher-portal--actions))))

(ert-deftest launcher-portal-growing-result ()
  ;; A synthetic result grows from a timer while shown, past the 480
  ;; maximum, keeps the reader's place once they scroll, shrinks and
  ;; grows again: the panel follows, keeps focus and never moves.  The
  ;; panel opens higher than the user's, so that 480 fits on a small screen.
  (launcher-portal--with
    (let* ((portal-launcher-offset '(0 -200))
           (layouts
            (launcher-portal--recording
              (should (eq 'returned
                          (launcher-portal--drive
                           'my/app-launcher
                           (list "l growing" 'return (launcher-portal--snap "71-panel-short")
                                 (lambda () (launcher-portal--append 6 60) (launcher-portal--wait))
                                 (launcher-portal--snap "72-panel-capped")
                                 'C-v (launcher-portal--snap "73-panel-reader")
                                 (lambda () (launcher-portal--append 61 80) (launcher-portal--wait))
                                 (launcher-portal--snap)
                                 (lambda ()
                                   (let ((inhibit-read-only t))
                                     (delete-region (point-min) (point-max))
                                     (insert (portal-launcher-gui--lines 1 2))
                                     (goto-char (point-min))))
                                 (launcher-portal--snap "74-panel-shrunk")
                                 (lambda () (launcher-portal--append 3 60) (launcher-portal--wait))
                                 (launcher-portal--snap "75-panel-regrown")
                                 'escape)))))))
      (message "Portal growing result: %S" (launcher-portal--summary layouts))
      (launcher-portal--check-fitted layouts)
      (pcase-let ((`(,short ,capped ,reader ,appended ,shrunk ,regrown) layouts))
        (should (< (plist-get short :height) 480))
        (should (plist-get short :all))
        (should (= 480 (plist-get capped :height) (plist-get reader :height)
                   (plist-get appended :height) (plist-get regrown :height)))
        (should (> (plist-get reader :start) 1))
        ;; Text added below the reader leaves their place alone.
        (should (equal (mapcar (lambda (key) (plist-get appended key)) '(:start :point))
                       (mapcar (lambda (key) (plist-get reader key)) '(:start :point))))
        (should (> (plist-get appended :end) (plist-get reader :end)))
        (should (< (plist-get shrunk :height) (plist-get short :height)))))))

(ert-deftest launcher-portal-endings-and-reentry ()
  ;; Every way an interaction ends, one presentation after another in the
  ;; same Emacs: each hides the panel and leaves nothing behind.
  (launcher-portal--with
    (portal-global-shortcut-set 'app-launcher "Control-Option-Command-F17" #'my/app-launcher)
    (let ((cases
           `((escape-picker returned escape)
             (quit-picker returned quit)
             (quit-query returned "d " quit)
             (escape-result returned "d hello" return escape)
             (quit-result returned "d hello" return quit)
             (back-to-picker returned "d hello" return C-c C-b C-c C-b ,(launcher-portal--snap) escape)
             ;; The error shows until the next key, so a timer records it.
             (error returned "e boom" return wait wait wait
                    (:now ,(launcher-portal--snap "76-panel-error")) escape)
             (shortcut nil "d hello" return
                       ,(lambda () (portal-launcher-gui--press 'app-launcher)))
             (focus-away nil "d hello" return
                         ,(lambda ()
                            (portal-view--with-frame-title (window-frame ordinary)
                                                           #'portal-test-native-target-frame)
                            (portal-test-native-focus-target)))
             (app returned "calc" return)
             (bang returned "!gh portal launcher" return)
             (fallback returned "zz top" return)))
          outcomes)
      (pcase-dolist (`(,name ,expected . ,strokes) cases)
        (let* ((leaks (launcher-portal--leaks))
               (layouts (launcher-portal--recording
                          (push (list name (launcher-portal--drive 'my/app-launcher strokes))
                                outcomes))))
          (message "Portal ending %s: %S %S" name (cadar outcomes)
                   (launcher-portal--summary layouts))
          (when expected (should (eq expected (cadar outcomes))))
          (should (equal leaks (launcher-portal--leaks)))
          (should (eq (selected-window) ordinary))
          (pcase name
            ('back-to-picker
             (should (equal '((picker "d")) (mapcar (lambda (l) (list (plist-get l :view)
                                                                      (plist-get l :input)))
                                                    layouts))))
            ('error
             (should (equal '((query "boom")) (mapcar (lambda (l) (list (plist-get l :view)
                                                                        (plist-get l :input)))
                                                      layouts)))
             (should (string-match-p "Failing failed: Cannot look up boom"
                                     (plist-get (car layouts) :notice)))))
          (portal-launcher-gui--settle-focus)))
      (message "Portal endings: %S" (reverse outcomes))
      (should (equal (reverse launcher-portal--actions)
                     '((launch "/System/Applications/Calculator.app")
                       (browse "https://github.com/search?q=portal%20launcher")
                       (browse "https://www.google.com/search?q=zz%20top")))))))

(ert-deftest launcher-portal-rollback-and-completion-elsewhere ()
  ;; The rollback command is today's minibuffer panel: apps and search,
  ;; no tools.  Outside Launcher, the user's multiform rules, directory
  ;; keys, Orderless and Marginalia still apply as configured.
  (launcher-portal--with
    (let (kinds)
      (should (eq 'returned
                  (launcher-portal--drive
                   'my/app-launcher-minibuffer
                   (list (lambda ()
                           (push (portal-launcher--kind portal-launcher--frame) kinds)
                           (portal-launcher-gui--capture "77-panel-old-minibuffer"))
                         "calc" 'return))))
      (should (eq 'returned (launcher-portal--drive 'my/app-launcher-minibuffer
                                                    (list "d hello" 'return))))
      (should (equal kinds '(minibuffer)))
      (should (equal (reverse launcher-portal--actions)
                     '((launch "/System/Applications/Calculator.app")
                       (browse "https://www.google.com/search?q=d%20hello"))))))
  (let (states)
    (cl-flet ((modes (command &rest args)
                ;; ERT runs from a timer, where quitting is inhibited.
                ;; Multiform finds a command's rules by `this-command'.
                (let ((inhibit-quit nil)
                      (this-command command))
                  (minibuffer-with-setup-hook
                      (:append (lambda ()
                                 (push (list command vertico-buffer-mode
                                             (bound-and-true-p vertico-grid-mode)
                                             (bound-and-true-p vertico-indexed-mode))
                                       states)
                                 (setq unread-command-events (list 7))))
                    (condition-case nil (apply command args) (quit nil))))))
      (modes #'read-file-name "File: ")
      (with-temp-buffer
        (insert "(defun launcher-portal-a ())\n(defun launcher-portal-b ())\n")
        (emacs-lisp-mode)
        (modes #'consult-imenu))
      (modes #'consult-grep default-directory))
    (message "Completion elsewhere: %S" states)
    (should (equal (reverse states)
                   '((read-file-name nil t nil)
                     (consult-imenu t nil t)
                     (consult-grep t nil nil))))
    (should (eq (keymap-lookup vertico-map "RET") #'vertico-directory-enter))
    (should (eq (keymap-lookup vertico-map "DEL") #'vertico-directory-delete-char))
    (should (equal completion-styles '(orderless partial-completion basic)))
    (should marginalia-mode)
    (should vertico-multiform-mode)
    (should-not (default-value 'vertico-buffer-mode))))

;;; Runner

(defun launcher-portal--prepare ()
  "Load the real dictionary, build its helper and make the apps' icons."
  (launcher-dictionary-real--load)
  (setq osx-dictionary-current-dictionary nil)
  (let ((start (float-time)))
    (kill-buffer (launcher-osx-dictionary-lookup "hello"))
    (setq launcher-portal--warm (- (float-time) start)))
  (setq launcher-portal--icons (file-name-as-directory (make-temp-file "launcher-icons" t)))
  (let ((launcher-icon-cache-directory launcher-portal--icons)
        (paths (mapcar #'cdr launcher-portal--apps)))
    (launcher-icons-prepare paths)
    (with-timeout (60 (error "Icons were not made"))
      (while (process-live-p launcher-icons--process)
        (accept-process-output nil 0.1)))
    (should (seq-every-p #'file-exists-p (mapcar #'launcher-icons--file paths)))))

(defun launcher-portal--echo (format &rest args)
  "Copy a message made from FORMAT and ARGS to the log as it is made."
  (when format
    (princ (concat (apply #'format-message format args) "\n") #'external-debugging-output)))

(defun launcher-portal--watchdog ()
  "Report what Emacs is doing after too long, and exit."
  (message "Watchdog: depth %s, minibuffer %S, panel session %S, launcher %S, pending %S\n%s"
           (recursion-depth) (active-minibuffer-window) portal-launcher--session
           (and launcher-buffer--session (launcher-buffer--session-view launcher-buffer--session))
           unread-command-events (with-output-to-string (backtrace)))
  (kill-emacs 1))

(defun launcher-portal-run-and-exit ()
  "Run the checks matching $LAUNCHER_TEST_SELECTOR, then exit.
Exit 0 only if all ran as expected, with none skipped.  Messages go to
the log as they are made; after ten minutes, a watchdog exits."
  (let ((status 1)
        (message-log-max t))
    (advice-add 'message :after #'launcher-portal--echo)
    (run-at-time 600 nil #'launcher-portal--watchdog)
    (unwind-protect
        (condition-case err
            (progn
              (launcher-portal--prepare)
              (message "Environment: %s; repository %s; macOS %s; Portal %s; launcher.el %s; \
osx-dictionary %s; Vertico %s; font %s; theme %S; first lookup with helper build %.1fs"
                       (emacs-version) emacs-repository-version
                       (string-trim (shell-command-to-string "sw_vers -productVersion"))
                       (getenv "PORTAL_REVISION") (getenv "LAUNCHER_REVISION")
                       launcher-dictionary-real--revision
                       (with-temp-buffer
                         (insert-file-contents (locate-library "vertico.el"))
                         (and (re-search-forward "^;; Version: \\(.*\\)" nil t)
                              (match-string 1)))
                       (font-get (face-attribute 'default :font) :name)
                       custom-enabled-themes launcher-portal--warm)
              (message "Libraries: %S"
                       (mapcar #'locate-library '("launcher" "portal-launcher" "vertico"
                                                  "osx-dictionary" "consult")))
              (let ((stats (ert-run-tests-batch (getenv "LAUNCHER_TEST_SELECTOR"))))
                (when (and (> (ert-stats-total stats) 0)
                           (= (ert-stats-completed-expected stats) (ert-stats-total stats)))
                  (setq status 0))))
          (error (message "Launcher Portal checks failed: %S\n%s" err
                          (with-output-to-string (backtrace)))))
      (kill-emacs status))))

(provide 'launcher-portal-gui-tests)
;;; launcher-portal-gui-tests.el ends here
