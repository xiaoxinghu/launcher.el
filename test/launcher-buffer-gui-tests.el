;;; launcher-buffer-gui-tests.el --- Graphical launcher-buffer checks -*- lexical-binding: t; -*-

;; GUI ONLY: run in the test VM with `bash test/vm.sh', which runs
;; test/gui.sh.  Keys go through AppKit's event queue into a disposable
;; graphical Emacs with neither Portal nor a user init.  Launching and
;; browsing are stubbed: nothing is opened.

(require 'ert)
(require 'cl-lib)
(require 'launcher-buffer)
(require 'vertico)
(require 'vertico-buffer)
(require 'vertico-multiform)
(require 'vertico-grid)
(require 'vertico-flat)
(require 'vertico-directory)
(require 'orderless)
(require 'marginalia)

(declare-function launcher-test-key "native-input" (code flags text))
(declare-function launcher-test-capture "native-input" (file))

(defconst launcher-gui--apps
  (mapcar (lambda (name) (cons name (format "/Applications/%s.app" name)))
          '("Activity Monitor" "Calculator" "Calendar" "Chess" "Contacts"
            "Dictionary" "Mail" "Maps" "Notes"))
  "Fake application index: nine apps, so twelve candidates with bangs.")

(defconst launcher-gui--codes
  '((?a . 0) (?b . 11) (?c . 8) (?d . 2) (?e . 14) (?f . 3) (?g . 5)
    (?h . 4) (?i . 34) (?j . 38) (?k . 40) (?l . 37) (?m . 46) (?n . 45)
    (?o . 31) (?p . 35) (?q . 12) (?r . 15) (?s . 1) (?t . 17) (?u . 32)
    (?v . 9) (?w . 13) (?x . 7) (?y . 16) (?z . 6) (?\s . 49) (?! . 18)
    (?0 . 29))
  "macOS virtual key codes of the characters the checks type.")

(defconst launcher-gui--keys
  `((return 36 0 "\r") (escape 53 0 "\e") (backspace 51 0 "\177")
    (down 125 0 ,(string #xf701)) (up 126 0 ,(string #xf700))
    (C-c 8 262144 "\C-c") (C-b 11 262144 "\C-b") (C-x 7 262144 "\C-x"))
  "Named keys the checks press, as (NAME CODE FLAGS TEXT).")

(defvar launcher-gui--launched nil "Apps the stubbed launcher opened.")
(defvar launcher-gui--searched nil "URLs the stubbed browser opened.")

(defun launcher-gui--strokes (strokes)
  "Expand STROKES into native key presses, quits and actions.
A string types its characters, a symbol presses a named key or quits,
and a function is an action called between key presses."
  (mapcan (lambda (stroke)
            (cond ((stringp stroke)
                   (mapcar (lambda (char)
                             (list (alist-get (downcase char) launcher-gui--codes)
                                   (if (or (<= ?A char ?Z) (= char ?!)) 131072 0)
                                   (string char)))
                           stroke))
                  ((eq stroke 'quit) (list 'quit))
                  ((symbolp stroke)
                   (list (or (cdr (assq stroke launcher-gui--keys))
                             (error "Unknown key %s" stroke))))
                  (t (list stroke))))
          strokes))

(defun launcher-gui--drive (start strokes)
  "Call START with STROKES delivered by the event loop; return its value.
Strokes go one at a time, by timer, while START waits for input.  Native
C-g is held by this Emacs build, so `quit' queues the Emacs event."
  (let* ((queue (launcher-gui--strokes strokes))
         ;; ERT runs from a timer, where quitting is inhibited.
         (inhibit-quit nil)
         (quit-flag nil)
         (timer (run-with-timer
                 0.15 0.15
                 (lambda ()
                   (when queue
                     (let ((stroke (pop queue)))
                       (cond ((functionp stroke)
                              (redisplay t)
                              (funcall stroke))
                             ((eq stroke 'quit)
                              (setq unread-command-events
                                    (append unread-command-events '(7))))
                             (t (apply #'launcher-test-key stroke)))))))))
    (unwind-protect
        (with-timeout (30 (error "Launcher check timed out; pending: %S" queue))
          (funcall start))
      (cancel-timer timer)
      (should-not queue))))

(defun launcher-gui--capture (name)
  "Save the frame's drawing as NAME.png in $LAUNCHER_TEST_SCREENSHOTS."
  (when-let* ((directory (getenv "LAUNCHER_TEST_SCREENSHOTS")))
    (redisplay t)
    (launcher-test-capture (expand-file-name (concat name ".png") directory))))

(defun launcher-gui--selected-row (string)
  "Return the line number of Vertico's selected candidate in STRING."
  (let ((pos 0) found)
    (while (and pos (not found) (< pos (length string)))
      (if (memq 'vertico-current (ensure-list (get-text-property pos 'face string)))
          (setq found pos)
        (setq pos (next-single-property-change pos 'face string))))
    (and found (cl-count ?\n string :end found))))

(defun launcher-gui--picker-buffer ()
  "Return the active interaction's picker minibuffer, if it is reading."
  (when-let* ((session launcher-buffer--session)
              (minibuffer (launcher-buffer--session-minibuffer session))
              ((buffer-live-p minibuffer)))
    minibuffer))

(defun launcher-gui--snapshot (&optional name)
  "Return the observable state of the active interaction.
With NAME, also capture the frame's drawing."
  (redisplay t)
  (when name (launcher-gui--capture name))
  (let* ((session launcher-buffer--session)
         (window (launcher-buffer--session-window session))
         (picker (launcher-gui--picker-buffer))
         (mini (active-minibuffer-window))
         (string (and picker (buffer-local-value 'vertico--candidates-ov picker)
                      (or (overlay-get (buffer-local-value 'vertico--candidates-ov picker)
                                       'before-string)
                          "")))
         ;; Candidate rows, then any blank rows padding them to the cap.
         (lines (and string (cdr (split-string string "\n"))))
         (blank (and lines (cl-count "" (butlast lines) :test #'equal)))
         (row (and string (launcher-gui--selected-row string)))
         (height (default-line-height))
         (vscroll (window-vscroll window t)))
    (list :view (car (launcher-buffer--session-view session))
          :window window
          :frame (window-frame window)
          :frames (length (frame-list))
          :windows (length (window-list (window-frame window) 'never))
          :buffer (window-buffer window)
          :start (window-start window)
          :noted (with-current-buffer (window-buffer window)
                   (and (string-match-p "Noted\\." (buffer-string)) t))
          :mode (buffer-local-value 'major-mode (window-buffer window))
          :depth (recursion-depth)
          :input (and picker (with-current-buffer picker
                               (minibuffer-contents-no-properties)))
          :rows (and string (if lines (- (length lines) 1 blank) 0))
          :padding (and string (or blank 0))
          :string string
          :selected (and picker (with-current-buffer picker
                                  (and (>= vertico--index 0) (vertico--candidate))))
          :selected-visible
          (or (null row)
              (<= (* (1+ row) height) (- (window-body-height window t) vscroll)))
          ;; The picker starts at the window's first line, unless scrolled.
          :top (and (zerop vscroll)
                    (= (window-start window)
                       (with-current-buffer (window-buffer window) (point-min))))
          :mini-buffer (and mini (window-buffer mini))
          :mini-hidden (and mini (> (window-vscroll mini t) 0))
          :mini-lines (and mini (window-total-height mini))
          :local-grid (and picker (buffer-local-value 'vertico-grid-mode picker))
          :global-grid (default-value 'vertico-grid-mode)
          :global-flat (default-value 'vertico-flat-mode)
          :body-lines (window-body-height window)
          :frame-lines (frame-height (window-frame window)))))

(defmacro launcher-gui--with-fixture (&rest body)
  "Run BODY in a fresh window layout with stubbed apps and browser.
Afterwards, check that no interaction state, hook or keymap remains."
  (declare (indent 0))
  `(progn
     (skip-unless (eq window-system 'ns))
     (let* ((frames (frame-list))
            (frame (selected-frame))
            (size (list (frame-pixel-width frame) (frame-pixel-height frame)))
            (position (frame-position frame))
            (origin (get-buffer-create "*launcher GUI origin*"))
            (mode-line (default-value 'mode-line-format))
            (setup-hook minibuffer-setup-hook)
            (exit-hook (default-value 'minibuffer-exit-hook))
            (emulation emulation-mode-map-alists)
            (launcher--apps launcher-gui--apps)
            (launcher-gui--launched nil)
            (launcher-gui--searched nil)
            (vertico-count 6))
       (delete-other-windows)
       (with-current-buffer origin
         (erase-buffer)
         (dotimes (i 200) (insert (format "Origin line %d\n" i)))
         (goto-char (point-min))
         (forward-line 40))
       (switch-to-buffer origin)
       (set-window-start nil (save-excursion (forward-line -10) (point)))
       (unwind-protect
           (cl-letf (((symbol-function 'launcher--launch)
                      (lambda (path) (push path launcher-gui--launched)))
                     ((symbol-function 'browse-url)
                      (lambda (url &rest _) (push url launcher-gui--searched))))
             ,@body)
         (delete-other-windows)
         (switch-to-buffer origin))
       (should-not launcher-buffer--session)
       (should-not launcher-buffer--emulation)
       (should (equal emulation emulation-mode-map-alists))
       (should-not (memq #'launcher-buffer--watch (default-value 'post-command-hook)))
       (should (equal setup-hook minibuffer-setup-hook))
       (should (equal exit-hook (default-value 'minibuffer-exit-hook)))
       (should (eq mode-line (default-value 'mode-line-format)))
       (should-not (default-value 'vertico-buffer-mode))
       (should-not (active-minibuffer-window))
       (should (zerop (recursion-depth)))
       (should (zerop (window-vscroll (minibuffer-window frame) t)))
       ;; Launcher starts no timers, so none may run its code later.
       (should-not (cl-some #'launcher-gui--launcher-timer-p
                            (append timer-list timer-idle-list)))
       (should (equal frames (frame-list)))
       (should (equal size (list (frame-pixel-width frame) (frame-pixel-height frame))))
       (should (equal position (frame-position frame))))))

(defun launcher-gui--window-state (window)
  "Return WINDOW's buffer, start, point and dedication."
  (list (window-buffer window) (window-start window) (window-point window)
        (window-dedicated-p window)))

(defun launcher-gui--messages-since (start)
  "Return *Messages* text after position START."
  (with-current-buffer "*Messages*"
    (buffer-substring-no-properties (min start (point-max)) (point-max))))

(defun launcher-gui--messages-end ()
  (with-current-buffer "*Messages*" (point-max)))

(defun launcher-gui--launcher-timer-p (timer)
  "Return non-nil if TIMER would call a Launcher function."
  (let ((function (timer--function timer)))
    (and (symbolp function)
         (string-prefix-p "launcher-" (symbol-name function))
         (not (string-prefix-p "launcher-gui-" (symbol-name function))))))

;;; Result fixture

(defvar launcher-gui--updates nil "Timer appending to the synthetic result.")

(defvar-keymap launcher-gui-result-mode-map
  "n" #'launcher-gui--note
  "k" #'launcher-gui--kill-result
  "e" #'launcher-gui--fail)

(define-derived-mode launcher-gui-result-mode special-mode "Result"
  "Read-only synthetic result with its own keys.")

(defun launcher-gui--note ()
  "Record a note in the result, through the result mode's own key."
  (interactive)
  (let ((inhibit-read-only t))
    (save-excursion (goto-char (point-max)) (insert "Noted.\n"))))

(defun launcher-gui--kill-result ()
  "Kill the result buffer, as a user might."
  (interactive)
  (kill-buffer (current-buffer)))

(defun launcher-gui--fail ()
  "Fail, as a buggy result command might."
  (interactive)
  (error "Synthetic result command failed"))

(defun launcher-gui--result-buffer ()
  "Return a fresh synthetic result buffer, updated by a timer."
  (let ((buffer (get-buffer-create "*launcher GUI result*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "Synthetic result\n\n")
        (dotimes (i 80) (insert (format "Result line %d\n" i))))
      (launcher-gui-result-mode)
      (goto-char (point-min)))
    (when launcher-gui--updates (cancel-timer launcher-gui--updates))
    (setq launcher-gui--updates
          (run-with-timer
           0.2 0.2
           (lambda ()
             (if (buffer-live-p buffer)
                 (with-current-buffer buffer
                   (let ((inhibit-read-only t))
                     (save-excursion (goto-char (point-max)) (insert "Update\n"))))
               (cancel-timer launcher-gui--updates)
               (setq launcher-gui--updates nil)))))
    buffer))

(defun launcher-gui--open-result ()
  "Open the synthetic result as the next launcher view."
  (interactive)
  (launcher-buffer--visit (launcher-gui--result-buffer)))

(defun launcher-gui--stop-updates ()
  (when launcher-gui--updates
    (cancel-timer launcher-gui--updates)
    (setq launcher-gui--updates nil)))

(defmacro launcher-gui--with-result-key (&rest body)
  "Run BODY with C-c r opening the synthetic result from the picker."
  (declare (indent 0))
  `(let ((launcher-buffer-picker-map
          (define-keymap :parent launcher-buffer-picker-map
            "C-c r" #'launcher-gui--open-result)))
     (unwind-protect (progn ,@body)
       (launcher-gui--stop-updates)
       (when (get-buffer "*launcher GUI result*")
         (kill-buffer "*launcher GUI result*")))))

;;; Checks

(ert-deftest launcher-gui-portal-absent ()
  (should-not (featurep 'portal))
  (should-not (featurep 'portal-launcher))
  (should-not (locate-library "portal"))
  (should-not (locate-library "portal-launcher")))

(ert-deftest launcher-gui-picker-rows-and-selection ()
  (launcher-gui--with-fixture
    (vertico-mode 1)
    (unwind-protect
        (let* ((window (selected-window))
               (before (launcher-gui--window-state window))
               (messages (launcher-gui--messages-end))
               states)
          ;; A long message grows the echo area; the picker's minibuffer
          ;; window returns to one line.
          (message "%s" (make-string 300 ?m))
          (cl-flet ((snap (name) (lambda () (push (launcher-gui--snapshot name) states))))
            (launcher-gui--drive
             #'launcher-buffer
             (list (snap "01-picker") "Cal" (snap "02-filtered")
                   "zz" (snap "03-no-match")
                   'backspace 'backspace 'backspace 'backspace 'backspace
                   ;; Bangs come first; the fifth candidate is an app.
                   (snap "04-regrown") 'down 'down 'down 'down
                   (snap "05-arrow-selection") 'return)))
          (setq states (nreverse states))
          (message "Picker states: %S"
                   (mapcar (lambda (s) (list (plist-get s :input) (plist-get s :rows)
                                             (plist-get s :selected)))
                           states))
          (should (equal '(6 2 0 6 6) (mapcar (lambda (s) (plist-get s :rows)) states)))
          (should (equal '(0 0 0 0 0) (mapcar (lambda (s) (plist-get s :padding)) states)))
          (should (equal '("" "Cal" "Calzz" "" "")
                         (mapcar (lambda (s) (plist-get s :input)) states)))
          (dolist (state states)
            (should (eq (plist-get state :view) 'picker))
            (should (eq (plist-get state :window) window))
            (should (minibufferp (plist-get state :buffer)))
            (should (plist-get state :top))
            (should (plist-get state :mini-hidden))
            (should (= 1 (plist-get state :mini-lines)))
            (should (= 1 (plist-get state :windows)))
            (should (= 1 (plist-get state :frames))))
          ;; Annotations come from the shared app index.
          (should (string-match-p "/Applications/Calculator.app"
                                  (plist-get (nth 1 states) :string)))
          (let ((second (plist-get (nth 4 states) :selected)))
            (should (equal (list (cdr (assoc second launcher-gui--apps)))
                           launcher-gui--launched)))
          (should-not launcher-gui--searched)
          (should (equal before (launcher-gui--window-state window)))
          (should-not (string-match-p "Error\\|error" (launcher-gui--messages-since messages))))
      (vertico-mode -1))))

(ert-deftest launcher-gui-presentations-agree ()
  "Selection and multiword input act alike in both entry points."
  (launcher-gui--with-fixture
    (vertico-mode 1)
    (unwind-protect
        (let (results minibuffer-states)
          (dolist (command '(launcher launcher-buffer))
            (let ((launcher-gui--launched nil)
                  (launcher-gui--searched nil)
                  (minibuffer-history nil))
              (launcher-gui--drive
               command
               (list (lambda ()
                       ;; Does an ordinary window show the minibuffer?
                       (push (list command
                                   (cl-some (lambda (w) (minibufferp (window-buffer w)))
                                            (window-list nil 'never)))
                             minibuffer-states))
                     'down 'down 'down 'down 'return))
              (launcher-gui--drive command (list "some words" 'return))
              (launcher-gui--drive command (list "!gh portal launcher" 'return))
              (push (list command launcher-gui--launched launcher-gui--searched) results)))
          (message "Presentations: %S %S" results minibuffer-states)
          (should (equal (cdr (nth 0 results)) (cdr (nth 1 results))))
          (should (= 1 (length (nth 1 (car results)))))
          (should (equal (list (concat "https://github.com/search?q="
                                       (url-hexify-string "portal launcher"))
                               (concat "https://www.google.com/search?q="
                                       (url-hexify-string "some words")))
                         (nth 2 (car results))))
          ;; The original command reads in the minibuffer, not in a window.
          (should (equal '((launcher-buffer t) (launcher nil))
                         minibuffer-states)))
      (vertico-mode -1))))

(ert-deftest launcher-gui-small-window-and-host-resize ()
  (launcher-gui--with-fixture
    (vertico-mode 1)
    (unwind-protect
        (let* ((frame (selected-frame))
               (lines (frame-height frame))
               (text-size (list (frame-text-width frame) (frame-text-height frame)))
               (position (frame-position frame))
               (small (split-window (selected-window) -4 'below))
               (messages (launcher-gui--messages-end))
               states)
          (select-window small)
          (cl-flet ((snap (name) (lambda () (push (launcher-gui--snapshot name) states))))
            (should
             (condition-case nil
                 (launcher-gui--drive
             #'launcher-buffer
             (list (snap "06-small-window") 'down 'down 'down 'down 'down
                   (snap "07-small-window-scrolled")
                   'up 'up 'up 'up 'up (snap "small-window-back-up")
                   (lambda () (delete-other-windows small))
                   (snap "regrown-window")
                   (lambda () (set-frame-height frame 6))
                   (snap "08-shrunk-frame") 'down 'down 'down
                   (snap "shrunk-frame-scrolled")
                   (lambda () (set-frame-height frame lines))
                   (snap "09-regrown-frame") 'escape))
               (quit t))))
          (setq states (nreverse states))
          (message "Resize states: %S"
                   (mapcar (lambda (s) (list (plist-get s :rows) (plist-get s :padding)
                                             (plist-get s :selected) (plist-get s :selected-visible)
                                             (plist-get s :top) (plist-get s :body-lines)
                                             (plist-get s :frame-lines)))
                           states))
          ;; A short window shows the rows that fit, at most the cap of 6,
          ;; and blank rows complete the cap's height below its bottom.
          (dolist (state states)
            (should (= (plist-get state :rows)
                       (min 6 (1- (plist-get state :body-lines)))))
            (should (= 6 (+ (plist-get state :rows) (plist-get state :padding))))
            (should (plist-get state :selected-visible))
            (should (plist-get state :top))
            (should (plist-get state :mini-hidden)))
          (should (equal '(2 2 2 6 3 3 6)
                         (mapcar (lambda (s) (plist-get s :rows)) states)))
          (should (equal "Calendar" (plist-get (nth 1 states) :selected)))
          (should (< (plist-get (nth 4 states) :frame-lines) lines))
          (should (= lines (plist-get (nth 6 states) :frame-lines)))
          (should-not (string-match-p "Error\\|error\\|args-out-of-range"
                                      (launcher-gui--messages-since messages)))
          ;; Undo this check's own resizing exactly, for the fixture's checks.
          (set-frame-size frame (car text-size) (cadr text-size) t)
          (set-frame-position frame (car position) (cdr position))
          (sit-for 0.3))
      (vertico-mode -1))))

(ert-deftest launcher-gui-result-back-and-quit ()
  (launcher-gui--with-fixture
    (vertico-mode 1)
    (launcher-gui--with-result-key
      (unwind-protect
          (let* ((window (selected-window))
                 (before (launcher-gui--window-state window))
                 (returned nil)
                 (outcome nil)
                 states)
            (cl-flet ((snap (name) (lambda () (push (append (launcher-gui--snapshot name)
                                                            (list :returned returned))
                                                    states))))
              (setq outcome
                    (condition-case nil
                        (progn
                          (launcher-gui--drive
                           (lambda () (launcher-buffer) (setq returned t))
                           (list "Ca" 'C-c "r" (snap "10-result")
                                 "n" " " (snap "result-interactive")
                                 'C-c 'C-b (snap "11-back-to-picker")
                                 "l" (snap "back-filtered")
                                 'C-c "r" (snap "result-again")
                                 'escape))
                          'returned)
                      (quit 'quit))))
            (setq states (nreverse states))
            (message "Result states: %S"
                     (mapcar (lambda (s) (list (plist-get s :view) (buffer-name (plist-get s :buffer))
                                               (plist-get s :mode) (plist-get s :input)
                                               (plist-get s :depth)))
                             states))
            (should (eq outcome 'quit))
            (should-not returned)
            (let ((result (nth 0 states)) (interactive (nth 1 states))
                  (back (nth 2 states)) (filtered (nth 3 states)))
              (should (eq (plist-get result :view) 'result))
              (should (eq (plist-get result :window) window))
              (should (equal "*launcher GUI result*" (buffer-name (plist-get result :buffer))))
              (should (eq (plist-get result :mode) 'launcher-gui-result-mode))
              (should (= 1 (plist-get result :depth)))
              (should (plist-get result :top))
              ;; The result's own keys work: n notes, Space scrolls.
              (should-not (plist-get result :noted))
              (should (plist-get interactive :noted))
              (should (> (plist-get interactive :start) 1))
              (should (eq (plist-get back :view) 'picker))
              (should (eq (plist-get back :window) window))
              (should (equal "Ca" (plist-get back :input)))
              (should (equal "Cal" (plist-get filtered :input)))
              (should (= 2 (plist-get filtered :rows))))
            (dolist (state states)
              (should (= 1 (plist-get state :frames)))
              (should (= 1 (plist-get state :windows)))
              (should-not (plist-get state :returned)))
            (should-not launcher-gui--launched)
            (should-not launcher-gui--searched)
            (should (equal before (launcher-gui--window-state window)))
            ;; Launcher never kills or changes a result buffer.
            (let ((result (get-buffer "*launcher GUI result*")))
              (should (buffer-live-p result))
              (should (eq 'launcher-gui-result-mode
                          (buffer-local-value 'major-mode result)))
              ;; Late updates change only their own buffer.
              (let ((size (buffer-size result)))
                (sit-for 0.5)
                (should (> (buffer-size result) size)))
              (should (equal before (launcher-gui--window-state window)))
              (should (eq window (selected-window)))))
        (vertico-mode -1)))))

(ert-deftest launcher-gui-cancel-error-kill-and-reenter ()
  (launcher-gui--with-fixture
    (vertico-mode 1)
    (launcher-gui--with-result-key
      (unwind-protect
          (let* ((window (selected-window))
                 (before (progn (set-window-dedicated-p window t)
                                (launcher-gui--window-state window)))
                 outcomes states)
            (cl-flet ((run (strokes)
                        (push (condition-case err
                                  (progn (launcher-gui--drive #'launcher-buffer strokes)
                                         'returned)
                                (quit 'quit)
                                (error (list 'error (error-message-string err))))
                              outcomes)
                        (push (launcher-gui--window-state window) states))
                      (snap () (lambda () (push (launcher-gui--snapshot) states))))
              ;; Escape and C-g quit the picker; C-g quits a result.
              (run '(escape))
              (run '("Cal" quit))
              (run '(C-c "r" quit))
              ;; A failing result command reports and stays; killing the
              ;; result returns to the picker.
              (run (list 'C-c "r" "e" (snap) "k" (snap) 'escape))
              ;; A host's throw unwinds the interaction.
              (push (catch 'launcher-gui-host
                      (let ((map (define-keymap "C-c h"
                                   (lambda () (interactive)
                                     (throw 'launcher-gui-host 'host)))))
                        (push `((t . ,map)) emulation-mode-map-alists)
                        (unwind-protect
                            (launcher-gui--drive #'launcher-buffer '(C-c "r" C-c "h"))
                          (pop emulation-mode-map-alists))))
                    outcomes)
              (push (launcher-gui--window-state window) states)
              ;; A failing launch ends the interaction with its error.
              (cl-letf (((symbol-function 'launcher--launch)
                         (lambda (_) (user-error "Synthetic launch failure"))))
                (run '("Chess" return)))
              ;; Reentry works after all of the above.
              (run '("Notes" return)))
            (setq outcomes (nreverse outcomes) states (nreverse states))
            (message "Cancel outcomes: %S" outcomes)
            (should (equal '(quit quit quit quit host
                                  (error "Synthetic launch failure") returned)
                           outcomes))
            (should (equal '("/Applications/Notes.app") launcher-gui--launched))
            ;; Snapshots inside the fourth run: failure kept the result view,
            ;; then killing the result returned to the picker.
            (let ((failed (nth 3 states)) (killed (nth 4 states)))
              (should (eq 'result (plist-get failed :view)))
              (should (eq 'picker (plist-get killed :view)))
              (should (eq window (plist-get killed :window))))
            (dolist (state (cl-remove-if-not (lambda (s) (bufferp (car-safe s))) states))
              (should (equal before state)))
            (should (window-dedicated-p window)))
        (set-window-dedicated-p (selected-window) nil)
        (vertico-mode -1)))))

(ert-deftest launcher-gui-deleted-window-ends ()
  (launcher-gui--with-fixture
    (vertico-mode 1)
    (launcher-gui--with-result-key
      (unwind-protect
          (let* ((other (selected-window))
                 (window (split-window other nil 'below))
                 (other-state (launcher-gui--window-state other))
                 outcome)
            (select-window window)
            (setq outcome (condition-case nil
                              (progn (launcher-gui--drive #'launcher-buffer
                                                          '(C-c "r" C-x "0"))
                                     'returned)
                            (quit 'quit)))
            (should (eq outcome 'quit))
            (should-not (window-live-p window))
            (should (equal other-state (launcher-gui--window-state other))))
        (vertico-mode -1)))))

(defvar launcher-gui--nested nil "Result of the nested prompt.")

(defun launcher-gui--nested-prompt ()
  "Read a nested completion from inside the picker."
  (interactive)
  (setq launcher-gui--nested (completing-read "Nested: " '("alpha" "beta"))))

(ert-deftest launcher-gui-reader-keys-multiform-and-nested ()
  (launcher-gui--with-fixture
    (let ((completion-styles '(orderless basic))
          (completing-read-function completing-read-function)
          (enable-recursive-minibuffers t)
          (vertico-multiform-commands '((launcher-buffer grid)
                                        (launcher-gui--nested-prompt flat)))
          (vertico-multiform-categories '((file grid)))
          (directory-keys (list (keymap-lookup vertico-map "RET")
                                (keymap-lookup vertico-map "DEL")))
          (launcher-gui--nested nil)
          (reader-prompts nil)
          (file-state nil)
          states)
      (keymap-set vertico-map "RET" #'vertico-directory-enter)
      (keymap-set vertico-map "DEL" #'vertico-directory-delete-char)
      (keymap-global-set "C-c n" #'launcher-gui--nested-prompt)
      (setq completing-read-function
            (lambda (prompt &rest args)
              (push prompt reader-prompts)
              (apply #'completing-read-default prompt args)))
      (vertico-mode 1)
      (vertico-multiform-mode 1)
      (marginalia-mode 1)
      (unwind-protect
          (progn
            (cl-flet ((snap (name) (lambda () (push (launcher-gui--snapshot name) states))))
              (let ((this-command 'launcher-buffer))
                (launcher-gui--drive
                 #'launcher-buffer
                 (list "mon" (snap "picker-multiform")
                       'C-c "n" (snap "12-nested-prompt") "be" 'return
                       (snap "13-nested-returned") "x" 'backspace " act"
                       (snap "orderless")
                       'return))))
            ;; Multiform rules still apply outside the launcher.
            (launcher-gui--drive
             (lambda ()
               (let ((this-command 'find-file))
                 (condition-case nil (read-file-name "File: ") (quit nil))))
             (list (lambda () (setq file-state (list (default-value 'vertico-grid-mode)
                                                     (minibufferp (window-buffer (selected-window))))))
                   'quit)))
        (vertico-multiform-mode -1)
        (vertico-mode -1)
        (marginalia-mode -1)
        (keymap-set vertico-map "RET" (nth 0 directory-keys))
        (keymap-set vertico-map "DEL" (nth 1 directory-keys))
        (keymap-global-unset "C-c n"))
      (setq states (nreverse states))
      (message "Reader states: %S nested %S file %S prompts %S"
               (mapcar (lambda (s) (list (plist-get s :input) (plist-get s :rows)
                                         (plist-get s :local-grid) (plist-get s :global-grid)
                                         (buffer-name (plist-get s :mini-buffer))))
                       states)
               launcher-gui--nested file-state reader-prompts)
      (let ((picker (nth 0 states)) (nested (nth 1 states))
            (returned (nth 2 states)) (orderless (nth 3 states)))
        ;; The launcher's multiform rule asked for a grid; the picker keeps
        ;; its buffer list locally while the rule's global mode is on.
        (should (plist-get picker :global-grid))
        (should-not (plist-get picker :local-grid))
        (should (= 1 (plist-get picker :rows)))
        (should (plist-get picker :top))
        ;; A nested prompt reads in the minibuffer window, beside the picker.
        (should (eq 'picker (plist-get nested :view)))
        (should (minibufferp (plist-get nested :buffer)))
        (should-not (eq (plist-get nested :buffer) (plist-get nested :mini-buffer)))
        (should-not (plist-get nested :mini-hidden))
        ;; Multiform applies its own rule to the nested prompt.
        (should (plist-get nested :global-flat))
        (should-not (plist-get nested :global-grid))
        ;; Returning restores the picker view.
        (should (eq (plist-get returned :buffer) (plist-get returned :mini-buffer)))
        (should (plist-get returned :mini-hidden))
        (should (plist-get returned :top))
        (should (equal "mon" (plist-get returned :input)))
        (should (= 1 (plist-get orderless :rows))))
      (should (equal "beta" launcher-gui--nested))
      (should (equal '("/Applications/Activity Monitor.app") launcher-gui--launched))
      (should (equal '(t t) file-state))
      (should (member "Launch: " reader-prompts)))))

(defun launcher-gui-run-and-exit ()
  "Run the graphical checks matching $LAUNCHER_TEST_SELECTOR, then exit.
Exit 0 only if all ran as expected, with none skipped or quit."
  (let ((status 1))
    (message "Environment: %s; repository %s; %s; Vertico %s; frame %sx%s chars, %sx%s px, font %s"
             (emacs-version) emacs-repository-version
             (string-trim (shell-command-to-string "sw_vers -productVersion"))
             (with-temp-buffer
               (insert-file-contents (locate-library "vertico.el"))
               (and (re-search-forward "^;; Version: \\(.*\\)" nil t) (match-string 1)))
             (frame-width) (frame-height) (frame-pixel-width) (frame-pixel-height)
             (face-attribute 'default :family))
    (unwind-protect
        (condition-case err
            (let ((stats (ert-run-tests-batch (getenv "LAUNCHER_TEST_SELECTOR"))))
              (when (and (> (ert-stats-total stats) 0)
                         (= (ert-stats-completed-expected stats)
                            (ert-stats-total stats)))
                (setq status 0)))
          (error (message "Launcher GUI checks failed: %S\n%s" err
                          (with-output-to-string (backtrace)))))
      (with-current-buffer "*Messages*"
        (princ (buffer-string) #'external-debugging-output))
      (kill-emacs status))))

(provide 'launcher-buffer-gui-tests)
;;; launcher-buffer-gui-tests.el ends here
