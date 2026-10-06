;;; launcher-buffer-tests.el --- launcher-buffer interaction checks -*- lexical-binding: t; -*-

;; Batch checks of the interaction's views, keys and window ownership.
;; A fake reader stands in for Vertico, whose display only a graphical
;; Emacs can show: see launcher-buffer-gui-tests.el for that.  Result
;; views run a real recursive edit, reading queued key events.  Batch
;; Emacs exits on a command's error there, so errors in result commands
;; are checked graphically only.

(require 'ert)
(require 'cl-lib)
(require 'launcher-buffer)
(require 'url-util)

(defvar vertico-count)

(defvar launcher-buffer-test--launched nil "Apps the stubbed launcher opened.")
(defvar launcher-buffer-test--searched nil "URLs the stubbed browser opened.")

(defun launcher-buffer-test--reader (answers)
  "Return a fake reader giving ANSWERS to successive prompts.
An answer is a string to return, a symbol to signal, or a function to
call inside the prompt, as a command typed there would run."
  (lambda (&rest _)
    (with-temp-buffer
      ;; Emulate minibuffer setup while retaining the caller's reader.
      (run-hooks 'minibuffer-setup-hook)
      (let ((answer (pop answers)))
        (cond ((stringp answer) answer)
              ((functionp answer) (funcall answer))
              (t (signal answer nil)))))))

(defmacro launcher-buffer-test--with-fixture (&rest body)
  "Run BODY with fake apps and browser, in a window on a fresh buffer.
Afterwards, check that the interaction left no state behind."
  (declare (indent 0))
  `(let* ((launcher--apps '(("Calculator" . "/Applications/Calculator.app")
                            ("Activity Monitor" . "/Applications/Activity Monitor.app")))
          (launcher--current-entries nil)
          (launcher-buffer-test--launched nil)
          (launcher-buffer-test--searched nil)
          (vertico-count 6)
          (minibuffer-setup-hook nil)
          (emulation emulation-mode-map-alists)
          (origin (generate-new-buffer "*launcher test origin*")))
     (delete-other-windows)
     (switch-to-buffer origin)
     (with-current-buffer origin
       (dotimes (i 100) (insert (format "Line %d\n" i)))
       (goto-char (point-min))
       (forward-line 30))
     (set-window-start nil (save-excursion (forward-line -5) (point)))
     (unwind-protect
         (cl-letf (((symbol-function 'launcher--launch)
                    (lambda (path) (push path launcher-buffer-test--launched)))
                   ((symbol-function 'browse-url)
                    (lambda (url &rest _) (push url launcher-buffer-test--searched)))
                   ;; Vertico's buffer display needs a graphical minibuffer.
                   ((symbol-function 'launcher-buffer--check) #'ignore)
                   ((symbol-function 'launcher-buffer--before-reader) #'ignore)
                   ((symbol-function 'launcher-buffer--after-reader) #'ignore))
           ,@body)
       (setq unread-command-events nil)
       (delete-other-windows)
       (set-window-dedicated-p nil nil)
       (kill-buffer origin))
     (should-not launcher-buffer--session)
     (should-not launcher-buffer--emulation)
     (should (equal emulation emulation-mode-map-alists))
     (should-not (memq #'launcher-buffer--watch (default-value 'post-command-hook)))
     (should-not minibuffer-setup-hook)
     (should-not launcher--current-entries)
     (should (zerop (recursion-depth)))))

(defun launcher-buffer-test--run (answers &optional keys refresh)
  "Run `launcher-buffer' with fake reader ANSWERS, then KEYS in views.
Pass REFRESH as its argument.  Return its outcome: `accepted', `quit',
or the error's message."
  (let ((completing-read-function (launcher-buffer-test--reader answers)))
    (setq unread-command-events (and keys (listify-key-sequence (kbd keys))))
    (condition-case err
        (progn (launcher-buffer refresh) 'accepted)
      (quit 'quit)
      (error (error-message-string err)))))

(defun launcher-buffer-test--window-state (&optional window)
  "Return WINDOW's buffer, start, point, dedication and buffer history."
  (list (window-buffer window) (window-start window) (window-point window)
        (window-dedicated-p window) (mapcar #'car (window-prev-buffers window))))

(defun launcher-buffer-test--result (&optional name)
  "Return a fresh read-only result buffer called NAME."
  (with-current-buffer (get-buffer-create (or name "*launcher test result*"))
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert "Result\n"))
    (special-mode)
    (current-buffer)))

(defun launcher-buffer-test--visit (&optional buffer)
  "Return a reader answer that visits BUFFER, or a fresh result."
  (lambda () (launcher-buffer--visit (or buffer (launcher-buffer-test--result)))))

(defmacro launcher-buffer-test--with-result-cleanup (&rest body)
  (declare (indent 0))
  `(unwind-protect (progn ,@body)
     (when (get-buffer "*launcher test result*")
       (kill-buffer "*launcher test result*"))))

(ert-deftest launcher-buffer-loads-without-vertico ()
  (should (featurep 'launcher-buffer))
  (should-not (featurep 'vertico))
  (should (commandp #'launcher-buffer))
  (should (commandp #'launcher-back))
  (should (commandp #'launcher-quit)))

(defun launcher-buffer-test--emacs (form)
  "Return what FORM prints in a fresh batch Emacs with launcher on its path."
  (with-temp-buffer
    (should (zerop (call-process
                    (expand-file-name invocation-name invocation-directory)
                    nil t nil "--batch" "-Q" "-L"
                    (file-name-directory (locate-library "launcher"))
                    "--eval" form)))
    (buffer-string)))

(ert-deftest launcher-buffer-available-from-launcher ()
  ;; Loading launcher.el defines the command, not its interface.
  (should (equal "(t nil nil)"
                 (launcher-buffer-test--emacs
                  "(progn (require 'launcher)
                          (prin1 (list (commandp 'launcher-buffer)
                                       (featurep 'launcher-buffer)
                                       (featurep 'vertico))))")))
  ;; An autoload to launcher, as use-package :commands makes, reaches
  ;; the interface, here missing Vertico.
  (should (string-match-p
           "\\`(t t nil \".*needs the Vertico package"
           (launcher-buffer-test--emacs
            "(progn (autoload 'launcher-buffer \"launcher\" nil t)
                    (prin1 (condition-case err
                               (call-interactively 'launcher-buffer)
                             (user-error (list (featurep 'launcher)
                                               (featurep 'launcher-buffer)
                                               (featurep 'vertico)
                                               (cadr err))))))"))))

(ert-deftest launcher-buffer-vertico-requirements-are-local ()
  ;; Without Vertico, only this entry point fails, clearly.
  (let ((launcher-buffer--session nil)
        (load-path (seq-remove (lambda (dir) (string-match-p "vertico" dir)) load-path)))
    (should (string-match-p
             "needs the Vertico package"
             (cadr (should-error (launcher-buffer--check) :type 'user-error))))
    ;; A Vertico without the internals the picker relies on.
    (let ((stub (make-temp-file "launcher-vertico" t)))
      (unwind-protect
          (with-temp-buffer
            (with-temp-file (expand-file-name "vertico.el" stub)
              (insert "(defvar vertico-mode t) (provide 'vertico)\n"))
            (with-temp-file (expand-file-name "vertico-buffer.el" stub)
              (insert "(provide 'vertico-buffer)\n"))
            (should (zerop (call-process
                            (expand-file-name invocation-name invocation-directory)
                            nil t nil "--batch" "-Q" "-L"
                            (file-name-directory (locate-library "launcher-buffer"))
                            "-L" stub "--eval"
                            "(progn (require 'launcher-buffer)
                                    (condition-case err (launcher-buffer--check)
                                      (user-error (princ (cadr err)))))")))
            (should (string-match-p "needs Vertico 2" (buffer-string))))
        (delete-directory stub t))))
  ;; The minibuffer launcher is unaffected.
  (let ((launcher--apps '(("Calculator" . "/Applications/Calculator.app")))
        (completing-read-function (launcher-buffer-test--reader '("Calculator")))
        launched)
    (cl-letf (((symbol-function 'launcher--launch) (lambda (path) (push path launched))))
      (launcher))
    (should (equal '("/Applications/Calculator.app") launched))))

(ert-deftest launcher-buffer-shares-app-and-web-behavior ()
  ;; Each answer acts as `launcher' acts on it.
  (dolist (case `(("Calculator" accepted ("/Applications/Calculator.app") nil)
                  ("some words" accepted nil
                   (,(concat "https://www.google.com/search?q="
                             (url-hexify-string "some words"))))
                  ("!gh portal 中文" accepted nil
                   (,(concat "https://github.com/search?q="
                             (url-hexify-string "portal 中文"))))
                  ("" quit nil nil)
                  ("  " quit nil nil)
                  ("!gh " quit nil nil)
                  (quit quit nil nil)))
    (launcher-buffer-test--with-fixture
      (let ((before (launcher-buffer-test--window-state)))
        (should (eq (nth 1 case) (launcher-buffer-test--run (list (car case)))))
        (should (equal (nth 2 case) launcher-buffer-test--launched))
        (should (equal (nth 3 case) launcher-buffer-test--searched))
        (should (equal before (launcher-buffer-test--window-state)))))))

(ert-deftest launcher-buffer-prefix-refreshes ()
  (launcher-buffer-test--with-fixture
    (let (refreshed)
      (cl-letf (((symbol-function 'launcher-refresh) (lambda () (setq refreshed t))))
        (launcher-buffer-test--run '("Calculator"))
        (should-not refreshed)
        (launcher-buffer-test--run '("Calculator") nil '(4))
        (should refreshed)))))

(ert-deftest launcher-buffer-reads-in-its-window ()
  (launcher-buffer-test--with-fixture
    (let ((window (selected-window)) seen)
      (launcher-buffer-test--run
       (list (lambda ()
               (setq seen (list (eq (selected-window) window)
                                (launcher-buffer--session-view launcher-buffer--session)
                                (launcher-buffer--session-cap launcher-buffer--session)
                                (memq 'launcher-buffer--emulation emulation-mode-map-alists)))
               "Calculator")))
      (should (equal '(t (picker nil) 6) (butlast seen)))
      (should (car (last seen))))))

(ert-deftest launcher-buffer-result-back-and-quit ()
  (launcher-buffer-test--with-fixture
    (launcher-buffer-test--with-result-cleanup
      (let* ((window (selected-window))
             (before (launcher-buffer-test--window-state))
             (result (launcher-buffer-test--result))
             views)
        ;; Visit a result, go back, select an app.
        (should (eq 'accepted
                    (launcher-buffer-test--run
                     (list (launcher-buffer-test--visit result)
                           (lambda ()
                             (push (launcher-buffer--session-view launcher-buffer--session)
                                   views)
                             "Calculator"))
                     "C-c C-b")))
        (should (equal '((picker nil)) views))
        (should (equal '("/Applications/Calculator.app") launcher-buffer-test--launched))
        (should (equal before (launcher-buffer-test--window-state)))
        ;; The result keeps its buffer, mode and contents.
        (should (buffer-live-p result))
        (should (eq 'special-mode (buffer-local-value 'major-mode result)))
        (should (equal "Result\n" (with-current-buffer result (buffer-string))))
        ;; C-g and Escape quit a result view; so does a command in it.
        (dolist (keys '("C-g" "<escape>"))
          (should (eq 'quit (launcher-buffer-test--run
                             (list (launcher-buffer-test--visit result)) keys)))
          (should (equal before (launcher-buffer-test--window-state))))
        (should (eq window (selected-window)))))))

(ert-deftest launcher-buffer-result-is-interactive ()
  (launcher-buffer-test--with-fixture
    (launcher-buffer-test--with-result-cleanup
      (let* ((result (launcher-buffer-test--result))
             (map (make-sparse-keymap))
             state)
        (keymap-set map "x" (lambda () (interactive)
                              (let ((inhibit-read-only t)) (insert "x"))))
        (keymap-set map "s" (lambda () (interactive)
                              (setq state (list (current-buffer) (selected-window)
                                                (recursion-depth) (point)))))
        (with-current-buffer result (use-local-map map))
        (should (eq 'quit (launcher-buffer-test--run
                           (list (launcher-buffer-test--visit result))
                           "x x s C-g")))
        ;; The result's own keys edited it, in the launcher's window.
        (should (equal "Result\nxx" (with-current-buffer result (buffer-string))))
        (should (eq result (nth 0 state)))
        (should (= 1 (nth 2 state)))))))

(ert-deftest launcher-buffer-back-from-picker-stays ()
  (launcher-buffer-test--with-fixture
    (let (messages)
      (cl-letf (((symbol-function 'message)
                 (lambda (format &rest args) (push (apply #'format format args) messages))))
        (should (eq 'accepted
                    (launcher-buffer-test--run
                     (list (lambda () (launcher-back) "Calculator"))))))
      (should (member "Already at the launcher picker" messages))
      (should (equal '("/Applications/Calculator.app") launcher-buffer-test--launched)))))

(ert-deftest launcher-buffer-keys-only-in-its-window ()
  (launcher-buffer-test--with-fixture
    (launcher-buffer-test--with-result-cleanup
      (let* ((window (selected-window))
             (other (split-window window nil 'below))
             (result (launcher-buffer-test--result))
             keys)
        (keymap-set (current-global-map) "C-c t"
                    (lambda () (interactive)
                      (push (list (selected-window) (key-binding (kbd "C-c C-b"))
                                  (key-binding (kbd "C-g")))
                            keys)))
        (unwind-protect
            (should (eq 'quit (launcher-buffer-test--run
                               (list (launcher-buffer-test--visit result))
                               "C-c t C-x o C-c t C-x o C-g")))
          (keymap-global-unset "C-c t"))
        (setq keys (nreverse keys))
        (should (equal (list window #'launcher-back #'launcher-quit) (nth 0 keys)))
        (should (equal (list other nil #'keyboard-quit) (nth 1 keys)))))))

(ert-deftest launcher-buffer-respects-user-window-changes ()
  (launcher-buffer-test--with-fixture
    (launcher-buffer-test--with-result-cleanup
      (let ((window (selected-window))
            (mine (generate-new-buffer "*launcher test user*")))
        (unwind-protect
            (progn
              ;; The user shows another buffer in the launcher's window.
              (keymap-set (current-global-map) "C-c u"
                          (lambda () (interactive) (switch-to-buffer mine)))
              (should (eq 'quit (launcher-buffer-test--run
                                 (list (launcher-buffer-test--visit))
                                 "C-c u C-g")))
              (should (eq mine (window-buffer window))))
          (keymap-global-unset "C-c u")
          (kill-buffer mine))))))

(ert-deftest launcher-buffer-restores-dedication-and-history ()
  (launcher-buffer-test--with-fixture
    (launcher-buffer-test--with-result-cleanup
      (set-window-dedicated-p nil t)
      (let ((before (launcher-buffer-test--window-state)))
        (should (eq 'accepted (launcher-buffer-test--run
                               (list (launcher-buffer-test--visit) "Calculator")
                               "C-c C-b")))
        (should (equal before (launcher-buffer-test--window-state)))
        (should (eq t (window-dedicated-p)))
        ;; Results do not stay in the window's buffer history.
        (should-not (memq (get-buffer "*launcher test result*")
                          (mapcar #'car (window-prev-buffers))))))))

(ert-deftest launcher-buffer-killed-result-returns-to-picker ()
  (launcher-buffer-test--with-fixture
    (launcher-buffer-test--with-result-cleanup
      (let ((before (launcher-buffer-test--window-state))
            views)
        (keymap-set (current-global-map) "C-c k"
                    (lambda () (interactive) (kill-buffer (current-buffer))))
        (unwind-protect
            (should (eq 'accepted
                        (launcher-buffer-test--run
                         (list (launcher-buffer-test--visit)
                               (lambda ()
                                 (push (launcher-buffer--session-view launcher-buffer--session)
                                       views)
                                 "Calculator"))
                         "C-c k")))
          (keymap-global-unset "C-c k"))
        (should (equal '((picker nil)) views))
        (should (equal before (launcher-buffer-test--window-state)))))))

(ert-deftest launcher-buffer-deleted-window-ends ()
  (launcher-buffer-test--with-fixture
    (launcher-buffer-test--with-result-cleanup
      (let* ((other (selected-window))
             (window (split-window other nil 'below))
             (other-state (launcher-buffer-test--window-state other)))
        (select-window window)
        (should (eq 'quit (launcher-buffer-test--run
                           (list (launcher-buffer-test--visit)) "C-x 0")))
        (should-not (window-live-p window))
        (should (equal other-state (launcher-buffer-test--window-state other)))))))

(ert-deftest launcher-buffer-unwinds-errors-and-host-throws ()
  (launcher-buffer-test--with-fixture
    (launcher-buffer-test--with-result-cleanup
      (let ((before (launcher-buffer-test--window-state)))
        ;; A failing launch ends the interaction with its error.
        (cl-letf (((symbol-function 'launcher--launch)
                   (lambda (_) (user-error "Launch failed"))))
          (should (equal "Launch failed" (launcher-buffer-test--run '("Calculator")))))
        (should (equal before (launcher-buffer-test--window-state)))
        ;; A host's throw unwinds the interaction from a result view.
        (keymap-set (current-global-map) "C-c h"
                    (lambda () (interactive) (throw 'launcher-buffer-test-host 'host)))
        (unwind-protect
            (should (eq 'host
                        (catch 'launcher-buffer-test-host
                          (launcher-buffer-test--run (list (launcher-buffer-test--visit))
                                                     "C-c h"))))
          (keymap-global-unset "C-c h"))
        (should (equal before (launcher-buffer-test--window-state)))
        ;; Reentry works afterwards.
        (should (eq 'accepted (launcher-buffer-test--run '("Calculator"))))))))

(ert-deftest launcher-buffer-refuses-nested-interactions ()
  (launcher-buffer-test--with-fixture
    (launcher-buffer-test--with-result-cleanup
      (let (refusal)
        (keymap-set (current-global-map) "C-c l"
                    (lambda () (interactive)
                      (cl-letf (((symbol-function 'launcher-buffer--check)
                                 (symbol-function 'launcher-buffer--check-unstubbed)))
                        (setq refusal (condition-case err
                                          (progn (launcher-buffer) nil)
                                        (user-error (cadr err)))))))
        (unwind-protect
            (should (eq 'quit (launcher-buffer-test--run
                               (list (launcher-buffer-test--visit)) "C-c l C-g")))
          (keymap-global-unset "C-c l"))
        (should (equal "A launcher interaction is already active" refusal))))))

(defalias 'launcher-buffer--check-unstubbed (symbol-function 'launcher-buffer--check)
  "The real check, for tests that stub it in the fixture.")

;;; launcher-buffer-tests.el ends here
