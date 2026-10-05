;;; launcher-tests.el --- Launcher input regressions -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'launcher)
(require 'url-util)

(defun launcher-test--run (choice &optional space-binding refresh)
  "Run `launcher' with a fake reader returning CHOICE and record effects.
SPACE-BINDING defaults to stock completion.  Pass REFRESH to `launcher'.
A symbol CHOICE signals that condition from the reader."
  (let* ((launcher--apps '(("Calculator" . "/Applications/Calculator.app")
                           ("Activity Monitor" . "/Applications/Activity Monitor.app")))
         (launcher--current-entries nil)
         (map (copy-keymap minibuffer-local-completion-map))
         (original-space (or space-binding #'minibuffer-complete-word))
         (minibuffer-setup-hook nil)
         launched searched observed-space refreshed outcome
         (completing-read-function
          (lambda (&rest _)
            ;; Emulate minibuffer setup while retaining the caller's reader.
            (with-temp-buffer
              (use-local-map map)
              (run-hooks 'minibuffer-setup-hook)
              (setq observed-space (key-binding " "))
              (if (symbolp choice) (signal choice nil) choice)))))
    (define-key map " " original-space)
    (cl-letf (((symbol-function 'launcher--launch)
               (lambda (path) (push path launched)))
              ((symbol-function 'browse-url)
               (lambda (url &rest _) (push url searched)))
              ((symbol-function 'launcher-refresh)
               (lambda () (setq refreshed t))))
      (setq outcome (condition-case nil
                        (progn (launcher refresh) 'accepted)
                      (quit 'quit)
                      (error 'error))))
    (should-not launcher--current-entries)
    (should-not minibuffer-setup-hook)
    (should (eq (lookup-key map " ") original-space))
    (list :outcome outcome :launched launched :searched searched
          :space observed-space :refreshed refreshed)))

(ert-deftest launcher-empty-input-cancels ()
  (dolist (choice '("" " " " \t\n"))
    (let ((result (launcher-test--run choice)))
      (should (eq (plist-get result :outcome) 'quit))
      (should-not (plist-get result :launched))
      (should-not (plist-get result :searched)))))

(ert-deftest launcher-bang-without-query-cancels ()
  (let ((launcher-bangs '(("!gh" "GitHub" "https://github.com/search?q=%s")
                           ("!custom" "Custom" "https://example.com/?q=%s"))))
    (dolist (choice '("!gh" "!gh " "!gh \t " "!custom" "!custom  "))
      (let ((result (launcher-test--run choice)))
        (should (eq (plist-get result :outcome) 'quit))
        (should-not (plist-get result :launched))
        (should-not (plist-get result :searched))))))

(ert-deftest launcher-accepts-completion-result ()
  ;; A completion UI may return a highlighted app without any typed input.
  (dolist (name '("Calculator" "Activity Monitor"))
    (let ((result (launcher-test--run name)))
      (should (eq (plist-get result :outcome) 'accepted))
      (should (equal (plist-get result :launched)
                     (list (concat "/Applications/" name ".app"))))
      (should-not (plist-get result :searched)))))

(ert-deftest launcher-bang-search-preserves-query ()
  (let* ((launcher-bangs '(("!gh" "GitHub" "https://github.com/search?q=%s")))
         (query "portal launcher & 中文")
         (result (launcher-test--run (concat "!gh " query))))
    (should (eq (plist-get result :outcome) 'accepted))
    (should-not (plist-get result :launched))
    (should (equal (plist-get result :searched)
                   (list (concat "https://github.com/search?q="
                                 (url-hexify-string query)))))))

(ert-deftest launcher-unmatched-input-searches ()
  (let ((launcher-fallback-search-url "https://example.com/?q=%s"))
    (dolist (query '("some words" "!unknown"))
      (let ((result (launcher-test--run query)))
        (should (eq (plist-get result :outcome) 'accepted))
        (should-not (plist-get result :launched))
        (should (equal (plist-get result :searched)
                       (list (concat "https://example.com/?q="
                                     (url-hexify-string query)))))))))

(ert-deftest launcher-stock-space-inserts-locally ()
  (let ((result (launcher-test--run "!gh portal launcher")))
    (should (eq (plist-get result :space) #'self-insert-command))))

(ert-deftest launcher-custom-space-binding-is-preserved ()
  (let ((result (launcher-test--run "Calculator" #'ignore)))
    (should (eq (plist-get result :space) #'ignore))))

(ert-deftest launcher-reader-quit-and-error-clean-up ()
  (dolist (condition '(quit error))
    (let ((result (launcher-test--run condition)))
      (should (eq (plist-get result :outcome) condition))
      (should-not (plist-get result :launched))
      (should-not (plist-get result :searched)))))

(ert-deftest launcher-prefix-refresh-is-preserved ()
  (should (plist-get (launcher-test--run "Calculator" nil '(4)) :refreshed))
  (should-not (plist-get (launcher-test--run "Calculator") :refreshed)))

;;; launcher-tests.el ends here
