;;; launcher-tests.el --- Dynamic launcher input regressions -*- lexical-binding: t; -*-
(require 'ert)
(require 'cl-lib)
(require 'launcher-test-support)

(defconst launcher-test--apps
  '(("Calculator" . "/Applications/Calculator.app")
    ("Activity Monitor" . "/Applications/Activity Monitor.app")))

(defun launcher-test--run (choice &optional space-binding refresh)
  "Run launcher with CHOICE, recording actions and the Space binding."
  (let* ((launcher--apps launcher-test--apps) (launcher-show-icons nil)
         (launcher-tools nil) (minibuffer-setup-hook nil)
         (base-reader (launcher-test--reader (list choice)))
         launched searched observed-space refreshed outcome
         (completing-read-function
          (lambda (&rest args)
            (minibuffer-with-setup-hook
                (lambda ()
                  (when space-binding
                    (let ((map (make-sparse-keymap)))
                      (set-keymap-parent map (current-local-map))
                      (define-key map " " space-binding)
                      (use-local-map map)))
                  (setq observed-space (key-binding " ")))
              (apply base-reader args)))))
    (cl-letf (((symbol-function 'launcher--launch) (lambda (path) (push path launched)))
              ((symbol-function 'browse-url) (lambda (url &rest _) (push url searched)))
              ((symbol-function 'launcher-refresh) (lambda () (setq refreshed t))))
      (setq outcome (condition-case nil (progn (launcher refresh) 'accepted)
                      (quit 'quit) (error 'error))))
    (list :outcome outcome :launched launched :searched searched
          :space observed-space :refreshed refreshed)))

(ert-deftest launcher-empty-input-cancels ()
  (dolist (input '("" " " " \t\n"))
    (let ((result (launcher-test--run input)))
      (should (eq (plist-get result :outcome) 'quit))
      (should-not (plist-get result :launched))
      (should-not (plist-get result :searched)))))

(ert-deftest launcher-accepts-selected-application ()
  (let ((result (launcher-test--run "calc")))
    (should (eq (plist-get result :outcome) 'accepted))
    (should (equal (plist-get result :launched) '("/Applications/Calculator.app")))
    (should-not (plist-get result :searched))))

(ert-deftest launcher-bang-search-preserves-query ()
  (let* ((query "portal launcher & 中文")
         (result (launcher-test--run (concat "!gh " query))))
    (should (equal (plist-get result :searched)
                   (list (concat "https://github.com/search?q=" (url-hexify-string query)))))))

(ert-deftest launcher-bang-entry-inserts-prefix-and-empty-query-stays-in-tool ()
  (let* ((session (launcher--query-create nil nil)) (launcher--query-session session))
    (unwind-protect
        (progn
          (launcher--query-update session "!gh")
          (should (equal (launcher--result-execute (car (launcher--query-session-results session)))
                         '(:input "!gh ")))
          (dolist (input '("!gh " "!gh \t "))
            (launcher--query-update session input)
            (should (launcher--query-session-route session))
            (let ((results (launcher--query-session-results session)))
              (should (= 1 (length results)))
              (should (eq 'prefix-entry (plist-get (car results) :id)))
              (should (equal (launcher--result-execute (car results))
                             (list :input input))))))
      (launcher--query-close session))))

(ert-deftest launcher-web-fallback-only-without-results ()
  (let* ((session (launcher--query-create launcher-test--apps nil))
         (launcher--query-session session))
    (unwind-protect
        (progn
          (dolist (input '("calc" "1+2" "!gh" "!gh emacs"))
            (launcher--query-update session input)
            (should (launcher--query-session-results session))
            (should-not (seq-find (lambda (r) (eq (plist-get r :id) 'fallback))
                                  (launcher--query-session-results session))))
          (launcher--query-update session "no matching apps")
          (should (= 1 (length (launcher--query-session-results session))))
          (should (eq 'fallback (plist-get (car (launcher--query-session-results session)) :id)))
          (should-not (launcher--query-session-route session)))
      (launcher--query-close session))))

(ert-deftest launcher-app-results-capture-distinct-paths ()
  (let* ((session (launcher--query-create
                   '(("Calculator" . "/Applications/Calculator.app")
                     ("Calendar" . "/Applications/Calendar.app")) nil))
         (launcher--query-session session) launched)
    (launcher--query-update session "cal")
    (cl-letf (((symbol-function 'launcher--launch) (lambda (path) (push path launched))))
      (dolist (result (launcher--query-session-results session))
        (launcher--result-execute result)))
    (should (equal '("/Applications/Calendar.app" "/Applications/Calculator.app") launched))
    (launcher--query-close session)))

(ert-deftest launcher-unmatched-input-searches ()
  (let ((launcher-fallback-search-url "https://example.com/?q=%s"))
    (dolist (input '("some words" "!unknown" "example.com" "example.com/path"
                    "ftp://example.com" "see https://example.com"))
      (should (equal (plist-get (launcher--fallback-result input) :title)
                     (concat "Search web: " input)))
      (should (equal (plist-get (launcher-test--run input) :searched)
                     (list (concat "https://example.com/?q=" (url-hexify-string input))))))))

(ert-deftest launcher-url-fallback-opens-input-unchanged ()
  (let ((case-fold-search nil)
        (launcher-fallback-search-url "https://search.example/?q=%s"))
    (dolist (input '("http://example.com" "https://example.com"
                    "HTTP://example.com" "HtTpS://example.com"
                    "https://example.com/path?q=hello%20world&lang=中文#section"
                    "https://"))
      (let ((result (launcher--fallback-result input)))
        (should (eq (plist-get result :id) 'fallback))
        (should (equal (plist-get result :title) (concat "Open URL: " input))))
      (should (equal (plist-get (launcher-test--run input) :searched) (list input))))))

(ert-deftest launcher-bang-url-input-still-searches ()
  (let* ((query "https://example.com/path?q=emacs#section")
         (result (launcher-test--run (concat "!gh " query))))
    (should (equal (plist-get result :searched)
                   (list (concat "https://github.com/search?q=" (url-hexify-string query)))))))

(ert-deftest launcher-custom-space-binding-is-preserved ()
  (should (eq #'ignore (plist-get (launcher-test--run "calc" #'ignore) :space))))

(ert-deftest launcher-refresh-only-on-request ()
  (should-not (plist-get (launcher-test--run "calc") :refreshed))
  (should (plist-get (launcher-test--run "calc" nil '(4)) :refreshed)))

(ert-deftest launcher-empty-index-refreshes-once-per-invocation ()
  (dolist (refresh '(nil t))
    (let ((launcher--apps nil) (calls 0))
      (cl-letf (((symbol-function 'launcher-refresh) (lambda () (cl-incf calls))))
        (should-not (launcher--entries refresh))
        (should (= calls 1))))))

(ert-deftest launcher-app-entries-sort-and-disambiguate-names ()
  (let ((paths '("/Applications/Notes.app" "/Applications/Calculator.app/"
                 "/System/Applications/Notes.app")))
    (should (equal (launcher--build-entries paths)
                   '(("Calculator" . "/Applications/Calculator.app/")
                     ("Notes  (/Applications/Notes.app)" . "/Applications/Notes.app")
                     ("Notes  (/System/Applications/Notes.app)" . "/System/Applications/Notes.app"))))
    (should (equal paths '("/Applications/Notes.app" "/Applications/Calculator.app/"
                          "/System/Applications/Notes.app")))))

(ert-deftest launcher-reader-quit-and-error-clean-up ()
  (dolist (condition '(quit error))
    (let ((minibuffer-setup-hook nil))
      (should (eq (plist-get (launcher-test--run condition) :outcome) condition))
      (should-not minibuffer-setup-hook))))

(ert-deftest launcher-no-raw-text-dispatch ()
  (let ((launcher--apps launcher-test--apps) opened
        (completing-read-function (lambda (&rest _) "Calculator")))
    (cl-letf (((symbol-function 'launcher--launch) (lambda (_) (setq opened t)))
              ((symbol-function 'browse-url) (lambda (&rest _) (setq opened t))))
      (should (eq 'quit (condition-case nil (launcher) (quit 'quit))))
      (should-not opened))))

(provide 'launcher-tests)
