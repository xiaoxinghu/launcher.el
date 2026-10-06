;;; launcher-osx-dictionary-gui-tests.el --- Real Apple Dictionary checks -*- lexical-binding: t; -*-

;; GUI ONLY, OPT-IN: run in the test VM with
;;
;;   bash test/vm.sh sh test/gui.sh '^launcher-dictionary-real-'
;;
;; These checks look words up in the VM's real Apple dictionaries with
;; the pinned osx-dictionary package, which `sh test/elpa.sh' fetches.
;; They copy the package to a temporary directory and load it from there,
;; so the first lookup builds its native helper there with the VM's
;; compiler.  Keys go through AppKit's event queue, as in the other
;; graphical checks; launching apps and browsing stay stubbed.  The log
;; records the environment and the definitions found.

(require 'launcher-tools-gui-tests)
(require 'launcher-osx-dictionary)

(defconst launcher-dictionary-real--revision "655bca5cea78440a1ac41f9cd78711b9c8aff8f3"
  "The pinned osx-dictionary commit.")

(defvar osx-dictionary-last-dictionary-file)
(defvar osx-dictionary-current-dictionary)
(defvar osx-dictionary-previous-window-configuration)
(defvar osx-dictionary--current-word)

(defvar launcher-dictionary-real--dir nil
  "Temporary copy of the package the checks load, once loaded.")

(defun launcher-dictionary-real--load ()
  "Load a temporary copy of the pinned package, once; return its directory."
  (or launcher-dictionary-real--dir
      (let ((source (expand-file-name
                     (concat ".cache/elpa/osx-dictionary.el-" launcher-dictionary-real--revision)
                     (getenv "LAUNCHER_TEST_ROOT")))
            (copy (make-temp-file "launcher-dictionary-real" t)))
        (copy-directory source (file-name-as-directory copy) nil nil t)
        ;; No choice saved by a user: search the active dictionaries.
        (setq osx-dictionary-last-dictionary-file nil)
        (load (expand-file-name "osx-dictionary.el" copy) nil 'nomessage)
        (setq launcher-dictionary-real--dir copy))))

(defun launcher-dictionary-real--command (program &rest args)
  "Return the first line PROGRAM prints with ARGS, or its failure."
  (with-temp-buffer
    (let ((status (apply #'call-process program nil t nil args)))
      (format "%s%s" (car (split-string (buffer-string) "\n"))
              (if (eql status 0) "" (format " (exit %s)" status))))))

(defmacro launcher-dictionary-real--with (&rest body)
  "Run BODY in the graphical fixture with a real dictionary tool \"d\"."
  (declare (indent 0))
  `(progn
     (launcher-dictionary-real--load)
     (launcher-gui-tool--with
       (let ((launcher-tools '(("d" :name "Dictionary" :prompt "Word: "
                                :function launcher-osx-dictionary-lookup)))
             (osx-dictionary-current-dictionary nil)
             (osx-dictionary-previous-window-configuration nil))
         (unwind-protect (progn ,@body)
           (when (get-buffer launcher-osx-dictionary-buffer-name)
             (kill-buffer launcher-osx-dictionary-buffer-name)))
         (should-not (get-buffer "*osx-dictionary*"))
         (should-not osx-dictionary-previous-window-configuration)))))

(defun launcher-dictionary-real--result ()
  "Return the dictionary result's word and text."
  (when-let* ((buffer (get-buffer launcher-osx-dictionary-buffer-name)))
    (with-current-buffer buffer
      (list osx-dictionary--current-word
            (buffer-substring-no-properties (point-min) (point-max))))))

(defun launcher-dictionary-real--snaps-summary (states)
  "Return the gist of STATES, with result text shortened, for the log."
  (mapcar (lambda (s)
            (list (plist-get s :view) (buffer-name (plist-get s :buffer))
                  (plist-get s :mode) (plist-get s :input) (plist-get s :notice)
                  (plist-get s :windows) (plist-get s :frames)))
          states))

(ert-deftest launcher-dictionary-real-buffer-flow ()
  "`launcher-buffer': real lookups, an unknown word, back and the result keys."
  (launcher-dictionary-real--with
    (let* ((window (selected-window))
           (before (launcher-gui--window-state window))
           (frames (frame-list))
           results outcome)
      (let ((states
             (launcher-gui-tool--snaps
               (cl-flet ((result () (lambda () (push (launcher-dictionary-real--result) results))))
                 (setq outcome
                       (launcher-gui-tool--run
                        #'launcher-buffer
                        (list "d " (snap "40-dictionary-query")
                              "hello" 'return (snap "41-dictionary-result") (result)
                              ;; s: back to the query, keeping the word.
                              "s" (snap "42-dictionary-back")
                              'backspace 'backspace 'backspace 'backspace 'backspace
                              "qwxzv" 'return (snap "43-dictionary-unknown")
                              'backspace 'backspace 'backspace 'backspace 'backspace
                              "ice cream" 'return (snap "44-dictionary-multiword") (result)
                              ;; q: back to the query too.
                              "q" (snap)
                              'escape)))))))
        (setq results (nreverse results))
        (message "Dictionary states: %S" (launcher-dictionary-real--snaps-summary states))
        (message "Dictionary results: %S" results)
        (should (eq 'quit outcome))
        (pcase-let ((`(,query ,shown ,back ,unknown ,multiword ,quit) states))
          (should (equal "Dictionary — Word: " (plist-get query :prompt)))
          (dolist (state (list shown multiword))
            (should (eq 'result (plist-get state :view)))
            (should (eq window (plist-get state :window)))
            (should (equal launcher-osx-dictionary-buffer-name
                           (buffer-name (plist-get state :buffer))))
            (should (eq 'osx-dictionary-mode (plist-get state :mode)))
            (should (plist-get state :top))
            (should (= 1 (plist-get state :windows)))
            (should (= (length frames) (plist-get state :frames))))
          (should (eq 'query (plist-get back :view)))
          (should (equal "hello" (plist-get back :input)))
          (should (eq 'query (plist-get unknown :view)))
          (should (equal "qwxzv" (plist-get unknown :input)))
          (should (string-match-p "Dictionary failed: No definition for \"qwxzv\""
                                  (plist-get unknown :notice)))
          (should (eq 'query (plist-get quit :view)))
          (should (equal "ice cream" (plist-get quit :input))))
        (pcase-let ((`((,hello ,hello-text) (,ice ,ice-text)) results))
          (should (equal "hello" hello))
          (should (string-match-p "\\bhello\\b" hello-text))
          (should (string-match-p "greeting" hello-text))
          (should (equal "ice cream" ice))
          (should (string-match-p "frozen" ice-text))))
      (should (equal before (launcher-gui--window-state window)))
      (should (equal frames (frame-list)))
      (should-not launcher-gui--launched)
      (should-not launcher-gui--searched))))

(ert-deftest launcher-dictionary-real-pasted-unicode-and-minibuffer ()
  "Pasted Unicode in `launcher-buffer', and `launcher' with ordinary display."
  (launcher-dictionary-real--with
    (let ((frames (frame-list))
          results outcomes)
      (kill-new "d café")
      (let ((states
             (launcher-gui-tool--snaps
               (cl-flet ((result () (lambda () (push (launcher-dictionary-real--result) results))))
                 (push (launcher-gui-tool--run
                        #'launcher-buffer
                        (list 's-v (snap) 'return (snap "45-dictionary-unicode") (result)
                              'escape))
                       outcomes)
                 (push (launcher-gui-tool--run #'launcher (list "d hello" 'return))
                       outcomes)
                 (funcall (snap "46-dictionary-minibuffer-result"))
                 (funcall (result))))))
        (setq results (nreverse results) outcomes (nreverse outcomes))
        (message "Unicode states: %S" (launcher-dictionary-real--snaps-summary states))
        (message "Unicode results: %S" results)
        (should (equal '(quit returned) outcomes))
        (pcase-let ((`(,pasted ,shown ,popped) states))
          (should (equal "café" (plist-get pasted :input)))
          (should (eq 'result (plist-get shown :view)))
          (should (equal launcher-osx-dictionary-buffer-name
                         (buffer-name (plist-get shown :buffer))))
          ;; `launcher' shows the result by ordinary display, selected.
          (should (equal launcher-osx-dictionary-buffer-name
                         (buffer-name (window-buffer (plist-get popped :selected)))))
          (should (= (length frames) (plist-get popped :frames))))
        (pcase-let ((`((,cafe ,cafe-text) (,hello ,_)) results))
          (should (equal "café" cafe))
          (should (string-match-p "restaurant" cafe-text))
          (should (equal "hello" hello))))
      (should (equal frames (frame-list)))
      (should-not launcher-gui--launched)
      (should-not launcher-gui--searched))))

(ert-deftest launcher-dictionary-real-environment ()
  "Record the environment of the real lookups, after them."
  (launcher-dictionary-real--load)
  (let* ((helper (launcher-osx-dictionary--find-helper))
         (dictionaries (and helper (launcher-osx-dictionary--dictionaries helper))))
    (message "Dictionary environment: osx-dictionary %s; macOS %s; %s; clang: %s; \
developer tools: %s; helper: %s; restricted to: %S; %d dictionaries listed, \
English ones: %S"
             launcher-dictionary-real--revision
             (launcher-dictionary-real--command "sw_vers" "-productVersion")
             (emacs-version)
             (launcher-dictionary-real--command "clang" "--version")
             (launcher-dictionary-real--command "xcode-select" "-p")
             helper osx-dictionary-current-dictionary (length dictionaries)
             (seq-filter (lambda (name) (string-match-p "English\\|American" name))
                         dictionaries))
    (should helper)
    (should (string-prefix-p (file-name-as-directory launcher-dictionary-real--dir)
                             helper))
    (should dictionaries)))

(provide 'launcher-osx-dictionary-gui-tests)
;;; launcher-osx-dictionary-gui-tests.el ends here
