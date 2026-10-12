;;; launcher-test-support.el --- Batch input fixture -*- lexical-binding: t; -*-
(require 'launcher)
(require 'ert)

(defun launcher-test--input (text)
  "Type TEXT in the fake minibuffer and run the real query hook."
  (erase-buffer)
  (insert text)
  (launcher--results-post-command))

(defun launcher-test--submit (text &optional index)
  "Query TEXT and submit result INDEX (default first) through the real action path."
  (launcher-test--input text)
  (let* ((result (nth (or index 0) (launcher--query-session-results launcher--query-session)))
         (candidate (plist-get result :candidate)))
    (cl-letf (((symbol-function 'launcher--results-current) (lambda () result))
              ((symbol-function 'exit-minibuffer) #'ignore))
      (launcher-results-submit))
    candidate))

(defun launcher-test--reader (answers)
  "Return a fake completion reader driving actual queries/actions with ANSWERS.
Strings query and submit; functions can inspect the input or drive a more
complex sequence; symbols signal a condition."
  (lambda (&rest args)
    (with-temp-buffer
      (use-local-map (copy-keymap minibuffer-local-completion-map))
      (when (stringp (nth 4 args)) (insert (nth 4 args)))
      (run-hooks 'minibuffer-setup-hook)
      (let ((answer (pop answers)))
        (cond ((stringp answer) (launcher-test--submit answer))
              ((functionp answer)
               (let ((value (funcall answer)))
                 (when (stringp value) (launcher-test--submit value))))
              (t (signal (or answer 'quit) nil))))
      "")))

(provide 'launcher-test-support)
