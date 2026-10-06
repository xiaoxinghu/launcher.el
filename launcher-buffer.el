;;; launcher-buffer.el --- Run the launcher at the top of a window -*- lexical-binding: t; -*-

;;; Commentary:

;; `launcher-buffer' offers the same app, bang, web and tool choices as
;; `launcher', but Vertico shows the prompt and candidates at the top of
;; the selected ordinary window instead of in the minibuffer.  The command
;; owns its interaction until a choice succeeds or the user quits: later
;; views, a tool's query and its result, replace the picker in the same
;; window, `launcher-back' returns to the previous view and
;; `launcher-quit' ends the interaction.
;;
;; The command `launcher-buffer', in launcher.el, loads this file on first
;; use.  Needs Emacs 29.1 and Vertico 2.x (tested with 2.15), with
;; `vertico-mode' enabled.  Loading this file does not load Vertico;
;; `launcher' works without it.

;;; Code:

(require 'cl-lib)
(require 'launcher)

(defvar vertico-mode)
(defvar vertico-count)
(defvar vertico-buffer-mode)
(defvar vertico-buffer-hide-prompt)
(defvar vertico--input)
(defvar vertico--candidates-ov)
(defvar vertico--total)
(declare-function vertico--exhibit "vertico" ())

(cl-defstruct (launcher-buffer--session
               (:constructor launcher-buffer--session-make)
               (:copier nil))
  "State of one `launcher-buffer' interaction."
  (tag (make-symbol "launcher-buffer") :documentation "Catch tag of views.")
  (window nil :documentation "Ordinary window the interaction runs in.")
  (buffer nil :documentation "Buffer WINDOW showed before.")
  (start nil :documentation "Marker of WINDOW's start in BUFFER.")
  (point nil :documentation "Marker of WINDOW's point in BUFFER.")
  (hscroll 0 :documentation "WINDOW's horizontal scroll.")
  (dedicated nil :documentation "WINDOW's dedication.")
  (prev-buffers nil :documentation "WINDOW's previous buffers.")
  (next-buffers nil :documentation "WINDOW's next buffers.")
  (cap nil :documentation "Candidate rows the picker shows at most.")
  (tools nil :documentation "The interaction's `launcher-tools'.")
  (view nil :documentation "Current view: (picker INPUT), (query TOOL INPUT)
or (result BUFFER).")
  (history nil :documentation "Views `launcher-back' returns to, newest first.")
  (minibuffer nil :documentation "The picker's or query's minibuffer while it reads.")
  (shown nil :documentation "Buffers the interaction showed in WINDOW.")
  (end nil :documentation "End of the result WINDOW follows, if it does."))

(defvar launcher-buffer--session nil
  "The active `launcher-buffer' interaction, or nil.")

(defvar-local launcher-buffer--picker nil
  "In a `launcher-buffer' picker's minibuffer, its interaction.")

(defvar-local launcher-buffer--query nil
  "In a `launcher-buffer' query's minibuffer, its interaction.")

(defvar-keymap launcher-buffer-map
  :doc "Keys of `launcher-buffer' result views.
They take precedence over the result buffer's own keys, only while the
interaction's window is selected."
  "C-c C-b" #'launcher-back
  "C-g" #'launcher-quit
  "<escape>" #'launcher-quit)

(defvar-keymap launcher-buffer-picker-map
  :doc "Keys added to the `launcher-buffer' picker's and queries' keys.
In the picker, they take precedence over the completion keys, and in a
tool's query, over `launcher-query-map'.  C-g quits as in any minibuffer."
  "C-c C-b" #'launcher-back
  "<escape>" #'launcher-quit)

(defvar launcher-buffer--emulation nil
  "Keymap alist installed in `emulation-mode-map-alists' while active.")

(defconst launcher-buffer--display-modes
  '(vertico-flat-mode vertico-grid-mode vertico-reverse-mode
    vertico-unobtrusive-mode vertico-posframe-mode)
  "Vertico display modes the picker turns off in its own minibuffer.")

(defun launcher-buffer--vertico-compatible-p ()
  "Return non-nil if the loaded Vertico provides what the picker uses.
Besides its options, the picker relies on Vertico 2.x internals:
display extensions dispatch on the value of their mode variable in the
minibuffer, `vertico-buffer--redisplay' derives `vertico-count' from the
window height, `vertico--exhibit' shows the candidates in the string of
the overlay `vertico--candidates-ov', and `vertico--total' counts them."
  (and (boundp 'vertico-buffer-hide-prompt)
       (boundp 'vertico-buffer-display-action)
       (boundp 'vertico--input)
       (boundp 'vertico--candidates-ov)
       (boundp 'vertico--total)
       (fboundp 'vertico--exhibit)
       (fboundp 'vertico-buffer--setup)
       (fboundp 'vertico-buffer--redisplay)))

(defun launcher-buffer--check ()
  "Signal a `user-error' unless an interaction can start now."
  (when launcher-buffer--session
    (user-error "A launcher interaction is already active"))
  (when (active-minibuffer-window)
    (user-error "Finish the current minibuffer before starting the launcher"))
  (unless (and (require 'vertico nil t) (require 'vertico-buffer nil t))
    (user-error "`launcher-buffer' needs the Vertico package; \
`launcher' works without it"))
  (unless (launcher-buffer--vertico-compatible-p)
    (user-error "`launcher-buffer' needs Vertico 2.x; \
`launcher' works with this version"))
  (unless vertico-mode
    (user-error "`launcher-buffer' needs `vertico-mode' enabled")))

(defun launcher-buffer--active ()
  "Return the active interaction, or signal a `user-error'."
  (or launcher-buffer--session
      (user-error "No launcher interaction is active")))

(defun launcher-buffer--back ()
  "Return to the previous view of the active interaction, if any.
The interaction's value of `launcher--back-function'."
  (let ((session (launcher-buffer--active)))
    (if-let* ((view (pop (launcher-buffer--session-history session))))
        (throw (launcher-buffer--session-tag session) view)
      (if (minibufferp)
          (minibuffer-message "Already at the launcher picker")
        (message "Already at the launcher picker")))))

(defun launcher-quit ()
  "End the active launcher interaction, as with \\[keyboard-quit]."
  (interactive)
  (throw (launcher-buffer--session-tag (launcher-buffer--active)) 'quit))

(defun launcher-buffer--current-view (session)
  "Return SESSION's current view, with the picker's or query's input."
  (let ((view (launcher-buffer--session-view session))
        (minibuffer (launcher-buffer--session-minibuffer session)))
    (if (and (memq (car view) '(picker query)) (buffer-live-p minibuffer))
        (append (butlast view)
                (list (with-current-buffer minibuffer
                        (minibuffer-contents-no-properties))))
      view)))

(defun launcher-buffer--visit (buffer)
  "Show BUFFER as the next view of the active launcher interaction.
End the current view's input first; `launcher-back' returns to it.
BUFFER keeps its mode and contents, and is never killed by Launcher."
  (let ((session (launcher-buffer--active)))
    (unless (buffer-live-p buffer)
      (error "Launcher result is not a live buffer: %S" buffer))
    (push (launcher-buffer--current-view session)
          (launcher-buffer--session-history session))
    (throw (launcher-buffer--session-tag session) (list 'result buffer))))

(defun launcher-buffer--key-filter (command)
  "Return COMMAND while a result view's window is selected."
  (when-let* ((session launcher-buffer--session)
              ((eq (car (launcher-buffer--session-view session)) 'result))
              ((eq (selected-window) (launcher-buffer--session-window session))))
    command))

(defun launcher-buffer--filtered-keys ()
  "Return `launcher-buffer-map' with each binding behind the key filter."
  (let ((map (make-sparse-keymap)))
    (map-keymap (lambda (event binding)
                  (define-key map (vector event)
                              `(menu-item "" ,binding
                                          :filter launcher-buffer--key-filter)))
                launcher-buffer-map)
    map))

(defun launcher-buffer--watch ()
  "End a view whose window or result buffer is gone."
  (when-let* ((session launcher-buffer--session))
    (let ((tag (launcher-buffer--session-tag session)))
      (pcase (launcher-buffer--session-view session)
        ((guard (not (window-live-p (launcher-buffer--session-window session))))
         (throw tag 'quit))
        (`(result ,buffer)
         (unless (buffer-live-p buffer)
           (throw tag (or (pop (launcher-buffer--session-history session))
                          'quit))))))))

(defun launcher-buffer--make (window)
  "Return a new interaction in WINDOW, recording its state."
  (when (window-minibuffer-p window)
    (user-error "Select an ordinary window to start the launcher"))
  (launcher-buffer--session-make
   :window window
   :buffer (window-buffer window)
   :start (set-marker (make-marker) (window-start window) (window-buffer window))
   :point (set-marker (make-marker) (window-point window) (window-buffer window))
   :hscroll (window-hscroll window)
   :dedicated (window-dedicated-p window)
   :prev-buffers (window-prev-buffers window)
   :next-buffers (window-next-buffers window)
   :cap vertico-count))

(defun launcher-buffer--install (session)
  "Install SESSION's keys and watcher, and free its window for views."
  (setq launcher-buffer--emulation
        `((launcher-buffer--session . ,(launcher-buffer--filtered-keys))))
  (push 'launcher-buffer--emulation emulation-mode-map-alists)
  (add-hook 'post-command-hook #'launcher-buffer--watch)
  (add-hook 'pre-redisplay-functions #'launcher-buffer--follow)
  ;; Views replace the window's buffer; its dedication returns on exit.
  (set-window-dedicated-p (launcher-buffer--session-window session) nil))

(defun launcher-buffer--fallback (session)
  "Return a buffer to show if SESSION's window lost its original buffer.
Prefer the window's earlier buffers, then the frame's recent ones, but
never a view of the interaction or an internal buffer."
  (let ((shown (launcher-buffer--session-shown session)))
    (cl-flet ((usable (buffer)
                (and (buffer-live-p buffer)
                     (not (memq buffer shown))
                     (not (string-prefix-p " " (buffer-name buffer))))))
      (or (seq-find #'usable (mapcar #'car (launcher-buffer--session-prev-buffers
                                            session)))
          (seq-find #'usable (buffer-list (window-frame
                                           (launcher-buffer--session-window
                                            session))))
          (get-scratch-buffer-create)))))

(defun launcher-buffer--finish (session)
  "Undo SESSION's changes, restoring its window if it still owns it.
The interaction stops owning the window when the window is deleted or
shows a buffer the interaction did not put there."
  (setq emulation-mode-map-alists
        (delq 'launcher-buffer--emulation emulation-mode-map-alists))
  (setq launcher-buffer--emulation nil)
  (remove-hook 'post-command-hook #'launcher-buffer--watch)
  (remove-hook 'pre-redisplay-functions #'launcher-buffer--follow)
  (let ((window (launcher-buffer--session-window session))
        (buffer (launcher-buffer--session-buffer session))
        (start (launcher-buffer--session-start session))
        (point (launcher-buffer--session-point session)))
    (when (and (window-live-p window)
               (let ((shown (window-buffer window)))
                 (or (eq shown buffer)
                     (minibufferp shown)
                     (memq shown (launcher-buffer--session-shown session)))))
      (if (not (buffer-live-p buffer))
          (set-window-buffer window (launcher-buffer--fallback session))
        (set-window-buffer window buffer)
        (set-window-start window start t)
        (set-window-point window point)
        (set-window-hscroll window (launcher-buffer--session-hscroll session)))
      (set-window-dedicated-p
       window (launcher-buffer--session-dedicated session))
      ;; After showing the buffer, which records the view it replaces:
      ;; views do not stay in the window's buffer history.
      (set-window-prev-buffers
       window (seq-filter (lambda (entry) (buffer-live-p (car entry)))
                          (launcher-buffer--session-prev-buffers session)))
      (set-window-next-buffers
       window (seq-filter #'buffer-live-p
                          (launcher-buffer--session-next-buffers session))))
    (set-marker start nil)
    (set-marker point nil)))

(defun launcher-buffer--display (session buffer)
  "Show BUFFER in SESSION's window, regardless of display rules."
  (let ((window (launcher-buffer--session-window session)))
    (cl-pushnew buffer (launcher-buffer--session-shown session))
    (set-window-buffer window buffer)
    window))

(defvar-local launcher-buffer--shown-count nil
  "The `vertico-count' Vertico last showed the picker's candidates with.")

(defun launcher-buffer--fit-count (session)
  "Return how many candidate rows SESSION's window shows, at most its cap.
A row cap above the window's height would put the selected candidate
below the window's bottom, out of reach: the window cannot scroll within
Vertico's candidate overlay."
  (min (launcher-buffer--session-cap session)
       (max 1 (1- (/ (window-body-height (launcher-buffer--session-window session) t)
                     (default-line-height))))))

(defun launcher-buffer--pad ()
  "Keep the candidates as tall as the cap would show them.
When the window shows fewer rows than the cap, blank rows fill the
rest, below the window's bottom: a host fitting its window to content
still sees the cap's height, and can grow the window to show all."
  (let* ((cap (launcher-buffer--session-cap launcher-buffer--picker))
         (string (or (overlay-get vertico--candidates-ov 'before-string) ""))
         (missing (- (min cap vertico--total)
                     (max 0 (1- (cl-count ?\n string))))))
    (setq launcher-buffer--shown-count vertico-count)
    (when (> missing 0)
      (overlay-put vertico--candidates-ov 'before-string
                   (concat string (make-string missing ?\n))))))

(defun launcher-buffer--redisplay (window)
  "Update the picker's display before redisplaying WINDOW.
Run after `vertico-buffer--redisplay', which derives `vertico-count' from
the window height instead of the caller's cap."
  (when-let* ((session launcher-buffer--picker)
              (mini (active-minibuffer-window))
              ((eq (window-buffer mini) (current-buffer))))
    ;; Hide the minibuffer window's copy of the prompt by scrolling, not
    ;; by shrinking the window below its legal height.
    (unless (> (window-vscroll mini) 0)
      (set-window-vscroll mini 3))
    (when (eq window (launcher-buffer--session-window session))
      (setq-local vertico-count (launcher-buffer--fit-count session))
      ;; Show the new number of rows now, not after the next key.
      (unless (eql vertico-count launcher-buffer--shown-count)
        (vertico--exhibit)
        (launcher-buffer--pad)))))

(defun launcher-buffer--exit-picker ()
  "Undo the picker's scrolling of the minibuffer window."
  (when-let* ((mini (active-minibuffer-window)))
    (set-window-vscroll mini 0)))

(defun launcher-buffer--before-reader (session)
  "Ask Vertico to show this minibuffer in SESSION's window.
Run in the picker's minibuffer before Vertico's own setup."
  (setf (launcher-buffer--session-minibuffer session) (current-buffer))
  ;; Vertico's display extensions dispatch on their mode's value in the
  ;; minibuffer.  Local values affect only this prompt: nested prompts
  ;; keep the user's display modes and multiform rules.
  (setq-local vertico-buffer-mode t
              ;; Vertico would shrink the minibuffer window to hide its
              ;; prompt, which fails in a small frame.
              vertico-buffer-hide-prompt nil
              ;; Instead, let Emacs fit the window to its one line, after
              ;; a long message grew it.
              resize-mini-windows t
              vertico-count (launcher-buffer--session-cap session))
  (dolist (mode launcher-buffer--display-modes)
    (when (boundp mode)
      (set (make-local-variable mode) nil))))

(defun launcher-buffer--after-reader (session)
  "Add the picker's keys and redisplay to this Vertico minibuffer.
Run in the picker's minibuffer after Vertico's own setup."
  (let ((window (launcher-buffer--session-window session)))
    (unless (and vertico--input (overlayp vertico--candidates-ov)
                 (eq (overlay-get vertico--candidates-ov 'window) window))
      (error "`launcher-buffer' could not show Vertico in the window; \
`completing-read-function' must use Vertico"))
    (setq-local launcher-buffer--picker session
                ;; Vertico's buffer setup derived it from the window height.
                vertico-count (launcher-buffer--fit-count session))
    (use-local-map (make-composed-keymap launcher-buffer-picker-map
                                         (current-local-map)))
    ;; After Vertico shows candidates, and after its buffer redisplay.
    (add-hook 'post-command-hook #'launcher-buffer--pad 90 t)
    (add-hook 'pre-redisplay-functions #'launcher-buffer--redisplay 90 t)
    (add-hook 'minibuffer-exit-hook #'launcher-buffer--exit-picker nil t)))

(defun launcher-buffer--read (session entries initial)
  "Read a launcher choice among ENTRIES and SESSION's tools in its window.
INITIAL is the initial input."
  (let* ((window (launcher-buffer--session-window session))
         (outer display-buffer-overriding-action)
         ;; Rebound so that setting it below cannot outlive this prompt.
         (display-buffer-overriding-action outer)
         (before (make-symbol "launcher-buffer--before")))
    (fset before
          (lambda ()
            (remove-hook 'minibuffer-setup-hook before)
            (launcher-buffer--before-reader session)
            ;; Vertico displays the minibuffer with `display-buffer'.
            ;; Only during its setup, put it in this window whatever the
            ;; display rules, including a host's overriding action.
            (setq display-buffer-overriding-action
                  (list (lambda (buffer _alist)
                          (launcher-buffer--display session buffer))))))
    (select-window window 'norecord)
    (unwind-protect
        (progn
          (add-hook 'minibuffer-setup-hook before -90)
          (launcher--read entries (launcher-buffer--session-tools session) initial
                          (lambda ()
                            (setq display-buffer-overriding-action outer)
                            (launcher-buffer--after-reader session))))
      (remove-hook 'minibuffer-setup-hook before)
      (setf (launcher-buffer--session-minibuffer session) nil))))

(defun launcher-buffer--query-redisplay (window)
  "Show the query's input and cursor at the top of its interaction's window.
Run before redisplaying WINDOW, which shows the query's minibuffer."
  (when-let* ((session launcher-buffer--query)
              (mini (active-minibuffer-window))
              ((eq (window-buffer mini) (current-buffer))))
    ;; As in the picker, hide the minibuffer window's copy by scrolling.
    (unless (> (window-vscroll mini) 0)
      (set-window-vscroll mini 3))
    (when (eq window (launcher-buffer--session-window session))
      (set-window-point window (point)))))

(defun launcher-buffer--query-exit ()
  "Undo the query's changes to the minibuffer window, as it exits."
  (when-let* ((session launcher-buffer--query))
    (setf (launcher-buffer--session-minibuffer session) nil))
  (launcher-buffer--exit-picker))

(defun launcher-buffer--query-setup (session)
  "Show this tool query's minibuffer at the top of SESSION's window.
Run in the query's minibuffer, which keeps reading plain text."
  (let ((window (launcher-buffer--session-window session))
        (label (string-trim-right (minibuffer-prompt) ":? *")))
    (setf (launcher-buffer--session-minibuffer session) (current-buffer))
    ;; Local values, as Vertico's buffer display sets for the picker.
    (setq-local launcher-buffer--query session
                resize-mini-windows t
                cursor-in-non-selected-windows (if (memq cursor-type '(nil t))
                                                   'box
                                                 cursor-type)
                face-remapping-alist (cons '(mode-line-inactive mode-line)
                                           face-remapping-alist))
    (when mode-line-format
      (setq-local mode-line-format
                  (list " " (propertize label 'face 'mode-line-buffer-id) " ")))
    (use-local-map (make-composed-keymap launcher-buffer-picker-map
                                         (current-local-map)))
    (add-hook 'pre-redisplay-functions #'launcher-buffer--query-redisplay nil t)
    (add-hook 'minibuffer-exit-hook #'launcher-buffer--query-exit nil t)
    (launcher-buffer--display session (current-buffer))
    (set-window-start window (point-min))))

(defun launcher-buffer--read-query (session tool input)
  "Read TOOL's query in SESSION's window, starting with INPUT.
Return (BUFFER . QUERY), as `launcher--query' does."
  (select-window (launcher-buffer--session-window session) 'norecord)
  (unwind-protect
      (launcher--query tool input
                       (lambda () (launcher-buffer--query-setup session)))
    (setf (launcher-buffer--session-minibuffer session) nil)))

(defun launcher-buffer--follow (window)
  "Keep a result view at the end of its buffer while it is there.
Run before redisplaying WINDOW.  If the window's point was at the end
of a nonempty result when last redisplayed, and text was added there,
move it to the new end.  Anywhere else, leave the reader's point."
  (when-let* ((session launcher-buffer--session)
              ((eq window (launcher-buffer--session-window session)))
              (view (launcher-buffer--session-view session))
              ((eq (car view) 'result))
              ((eq (window-buffer window) (cadr view))))
    (let ((end (with-current-buffer (cadr view) (point-max)))
          (last (launcher-buffer--session-end session)))
      (when (and last (> end last) (= (window-point window) last))
        (set-window-point window end))
      (setf (launcher-buffer--session-end session)
            (and (= (window-point window) end)
                 (> end (with-current-buffer (cadr view) (point-min)))
                 end)))))

(defun launcher-buffer--result (session buffer)
  "Show BUFFER in SESSION's window and edit there until the view ends.
Return nil if the user exits the recursive edit normally."
  (setf (launcher-buffer--session-end session) nil)
  (select-window (launcher-buffer--display session buffer) 'norecord)
  (recursive-edit)
  nil)

(defun launcher-buffer--run (session entries)
  "Show SESSION's views until a choice among ENTRIES succeeds.
Signal `quit' if the user quits instead."
  (let ((view '(picker nil))
        (tools (launcher-buffer--session-tools session)))
    (while view
      (when (or (eq view 'quit)
                (not (window-live-p (launcher-buffer--session-window session))))
        (signal 'quit nil))
      (setf (launcher-buffer--session-view session) view)
      (setq view
            (catch (launcher-buffer--session-tag session)
              (pcase-exhaustive view
                (`(picker ,input)
                 (let* ((choice (launcher-buffer--read session entries input))
                        (route (launcher--route choice tools)))
                   (if (not route)
                       (progn (launcher--act choice entries) nil)
                     ;; Back shows the prefix without its space.
                     (push (list 'picker (launcher--tool-prefix (car route)))
                           (launcher-buffer--session-history session))
                     (list 'query (car route) (cdr route)))))
                (`(query ,tool ,input)
                 (pcase-let ((`(,buffer . ,query)
                              (launcher-buffer--read-query session tool input)))
                   (push (list 'query tool query)
                         (launcher-buffer--session-history session))
                   (list 'result buffer)))
                (`(result ,buffer)
                 (launcher-buffer--result session buffer))))))))

(defun launcher-buffer--interact (refresh)
  "Run a `launcher-buffer' interaction in the selected window.
With REFRESH, rebuild the app index first."
  (launcher-buffer--check)
  (let* ((tools (launcher--tools))
         (entries (launcher--entries refresh tools))
         (session (launcher-buffer--make (selected-window)))
         (launcher-buffer--session session)
         (launcher--back-function #'launcher-buffer--back))
    (setf (launcher-buffer--session-tools session) tools)
    (unwind-protect
        (progn
          (launcher-buffer--install session)
          (launcher-buffer--run session entries))
      (launcher-buffer--finish session))))

(provide 'launcher-buffer)
;;; launcher-buffer.el ends here
