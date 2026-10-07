;;; portal-acceptance-config.el --- The user's setup, for Portal checks -*- lexical-binding: t; -*-

;; Loaded by test/portal-gui.sh into a disposable `emacs -Q' with Portal.
;; Only what the Portal-hosted launcher checks need from the user's init,
;; copied from its managed source as inspected on 2026-10-07: never the
;; whole init, services or credentials.

;;; Completion and appearance (navigation.el, ui.el)

(require 'vertico)
(require 'vertico-multiform)
(require 'vertico-directory)
;; The extensions the rules below name, which package.el autoloads.
(require 'vertico-buffer)
(require 'vertico-grid)
(require 'vertico-indexed)
(require 'orderless)
(require 'marginalia)

(vertico-mode)
(vertico-multiform-mode)
(setq vertico-multiform-commands
      '((consult-imenu buffer indexed)
        (consult-outline buffer ,(lambda (_) (text-scale-set -1)))))
(setq vertico-multiform-categories
      '((file grid)
        (consult-grep buffer)))
(keymap-set vertico-map "RET" #'vertico-directory-enter)
(keymap-set vertico-map "DEL" #'vertico-directory-delete-char)
(keymap-set vertico-map "M-DEL" #'vertico-directory-delete-word)
(add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)
(setq completion-styles '(orderless partial-completion basic))
(marginalia-mode)

;; The first of the user's fonts the machine has, as ui.el chooses.
(let ((font (seq-find (lambda (name) (member (car (split-string name "-")) (font-family-list)))
                      '("JetBrainsMono Nerd Font-17" "Hack Nerd Font-17" "Fira Code-17"))))
  (when font (set-frame-font font nil t)))
;; ui.el's light theme; the dark one is `modus-vivendi-tinted'.
(load-theme 'modus-operandi t)

;;; The proposed launcher setup (macos.el's `use-package launcher' :config)

(require 'launcher-osx-dictionary)
(setq launcher-tools
      '(("d" :name "Dictionary" :prompt "Word: "
              :function launcher-osx-dictionary-lookup)))

;;; The proposed Portal command (tools.el's `use-package portal' :config)

(require 'portal-launcher)
(require 'portal-global-shortcut)

(defun my/app-launcher ()
  "Launch apps, search the web, or query tools in Portal's panel.
With a prefix argument, rebuild the application index first."
  (interactive)
  (let ((vertico-count 6)
        (portal-launcher-size '(720 auto))
        (portal-launcher-height-limits '(content 480)))
    (portal-launcher-present #'launcher-buffer
                             :kind 'buffer
                             :mode-line nil)))

;; The rollback: today's minibuffer-only panel, under another name here.
(defun my/app-launcher-minibuffer ()
  "Launch an app or search the web in Portal's minibuffer-only panel."
  (interactive)
  (let ((vertico-count 6)
        ;; A minibuffer-only host cannot show tool result buffers.
        ;; Disable tools only for this old app/search presentation.
        (launcher-tools nil))
    (portal-launcher-present #'launcher :kind 'minibuffer)))

;; The checks register these on a test chord, not Command-Space.

(provide 'portal-acceptance-config)
;;; portal-acceptance-config.el ends here
