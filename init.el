(require 'package)
(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

;; Hide menu bar
(menu-bar-mode 0)

;; OS level copy / paste
(setq x-select-enable-clipboard t)

;; Scroll up with cursor
(global-set-key (kbd "M-P") (lambda () (interactive) (previous-line) (scroll-down-line)))

;; Scroll down with cursor
(global-set-key (kbd "M-N") (lambda () (interactive) (next-line) (scroll-up-line)))

;; Goto line
(global-set-key (kbd "M-g") 'goto-line)

;; Go back a buffer
(global-set-key (kbd "C-x p") (lambda () (interactive) (other-window -1)))

;; Clean whitespace
(global-set-key (kbd "C-x M-w") (lambda () (interactive) (whitespace-cleanup)))

(defun save-to-clipboard (start end)
  "Save region to clipboard through pbcopy"
  (interactive "r")
  (shell-command-on-region start end "pbcopy")
  (setq deactivate-mark t))

;; Copy to Mac OS X Clipboard
(global-set-key (kbd "M-W") 'save-to-clipboard)

(setq linum-format "%d ")
(global-linum-mode 1)
(column-number-mode 1)
(setq python-indent 4)

(setq js-indent-level 4)

;; Disable auto-save and backups
(setq backup-inhibited t)
(setq auto-save-default nil)

(add-hook 'html-mode-hook
          (lambda ()
            ;; Default indentation is usually 2 spaces, changing to 4.
            (set (make-local-variable 'sgml-basic-offset) 8)))

;; Adding geiser mode
(load-file "~/lisp/geiser-0.1.4/elisp/geiser.el")

;; ido-mode everywhere
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

;; Pretty Lambda for Python
(setq pretty-lambda-auto-modes '(lisp-mode emacs-lisp-mode lisp-interaction-mode scheme-mode python-mode))
;; Turn it on
(require 'pretty-lambdada)
(pretty-lambda-for-modes)

;; Show Paren Mode
(show-paren-mode)

;; Magit cosmetics
(eval-after-load 'magit
  '(progn
     (set-face-foreground 'magit-diff-add "green4")
     (set-face-foreground 'magit-diff-del "red3")))

;; Find-File-In-Project Settings
(require 'find-file-in-project)
(setq ffip-limit 2048)
(setq ffip-project-file ".emacs-project")
(setq ffip-patterns (append '("*.hamlpy" "*.sass" "*.coffee") ffip-patterns))
(global-set-key (kbd "C-c C-f") 'ffip)


(autoload 'paredit-mode "paredit"
  "Minor mode for pseudo-structurally editing Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       (lambda () (paredit-mode +1)))
(add-hook 'lisp-mode-hook             (lambda () (paredit-mode +1)))
(add-hook 'lisp-interaction-mode-hook (lambda () (paredit-mode +1)))
(add-hook 'scheme-mode-hook           (lambda () (paredit-mode +1)))
(add-hook 'clojure-mode-hook          (lambda () (paredit-mode +1)))

;; Paredit Mode Keybindings
(define-key paredit-mode-map (kbd "M-)") 'paredit-forward-slurp-sexp)
(define-key paredit-mode-map (kbd "M-(") 'paredit-backward-slurp-sexp)
