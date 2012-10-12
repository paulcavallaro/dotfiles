(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

;; Hide menu bar
(menu-bar-mode 0)

;; Hide tool bar
(tool-bar-mode 0)

;; OS level copy / paste
(setq x-select-enable-clipboard t)

;; Scroll up with cursor
(global-set-key (kbd "M-P") (lambda (arg) (interactive "p") (previous-line arg) (scroll-down-line arg)))

;; Scroll down with cursor
(global-set-key (kbd "M-N") (lambda (arg) (interactive "p") (next-line arg) (scroll-up-line arg)))

;; Goto line
(global-set-key (kbd "M-g") 'goto-line)

;; Go back a buffer
(global-set-key (kbd "C-x p") (lambda () (interactive) (other-window -1)))

(setq indent-tabs-mode 'nil)
(setq whitespace-style '(face tabs spaces trailing lines space-before-tab::space newline indentation::space empty space-after-tab::space space-before-tab::space space-mark tab-mark newline-mark))

;; Clean whitespace
(global-set-key (kbd "C-x M-w") (lambda () (interactive)
                                  (whitespace-cleanup)))

;; C indentation level to 8
(setq-default c-basic-offset 8)

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

;; Html mode hooks
(add-hook 'html-mode-hook
          (lambda ()
            (set (make-local-variable 'sgml-basic-offset) 2)))

;; Adding geiser mode
(load-file "~/lisp/geiser-0.2.1/elisp/geiser.el")
;; Adding quack mode after geiser
;; (load-file "~/lisp/quack-0.45/quack.el")
;; (require 'quack)

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
(setq ffip-project-file ".git")
(setq ffip-patterns (append '("*.hamlpy" "*.sass" "*.coffee" "*.hs" "*.lhs" "*.hsc" "*.ocaml" "*.rkt" "*.scm" "*.proto" "*.java" "*.c" "*.h") ffip-patterns))
(global-set-key (kbd "C-c C-f") 'ffip)

(autoload 'paredit-mode "paredit"
  "Minor mode for pseudo-structurally editing Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       (lambda () (paredit-mode +1)))
(add-hook 'lisp-mode-hook             (lambda () (paredit-mode +1)))
(add-hook 'lisp-interaction-mode-hook (lambda () (paredit-mode +1)))
(add-hook 'scheme-mode-hook           (lambda () (paredit-mode +1)))
(add-hook 'clojure-mode-hook          (lambda () (paredit-mode +1)))

;; Paredit Mode Keybindings
(require 'paredit)
(define-key paredit-mode-map (kbd "M-)") 'paredit-forward-slurp-sexp)
(define-key paredit-mode-map (kbd "M-(") 'paredit-forward-barf-sexp)
(define-key paredit-mode-map (kbd "M-{") 'paredit-backward-slurp-sexp)
(define-key paredit-mode-map (kbd "M-}") 'paredit-backward-barf-sexp)

;; Ace Jump Mode
(require 'ace-jump-mode)
(define-key global-map (kbd "C-x j") 'ace-jump-mode)

;; Hide welcome message
(setq inhibit-startup-message t)

;; Pylint and Flymake
(require 'flymake)

(exec-path-from-shell-initialize)
(when (load "flymake" t)
  (defun flymake-pylint-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list "epylint" (list local-file))))

  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pylint-init)))

(add-hook 'python-mode-hook (lambda () (flymake-mode t)))

;; Flymake-Cursor
(require 'flymake-cursor)

;; Mac OS X
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'super)
(global-set-key (kbd "M-`") 'other-frame)

;; Browse Kill Ring
(require 'browse-kill-ring)
(browse-kill-ring-default-keybindings)

;; Org-Mode
(require 'org)
(define-key org-mode-map (kbd "M-p") 'org-metaup)
(define-key org-mode-map (kbd "M-n") 'org-metadown)

;; Haskell-Mode
(add-hook 'haskell-mode-hook (lambda () (haskell-indentation-mode t)))

;; Turn off fringe mode because it doesn't work nicely in OS X
;; and also hide scroll bar
(defun osx-frame-fixes (&optional frame)
  (set-fringe-mode 0)
  (set-scroll-bar-mode nil))

(osx-frame-fixes)

(add-hook 'after-make-frame-functions 'osx-frame-fixes)

;; Settings for just Mac OS X Emacs with window
(when window-system
  (require 'color-theme-solarized)
  (color-theme-solarized-dark)
  (setq default-frame-alist '((width . 190)
                              (height . 60)))
  (custom-set-faces
   '(default ((t (:inherit nil :stipple nil :background "#042028" :foreground "#708183" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight bold :height 135 :width extra-expanded :foundry "apple" :family "Inconsolata"))))
   '(flymake-errline ((t (:background "#9E0000"))))
   '(org-todo ((t (:background "#BD1111")))))
  (custom-set-faces
   '(flymake-errline ((t (:background "#9E0000"))))
   '(org-todo ((t (:background "#BD1111"))))))

;; Ghetto way to get the path properly set
(setenv "PATH" (shell-command-to-string "source ~/.zshrc && echo $PATH"))

;; hl-line-mode for line highlighting
(global-hl-line-mode 1)

;; Add puppet mode to .pp files
(add-to-list 'auto-mode-alist '("\\.pp\\'" . puppet-mode))

;; Add markdown mode to .md files
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; Setup haskell-mode
(defun haskell-mode-hooks ()
  (turn-on-haskell-doc-mode)
  (turn-on-haskell-indentation)
  ;;(turn-on-haskell-indent)
  ;;(turn-on-haskell-simple-indent)
  )

(add-hook 'haskell-mode-hook 'haskell-mode-hooks)

(add-to-list 'load-path "~/.emacs.d/unmanaged")

(require 'parselt)

(require 'racket)

(require 'multiple-cursors)

(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
