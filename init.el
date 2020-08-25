(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Add unmanaged load path
(add-to-list 'load-path "~/.emacs.d/unmanaged")
(add-to-list 'load-path "~/.emacs.d/unmanaged/highlight-80+.el")

;; Take that emacs version control integration!
(setq vc-handled-backends nil)

;; Turn off bell
(defun ring-bell-function ())

;; custom-set-variables for customize stuff
;; ack.el fixes for git grep --no-color
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ack-command "git grep ")
 '(ack-vc-grep-commands
   (quote
    ((".git" . "git --no-pager grep --no-color -n -i")
     (".hg" . "hg grep -n -i")
     (".bzr" . "bzr grep --color=always -n -i"))))
 '(background-color "#7f7f7f")
 '(background-mode dark)
 '(cursor-color "#5c5cff")
 '(custom-safe-themes
   (quote
    ("1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" default)))
 '(foreground-color "#5c5cff")
 '(package-selected-packages
   (quote
    (ess yaml-mode bazel-mode protobuf-mode multiple-cursors xterm-color tuareg solarized-theme smex rust-mode pretty-lambdada php-mode paredit org nlinum nasm-mode markdown-mode magit lua-mode ido-ubiquitous iasm-mode ghci-completion ghc geiser flymake find-file-in-project fic-ext-mode d-mode color-theme-solarized cm-mode browse-kill-ring ack ace-window ace-jump-mode))))

;; protobuf-mode
(require 'protobuf-mode)

(global-set-key (kbd "C-M-q") 'clang-format-region)

;; Revert all buffers visiting a file function
(defun revert-all-buffers ()
  "Refreshes all open buffers from their respective files."
  (interactive)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and (buffer-file-name) (file-exists-p (buffer-file-name)) (not (buffer-modified-p)))
        (revert-buffer t t t) )))
  (message "Refreshed open files."))

;; Give me my definition please and thank you
(global-set-key (kbd "M-?") (lambda () (interactive) (find-tag (find-tag-default))))

;; Make things kind of more intuitive
(global-set-key (kbd "C-x |") (lambda () (interactive) (split-window-right)))
(global-set-key (kbd "C-x _") (lambda () (interactive) (split-window-below)))

;; ace-window
(require 'ace-window)
(global-set-key (kbd "M-o") 'ace-window)

;; Hide menu bar
(menu-bar-mode 0)

;; Hide tool bar
(tool-bar-mode 0)

;; OS level copy / paste
(setq x-select-enable-clipboard t)

;; Revert buffer with no regard for human life
(global-set-key (kbd "s-u") (lambda () (interactive) (revert-buffer t t)))

;; Kill whole line
(global-set-key (kbd "C-S-k") 'kill-whole-line)

;; I hate you upcase region
(global-set-key (kbd "C-x C-u") 'undo)

;; Scroll up with cursor
(global-set-key (kbd "M-p") (lambda (arg) (interactive "p") (previous-line arg) (scroll-down-line arg)))

;; Scroll up w/o cursor
(global-set-key (kbd "M-P") 'scroll-down-line)

;; Scroll down with cursor
(global-set-key (kbd "M-n") (lambda (arg) (interactive "p") (next-line arg) (scroll-up-line arg)))

;; Scroll down w/o cursor
(global-set-key (kbd "M-N") 'scroll-up-line)

;; Goto line
(global-set-key (kbd "M-g") 'goto-line)

;; Go back a buffer
(global-set-key (kbd "C-x p") (lambda () (interactive) (other-window -1)))

;; Clean up tramp connections
(global-set-key (kbd "C-x C-t") 'tramp-cleanup-all-connections)

;; Paredit overrides C-j binding, add new binding
(global-set-key (kbd "C-S-j") 'eval-print-last-sexp)

;; No tabs for you!
(setq-default indent-tabs-mode nil)

;; Clean whitespace
(global-set-key (kbd "C-x M-w") (lambda () (interactive)
                                  (whitespace-cleanup)))

;; C indentation level to 2
(setq-default c-basic-offset 2)

;; Allow inserting of today's date
(require 'calendar)
(defun insdate-insert-current-date (&optional omit-day-of-week-p)
  "Insert today's date using the current locale.
  With a prefix argument, the date is inserted without the day of
  the week."
  (interactive "P*")
  (insert (calendar-date-string (calendar-current-date) nil
                                omit-day-of-week-p)))
(global-set-key "\C-x\M-d" `insdate-insert-current-date)

(require 'cc-mode)

(defun save-to-clipboard (start end)
  "Save region to clipboard through pbcopy"
  (interactive "r")
  (shell-command-on-region start end "pbcopy")
  (setq deactivate-mark t))

;; Copy to Mac OS X Clipboard
(global-set-key (kbd "M-W") 'save-to-clipboard)

(require 'nlinum)
(setq nlinum-format "%d ")
(global-nlinum-mode 1)
(column-number-mode 1)
(setq python-indent 2)
(setq rust-indent-unit 4)

(setq js-indent-level 2)

;; Disable auto-save and backups
(setq backup-inhibited t)
(setq auto-save-default nil)

;; Html mode hooks
(add-hook 'html-mode-hook
          (lambda ()
            (set (make-local-variable 'sgml-basic-offset) 2)))

;; ido-mode everywhere
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)
(setq ido-create-new-buffer 'prompt)

;; smex (ido-mode for M-x)
(require 'smex)
(global-set-key (kbd "M-x") 'smex)

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

;; Paredit Mode Keybindings
(require 'paredit)
(define-key paredit-mode-map (kbd "M-)") 'paredit-forward-slurp-sexp)
(define-key paredit-mode-map (kbd "M-(") 'paredit-forward-barf-sexp)
(define-key paredit-mode-map (kbd "M-{") 'paredit-backward-slurp-sexp)
(define-key paredit-mode-map (kbd "M-}") 'paredit-backward-barf-sexp)

(add-hook 'emacs-lisp-mode-hook       (lambda () (paredit-mode +1)))
(add-hook 'lisp-mode-hook             (lambda () (paredit-mode +1)))
(add-hook 'lisp-interaction-mode-hook (lambda () (paredit-mode +1)))
(add-hook 'scheme-mode-hook           (lambda () (paredit-mode +1)))
(add-hook 'clojure-mode-hook          (lambda () (paredit-mode +1)))

;; Ace Jump Mode
(require 'ace-jump-mode)
(define-key global-map (kbd "C-x j") 'ace-jump-mode)
(define-key global-map (kbd "C-x J") (lambda () (interactive) (ace-jump-mode 4)))

;; Hide welcome message
(setq inhibit-startup-message t)

;; Pylint and Flymake
(require 'flymake)

;;(exec-path-from-shell-initialize)
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

;; 80 character default fill width
(setq-default fill-column 80)

;; Prog-Mode hooks
(require 'whitespace)
(setq whitespace-line-column 80) ;; limit line length
(setq whitespace-style '(face lines-tail))

(add-hook 'prog-mode-hook 'whitespace-mode)

;; Turn off fringe mode because it doesn't work nicely in OS X
;; and also hide scroll bar
(defun osx-frame-fixes (&optional frame)
  (set-fringe-mode 0)
  (set-scroll-bar-mode nil))

(osx-frame-fixes)

(add-hook 'after-make-frame-functions 'osx-frame-fixes)

;; Settings for just Mac OS X Emacs with window
(when window-system
  (require 'solarized)
  (load-theme 'solarized-dark t)
  ;; hl-line-mode for line highlighting
  (global-hl-line-mode 1))

;; org mode hook
(add-hook
 'org-mode-hook
 (lambda ()
   ;; Turn off hl-line-mode in org-mode
   (global-hl-line-mode 0)))

;; Ghetto way to get the path properly set
(setenv "PATH" (shell-command-to-string "source ~/.zshrc && echo $PATH"))

;; Add puppet mode to .pp files
(add-to-list 'auto-mode-alist '("\\.pp\\'" . puppet-mode))

;; Add proto mode to .proto files
(add-to-list 'auto-mode-alist '("\\.proto\\'" . protobuf-mode))

;; Add bazel mode to BUILD files
(add-to-list 'auto-mode-alist '("BUILD\\'" . bazel-mode))

;; Add bazel mode to BUILD files
(add-to-list 'auto-mode-alist '("\\.bazel\\'" . bazel-mode))

;; Add bazel mode to WORKSPACE files
(add-to-list 'auto-mode-alist '("WORKSPACE\\'" . bazel-mode))

;; Add python mode to .tw files
(add-to-list 'auto-mode-alist '("\\.tw\\'" . python-mode))

;; Add markdown mode to .md files
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; Add tuareg mode to .ml .mli files
(add-to-list 'auto-mode-alist '("\\.ml\\'" . tuareg-mode))
(add-to-list 'auto-mode-alist '("\\.mli\\'" . tuareg-mode))

;; Set tuareg mode (ml/ocaml) default indent
(setq tuareg-default-indent 2)

;; Setup haskell-mode
(defun haskell-mode-hooks ()
  (turn-on-haskell-doc-mode)
  (turn-on-haskell-indentation)
  (haskell-indentation-mode t)
  (highlight-80+-mode))

(add-hook 'haskell-mode-hook 'haskell-mode-hooks)

;; (require 'racket)

(require 'multiple-cursors)

(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; Compile command
(global-set-key (kbd "M-c") (lambda () (interactive)
                              (compile compile-command)))

;; Disable bell
(setq ring-bell-function 'ignore)

;; rust-mode hooks
(defun rust-mode-hooks ()
  (set (make-local-variable 'compile-command)
       "cargo build"))

(add-hook 'rust-mode-hook 'rust-mode-hooks)

;; c++-mode hooks
(defun c++-mode-hooks ()
  (font-lock-add-keywords
   'c++-mode
   '(("\\<\\(alignof\\|alignas\\|constexpr\\|decltype\\|\
noexcept\\|nullptr\\|static_assert\\|thread_local\\|\
override\\|final\\)\\>" . font-lock-keyword-face)
     ("\\<\\(char[0-9]+_t\\)\\>" . font-lock-type-face))))

(add-hook 'c++-mode-hook 'c++-mode-hooks)

;; Don't open new files in a new frame/window
(setq ns-pop-up-frames 'nil)

;; Add /usr/loca/bin to exec-path
(setq exec-path (append exec-path '("/usr/local/bin")))

;; Add clang-format support
(load "/usr/local/Cellar/clang-format/10.0.1/share/clang/clang-format.el")
(global-set-key [C-M-tab] 'clang-format-region)
(global-set-key (kbd "C-x M-t") 'clang-format-buffer)
(global-set-key (kbd "C-M-w") 'clang-format-buffer)

(put 'narrow-to-region 'disabled nil)
