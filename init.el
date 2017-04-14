(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("org" . "http://orgmode.org/elpa/") t)
(package-initialize)

;; Add unmanaged load path
(add-to-list 'load-path "~/.emacs.d/unmanaged")
(add-to-list 'load-path "~/.emacs.d/unmanaged/thrift.el")
(add-to-list 'load-path "~/.emacs.d/unmanaged/highlight-80+.el")

;; Change default font to Space Mono
(set-frame-font "Space Mono-12")
;; Old default
;; (set-frame-font "Menlo-12")
(make-face 'fancy-comment-face)
(set-face-foreground 'fancy-comment-face "#586e75")
(set-face-font 'fancy-comment-face "Space Mono-12:italic")

;; Tramp handle 2fac auth
(setq
 tramp-password-prompt-regexp
 (concat
  "^.*"
  (regexp-opt
   '("passcode" "Passcode"
     "password" "Password") t)
  ".*:\0? *"))

;; Tramp use sshx
(setq tramp-default-method "sshx")

;; Tramp over mosh for OMGWTFBBQ goodness
;; -- Doesn't work, cause mosh is a remote terminal emulator
;; -- more than it's a ssh replacement. Maybe some day.
;; (add-to-list
;;  'tramp-methods
;;  '("mosh"
;;    (tramp-remote-shell "/bin/sh")
;;    (tramp-remote-shell-args ("-c"))
;;    (tramp-login-program "/usr/local/bin/mosh")
;;    (tramp-login-args (("%u@%h")))
;;    (tramp-async-args (()))
;;    (tramp-gw-args ())
;;    (tramp-default-port 22)))

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
    (buffer-move multiple-cursors yaml-mode tuareg solarized-theme smex racer pylint pretty-lambdada php-mode paredit nlinum markdown-mode magit lua-mode jinja2-mode ido-ubiquitous go-mode ghci-completion ghc geiser flymake-go flymake-cursor flymake flycheck-rust find-file-in-project fic-ext-mode d-mode cm-mode clang-format browse-kill-ring ack ace-jump-mode))))

;; super awesome project git grep from emacs
(require 'ack)
(global-set-key (kbd "M-F") 'ack)

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
(global-set-key (kbd "M-P") (lambda (arg) (interactive "p") (previous-line arg) (scroll-down-line arg)))

;; Scroll up w/o cursor
(global-set-key (kbd "M-p") 'scroll-down-line)

;; Scroll down with cursor
(global-set-key (kbd "M-N") (lambda (arg) (interactive "p") (next-line arg) (scroll-up-line arg)))

;; Scroll down w/o cursor
(global-set-key (kbd "M-n") 'scroll-up-line)

;; Goto line
(global-set-key (kbd "M-g") 'goto-line)

;; Go back a buffer
(global-set-key (kbd "C-x p") (lambda () (interactive) (other-window -1)))

;; Clean up tramp connections
(global-set-key (kbd "C-x C-t") 'tramp-cleanup-all-connections)

;; Paredit overrides C-j binding, add new binding
(global-set-key (kbd "C-S-j") 'eval-print-last-sexp)

;; Remap M-% to isearch-query-replace
(global-set-key (kbd "M-%") 'isearch-query-replace-regexp)

;; No tabs for you!
(setq-default indent-tabs-mode nil)

;; Clean whitespace
(global-set-key (kbd "C-x M-w") (lambda () (interactive)
                                  (whitespace-cleanup)))

;; Scala 2 Mode
;; https://github.com/hvesalai/scala-mode2
;; (add-to-list 'load-path "~/.emacs.d/scala-mode2/")
;; (require 'scala-mode)

;; C indentation level to 4
(setq-default c-basic-offset 4)

(require 'cc-mode)

(add-to-list 'c-default-style '(php-mode . "fb"))

(c-add-style "fb" '("k&r"
                    (c-basic-offset . 2)
                    (c-hanging-braces-alist
                     (defun-open after)
                     (substatement-open after))
                    (c-offsets-alist
                     (arglist-close . 0)
                     (arglist-intro . +))))

(defun basic-c-mode-hook ()
  "Basic C/C++ mode hook"
  (c-set-style "fb")
  (setq indent-tabs-mode nil)
)

(add-hook 'c-mode-hook (function (lambda () (basic-c-mode-hook))))
(add-hook 'c++-mode-hook (function (lambda () (basic-c-mode-hook))))

(add-hook 'php-mode-hook
          (function (lambda ()
                      (c-set-style "fb")
                      (setq indent-tabs-mode nil))))

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
(setq python-indent 4)
(setq rust-indent-unit 4)

(setq js-indent-level 2)

;; Disable auto-save and backups
(setq backup-inhibited t)
(setq auto-save-default nil)

;; Html mode hooks
(add-hook 'html-mode-hook
          (lambda ()
            (set (make-local-variable 'sgml-basic-offset) 2)))

;; Adding geiser mode
;; (load-file "~/lisp/geiser-0.2.1/elisp/geiser.el")
;; Adding quack mode after geiser
;; (load-file "~/lisp/quack-0.45/quack.el")
;; (require 'quack)

;; ido-mode everywhere
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)
(setq ido-create-new-buffer 'prompt)

;; smex (ido-mode for M-x)
(autoload 'smex "smex"
  "Smex is a M-x enhancement for Emacs, it provides a convenient interface to
your recently and most frequently used commands.")
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

;; Find-File-In-Project Settings
(require 'find-file-in-project)
(setq ffip-limit 2048)
(setq ffip-project-file ".git")
(setq ffip-patterns (append '("*.hamlpy" "*.sass" "*.coffee" "*.hs" "*.lhs" "*.hsc" "*.ocaml" "*.rkt" "*.scm" "*.proto" "*.java" "*.c" "*.h") ffip-patterns))
(global-set-key (kbd "C-c C-f") 'ffip)

;; Paredit Mode Keybindings
(require 'paredit)
(define-key paredit-mode-map (kbd "M-)") 'paredit-forward-slurp-sexp)
(define-key paredit-mode-map (kbd "M-(") 'paredit-forward-barf-sexp)
(define-key paredit-mode-map (kbd "M-{") 'paredit-backward-slurp-sexp)
(define-key paredit-mode-map (kbd "M-}") 'paredit-backward-barf-sexp)

(autoload 'paredit-mode "paredit"
  "Minor mode for pseudo-structurally editing Lisp code." t)
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

;; Add /usr/local/bin (homebrew) to exec-path
(add-to-list 'exec-path "/usr/local/bin")

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

;; Flymake-Cursor
(require 'flymake-cursor)
(add-hook 'python-mode-hook
          (lambda ()
            (flymake-mode t)
            (flymake-cursor-mode)
            (define-key python-mode-map (kbd "M-RET") 'flymake-cursor-show-errors-at-point-now)
            (define-key python-mode-map (kbd "M-E") 'flymake-display-err-menu-for-current-line)
            ))

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
(add-hook 'org-mode-hook
          (lambda ()
            (setq org-log-done 'time)
            (setq org-todo-keywords
                  '((sequence "TODO" "IN PROGRESS" "|" "DONE" "WONTFIX")))))

;; Prog-Mode hooks
(require 'highlight-80+)
(add-hook 'prog-mode-hook
          (lambda ()
            (fic-ext-mode t)
            (highlight-80+-mode)
            ;; Oscar 120 line length limit
            (setq highlight-80+-columns 120)
            (set (make-local-variable 'font-lock-comment-face)
                 'fancy-comment-face)
            ))


;; Set default fill-column to 80 to make fill-paragraph nice
(setq-default fill-column 80)

;; Turn off fringe mode because it doesn't work nicely in OS X
;; and also hide scroll bar
(defun osx-frame-fixes (&optional frame)
  (set-fringe-mode 0)
  (set-scroll-bar-mode nil))

(osx-frame-fixes)

(add-hook 'after-make-frame-functions 'osx-frame-fixes)

;; Settings for just Mac OS X Emacs with window
(when window-system
  (require 'solarized-theme)
  (load-theme 'solarized-dark 't)
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

;; Add pythong mode to BUCK files
(add-to-list 'auto-mode-alist '("BUCK\\'" . python-mode))

;; Add python mode to .tw files
(add-to-list 'auto-mode-alist '("\\.tw\\'" . python-mode))
;; Add python mode to .aurora files
(add-to-list 'auto-mode-alist '("\\.aurora\\'" . python-mode))
;; Add python mode to pants BUILD files
(add-to-list 'auto-mode-alist '("BUILD\\'" . python-mode))

;; Add markdown mode to .md files
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; Add tuareg mode to .ml .mli files
(add-to-list 'auto-mode-alist '("\\.ml\\'" . tuareg-mode))
(add-to-list 'auto-mode-alist '("\\.mli\\'" . tuareg-mode))

;; 
(require 'calendar)
(defun insdate-insert-current-date (&optional omit-day-of-week-p)
  "Insert today's date using the current locale.
  With a prefix argument, the date is inserted without the day of
  the week."
  (interactive "P*")
  (insert (calendar-date-string (calendar-current-date) nil
                                omit-day-of-week-p)))
(global-set-key (kbd  "C-x M-d") 'insdate-insert-current-date)

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


;; c++-mode hooks
(defun c++-mode-hooks ()
  (font-lock-add-keywords
   'c++-mode
   '(("\\<\\(alignof\\|alignas\\|constexpr\\|decltype\\|\
noexcept\\|nullptr\\|static_assert\\|thread_local\\|\
override\\|final\\)\\>" . font-lock-keyword-face)
     ("\\<\\(char[0-9]+_t\\)\\>" . font-lock-type-face)))
  (set (make-local-variable 'compile-command)
       "fbmake --fast dbg -j60")
  (highlight-80+-mode))

(add-hook 'c++-mode-hook 'c++-mode-hooks)

;; Don't open new files in a new frame/window
(setq ns-pop-up-frames 'nil)

;; FB C stuff from master.emacs
;;=========================================================
;;C, C++, Objective-C Indentation Style
;;========================================================
(require 'fb-objc)

(defconst fb-c-style
  '((c-basic-offset . 2)
    (c-cleanup-list . (scope-operator
                       brace-else-brace brace-elseif-brace brace-catch-brace
                       defun-close-semi list-close-comma))
    (c-hanging-braces-alist . (
                               (arglist-cont-nonempty after)
                               (brace-entry-open after)
                               (class-open after)
                               (defun-open after)
                               (extern-lang-open after)
                               (inexpr-class-open after)
                               (inline-open after)
                               (statement-cont after)
                               (substatement-open after)
                               (brace-list-open after)
                               (namespace-close)
                               (namespace-open)
                               (block-close . c-snug-do-while)
                               ))
    (c-offsets-alist . (
                        ;; indent for public: protected: private:
                        ;; c-basic-offset + access-label == 1 char indent
                        (access-label . -1)
                        (arglist-intro . +)
                        (case-label . +)
                        (arglist-close . c-lineup-close-paren)
                        (innamespace . 0)
                        (member-init-intro . ++)
                        (inher-intro . ++)
                        (objc-method-args-cont . fb-c-lineup-ObjC-method-args)
                        (objc-method-call-cont
                         (fb-c-lineup-ObjC-method-call-colons
                          fb-c-lineup-ObjC-method-call +))
                        )))
  "Facebook's C, C++, and Objective-C programming style"
)
(c-add-style "fb-c-style" fb-c-style)

;; (add-hook 'c-mode-common-hook
;;           (lambda ()
;;             (c-set-style "fb-c-style")
;;             (highlight-80+-mode t)
;;             ))
(put 'narrow-to-region 'disabled nil)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


(require 'clang-format)
;; Add /usr/local/bin to exec-path to find clang-format and other brew
;; installed binaries
(setq exec-path (append exec-path '("/usr/local/bin")))
(global-set-key (kbd "<C-M-tab>") 'clang-format-region)
(global-set-key (kbd "C-x M-t") 'clang-format-buffer)

