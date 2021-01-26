;;; Turn off mouse interface early in startup to avoid momentary display
(menu-bar-mode -1)
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(require 'cl)

;; (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
;; Define package repositories
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives '("gnu" . (concat proto "://elpa.gnu.org/packages/")))))


(add-to-list 'package-archives
             '("marmalade" . "https://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("tromey" . "https://tromey.com/elpa/") t)
(add-to-list 'package-archives
             '("gnu" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives
             '("org" . "https://orgmode.org/elpa/") t)

(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

(use-package better-defaults
  :ensure t)

(use-package diminish
  :ensure t)

(add-to-list 'load-path "./vendor")

;; Borrowed from https://sam217pa.github.io/2016/09/02/how-to-build-your-own-spacemacs/
(setq delete-old-versions -1)
(setq version-control t)
(setq vc-make-backup-files t)
(setq backup-directory-alist `(("." . "~/.emacs.d/backups")) ) ; which directory to put backups file
(setq vc-follow-symlinks t )				       ; don't ask for confirmation when opening symlinked file
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)) ) ;transform backups file name
(setq inhibit-startup-screen t )	; inhibit useless and old-school startup screen
(setq ring-bell-function 'ignore )	; silent bell when you make a mistake
(setq coding-system-for-read 'utf-8 )	; use utf-8 by default
(setq coding-system-for-write 'utf-8 )
(setq default-fill-column 80)


(defun run-server ()
  "Runs emacs server if it is not running"
  (require 'server)
  (unless (server-running-p)
    (server-start)))

(run-server)

(use-package restart-emacs
  :ensure t)


;; Ask before quiting
(setq-default confirm-kill-emacs 'yes-or-no-p)

(use-package flycheck
  :ensure t
  :diminish 'flycheck-mode
  :config
  (global-set-key (kbd "C-c ! v") 'flycheck-verify-setup)
  (add-hook 'after-init-hook #'global-flycheck-mode)
  (add-hook 'after-init-hook
            (lambda ()
              (flycheck-add-mode 'ruby-rubocop 'ruby-mode)))
  (setq-default flycheck-temp-prefix ".flycheck")
  ;; disable jshint snce we prefer eslint checking
  (setq-default flycheck-disabled-checkers
                (append flycheck-disabled-checkers
                        '(javascript-jshint))))

;;; Some Bascis
(use-package exec-path-from-shell
  :ensure t
  :init
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)
    (exec-path-from-shell-copy-envs
     '("PATH" "ANDROID_HOME"))))

;; Mac key admustments
(setq mac-option-modifier 'control)
(setq mac-command-modifier 'meta)

(global-set-key (kbd "C-h") 'help)
;; join line to next line
(defun concat-lines ()
  (interactive)
  (join-line -1))

(global-set-key (kbd "C-j") 'concat-lines)

(use-package window-numbering
  :ensure t
  :config
  (window-numbering-mode 1))

(use-package windmove
  :ensure t
  :config
  (when (fboundp 'windmove-default-keybindings)
    (windmove-default-keybindings)))

;; (global-unset-key (kbd "C-o"))

(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :config
  (add-hook 'after-init-hook 'global-undo-tree-mode))

(defun set-relative-lines ()
  (interactive)
  (setq-default display-line-numbers 'relative))

(defun set-absolute-lines ()
  (interactive)
  (setq-default display-line-numbers t))

(defun toggle-relative-lines ()
  (interactive)
  (if (eq display-line-numbers 'relative)
      (setq display-line-numbers t)
    (setq display-line-numbers 'relative)))

(global-set-key (kbd "C-c RET RET") 'save-buffer)

(defun jas/duplicate-line-below ()
  (interactive)
  (save-excursion
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (next-line 1)
  (yank))
  (next-line 1))

(global-set-key (kbd "C-c l d") 'jas/duplicate-line-below)

;;; goto last change
(use-package goto-last-change
  :ensure t
  :init
  (global-set-key (kbd "C-c l c") 'goto-last-change))

;;; Avy mode (vim easymotion-esque)
(use-package avy
  :ensure t)
(global-set-key (kbd "C-\\") 'avy-goto-char-2)

(use-package uuidgen
  :ensure t)

;; Which key to show keybindings for partially completed
;; combinations
(use-package which-key
  :ensure t
  :diminish which-key-mode
  :init
  (which-key-mode))

;; Projectile
;; projectile everywhere!
(use-package projectile
  :ensure t
  :diminish projectile-mode
  :config
  (add-hook 'after-init-hook (projectile-mode)))

(use-package counsel-projectile
  :ensure t
  :init
  (counsel-projectile-mode t)
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(use-package ripgrep
  :ensure t)

(use-package general
  :ensure t
  :config
  (general-define-key
   :prefix "C-c"
   "C-c" 'recompile
   "a" 'org-agenda
   "b" '(:ignore t :which-key "buffer")
   "b b" 'ibuffer
   "c" '(:ignore t :which-key "company")
   "c c" 'company-complete
   "g" '(:ignore t :which-key "git")
   "g" '(:ignore t :which-key "git")
   "g s" 'magit-status
   "i" '(:ignore t :which-key "insert")
   "i r" 'xah-insert-random-number
   "i n" 'jas/insert-note
   "i t" 'jas/insert-todo
   "j" '(:ingore t :which-key "jump")
   "j j" 'dumb-jump-go
   "j l" 'avy-goto-line
   "j w" 'avy-goto-char-2
;   "l T i" 'lsp-ui-imenu
   "o" '(:ignore t :which-key "org")
   "o c" 'counsel-org-capture
   "o p" 'jas/go-to-personal-org-file
   "o w" 'jas/go-to-work-org-file
   "p" '(:ignore t :which-key "project")
   "p p" 'projectile-switch-project
   "p f" 'projectile-find-file
   "p s" 'projectile-ripgrep
   "s" '(:ignore t :which-key "shell")
   "s e" 'eshell
   "s s" 'multi-term-dedicated-toggle
   "s n" 'multi-term
   "u" '(:ignore t :which-key "UI")
   "u c" 'counsel-load-theme
   "u b" 'load-blue
   "u L" 'load-very-light
   "u l" 'load-light
   "u d" 'load-dark
   "u D" 'load-very-dark
   "u a" 'load-acme
   "u m" 'load-basic
   "u t" 'toggle-transparency
   "u n" 'global-display-line-numbers-mode
   ";" '(:ignore t :which-key "commenting")
   "; r" 'comment-region
   "; u" 'uncomment-region))

(use-package prescient
  :ensure t)

(use-package ivy-prescient
  :ensure t)

(use-package company-prescient
  :ensure t)

(use-package ivy
  :ensure t
  :diminish ivy-mode
  :init
  (ivy-mode t)
  (ivy-prescient-mode t)
  (setq ivy-re-builders-alist
      '((swiper . ivy--regex-plus)
        (t      . ivy--regex-fuzzy))))

(use-package swiper
  :ensure t
  :config
  (global-set-key (kbd "C-s") 'swiper)
  )

(use-package counsel
  :ensure t
  :config
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (global-set-key (kbd "C-x b") 'counsel-switch-buffer)
  (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history))

(use-package counsel-projectile
  :ensure t
  :init
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (counsel-projectile-mode t))

(use-package ag
  :ensure t)

(global-set-key (kbd "C-x C-b") 'ibuffer)

;; All the Icons
(use-package all-the-icons
  :ensure t)

;; Magit
(use-package magit
  :ensure t)
(global-set-key (kbd "C-x g") 'magit-status)

;; Highlights matching parenthesis
(show-paren-mode 1)

(electric-pair-mode 1)

;; Lisp-friendly hippie expand
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
         try-expand-dabbrev-all-buffers
         try-expand-dabbrev-from-kill
         try-complete-lisp-symbol-partially
         try-complete-lisp-symbol))

(setq make-backup-files nil) ; stop creating backup~ files
(setq auto-save-default nil) ; stop creating #autosave# files
(setq create-lockfiles nil)

;; Show tabs as 4 spaces
(setq tab-width 4)

(add-hook 'after-init-hook
          (lambda ()
            (define-key global-map (kbd "<C-s-up>") 'move-line-up)
            (define-key global-map  (kbd "<C-s-down>") 'move-line-down)))

;; Use subword mode
(global-subword-mode)
(diminish 'subword-mode)

(use-package fish-mode
  :ensure t)

;; Fix Org Mode syntax stuff
(setq org-src-fontify-natively t)

(defun my/setup-shells ()
  (font-lock-mode -1)
  (make-local-variable 'font-lock-function)
  (setq font-lock-function (lambda (_) nil))
  (add-hook 'comint-preoutput-filter-functions 'xterm-color-filter nil t))

(setq compilation-environment '("TERM=xterm-256color"))

(defun my/advice-compilation-filter (f proc string)
  (funcall f proc (xterm-color-filter string)))

(advice-add 'compilation-filter :around #'my/advice-compilation-filter)

(use-package xterm-color
  :ensure t
  :init
  (add-hook 'shell-mode-hook #'my/setup-shells)
  (add-hook 'eshell-before-prompt-hook
            (lambda ()
              (setq xterm-color-preserve-properties t)))
  (setenv "TERM" "xterm-256color"))

(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region compilation-filter-start (point))
  (toggle-read-only))

(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

(use-package company
  :ensure t
  :diminish company-mode
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  (global-set-key (kbd "C-.") 'company-files)
  (setq company-idle-delay 0.2)
  (company-prescient-mode t))

(global-set-key (kbd "C-'") 'company-complete)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-t") 'transpose-chars)

;;; LSP
;(use-package lsp-mode
;  :ensure t
;  :init
;  (setq lsp-keymap-prefix "C-c l")
;  :hook ((ruby-mode . lsp)
;		 (rspec-mode . lsp)
;		 (go-mode . lsp-deferred)
;		 (java-mode . lsp)
;		 (js-mode . lsp)
;		 (php-mode . lsp)
;		 (elm-mode . lsp)
;		 (typescript-mode . lsp)
;		 (rjsx-mode . lsp)
;		 (web-mode . lsp)
;		 (c-mode . lsp)
;		 (c++-mode . lsp)
;		 (lsp-mode . lsp-enable-which-key-integration))
;   :commands lsp)

;; optionally
;(use-package lsp-ui
;  :ensure t
;  :commands lsp-ui-mode)
;; if you are ivy user
;(use-package lsp-ivy :ensure t :commands lsp-ivy-workspace-symbol)
;(use-package lsp-treemacs :ensure t :commands lsp-treemacs-errors-list)
;(use-package company-lsp :ensure t)
;; optionally if you want to use debugger
;(use-package dap-mode :ensure t)
;;; End LSP

;; Enable paredit for Clojure
(use-package paredit
  :ensure t
  :config
  ;; Use Paredit to allow slurping
  (global-set-key (kbd "C-)") 'paredit-forward-slurp-sexp)
  (define-key paredit-mode-map (kbd "C-j") 'concat-lines)
  (add-hook 'emacs-lisp-mode-hook #'paredit-mode))

(use-package cider
  :ensure t)

;; Expand Region
(use-package expand-region
             :ensure t)
(global-set-key (kbd "C-]") 'er/expand-region)


(use-package wrap-region
  :ensure t
  :diminish wrap-region-mode
  :init
  (wrap-region-global-mode))

;; Case sensitive company mode
(setq company-dabbrev-downcase nil)

;; Snippets
(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :init
  (yas-global-mode 1)
  ;; global key to get suggestions for snippets
  (global-set-key (kbd "C-x y") 'company-yasnippet)

  :config
  ;(define-key yas-minor-mode-map [(tab)] nil)
  ;(define-key yas-minor-mode-map (kbd "TAB") nil)
  )

(with-eval-after-load 'company
                      '(add-to-list 'company-backends 'company-yasnippet)
                      '(add-to-list 'company-backends 'company-web)
                      '(add-to-list 'company-backends 'company-css)
                      '(add-to-list 'company-backends 'company-go)
                      '(add-to-list 'company-backends 'company-lua)
					  '(add-to-list 'company-backends 'company-php)
                      ;; '(add-to-list 'company-backends 'company-irony)
                      '(add-to-list 'company-backends 'company-racer)
                      '(add-to-list 'company-backends 'company-elm)
                      '(add-to-list 'company-backends 'company-omnisharp))

;; Use Key Chords
(use-package key-chord
             :ensure t)

(key-chord-mode 1)

;; Multiple Cursors
(use-package multiple-cursors
             :ensure t)
(multiple-cursors-mode)
(global-set-key (kbd "C-c C-l") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C->") 'mc/mark-next-like-this-word)
(global-set-key (kbd "C-c C-<") 'mc/mark-previous-like-this-word)

;; Rainbow Mode hooks
(use-package rainbow-delimiters
  :ensure t)
(add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)

(use-package racket-mode
  :ensure t)

(add-hook 'racket-mode-hook #'rainbow-delimiters-mode)

;; Interactive search key bindings. By default, C-s runs
;; isearch-forward, so this swaps the bindings.
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

;; Keybinding for toggling window split direction
(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
               (if (= (car this-win-edges)
                      (car (window-edges (next-window))))
                   'split-window-horizontally
                   'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

(global-set-key (kbd "C-x |") 'toggle-window-split)
;; Don't use hard tabs
(setq-default indent-tabs-mode nil)

;; When you visit a file, point goes to the last place where it
;; was when you previously visited the same file.
;; http://www.emacswiki.org/emacs/SavePlace
(use-package saveplace
  :ensure t)

(setq-default save-place t)

;; keep track of saved places in ~/.emacs.d/places
(setq save-place-file (concat user-emacs-directory "places"))

(add-hook 'before-save-hook #'delete-trailing-whitespace)

;; comments
(defun toggle-comment-on-line ()
  "comment or uncomment current line"
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))
(global-set-key (kbd "C-;") 'toggle-comment-on-line)

;; fix weird os x kill error
(defun ns-get-pasteboard ()
  "Returns the value of the pasteboard, or nil for unsupported formats."
  (condition-case nil
                  (ns-get-selection-internal 'CLIPBOARD)
                  (quit nil)))

(setq electric-indent-mode 1)
;;; End Basics


;;; Javascript
(use-package add-node-modules-path
  :ensure t)

(use-package pug-mode
  :ensure t)

(use-package web-mode
  :ensure t
  :init
  (setq web-mode-script-padding 0)
  (setq web-mode-style-padding 0)
  (setq web-mode-markup-indent-offset 2)
  (setq-default indent-tabs-mode nil)
  (add-hook 'web-mode-hook #'add-node-modules-path)
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  :config
  (set-face-attribute 'web-mode-html-tag-bracket-face nil :foreground "Grey")
  (setq indent-tabs-mode nil))

(use-package prettier
  :ensure t
  :diminish prettier-mode
  :init
  (add-hook 'js-mode-hook #'prettier-mode)
  (add-hook 'web-mode-hook #'prettier-mode)
  (add-hook 'typescript-mode-hook #'prettier-mode))

(use-package rjsx-mode
  :ensure t
  :init
  (add-hook 'rjsx-mode-hook #'add-node-modules-path)
  (flycheck-add-mode 'javascript-eslint 'rjsx-mode)
  (flycheck-add-mode 'javascript-eslint 'tide-mode)
  (add-hook 'rjsx-mode-hook #'setup-tide-mode))

(eval-after-load 'js2-mode
  '(add-hook 'js2-mode-hook #'add-node-modules-path))

(defun codefalling/reset-eslint-rc ()
  (let ((rc-path (if (projectile-project-p)
                     (concat (projectile-project-root) ".eslintrc"))))
    (if (file-exists-p rc-path)
        (progn
          (message rc-path)
          (setq flycheck-eslintrc rc-path)))))


(defun my/set-web-mode-indent ()
  (interactive)
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  ;; Don't let web mode try to line things up strangely in js
  (add-to-list 'web-mode-indentation-params '("lineup-args" . nil))
  (add-to-list 'web-mode-indentation-params '("lineup-calls" . nil))
  (add-to-list 'web-mode-indentation-params '("lineup-concats" . nil))
  (add-to-list 'web-mode-indentation-params '("lineup-ternary" . nil)))

(add-hook 'web-mode-hook 'my/set-web-mode-indent)

(setq css-indent-offset 2)

;; always use jsx mode for JS
(setq web-mode-content-types-alist '(("jsx"  . "\\.js[x]?\\'")))

;; Fix rjsx-mode indentation
(defun js-jsx-indent-line-align-closing-bracket ()
  "Workaround sgml-mode and align closing bracket with opening bracket"
  (save-excursion
    (beginning-of-line)
    (when (looking-at-p "^ +\/?> *$")
      (delete-char sgml-basic-offset))))
(advice-add #'js-jsx-indent-line :after #'js-jsx-indent-line-align-closing-bracket)


(use-package json-mode
  :ensure t)

(add-to-list 'auto-mode-alist '("\\.json\\'" . json-mode))

(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . my/setup-erb))
(add-to-list 'auto-mode-alist '("\\.html\\.erb\\'" . my/setup-erb))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.vue\\'" . web-mode))


;;; JS-mode and derivatives
;; Turn off js2 mode errors & warnings (we lean on eslint/standard)
;; this also affects rjsx mode (yay!)
(setq js2-mode-show-parse-errors nil)
(setq js2-mode-show-strict-warnings nil)
(setq js2-basic-offset 2)
(add-hook 'js2-mode-hook #'js2-refactor-mode)
(add-hook 'js2-mode-hook (lambda ()
                           (local-set-key (kbd "C-c , s") 'mocha-test-at-point)
                           ;; (evil-leader/set-key "t" 'mocha-test-at-point)
                           (local-set-key (kbd "C-c , v") 'mocha-test-file)
                           ;; (evil-leader/set-key "T" 'mocha-test-file)
                           ))

(define-key js2-mode-map (kbd "C-k") #'js2r-kill)

;; js-mode (which js2 is based on) binds "M-." which conflicts with xref, so
;; unbind it.
(define-key js-mode-map (kbd "M-.") nil)

(add-hook 'js2-mode-hook (lambda ()
                           (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))

;; Turn off aggressive-indent-mode for jsx until I figure out how to fix it
(add-hook 'rjsx-mode-hook (lambda ()
                            ;;(flow-minor-mode t)
                            ;; See https://github.com/CestDiego/emmet-mode/commit/3f2904196e856d31b9c95794d2682c4c7365db23
                            (eldoc-mode -1)
                            ;; See https://github.com/CestDiego/emmet-mode/commit/3f2904196e856d31b9c95794d2682c4c7365db23
                            (setq-local emmet-expand-jsx-className? t)
                            ;; Enable js-mode
                            ;;(yas-activate-extra-mode 'js-mode)
                            ;; Force jsx content type
                            ;;(web-mode-set-content-type "jsx")
                            ;;(aggressive-indent-mode -1)
                            ;;(global-aggressive-indent-mode -1)
                            ;; Don't auto-quote attribute values
                            (local-set-key (kbd "C-c t t") 'mocha-test-at-point)
                            (local-set-key (kbd "C-c t f") 'mocha-test-file)
                            (setq-local web-mode-enable-auto-quoting nil)))

(add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . rjsx-mode))
(add-to-list 'auto-mode-alist '("\\.rjsx.js\\'" . rjsx-mode))
(add-to-list 'auto-mode-alist '("\\.test.js\\'" . rjsx-mode))
(add-to-list 'auto-mode-alist '("\\.jest.js\\'" . rjsx-mode))
(add-to-list 'auto-mode-alist '("\\index.android.js\\'" . rjsx-mode))
(add-to-list 'auto-mode-alist '("\\index.ios.js\\'" . rjsx-mode))
(add-to-list 'magic-mode-alist '("/\\*\\* @jsx React\\.DOM \\*/" . rjsx-mode))
(add-to-list 'magic-mode-alist '("^\\/\\/ @flow" . rjsx-mode))

;; Vue Support
(use-package vue-mode
  :ensure t
  :init
  (add-hook 'vue-mode-hook #'prettier-mode)
  :config
  (setq mmm-submode-decoration-level 0)
  (add-to-list 'auto-mode-alist '("\\.vue\\'" . vue-mode))
)

(use-package svelte-mode
  :ensure t)

(use-package scss-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))
  (setq scss-compile-at-save nil)
  (add-hook 'scss-mode-hook (lambda ()
                              (set (make-local-variable 'company-backends) '(company-css)))))

(add-hook 'js2-mode-hook #'js2-imenu-extras-mode)

(use-package js2-refactor
  :ensure t
  :init
  (js2r-add-keybindings-with-prefix "C-c C-m"))

(use-package xref-js2
  :ensure t)

;; Add keybindings to run jest tests
(use-package mocha
  :ensure t)

;;; Jest (JS)

;;; Setup for using Mocha el to run Jest tests

(use-package mocha
  :ensure t
  :commands (mocha-test-project
             mocha-debug-project
             mocha-test-file
             mocha-debug-file
             mocha-test-at-point
             mocha-debug-at-point)
  :config
  ;; Clear up stray ansi escape sequences.
  (defvar jj*--mocha-ansi-escape-sequences
    ;; https://emacs.stackexchange.com/questions/18457/stripping-stray-ansi-escape-sequences-from-eshell
    (rx (or
         "\^\[\[[0-9]+[a-z]"
         "\^\[\[1A"
         "[999D")))

  (defun jj*--mocha-compilation-filter ()
    "Filter function for compilation output."
    (ansi-color-apply-on-region compilation-filter-start (point-max))
    (save-excursion
      (goto-char compilation-filter-start)
      (while (re-search-forward jj*--mocha-ansi-escape-sequences nil t)
        (replace-match ""))))

  (advice-add 'mocha-compilation-filter :override 'jj*--mocha-compilation-filter)

  ;; https://github.com/scottaj/mocha.el/issues/3
  (defcustom mocha-jest-command "node_modules/jest/bin/jest.js --colors --config=./jest.config.json"
    "The path to the jest command to run."
    :type 'string
    :group 'mocha)

  (defun mocha-generate-command--jest-command (debug &optional filename testname)
    "Generate a command to run the test suite with jest.
                If DEBUG is true, then make this a debug command.
                If FILENAME is specified run just that file otherwise run
                MOCHA-PROJECT-TEST-DIRECTORY.
                IF TESTNAME is specified run jest with a pattern for just that test."
    (let ((target (if testname (concat " --testNamePattern \"" testname "\"") ""))
          (path (if (or filename mocha-project-test-directory)
                    (concat " --testPathPattern \""
                            (if filename filename mocha-project-test-directory)
                            "\"")
                  ""))
          (node-command
           (concat mocha-which-node
                   (if debug (concat " --debug=" mocha-debug-port) ""))))
      (concat node-command " "
              mocha-jest-command
              target
              path)))

  (advice-add 'mocha-generate-command
              :override 'mocha-generate-command--jest-command))
;;; End Javascript

;;; Typescript
(use-package tide
  :ensure t
  :init
  (add-hook 'tide-mode-hook 'add-node-modules-path)
  :config
  ;; aligns annotation to the right hand side
  (setq company-tooltip-align-annotations t)
  (setq typescript-indent-level 2)
  (add-hook 'typescript-mode-hook #'setup-tide-mode)
  (flycheck-add-mode 'javascript-eslint 'typescript-mode)
  (flycheck-add-next-checker 'javascript-eslint 'jsx-tide 'append)
  (flycheck-add-next-checker 'javascript-eslint 'typescript-tide 'append)
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.routes.ts\\'" . typescript-mode))
  (add-to-list 'auto-mode-alist '("\\.module.ts\\'" . typescript-mode))
  (add-hook 'web-mode-hook
            (lambda ()
              (when (string-equal "tsx" (file-name-extension buffer-file-name))
                (setup-tide-mode))))
  (add-to-list 'magic-mode-alist '("\\.tsx\\'" . tide-mode))
  (add-to-list 'magic-mode-alist '("\\.ts\\'" . tide-mode))
  (add-to-list 'magic-mode-alist '("\\.routes.ts\\'" . tide-mode))
  (add-to-list 'magic-mode-alist '("\\.module.ts\\'" . tide-mode))

  ;; enable typescript-tslint checker
  ;; (flycheck-add-mode 'typescript-tslint 'rjsx-mode))
  )

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1))

;;; End Typescript


;;; Elm
(use-package elm-mode
  :ensure t)
;;; End Elm

;;; Java
(use-package meghanada
  :ensure t
  :init
  (add-hook 'java-mode-hook
			(lambda ()
			  ;; meghanada-mode on
			  (meghanada-mode t)
			  (flycheck-mode +1)
			  (setq c-basic-offset 2)
			  ;; use code format
			  (add-hook 'before-save-hook 'meghanada-code-beautify-before-save)))
  (cond
   ((eq system-type 'windows-nt)
    (setq meghanada-java-path (expand-file-name "bin/java.exe" (getenv "JAVA_HOME")))
    (setq meghanada-maven-path "mvn.cmd"))
   (t
    (setq meghanada-java-path "java")
    (setq meghanada-maven-path "mvn"))))
;;; End Java

(use-package graphql-mode
  :ensure t)

(use-package yaml-mode
  :ensure t)

(use-package haml-mode
  :ensure t)

(use-package dumb-jump
  :ensure t
  :init
  (dumb-jump-mode t)
  :bind (("C-c j o" . dumb-jump-go-other-window)
         ("C-c j j" . dumb-jump-go)
         ("C-c j b" . dumb-jump-back)
         ("C-c j i" . dumb-jump-go-prompt)
         ("C-c j x" . dumb-jump-go-prefer-external)
         ("C-c j z" . dumb-jump-go-prefer-external-other-window))
  :config (setq dumb-jump-selector 'ivy))

;;; Ruby
(use-package ruby-end
  :diminish ruby-end-mode
  :ensure t)

(use-package ruby-test-mode
  :diminish ruby-test-mode
  :ensure t)

(use-package inf-ruby
  :ensure t)

(use-package rspec-mode
  :ensure t
  :init
  (eval-after-load 'rspec-mode
    '(rspec-install-snippets))
  (setq rspec-use-spring-when-possible nil)
  (add-hook 'after-init-hook 'inf-ruby-switch-setup)
  (add-hook 'ruby-mode-hook 'rspec-mode)
  ;; use bundle exec for rubocop
  (add-hook 'ruby-mode-hook
			(lambda ()
			  (setq-local flycheck-command-wrapper-function
						  (lambda (command) (append '("bundle" "exec") command)))))
  :config
  ;; (evil-leader/set-key (kbd ", t") 'rspec-verify-single)
  ;; (evil-leader/set-key (kbd ", T") 'rspec-verify)
  )

(use-package minitest
  :ensure t)


(use-package enh-ruby-mode
  :ensure t)

(use-package ruby-refactor
  :ensure t
  :diminish 'ruby-refactor-mode
  :init
  (add-hook 'ruby-mode-hook #'ruby-refactor-mode-launch))

(use-package inf-ruby
  :ensure t
  :init
  (add-hook 'after-init-hook 'inf-ruby-switch-setup))

(use-package rubocopfmt
  :ensure t
  :diminish 'rubocopfmt-mode
  :init
  (add-hook 'ruby-mode-hook #'rubocopfmt-mode))

(use-package rbenv
  :ensure t
  :init
  (global-rbenv-mode))

(use-package erblint
  :ensure t)

(use-package projectile-rails
  :ensure t
  :init
  (projectile-rails-global-mode)
  (define-key projectile-rails-mode-map (kbd "C-c r") 'projectile-rails-command-map))

(add-hook 'ruby-mode-hook (lambda ()
                            (progn
                              (ruby-end-mode)
                              (ruby-test-mode)
                              )))

(defun my/setup-erb ()
  (interactive)
  (web-mode)
  (flycheck-mode -1)
  (setq indent-tabs-mode nil)
  (prettier-mode -1))
;;; End Ruby

;;; Php
(use-package company-php
  :ensure t)

(use-package phpunit
  :ensure t)
;;; Php


;;; Python
(use-package elpy
  :ensure t
  :config
  (setq python-shell-interpreter "python"
      python-shell-interpreter-args "-i"))
;;; End Python

;;; Coffeescript
(use-package coffee-mode
  :ensure t)
;;; End Coffeescript


;;; Crystal
(use-package crystal-mode
  :ensure t
  :init
  (add-hook 'crystal-mode-hook 'ruby-end-mode))
;;; End Crystal

;;; Nim
(use-package nim-mode
  :ensure t)
;;; End Nim

;;; Odin
(load "~/.emacs.d/vendor/odin-mode.el")
(require 'odin-mode)
(load "~/.emacs.d/vendor/flycheck-odin.el")
(require 'flycheck-odin)
(add-to-list 'magic-mode-alist '("\\.odin\\'") 'odin-mode)
(add-hook 'odin-mode #'flycheck-mode)
(add-hook 'after-init-hook (lambda ()
                             (flycheck-odin-setup)))
;;; End Odin

;;; Lua
(use-package flymake-lua
  :ensure t)

(use-package luarocks
  :ensure t)

(use-package company-lua
  :ensure t)

(use-package lua-mode
  :ensure t
  :init
  (add-hook 'lua-mode-hook #'flymake-mode-on))
;;; End Lua


;;; Lisp
;; (when (load (expand-file-name "~/quicklisp/slime-helper.el"))
  ;; (setq inferior-lisp-program "/home/linuxbrew/.linuxbrew/bin/sbcl"))
;;; End Lisp


;; Scheme
;; (use-package geiser
;;   :ensure t
;;   :init
;;   (set-variable 'geiser-chicken-binary "/home/linuxbrew/.linuxbrew/bin/csi"))

;;; Rust
;; (use-package company-racer
;;   :ensure t)

;; (use-package flycheck-rust
;;   :ensure t
;;   :init
;;   (with-eval-after-load 'rust-mode
;; 	(add-hook 'flycheck-mode-hook #'flycheck-rust-setup)))

;; (use-package rust-mode
;;   :ensure t
;;   :config
;;   (local-set-key (kbd "C-c C-c") 'recompile))


;;; Haxe
;; (use-package battle-haxe
  ;; :ensure t)
;;; End Haxe

;; debugging
(add-hook 'gdb-mode-hook
          (lambda ()
            (gdb-many-windows)))
;;; End Rust

;;; OrgMode
(use-package org
  :ensure t
  :config
  (setq org-agenda-files (list "~/org/work.org"
							   "~/org/personal.org"))
  (setq org-capture-templates
      '(("W" "Work todo" entry (file+headline "~/org/work.org" "Tasks")
         "* TODO %?\n  %i\n  %a")
		("P" "Personal todo" entry (file+headline "~/org/personal.org" "Tasks")
         "* TODO %?\n  %i\n  %a")
        ("w" "Work Notes" entry (file+datetree "~/org/work.org")
         "* %?\nEntered on %U\n  %i\n  %a")
		("p" "Personal Notes" entry (file+datetree "~/org/personal.org")
         "* %?\nEntered on %U\n  %i\n  %a"))))

(use-package org-bullets
  :ensure t)
(add-hook 'org-mode-hook (lambda ()
                           (org-bullets-mode 1)))


(defun jas/go-to-file-in-split (filepath)
  "Open file in split.  FILEPATH is an absolute path to file."
  (interactive)
  (split-window-right)
  (windmove-right)
  (find-file filepath))

(defun jas/go-to-work-org-file ()
  "Edit my work org file."
  (interactive)
  (jas/go-to-file-in-split "~/org/work.org"))

(defun jas/go-to-personal-org-file ()
  "Edit my personal org file."
  (interactive)
  (jas/go-to-file-in-split "~/org/personal.org"))

(use-package htmlize
  :ensure t)

;;; End OrgMode

;;; Bible (ESV)
(load-file "~/.emacs.d/vendor/esv-mode/esv.el")
(require 'esv)
(add-hook 'text-mode-hook 'turn-on-esv-mode)
;;; End Bible


;;; Utils
(defun xah-insert-random-number (*n)
  "Insert *N random digits.
*n default to 5.
Call `universal-argument' before for different count.
URL `http://ergoemacs.org/emacs/elisp_insert_random_number_string.html'
Version 2016-01-12"
  (interactive "P")
  (let ((-charset "1234567890" )
        (-baseCount 10))
    (dotimes (-i (if (numberp *n) (abs *n) 5 ))
      (insert (elt -charset (random -baseCount))))))


(defun jas/insert-todo ()
  "Insert Todo comment."
  (interactive)
    (insert "TODO(jsral): ")
    (toggle-comment-on-line))

(defun jas/insert-note ()
  "Insert Note comment."
  (interactive)
  (insert "NOTE(jsral): ")
  (toggle-comment-on-line))
;;; End Utils

;;; UI
;; Turn off the menu bar at the top of each frame because it's distracting
(menu-bar-mode -1)
(tool-bar-mode -1)
(global-hl-line-mode -1)

;; (set-frame-parameter (selected-frame) 'alpha '(98 . 50))
;; (add-to-list 'default-frame-alist '(alpha . (98 . 50)))
(defun toggle-transparency ()
  (interactive)
  (let ((alpha (frame-parameter nil 'alpha)))
    (set-frame-parameter
     nil 'alpha
     (if (eql (cond ((numberp alpha) alpha)
                    ((numberp (cdr alpha)) (cdr alpha))
              ((numberp (cadr alpha)) (cadr alpha)))
         100)
     '(98 . 50) '(100 . 100)))))
;; Customize mode-line
(setq mode-line-format
      (list
        " "
        mode-line-mule-info
        mode-line-modified
        mode-line-frame-identification
        mode-line-buffer-identification
        " "
        mode-line-position
        vc-mode
        " "
        mode-line-modes))

;; Set frame background to dark for terminal mode
;; (setq frame-background-mode 'dark)

(setq system-uses-terminfo nil)
(prefer-coding-system 'utf-8)

(add-to-list 'comint-output-filter-functions 'ansi-color-process-output)
(ansi-color-for-comint-mode-on)

(use-package multi-term
  :ensure t
  :init
  (if (memq window-system '(ns win32))
	  (setq multi-term-program "ps.exe")
	(setq multi-term-program "/home/linuxbrew/.linuxbrew/bin/fish")
	))

;; (use-package vterm
  ;; :ensure t)
;; Show time on status bar
(display-time-mode 1)

;; Show line numbers if activated manually
;; (setq-default display-line-numbers-type 'absolute)
;; (add-hook 'prog-mode-hook 'display-line-numbers-mode)

(setq linum-format " %d ")

(use-package linum-relative
  :ensure t)

(setq linum-relative-current-symbol "")

;; (global-linum-mode)
;; Don't show native OS scroll bars for buffers because they're redundant
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

(use-package sublime-themes
  :defer t
  :ensure t)

(use-package base16-theme
  :defer t
  :ensure t)

(use-package gruvbox-theme
  :defer t
  :ensure t)

(use-package color-theme-sanityinc-solarized
  :defer t
  :ensure t)

(use-package color-theme-sanityinc-tomorrow
  :defer t
  :ensure t)


(defun jas/reset-ansi-colors (&optional theme)
  "Undo damage caused by some themes"
  (interactive)
  (setq ansi-color-vector [term term-color-black term-color-red term-color-green term-color-yellow term-color-blue term-color-magenta term-color-cyan term-color-white])
  (setq ansi-term-color-vector [term term-color-black term-color-red term-color-green term-color-yellow term-color-blue term-color-magenta term-color-cyan term-color-white]))

(defadvice load-theme
    ;; Make sure to disable current colors before switching
    (before theme-dont-propagate activate)
  (mapc #'disable-theme custom-enabled-themes))

(advice-add 'enable-theme :after #'jas/reset-ansi-colors)

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(add-to-list 'load-path "~/.emacs.d/themes")

(use-package naysayer-theme
  :ensure t
  :defer t)

(use-package dracula-theme
  :ensure t
  :defer t)

(use-package acme-theme
  :ensure t
  :defer t)

(use-package srcery-theme
  :ensure t
  :defer t)

(use-package cyberpunk-theme
  :ensure t
  :defer t)

(use-package material-theme
  :ensure t
  :defer t)

(defun load-dark ()
  "Load Dark Color Scheme."
  (interactive)
  (if (window-system)
	  (load-theme 'dracula t)))

(defun load-very-dark ()
  "Load Very Dark Color Scheme."
  (interactive)
  (load-theme 'cyberpunk t))

(defun load-light ()
  "Load Light Color Scheme."
  (interactive)
 (load-theme 'gruvbox-light-hard t))

(defun load-very-light ()
  "Load Very Light Color Scheme."
  (interactive)
  (load-theme 'sanityinc-tomorrow-day))

(defun load-blue ()
  "Load Blue Color Scheme."
  (interactive)
  (load-theme 'sanityinc-tomorrow-blue t))

(defun load-acme ()
  "Load Acme Color Scheme."
  (interactive)
  (load-theme 'acme t))

(defun load-neutral ()
  "Load Neutral Color Scheme."
  (interactive)
  (load-theme 'naysayer t))

(defun load-minimal ()
  "Load Minimal Color Scheme."
  (interactive)
  (load-theme 'wheatgrass t))

(load-dark)

(global-set-key (kbd "C-c u l") 'load-light)
(global-set-key (kbd "C-c u L") 'load-very-light)
(global-set-key (kbd "C-c u d") 'load-dark)
(global-set-key (kbd "C-c u D") 'load-very-dark)
(global-set-key (kbd "C-c u b") 'load-blue)
(global-set-key (kbd "C-c u a") 'load-acme)
(global-set-key (kbd "C-c u n") 'load-neutral)
(global-set-key (kbd "C-c u m") 'load-minimal)

;; (global-prettify-symbols-mode t)


;; Font
(defun jas/load-font (font-name)
  "Helper to make it easier to switch fonts"
  (interactive)
  (set-face-attribute 'default nil :font font-name))

(defun jas/initialize-fonts ()
	"Fonts setup"
  (interactive)
  (if (memq window-system '(ns))
	  (jas/load-font "Inconsolata")
	;; (jas/load-font "JetBrains Mono"))
	(jas/load-font "Inconsolata"))
  (set-face-attribute 'default nil :height 160))

(add-hook 'after-init-hook #'jas/initialize-fonts)

;; These settings relate to how emacs interacts with your operating system
(setq ;; makes killing/yanking interact with the clipboard
  x-select-enable-clipboard t

  ;; I'm actually not sure what this does but it's recommended?
  x-select-enable-primary t

  ;; Save clipboard strings into kill ring before replacing them.
  ;; When one selects something in another program to paste it into Emacs,
  ;; but kills something in Emacs before actually pasting it,
  ;; this selection is gone unless this variable is non-nil
  save-interprogram-paste-before-kill t

  ;; Shows all options when running apropos. For more info,
  ;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Apropos.html
  apropos-do-all t

  ;; Mouse yank commands yank at point instead of at click.
  mouse-yank-at-point t)

;; No cursor blinking, it's distracting
;; (blink-cursor-mode 0)
(blink-cursor-mode 1)

(diminish 'auto-revert-mode)
(diminish 'eldoc-mode)

;; full path in title bar
(setq-default frame-title-format "%b (%f)")

;; don't pop up font menu
(global-set-key (kbd "s-t") '(lambda () (interactive)))

;; no bell
(setq ring-bell-function 'ignore)

;; Git Gutter Fringe
(use-package git-gutter-fringe+
  :ensure t
  :diminish git-gutter+-mode
  :config
  (global-git-gutter+-mode t))
;;; End UI

;;; Erlang/Elixir
(use-package erlang
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.P\\'" . erlang-mode))
  (add-to-list 'auto-mode-alist '("\\.E\\'" . erlang-mode))
  (add-to-list 'auto-mode-alist '("\\.S\\'" . erlang-mode))
  :config
  (add-hook 'erlang-mode-hook
            (lambda ()
              (setq mode-name "erl"
                    erlang-compile-extra-opts '((i . "../include"))
                    erlang-root-dir "/usr/local/lib/erlang"))))

(use-package flycheck-elixir
  :ensure t)

;;; End Erlang/Elixir

;;; Golang
(use-package go-autocomplete
             :ensure t)

(use-package company-go
  :ensure t)

(use-package go-projectile
  :ensure t)

(use-package gotest
  :ensure t)

(use-package go-mode
             :ensure t
             :config
			 (go-eldoc-setup)
			 (local-set-key (kbd "M-.") #'godef-jump)
             (setq gofmt-command "goimports")
             (setq tab-width 4)
             ; Call Gofmt before saving
             (add-hook 'before-save-hook 'gofmt-before-save)
             ; Customize compile command to run go build
             (if (not (string-match "go" compile-command))
                 (set (make-local-variable 'compile-command)
                      "go generate && go build -v && go test -v && go vet"))
             ; Godef jump key binding
             (local-set-key (kbd "M-.") 'godef-jump)
             (local-set-key (kbd "M-*") 'pop-tag-mark)
             (add-hook 'go-mode-hook (lambda ()
               (set (make-local-variable 'company-backends) '(company-go))
               (company-mode))))
;;; End Golang

;;; C#
(use-package omnisharp
  :ensure t
  :init
  (add-hook 'csharp-mode-hook 'omnisharp-mode)
  (add-hook 'csharp-mode-hook #'company-mode)
  (add-hook 'csharp-mode-hook #'flycheck-mode)
  :config
  (local-set-key (kbd "C-c r r") 'omnisharp-run-code-action-refactoring)
  (local-set-key (kbd "C-c C-c") 'recompile))

;;; End C#

;;; Godot
(use-package gdscript-mode
  :ensure t)
;;; End Godot


;;; C/C++
(setq-default c-default-style "linux"
      tab-width 4
      indent-tabs-mode t
      c-basic-offset 4)
(setq-default c-basic-offset 4)

(define-key c-mode-map (kbd "C-c C-c") 'recompile)
(define-key c++-mode-map (kbd "C-c C-c") 'recompile)
;; End C/C++

(put 'narrow-to-region 'disabled nil)

(setq custom-file (concat user-emacs-directory "/custom.el"))
(load-file custom-file)

;; Cleanup after custom

(setq ansi-color-vector [term term-color-black term-color-red term-color-green term-color-yellow term-color-blue term-color-magenta term-color-cyan term-color-white])
(setq ansi-term-color-vector [term term-color-black term-color-red term-color-green term-color-yellow term-color-blue term-color-magenta term-color-cyan term-color-white])
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
