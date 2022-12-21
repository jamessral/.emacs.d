;;; Gotta go fast
(when (boundp 'w32-pipe-read-delay)
  (setq w32-pipe-read-delay 0))

(let ((file-name-handler-alist nil))
;;; Turn off mouse interface early in startup to avoid momentary display
(menu-bar-mode -1)
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(require 'cl)
(require 'loadhist)
(file-dependents (feature-file 'cl))

(setf gc-cons-threshold 100000000)
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

;; Package initialize called in early-init
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
             '("melpa" . "http://melpa.org/packages/") t)
;; (add-to-list 'package-archives
             ;; '("marmalade" . "https://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("tromey" . "https://tromey.com/elpa/") t)
(add-to-list 'package-archives
             '("gnu" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives
             '("org" . "https://orgmode.org/elpa/") t)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

(use-package esup
  :ensure t
  :pin melpa)

;; (use-package auto-compile
  ;; :ensure t
  ;; :init
  ;; (auto-compile-on-load-mode)
  ;; (auto-compile-on-save-mode))

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

;; (use-package restart-emacs
  ;; :ensure t)


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
  :commands (exec-path-from-shell-initialize exec-path-from-shell-copy-env)
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

;; (use-package undo-tree
  ;; :ensure t
  ;; :defer t
  ;; :diminish undo-tree-mode
  ;; :config
  ;; (add-hook 'after-init-hook 'global-undo-tree-mode))

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

(use-package uuidgen
  :commands (uuidgen)
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
  :after projectile
  :ensure t
  :init
  (counsel-projectile-mode t)
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(use-package ripgrep
  :ensure t
)
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
   "d" '(:ingore t :which-key "debugger")
   "d d" 'dap-breakpoint-toggle
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
   "u l" 'load-light
   "u d" 'load-dark
   "u t" 'toggle-transparency
   "u n" 'global-display-line-numbers-mode
   ">" 'mc/mark-next-like-this
   "<" 'mc/mark-next-like-this
   ";" '(:ignore t :which-key "commenting")
   "; r" 'comment-region
   "; u" 'uncomment-region))

(use-package prescient
  :ensure t)

(use-package ivy-prescient
  :after ivy
  :ensure t)

(use-package company-prescient
  :after prescient
  :ensure t)

(use-package ivy
  :after prescient
  :ensure t
  :diminish ivy-mode
  :init
  (ivy-mode t)
  (ivy-prescient-mode t)
  (setq ivy-re-builders-alist
      '((swiper . ivy--regex-plus)
        (t      . ivy--regex-fuzzy))))

(use-package swiper
  :commands (swiper)
  :ensure t
  :config
  (global-set-key (kbd "C-s") 'swiper)
  )

(use-package counsel
  :after ivy
  :ensure t
  :config
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (global-set-key (kbd "C-x b") 'counsel-switch-buffer)
  (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history))

(use-package counsel-projectile
  :after counsel
  :ensure t
  :init
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (counsel-projectile-mode t))

(use-package ag
  :after ivy
  :ensure t)

(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Magit
(use-package magit
  :ensure t
  :commands (magit-status)
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

;; Use subword mode
(global-subword-mode)
(diminish 'subword-mode)

;; Fix Org Mode syntax stuff
(setq org-src-fontify-natively t)

;; (defun my/setup-shells ()
;;   (font-lock-mode -1)
;;   (make-local-variable 'font-lock-function)
;;   (setq font-lock-function (lambda (_) nil))
;;   (add-hook 'comint-preoutput-filter-functions 'xterm-color-filter nil t))

;; (setq compilation-environment '("TERM=xterm-256color"))

;; (defun my/advice-compilation-filter (f proc string)
;;   (funcall f proc (xterm-color-filter string)))

;; (advice-add 'compilation-filter :around #'my/advice-compilation-filter)

;; (use-package xterm-color
;;   :after term
;;   :ensure t
;;   :init
;;   (add-hook 'shell-mode-hook #'my/setup-shells)
;;   (add-hook 'eshell-before-prompt-hook
;;             (lambda ()
;;               (setq xterm-color-preserve-properties t)))
;;   (setenv "TERM" "xterm-256color"))

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
  (setq company-idle-delay 0.01)
  (company-prescient-mode t))

(global-set-key (kbd "C-'") 'company-complete)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-t") 'transpose-chars)

;;; LSP
(use-package lsp-mode
  :ensure t
  :init
  (setq lsp-keymap-prefix "C-c l")
  (setq lsp-clients-typescript-max-ts-server-memory 16384)
  :hook ((typescript-mode . lsp)
		 (web-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

(use-package lsp-ivy :ensure t :commands lsp-ivy-workspace-symbol)
(use-package lsp-treemacs :ensure t :commands lsp-treemacs-errors-list)
(use-package dap-mode :ensure t
  :init
  (progn
	(require 'dap-node)
	(dap-node-setup)))

;; Expand Region
(use-package expand-region
  :commands (er/expand-region)
  :ensure t)
(global-set-key (kbd "C-]") 'er/expand-region)


(use-package wrap-region
  :ensure t
  :diminish wrap-region-mode
  :init
  (wrap-region-global-mode))

;; Case sensitive company mode
(setq company-dabbrev-downcase nil)

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

;; Spelling
(when (eq system-type 'windows-nt)
  (setq-default ispell-local-dictionary "en_US")
  (setq ispell-dictionary-alist
        '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US") nil utf-8)))
  (setq ispell-local-dictionary-alist ispell-dictionary-alist))

;; Multiple Cursors
(use-package multiple-cursors
  :commands (mc/mark-next-like-this)
  :ensure t)
(multiple-cursors-mode)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)

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

(use-package js2-mode
  :ensure t
  :init
  ;;; JS-mode and derivatives
  ;; Turn off js2 mode errors & warnings (we lean on eslint/standard)
  ;; this also affects rjsx mode (yay!)
  (setq js2-mode-show-parse-errors nil)
  (setq js2-mode-show-strict-warnings nil)
  ;; (setq js2-basic-offset 2)
  (add-hook 'js2-mode-hook #'js2-refactor-mode)
  ;; js-mode (which js2 is based on) binds "M-." which conflicts with xref, so
  ;; unbind it.
  (add-hook 'js2-mode-hook (lambda ()
                             (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t))))

(use-package web-mode
  :ensure t
  :init
  (setq web-mode-script-padding 0)
  (setq web-mode-style-padding 0)
  (setq web-mode-markup-indent-offset 2)
  (setq-default indent-tabs-mode nil)
  (add-hook 'web-mode-hook #'add-node-modules-path)
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  ; don't auto-insert quotes for attributes
  (setq web-mode-enable-auto-quoting nil)
  :config
  (set-face-attribute 'web-mode-html-tag-bracket-face nil :foreground "Grey")
  (setq indent-tabs-mode nil))

(use-package prettier
  :hook ((js-mode . prettier-mode)
		 (web-mode . prettier-mode)
		 (typescript-mode . prettier-mode))
  :ensure t
  :diminish prettier-mode)

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

(use-package scss-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))
  (setq scss-compile-at-save nil)
  (add-hook 'scss-mode-hook (lambda ()
                              (set (make-local-variable 'company-backends) '(company-css)))))

(add-hook 'js2-mode-hook #'js2-imenu-extras-mode)

(use-package js2-refactor
  :defer t
  :ensure t
  :init
  (js2r-add-keybindings-with-prefix "C-c C-m"))

(use-package xref-js2
  :defer t
  :ensure t)

;;; Jest (JS)
(use-package jest-test-mode
  :ensure t
  :commands jest-test-mode
  :bind (("C-c C-c" . 'jest-test-rerun-test))
  :hook (typescript-mode js-mode typescriptreact-mode))
;;; End Javascript

;;; Typescript
(use-package typescript-mode
  :ensure t
  :after tree-sitter
  :config
  ;; we choose this instead of tsx-mode so that eglot can automatically figure out language for server
  ;; see https://github.com/joaotavora/eglot/issues/624 and https://github.com/joaotavora/eglot#handling-quirky-servers
  (define-derived-mode typescriptreact-mode typescript-mode
    "TypeScript TSX")

  (add-hook 'typescript-mode-hook #'add-node-modules-path)
  (add-hook 'typescriptreact-mode-hook #'add-node-modules-path)
  ;; use our derived mode for tsx files
  (add-to-list 'auto-mode-alist '("\\.tsx?\\'" . typescriptreact-mode))
  ;; by default, typescript-mode is mapped to the treesitter typescript parser
  ;; use our derived mode to map both .tsx AND .ts -> typescriptreact-mode -> treesitter tsx
  (add-to-list 'tree-sitter-major-mode-language-alist '(typescriptreact-mode . tsx)))

;;; End Typescript

(use-package graphql-mode
  :defer t
  :ensure t)

(use-package yaml-mode
  :defer t
  :ensure t)

(use-package dumb-jump
  :ensure t
  :init
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  :config
  (setq dumb-jump-selector 'ivy))


;;; Ruby
(use-package ruby-end
  :after ruby-mode
  :diminish ruby-end-mode
  :ensure t)

(use-package ruby-test-mode
  :after ruby-mode
  :diminish ruby-test-mode
  :ensure t)

(use-package inf-ruby
  :after ruby-mode
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
  )

(use-package minitest
  :after ruby-mode
  :ensure t)

(use-package enh-ruby-mode
  :after ruby-mode
  :ensure t)

(use-package ruby-refactor
  :hook ((ruby-mode . ruby-refactor-mode-launch))
  :ensure t
  :diminish 'ruby-refactor-mode)

(use-package inf-ruby
  :hook ((after-init . inf-ruby-switch-setup))
  :ensure t)

(use-package rubocopfmt
  :hook ((ruby-mode . rubocopfmt-mode))
  :ensure t
  :diminish 'rubocopfmt-mode)

(use-package rbenv
  :ensure t
  :init
  (global-rbenv-mode))

(use-package erblint
  :defer t
  :ensure t)

(use-package projectile-rails
  :after projectile
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

;;; Python
(use-package elpy
  :ensure t
  :config
  (setq python-shell-interpreter "python"
      python-shell-interpreter-args "-i"))
;;; End Python

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

;;; Lisp
;; (if (memq system-type '(windows-nt ms-dos win32))
;; 	(progn
;; 	  (load (expand-file-name "C:\\Users\\james\\quicklisp\\slime-helper.el"))
;; 	 (setq inferior-lisp-program "sbcl"))
;;   (progn
;; 	(load (expand-file-name "~/quicklisp/slime-helper.el"))
;; 	 (setq inferior-lisp-program "sbcl")))

;;; Rust
(use-package company-racer
  :after company
  :ensure t)

(use-package flycheck-rust
  :after rust-mode
  :ensure t
  :init
  (with-eval-after-load 'rust-mode
	(add-hook 'flycheck-mode-hook #'flycheck-rust-setup)))

(use-package rust-mode
  :ensure t
  :config
  (local-set-key (kbd "C-c C-c") 'recompile))


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

;;; Utils
(use-package olivetti
  :commands (ollivetti-mode)
  :ensure t
  :init
  ;; Make centered layout 120 chars wide
  (setq-default olivetti-body-width 120)
  (global-set-key (kbd "C-c u z") 'olivetti-mode))

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

;; Add some margins
(setq-default left-margin-width 2 right-margin-width 2)
(setq-default header-line-format " ")
(custom-set-faces
 '(header-line ((t (:background nil)))))
;; (set-frame-parameter (selected-frame) 'alpha '(98 . 50))

;; start with transparency on
;; (add-to-list 'default-frame-alist '(alpha . (93 . 50)))
(defun toggle-transparency ()
  (interactive)
  (let ((alpha (frame-parameter nil 'alpha)))
    (set-frame-parameter
     nil 'alpha
     (if (eql (cond ((numberp alpha) alpha)
                    ((numberp (cdr alpha)) (cdr alpha))
					((numberp (cadr alpha)) (cadr alpha)))
			  100)
		 '(93 . 50) '(100 . 100)))))

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


;; Treesitter
(use-package tree-sitter
  :ensure t
  :config
  ;; activate tree-sitter on any buffer containing code for which it has a parser available
  (global-tree-sitter-mode)
  ;; you can easily see the difference tree-sitter-hl-mode makes for python, ts or tsx
  ;; by switching on and off
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package tree-sitter-langs
  :ensure t
  :after tree-sitter)


;; Set frame background to dark for terminal mode
;; (setq frame-background-mode 'dark)

(setq system-uses-terminfo nil)
(prefer-coding-system 'utf-8)

(add-to-list 'comint-output-filter-functions 'ansi-color-process-output)
(ansi-color-for-comint-mode-on)

(use-package multi-term
  :ensure t
  :init
  (set-default 'multi-term-dedicated-window-height 30)
  (if (memq window-system '(win32))
	  (setq multi-term-program "powershell.exe -NoLogo -NonInteractive")
	(setq multi-term-program "zsh")
	))

(use-package powershell
  :ensure t
  :config
  ;; Change default compile command for powershell
  (add-hook 'powershell-mode-hook
			(lambda ()
			  (set (make-local-variable 'compile-command)
				   (format "powershell.exe -NoLogo -NonInteractive -Command \"& '%s'\""
						   (buffer-file-name))))))

(display-time-mode 1)

;; Show line numbers if activated manually
;; (setq-default display-line-numbers-type 'absolute)
;; (add-hook 'prog-mode-hook 'display-line-numbers-mode)

(setq linum-format " %d ")

(setq linum-relative-current-symbol "")

;; (global-linum-mode)
;; Don't show native OS scroll bars for buffers because they're redundant
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

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

(use-package modus-themes
  :ensure t)

(use-package naysayer-theme
  :ensure t)

(use-package minimal-theme
  :ensure t)

(use-package zenburn-theme
  :ensure t)

(defun load-dark ()
  "Load Dark Color Scheme."
  (interactive)
	(load-theme 'modus-vivendi t))

(defun load-light ()
  "Load Light Color Scheme."
  (interactive)
 (load-theme 'minimal-light t))

(load-dark)

(global-set-key (kbd "C-c u l") 'load-light)
(global-set-key (kbd "C-c u d") 'load-dark)

;; (global-prettify-symbols-mode t)


;; Font
(defun jas/load-font (font-name)
  "Helper to make it easier to switch fonts"
  (interactive)
  (set-face-attribute 'default nil :font font-name))

(defun jas/initialize-fonts ()
  "Fonts setup"
  (interactive)
  (jas/load-font "Liberation Mono")
  (set-face-attribute 'default nil :height 140))

(add-hook 'find-file-hook #'jas/initialize-fonts)
;; Set default font

(use-package lorem-ipsum
  :defer t
  :ensure t)

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

;; full path in title bar ()
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
			 ;; (go-eldoc-setup)
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
;; (use-package omnisharp
;;   :ensure t
;;   :init
;;   (add-hook 'csharp-mode-hook 'omnisharp-mode)
;;   (add-hook 'csharp-mode-hook #'company-mode)
;;   (add-hook 'csharp-mode-hook #'flycheck-mode)
;;   :config
;;   (local-set-key (kbd "C-c r r") 'omnisharp-run-code-action-refactoring)
;;   (local-set-key (kbd "C-c C-c") 'recompile))

;;; End C#

;;; C/C++
(setq-default c-default-style "linux"
      tab-width 4
      indent-tabs-mode t
      c-basic-offset 4)
(setq-default c-basic-offset 4)

(define-key c-mode-map (kbd "C-c C-c") 'recompile)
(define-key c++-mode-map (kbd "C-c C-c") 'recompile)
;; End C/C++

;;; Work functions
(defun jas/client-dev ()
  "Run all processes needed for Loadup Dev"
  (interactive)
  (let ((current-dir default-directory))
    (cd "~/code/client")
    (async-shell-command "yarn start:merchants")
    (cd current-dir)))
(defun jas/node-core-dev ()
  "Run all processes needed for Loadup Dev"
  (interactive)
  (let ((current-dir default-directory))
    (cd "~/code/node-core")
    (cd current-dir)))
;;; End Work Functions

(put 'narrow-to-region 'disabled nil)

(setq custom-file (concat user-emacs-directory "/custom.el"))
(load-file custom-file)

;; Cleanup after custom

(setq ansi-color-vector [term term-color-black term-color-red term-color-green term-color-yellow term-color-blue term-color-magenta term-color-cyan term-color-white])
(setq ansi-term-color-vector [term term-color-black term-color-red term-color-green term-color-yellow term-color-blue term-color-magenta term-color-cyan term-color-white])
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(jas/initialize-fonts))
