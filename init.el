; Turn off mouse interface early in startup to avoid momentary display
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

(use-package flycheck
  :ensure t
  :diminish 'flycheck-mode
  :config
  (global-set-key (kbd "C-c ! v") 'flycheck-verify-setup)
  (add-hook 'after-init-hook #'global-flycheck-mode)
  (setq-default flycheck-temp-prefix ".flycheck")
  ;; disable jshint since we prefer eslint checking
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
  (global-set-key (kbd "C-c l c") 'goto-last-change)
  )

;;; Avy mode (vim easymotion-esque)
(use-package avy
             :ensure t)
(global-set-key (kbd "C-:") 'avy-goto-char)
(global-set-key (kbd "C-\\") 'avy-goto-char-2)
(global-set-key (kbd "M-g f") 'avy-goto-line)
(global-set-key (kbd "M-g w") 'avy-goto-word-1)


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
   "b" '(:ignore t :which-key "buffer")
   "b b" 'ibuffer
   "c" '(:ignore t :which-key "company")
   "c c" 'company-complete
   "f" '(:ignore t :which-key "files")
   "f s" 'save-buffer
   "g" '(:ignore t :which-key "git")
   "g" '(:ignore t :which-key "git")
   "g s" 'magit-status
   "i" '(:ignore t :which-key "insert")
   "i n" 'xah-insert-random-number
   "j" '(:ingore t :which-key "jump")
   "j l" 'avy-goto-line
   "j w" 'avy-goto-char-2
   "n" '(:ignore t :which-key "notes")
   "n n" 'jas/insert-note
   "n t" 'jas/insert-todo
   "p" '(:ignore t :which-key "project")
   "p p" 'projectile-switch-project
   "p f" 'projectile-find-file
   "p s" 'projectile-ripgrep
   "s" '(:ignore t :which-key "shell")
   "s s" 'multi-term-dedicated-toggle
   "u" '(:ignore t :which-key "UI")
   "u c" 'counsel-load-theme
   "u l" 'load-light
   "u d" 'load-dark
   "u D" 'load-very-dark
   "u t" 'toggle-transparency
   "u n" 'global-display-line-numbers-mode
   "u z" 'writeroom-mode
   ";" '(:ignore t :which-key "commenting")
   "; r" 'comment-region))

(use-package helm
  :ensure t
  :diminish 'helm-mode
  :config
  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "C-x C-f") 'helm-find-files)
  (global-set-key (kbd "C-x b") 'helm-buffers-list)
  (global-set-key (kbd "C-s") 'swiper)
  (helm-mode 1))

;; (use-package helm-ag
;;   :ensure t)

(use-package helm-rg
  :ensure t)

(use-package helm-projectile
  :ensure t
  :init
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (helm-projectile-on))

;; (use-package ivy
;;   :ensure t
;;   :diminish ivy-mode
;;   :bind
;;   (:map ivy-mode-map
;; 		("C-'" . ivy-avy))
;;   :config
;;   (ivy-mode 1)
;;   (use-package flx
;;     :ensure t)
;;   (setq ivy-height 20)
;;   ;; add ‘recentf-mode’ and bookmarks to ‘ivy-switch-buffer’.
;;   (setq ivy-use-virtual-buffers t)
;;   ;; number of result lines to display
;;   (setq ivy-height 10)
;;   ;; does not count candidates
;;   (setq ivy-count-format "")
;;   ;; no regexp by default
;;   (setq ivy-initial-inputs-alist nil))

(use-package swiper
  :ensure t
  :config
  (global-set-key (kbd "C-s") 'swiper)
  )

;; (use-package counsel
  ;; :ensure t
  ;; :config
  ;; (global-set-key (kbd "C-c k") 'counsel-ag)
  ;; (global-set-key (kbd "C-x l") 'counsel-locate)
  ;; (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  ;; (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history))

(use-package ag
  :ensure t)

(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Enhances M-x to allow easier execution of commands. Provides
;; a filterable list of possible commands in the minibuffer
;; http://www.emacswiki.org/emacs/Smex
(use-package smex
             :ensure t)
(setq smex-save-file (concat user-emacs-directory ".smex-items"))
(smex-initialize)

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
  (setq company-idle-delay 0.2))

(global-set-key (kbd "C-'") 'company-complete)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-t") 'transpose-chars)

;; Enable paredit for Clojure
(use-package paredit
  :ensure t
  :config
  ;; Use Paredit to allow slurping
  (global-set-key (kbd "C-)") 'paredit-forward-slurp-sexp)
  (define-key paredit-mode-map (kbd "C-j") 'concat-lines)
  (add-hook 'emacs-lisp-mode-hook #'paredit-mode))

;; Expand Region
(use-package expand-region
             :ensure t)
(global-set-key (kbd "C-=") 'er/expand-region)


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
  (define-key yas-minor-mode-map [(tab)] nil)
  (define-key yas-minor-mode-map (kbd "TAB") nil))

(with-eval-after-load 'company
                      '(add-to-list 'company-backends 'company-yasnippet)
                      '(add-to-list 'company-backends 'company-web)
                      '(add-to-list 'company-backends 'company-css)
                      '(add-to-list 'company-backends 'company-go)
                      '(add-to-list 'company-backends 'company-lua)
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
;; (add-hook 'racket-mode-hook #'rainbow-delimiters-mode)

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

(use-package web-mode
  :ensure t
  :init
  (setq web-mode-script-padding 0)
  (setq web-mode-style-padding 0)
  (setq web-mode-markup-indent-offset 2)
  (add-hook 'web-mode-hook #'add-node-modules-path)
  (add-hook 'web-mode-hook #'prettier-js-mode)
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  :config
  (set-face-attribute 'web-mode-html-tag-bracket-face nil :foreground "Grey"))

(use-package prettier-js
  :ensure t
  :init
  (add-hook 'js-mode-hook #'prettier-js-mode)
  (add-hook 'typescript-mode-hook #'prettier-js-mode))

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
                           ;; (evil-leader/set-key "T" 'mocha-test-file)))
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
  (add-hook 'vue-mode-hook #'prettier-js-mode)
  :config
  (setq mmm-submode-decoration-level 0)
  ;; (add-to-list 'auto-mode-alist '("\\.vue\\'" . vue-mode))
)


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


(use-package graphql-mode
  :ensure t)

(use-package yaml-mode
  :ensure t)

(use-package haml-mode
  :ensure t)

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
  (add-hook 'after-init-hook 'inf-ruby-switch-setup))

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

;; (use-package robe
  ;; :ensure t)

(use-package projectile-rails
  :ensure t
  :init
  (projectile-rails-global-mode))

(add-hook 'ruby-mode-hook (lambda ()
                            (progn
                              (ruby-end-mode)
                              ;; (robe-mode)
                              (ruby-test-mode)
							  (rspec-mode)
                              )))

(defun my/setup-erb ()
  (interactive)
  (web-mode)
  (flycheck-mode -1)
  (prettier-js-mode -1))
;;; End Ruby

;;; Crystal
(use-package crystal-mode
  :ensure t
  :init
  (add-hook 'crystal-mode-hook 'ruby-end-mode))
;;; End Crystal

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

;;; Rust
(use-package company-racer
  :ensure t)

(use-package flycheck-rust
  :ensure t)

(use-package rust-mode
  :ensure t
  :config
  (local-set-key (kbd "C-c C-c") 'recompile))


;;; Haxe
(use-package battle-haxe
  :ensure t)
;;; End Haxe

;; debugging
(use-package dap-mode
  :ensure t)

(add-hook 'gdb-mode-hook
          (lambda ()
            (gdb-many-windows)))

;; (use-package eglot
;;   :ensure t
;;   :config
;;   (add-hook 'rust-mode-hook 'eglot-ensure)
;;   (add-hook 'go-mode-hook 'eglot-ensure)
;;   (add-hook 'ruby-mode-hook 'eglot-ensure)
;;   (add-hook 'rjsx-mode-hook 'eglot-ensure)
;;   (add-hook 'erb-mode-hook 'eglot-ensure))
;;; End Rust

;;; OrgMode
(use-package org
  :ensure t
  :config
  (setq org-agenda-files (list "~/org/work.org"
                             "~/org/home.org")))
(use-package org-bullets
  :ensure t)
(add-hook 'org-mode-hook (lambda ()
                           (org-bullets-mode 1)))

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
  "Insert *n random digits.
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
(global-hl-line-mode)

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
  (setq multi-term-program "/usr/bin/fish"))

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

(defadvice load-theme
    ;; Make sure to disable current colors before switching
    (before theme-dont-propagate activate)
  (mapc #'disable-theme custom-enabled-themes))

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(add-to-list 'load-path "~/.emacs.d/themes")

(use-package naysayer-theme
  :ensure t
  :defer t)

(use-package zenburn-theme
  :ensure t
  :defer t)

(use-package spacemacs-theme
  :defer t
  :ensure t)

(use-package dracula-theme
  :ensure t
  :defer t)

(use-package oceanic-theme
  :ensure t
  :defer t)

(defun load-dark ()
  (interactive)
  (load-theme 'zenburn t))


(defun load-very-dark ()
  (interactive)
  (load-theme 'sanityinc-tomorrow-bright t))

(defun load-light ()
  (interactive)
  (load-theme 'sanityinc-solarized-light t))

(defun load-blue ()
  (interactive)
  (load-theme 'sanityinc-tomorrow-blue t))

(load-light)

(global-set-key (kbd "C-c u l") 'load-light)
(global-set-key (kbd "C-c u d") 'load-dark)
(global-set-key (kbd "C-c u D") 'load-very-dark)
(global-set-key (kbd "C-c u b") 'load-blue)


;; Font
(set-face-attribute 'default nil :font "Ubuntu Mono")
(set-face-attribute 'default nil :height 130)

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
;;; Golang
(use-package go-autocomplete
             :ensure t)

(use-package company-go
             :ensure t)

(use-package go-mode
             :ensure t
             :config
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

;; (add-hook 'c-mode-hook 'irony-mode)

;; (add-hook 'c++-mode-hook 'irony-mode)


(use-package irony
  :ensure t
  :config
  (add-hook 'irony-mode-hook (lambda () (flycheck-mode -1)))
  )

(define-key irony-mode-map (kbd "C-c C-c") 'recompile)
(define-key c-mode-map (kbd "C-c C-c") 'recompile)
(define-key c++-mode-map (kbd "C-c C-c") 'recompile)
;; End C/C++

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   ["#0a0814" "#f2241f" "#67b11d" "#b1951d" "#4f97d7" "#a31db1" "#28def0" "#b2b2b2"])
 '(ansi-term-color-vector
   [default "#272822" "#f92672" "#a6e22e" "#f4bf75" "#66d9ef" "#ae81ff" "#66d9ef" "#f8f8f2"] t)
 '(beacon-color "#c82829")
 '(company-quickhelp-color-background "#4F4F4F")
 '(company-quickhelp-color-foreground "#DCDCCC")
 '(custom-safe-themes
   (quote
	(quote
	 (doom-modeline-mode nil))
	(quote
	 (fci-rule-color "#d6d6d6"))
	(quote
	 (flycheck-color-mode-line-face-to-color
	  (quote mode-line-buffer-id)))
	(quote
	 (frame-background-mode
	  (quote light)))
	(quote
	 (helm-completion-style
	  (quote helm)))
	(quote
	 (hl-paren-background-colors
	  (quote
	   ("#e8fce8" "#c1e7f8" "#f8e8e8"))))
	(quote
	 (hl-paren-colors
	  (quote
	   ("#40883f" "#0287c8" "#b85c57"))))
	(quote
	 (hl-todo-keyword-faces
	  (quote
	   (("TODO" . "#dc752f")
		("NEXT" . "#dc752f")
		("THEM" . "#2d9574")
		("PROG" . "#4f97d7")
		("OKAY" . "#4f97d7")
		("DONT" . "#f2241f")
		("FAIL" . "#f2241f")
		("DONE" . "#86dc2f")
		("NOTE" . "#b1951d")
		("KLUDGE" . "#b1951d")
		("HACK" . "#b1951d")
		("TEMP" . "#b1951d")
		("FIXME" . "#dc752f")
		("XXX+" . "#dc752f")
		("\\?\\?\\?+" . "#dc752f")))))
	(quote
	 (linum-format " %7i "))
	(quote
	 (nrepl-message-colors
	  (quote
	   ("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3"))))
	(quote
	 (org-agenda-files
	  (quote
	   ("~/org/work.org"))))
	(quote
	 (package-selected-packages
	  (quote
	   (gdscript-mode plan9-theme flycheck-crystal crystal-mode dracula-theme smart-modeline xterm-color ruby-refactor seeing-is-believing quack sly-quicklisp sly geiser psc-ide flycheck-purescript purescript-mode angular-html-mode fsharp-mode racket-mode cider rainbow-delimiters zenburn-theme yaml-mode yafolding xref-js2 writeroom-mode wrap-region window-numbering which-key web-mode vue-mode use-package undo-tree tide sublime-themes spotify spacemacs-theme smex smartparens scss-mode rust-mode ruby-test-mode ruby-end rubocopfmt rspec-mode robe rjsx-mode restart-emacs rbenv pyenv-mode-auto projectile-rails prettier-js poly-R paredit ox-reveal org-bullets omnisharp olivetti neotree naysayer-theme multi-term mocha lush-theme luarocks lsp-vue lsp-treemacs lsp-ruby lsp-haskell lsp-elixir linum-relative key-chord json-mode jedi irony indium htmlize helm-rg helm-projectile helm-ag haml-mode gruvbox-theme graphql-mode goto-last-change go-autocomplete git-gutter-fringe+ general forge flymake-lua flycheck-rust flycheck-haskell flycheck-elm flycheck-elixir fish-mode fiplr expand-region exec-path-from-shell ess enh-ruby-mode emmet-mode elpy elm-mode ein dap-mode d-mode counsel company-racer company-lua company-lsp company-jedi company-go color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized better-defaults beacon base16-theme all-the-icons alchemist ag add-node-modules-path))))
	(quote
	 (pdf-view-midnight-colors
	  (quote
	   ("#DCDCCC" . "#383838"))))
	(quote
	 (sml/active-background-color "#98ece8"))
	(quote
	 (sml/active-foreground-color "#424242"))
	(quote
	 (sml/inactive-background-color "#4fa8a8"))
	(quote
	 (sml/inactive-foreground-color "#424242"))
	(quote
	 (vc-annotate-background nil))
	(quote
	 (vc-annotate-color-map
	  (quote
	   ((20 . "#c82829")
		(40 . "#f5871f")
		(60 . "#eab700")
		(80 . "#718c00")
		(100 . "#3e999f")
		(120 . "#4271ae")
		(140 . "#8959a8")
		(160 . "#c82829")
		(180 . "#f5871f")
		(200 . "#eab700")
		(220 . "#718c00")
		(240 . "#3e999f")
		(260 . "#4271ae")
		(280 . "#8959a8")
		(300 . "#c82829")
		(320 . "#f5871f")
		(340 . "#eab700")
		(360 . "#718c00")))))
	(quote
	 (vc-annotate-very-old-color nil))
	(quote
	 (window-divider-mode nil))))
 '(doom-modeline-mode nil)
 '(fci-rule-color "#d6d6d6" t)
 '(flycheck-color-mode-line-face-to-color (quote mode-line-buffer-id))
 '(frame-background-mode (quote light))
 '(helm-completion-style (quote helm))
 '(hl-paren-background-colors (quote ("#e8fce8" "#c1e7f8" "#f8e8e8")))
 '(hl-paren-colors (quote ("#40883f" "#0287c8" "#b85c57")))
 '(hl-todo-keyword-faces
   (quote
	(("TODO" . "#dc752f")
	 ("NEXT" . "#dc752f")
	 ("THEM" . "#2d9574")
	 ("PROG" . "#4f97d7")
	 ("OKAY" . "#4f97d7")
	 ("DONT" . "#f2241f")
	 ("FAIL" . "#f2241f")
	 ("DONE" . "#86dc2f")
	 ("NOTE" . "#b1951d")
	 ("KLUDGE" . "#b1951d")
	 ("HACK" . "#b1951d")
	 ("TEMP" . "#b1951d")
	 ("FIXME" . "#dc752f")
	 ("XXX+" . "#dc752f")
	 ("\\?\\?\\?+" . "#dc752f"))))
 '(linum-format " %7i ")
 '(nrepl-message-colors
   (quote
	("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3")))
 '(org-agenda-files (quote ("~/org/work.org")))
 '(package-selected-packages
   (quote
	(battle-haxe ripgrep counsel-projectile uuidgen diminish alect-themes oceanic-theme gdscript-mode plan9-theme flycheck-crystal crystal-mode dracula-theme smart-modeline xterm-color ruby-refactor seeing-is-believing quack sly-quicklisp sly geiser psc-ide flycheck-purescript purescript-mode angular-html-mode fsharp-mode racket-mode cider rainbow-delimiters zenburn-theme yaml-mode yafolding xref-js2 writeroom-mode wrap-region window-numbering which-key web-mode vue-mode use-package undo-tree tide sublime-themes spotify spacemacs-theme smex smartparens scss-mode rust-mode ruby-test-mode ruby-end rubocopfmt rspec-mode robe rjsx-mode restart-emacs rbenv pyenv-mode-auto projectile-rails prettier-js poly-R paredit ox-reveal org-bullets omnisharp olivetti neotree naysayer-theme multi-term mocha lush-theme luarocks lsp-vue lsp-treemacs lsp-ruby lsp-haskell lsp-elixir linum-relative key-chord json-mode jedi irony indium htmlize helm-rg helm-projectile helm-ag haml-mode gruvbox-theme graphql-mode goto-last-change go-autocomplete git-gutter-fringe+ general forge flymake-lua flycheck-rust flycheck-haskell flycheck-elm flycheck-elixir fish-mode fiplr expand-region exec-path-from-shell ess enh-ruby-mode emmet-mode elpy elm-mode ein dap-mode d-mode counsel company-racer company-lua company-lsp company-jedi company-go color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized better-defaults beacon base16-theme all-the-icons alchemist ag add-node-modules-path)))
 '(pdf-view-midnight-colors (quote ("#DCDCCC" . "#383838")))
 '(sml/active-background-color "#98ece8")
 '(sml/active-foreground-color "#424242")
 '(sml/inactive-background-color "#4fa8a8")
 '(sml/inactive-foreground-color "#424242")
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
	((20 . "#c82829")
	 (40 . "#f5871f")
	 (60 . "#eab700")
	 (80 . "#718c00")
	 (100 . "#3e999f")
	 (120 . "#4271ae")
	 (140 . "#8959a8")
	 (160 . "#c82829")
	 (180 . "#f5871f")
	 (200 . "#eab700")
	 (220 . "#718c00")
	 (240 . "#3e999f")
	 (260 . "#4271ae")
	 (280 . "#8959a8")
	 (300 . "#c82829")
	 (320 . "#f5871f")
	 (340 . "#eab700")
	 (360 . "#718c00"))))
 '(vc-annotate-very-old-color nil)
 '(window-divider-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background nil))))
 '(sp-pair-overlay-face ((t nil))))
(put 'narrow-to-region 'disabled nil)

(setq ansi-term-color-vector [term term-color-black term-color-red term-color-green term-color-yellow term-color-blue term-color-magenta term-color-cyan term-color-white])
