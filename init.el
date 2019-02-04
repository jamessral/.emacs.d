;; Turn off mouse interface early in startup to avoid momentary display
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(require 'cl)

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

(package-initialize)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("tromey" . "http://tromey.com/elpa/") t)
(add-to-list 'package-archives
             '("gnu" . "http://elpa.gnu.org/packages/") t)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

(use-package better-defaults
  :ensure t)

(defun run-server ()
  "Runs emacs server if it is not running"
  (require 'server)
  (unless (server-running-p)
    (server-start)))

(run-server)

(use-package flycheck
  :ensure t
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
             :ensure t)

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-envs
   '("PATH")))

;; Mac key admustments
(setq mac-option-modifier 'control)
(setq mac-command-modifier 'meta)

(global-set-key (kbd "C-h") 'help)

(use-package writeroom-mode
  :ensure t
  :config
  (setq-default writeroom-width 100)
  ;; Turn off distraction-free mode by default
  (global-writeroom-mode -1)
  (global-set-key (kbd "C-c C-\\ z") 'writeroom-mode)
  (setq-default writeroom-maximize-window nil)
  (setq-default writeroom-major-modes
                '(prog-mode
                  text-mode
                  )))

(use-package window-numbering
  :ensure t
  :config
  (window-numbering-mode 1))

(use-package beacon
  :ensure t
  :config (beacon-mode 1))

(use-package windmove
  :ensure t
  :config
  (when (fboundp 'windmove-default-keybindings)
    (windmove-default-keybindings)))

(use-package undo-tree
  :ensure t
  :config
  (add-hook 'after-init-hook 'global-undo-tree-mode))

(use-package neotree
  :ensure t
  :config
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow)))

;; Let's get this nice and early
(setq evil-want-keybinding nil)
(setq evil-want-integration nil)

;;; Evil
(use-package evil-leader
  :ensure t
  :init
  :config
  (evil-leader/set-leader "<SPC>")
  (evil-leader/set-key
   "b" 'switch-to-buffer
   "<SPC>" 'counsel-M-x
   "n" 'neotree-toggle
   "m" 'neotree-find
   "<RET>" 'save-buffer
   "p" 'projectile-find-file
   "S" 'magit-status
   "j" 'avy-goto-line
   "v" 'evil-window-vsplit
   "s" 'evil-window-split
   "/" 'evil-ex-nohighlight
   "w" 'avy-goto-char-2)
  (global-evil-leader-mode))

(use-package evil-escape
  :ensure t
  :commands evil-escape-mode
  :init
  (setq evil-escape-excluded-states '(normal visual multiedit emacs motion)
        evil-escape-excluded-major-modes '(neotree-mode)
        evil-escape-key-sequence "jk"
        evil-escape-delay 0.25)
  (add-hook 'after-init-hook #'evil-escape-mode)
  :config
  ;; no `evil-escape' in minibuffer
  (cl-pushnew #'minibufferp evil-escape-inhibit-functions :test #'eq)

  (define-key evil-insert-state-map  (kbd "C-g") #'evil-escape)
  (define-key evil-replace-state-map (kbd "C-g") #'evil-escape)
  (define-key evil-visual-state-map  (kbd "C-g") #'evil-escape)
  (define-key evil-operator-state-map (kbd "C-g") #'evil-escape))

(use-package evil
  :ensure t
  :init
  :config
  (evil-mode 1)
  (key-chord-mode 1)
  ;;(key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
  ;;(key-chord-define evil-visual-state-map "jk" 'evil-normal-state)
  (define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
  (define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)
  (define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
  (define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
  (define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
  ; (define-key evil-normal-state-map (kbd "RET") 'save-buffer)
  (define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
  (define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
  (define-key evil-visual-state-map (kbd "C-u") 'evil-scroll-up)
  (define-key evil-visual-state-map (kbd "C-d") 'evil-delete-char)
  (define-key evil-insert-state-map (kbd "C-d") 'evil-delete-char)

                                        ; Shamelessly stolen from Amir Rajan
  (global-set-key [escape] 'evil-exit-emacs-state)

  (defun evil-send-string-to-terminal (string)
    (unless (display-graphic-p) (send-string-to-terminal string)))

  (defun evil-terminal-cursor-change ()
    (when (string= (getenv "TERM_PROGRAM") "iTerm.app")
      (add-hook 'evil-insert-state-entry-hook (lambda () (evil-send-string-to-terminal "\e]50;CursorShape=1\x7")))
      (add-hook 'evil-insert-state-exit-hook  (lambda () (evil-send-string-to-terminal "\e]50;CursorShape=0\x7"))))
    (when (and (getenv "TMUX")  (string= (getenv "TERM_PROGRAM") "iTerm.app"))
      (add-hook 'evil-insert-state-entry-hook (lambda () (evil-send-string-to-terminal "\ePtmux;\e\e]50;CursorShape=1\x7\e\\")))
      (add-hook 'evil-insert-state-exit-hook  (lambda () (evil-send-string-to-terminal "\ePtmux;\e\e]50;CursorShape=0\x7\e\\")))))

  (evil-terminal-cursor-change)
  )

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

(use-package evil-commentary
  :ensure t
  :config
  (evil-commentary-mode))

;; Evil disabled by default
(evil-mode -1)
(evil-escape-mode -1)
(evil-leader-mode -1)

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

(defun enable-evil ()
  (interactive)
  (set-relative-lines)
  (evil-mode 1)
  (evil-escape-mode 1)
  (evil-leader-mode 1))

(defun disable-evil ()
  (interactive)
  (set-relative-lines)
  (evil-mode -1)
  (evil-escape-mode -1)
  (evil-leader-mode -1))

(global-set-key (kbd "C-c e o") 'enable-evil)
(global-set-key (kbd "C-c e f") 'disable-evil)
(global-set-key (kbd "C-c e l") 'toggle-relative-lines)


(global-set-key (kbd "C-c RET RET") 'save-buffer)

;;; goto last change
(global-set-key (kbd "C-c l c") 'goto-last-change)

;;; Avy mode (vim easymotion-esque)
(use-package avy
             :ensure t)
(global-set-key (kbd "C-:") 'avy-goto-char)
(global-set-key (kbd "C-\\") 'avy-goto-char-2)
(global-set-key (kbd "M-g f") 'avy-goto-line)
(global-set-key (kbd "M-g w") 'avy-goto-word-1)


;; Which key to show keybindings for partially completed
;; combinations
(use-package which-key
  :ensure t
  :init
  (which-key-mode))

;; Projectile
;; projectile everywhere!
(use-package projectile
  :ensure t
  :config
  (add-hook 'after-init-hook (projectile-mode)))

(use-package helm
  :ensure t
  :config
  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "C-x C-f") 'helm-find-files)
  (global-set-key (kbd "C-x b") 'helm-buffers-list)
  (helm-mode 1))

(use-package helm-ag
  :ensure t)

(use-package helm-projectile
  :ensure t
  :init
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (helm-projectile-on))

(use-package swiper
  :ensure t
  :config
  (global-set-key (kbd "C-s") 'swiper))

(use-package counsel
  :ensure t
  :config
  (global-set-key (kbd "C-c k") 'counsel-ag)
  (global-set-key (kbd "C-x l") 'counsel-locate)
  (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history))

(use-package ag
  :ensure t)

(global-set-key (kbd "C-x C-b") 'ibuffer)

(use-package fiplr
  :ensure t
  :config
  (setq fiplr-ignored-globs '((directories (".git" ".svn" "node_modules" ".vscode"))
                              (files ("*.jpg" "*.png" "*.zip" "*~" "*.log" ".project"))))
  (global-set-key (kbd "C-x f") 'fiplr-find-file))

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

;; (use-package counsel-projectile
;;   :ensure t
;;   :config
;;   (add-hook 'after-init-hook (counsel-projectile-mode))
;;   :bind (("C-c p p" . counsel-projectile-switch-project)
;;          ("C-c p f" . counsel-projectile-find-file)
;;          ("C-c p s" . counsel-projectile-ag)))

;; Highlights matching parenthesis
(show-paren-mode 1)

(use-package smartparens
  :ensure t
  :config
  (add-hook 'after-init-hook 'smartparens-global-mode 1)
  (add-hook 'emacs-lisp-mode-hook (lambda () (smartparens-mode -1))))

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

;; Show tabs as 4 spaces
(setq tab-width 4)

;;; Editing
;; Use subword mode
(global-subword-mode)
;; Fix Org Mode syntax stuff
(setq org-src-fontify-natively t)
;; Use Asci for compile mode (running tests)
(use-package ansi-color
             :ensure t)

(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region compilation-filter-start (point))
  (toggle-read-only))

(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

(use-package company
  :ensure t
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  (global-set-key (kbd "C-<tab>") 'company-complete)
  (global-set-key (kbd "C-.") 'company-files)
  (setq company-idle-delay 200))

(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-t") 'transpose-chars)

;; Enable paredit for Clojure
(use-package paredit
  :ensure t
  :config
  ;; Use Paredit to allow slurping
  (global-set-key (kbd "C-)") 'paredit-forward-slurp-sexp)
  (add-hook 'emacs-lisp-mode-hook #'paredit-mode))

;; Expand Region
(use-package expand-region
             :ensure t)
(global-set-key (kbd "C-=") 'er/expand-region)


(use-package wrap-region
  :ensure t
  :init
  (wrap-region-global-mode))

;; Case sensitive company mode
(setq company-dabbrev-downcase nil)

;; Snippets
(use-package yasnippet
             :ensure t)
(yas-global-mode 1)

;; global key to get suggestions for snippets
(global-set-key (kbd "C-x y") 'company-yasnippet)

(with-eval-after-load 'company
                      '(add-to-list 'company-backends 'company-yasnippet)
                      '(add-to-list 'company-backends 'company-web)
                      '(add-to-list 'company-backends 'company-css)
                      '(add-to-list 'company-backends 'company-go)
                      '(add-to-list 'company-backends 'company-lua)
                      '(add-to-list 'company-backends 'company-irony)
                      '(add-to-list 'company-backends 'company-racer))

;; (use-package company-quickhelp
;;   :ensure t
;;   :config
;;   (company-quickhelp-mode 1))

;; Use Key Chords
(use-package key-chord
             :ensure t)

(key-chord-mode 1)

;; Multiple Cursors
(use-package multiple-cursors
             :ensure t)

(global-set-key (kbd "C-c C-l") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C->") 'mc/mark-next-like-this-word)
(global-set-key (kbd "C-c C-<") 'mc/mark-previous-like-this-word)

;; Folding
(use-package yafolding
  :ensure t
  :init
  (yafolding-mode 1))

;; Rainbow Mode hooks
(add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)
(add-hook 'racket-mode-hook #'rainbow-delimiters-mode)

;; Interactive search key bindings. By default, C-s runs
;; isearch-forward, so this swaps the bindings.
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

;; Keybinding for toggling window split direction
(global-set-key (kbd "C-x |") 'toggle-window-split)
;; Don't use hard tabs
(setq-default indent-tabs-mode nil)

;; When you visit a file, point goes to the last place where it
;; was when you previously visited the same file.
;; http://www.emacswiki.org/emacs/SavePlace
(use-package saveplace
  :ensure t)

(setq-default save-place t)

(use-package olivetti
  :ensure t
  :config
  (global-set-key (kbd "C-c C-z") 'olivetti-mode))

;; keep track of saved places in ~/.emacs.d/places
(setq save-place-file (concat user-emacs-directory "places"))

(add-hook 'before-save-hook #'delete-trailing-whitespace)

;; comments
(defun toggle-comment-on-line ()
  "comment or uncomment current line"
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))
(global-set-key (kbd "C-;") 'toggle-comment-on-line)

;; use 2 spaces for tabs
(defun die-tabs ()
  (interactive)
  (set-variable 'tab-width 2)
  (mark-whole-buffer)
  (untabify (region-beginning) (region-end))
  (keyboard-quit))

;; fix weird os x kill error
(defun ns-get-pasteboard ()
  "Returns the value of the pasteboard, or nil for unsupported formats."
  (condition-case nil
                  (ns-get-selection-internal 'CLIPBOARD)
                  (quit nil)))

(setq electric-indent-mode 1)

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

(define-key ctl-x-4-map "t" 'toggle-window-split)
;;; End Basics

;;; Javascript
(use-package add-node-modules-path
  :ensure t)

(use-package web-mode
  :ensure t)

(use-package emmet-mode
  :ensure t
  :init
  (add-hook 'web-mode-hook 'emmet-mode))

(use-package indium
  :ensure t
  :init
  (add-hook 'js-mode-hook #'indium-interaction-mode))

(use-package prettier-js
  :ensure t)

(use-package rjsx-mode
  :ensure t
  :config
  (add-hook 'rjsx-mode-hook #'add-node-modules-path))

(eval-after-load 'js2-mode
  '(add-hook 'js2-mode-hook #'add-node-modules-path))

;; customize flycheck temp file prefix

;; use local eslint from node_modules before global
;; http://emacs.stackexchange.com/questions/21205/flycheck-with-file-relative-eslint-executable
;; (defun my/use-eslint-from-node-modules ()
;;   (let* ((root (locate-dominating-file
;;                 (or (buffer-file-name) default-directory)
;;                 "node_modules"))
;;          (eslint (and root
;;                       (expand-file-name "node_modules/eslint/bin/eslint.js"
;;                                         root))))
;;     (when (and eslint (file-executable-p eslint))
;;       (setq-local flycheck-javascript-eslint-executable eslint))))

(defun codefalling/reset-eslint-rc ()
  (let ((rc-path (if (projectile-project-p)
                     (concat (projectile-project-root) ".eslintrc"))))
    (if (file-exists-p rc-path)
        (progn
          (message rc-path)
          (setq flycheck-eslintrc rc-path)))))

;; (add-hook 'flycheck-mode-hook 'my/use-eslint-from-node-modules)

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
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))


;;; JS-mode and derivatives
;; Turn off js2 mode errors & warnings (we lean on eslint/standard)
;; this also affects rjsx mode (yay!)
(setq js2-mode-show-parse-errors nil)
(setq js2-mode-show-strict-warnings nil)
(setq js2-basic-offset 2)
(add-hook 'js2-mode-hook #'js2-refactor-mode)
(add-hook 'js2-mode-hook (lambda ()
                           (local-set-key (kbd "C-c t t") 'mocha-test-at-point)
                           (evil-leader/set-key "t" 'mocha-test-at-point)
                           (local-set-key (kbd "C-c t f") 'mocha-test-file)
                            (evil-leader/set-key "T" 'mocha-test-file)))

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
  :config
  (setq mmm-submode-decoration-level 0)
  )
(add-to-list 'auto-mode-alist '("\\.vue\\'" . vue-mode))

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
  (add-hook 'tide-mode 'add-node-modules-path)
  :config
  ;; aligns annotation to the right hand side
  (setq company-tooltip-align-annotations t)

  (add-hook 'typescript-mode-hook #'setup-tide-mode)
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
  (add-hook 'web-mode-hook
            (lambda ()
              (when (string-equal "tsx" (file-name-extension buffer-file-name))
                (setup-tide-mode))))

  ;; enable typescript-tslint checker
  (flycheck-add-mode 'typescript-tslint 'web-mode))

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
  :ensure t)

(use-package ruby-test-mode
  :ensure t)

(use-package rspec-mode
  :ensure t
  :init
  (eval-after-load 'rspec-mode
    '(rspec-install-snippets)))

(use-package enh-ruby-mode
  :ensure t)

(use-package inf-ruby
  :ensure t
  :init
  (add-hook 'after-init-hook 'inf-ruby-switch-setup))

(use-package rubocopfmt
  :ensure t
  :init
  (add-hook 'ruby-mode-hook #'rubocopfmt-mode))

(use-package rbenv
  :ensure t
  :init
  (global-rbenv-mode))

(use-package robe
  :ensure t)

(use-package projectile-rails
  :ensure t
  :init
  (projectile-rails-global-mode))

;; (use-package rinari
;;   :ensure t
;;   :init
;;   (global-rinari-mode))

(add-hook 'ruby-mode-hook (lambda ()
                            (progn
                              (ruby-end-mode)
                              (ruby-test-mode)
                              (enh-ruby-mode)
                              ;; Use lsp instead now
                              ;;(robe-mode)
                              )))
;;; End Ruby

;;; Crystal
(use-package crystal-mode
  :ensure t)

(use-package flycheck-crystal
  :ensure t
  :init
  (add-hook 'crystal-mode-hook 'flycheck-mode))
;;; End Crystal

;;; Python
(use-package jedi
  :ensure t)

(use-package elpy
  :ensure t
  :config
  (add-hook 'python-mode (lambda () (elpy-mode))))
;;; End Python


;;; Haxe
(use-package haxe-mode
  :ensure t)

(use-package haxe-imports
  :ensure t)
;;; End Haxe

;;; Rust
(use-package company-racer
  :ensure t)

(use-package flycheck-rust
  :ensure t)

(use-package rust-mode
  :ensure t)

(use-package lsp-mode
  :ensure t
  :commands lsp
  :init
  (add-hook 'js-mode-hook #'lsp)
  (add-hook 'typescript-mode-hook #'lsp) ;; for typescript support
  (add-hook 'js3-mode-hook #'lsp) ;; for js3-mode support
  (add-hook 'js2-mode-hook #'lsp) ;; for rjsx-mode support
  (add-hook 'rust-mode-hook #'lsp)
  (add-hook 'ruby-mode-hook #'lsp))

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)

(use-package company-lsp
  :ensure t
  :commands company-lsp)

;;; End Rust


;;; OrgMode

(use-package org-bullets
  :ensure t)
(add-hook 'org-mode-hook (lambda ()
                           (org-bullets-mode 1)))

;;; End OrgMode

;;; Fun Stuff (Misc)
(use-package spotify
  :ensure t)
;;; End Fun Stuff (Misc)

;;; UI

;; These customizations change the way emacs looks and disable/enable
;; some user interface elements. Some useful customizations are
;; commented out, and begin with the line "CUSTOMIZE". These are more
;; a matter of preference and may require some fiddling to match your
;; preferences

;; Turn off the menu bar at the top of each frame because it's distracting
(menu-bar-mode -1)
(tool-bar-mode -1)

;; Show dashboard on startup
(use-package dashboard
             :ensure t
             :config
             (dashboard-setup-startup-hook)
             (setq dashboard-items '((bookmarks . 5)
                                     (projects . 5))))


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
(setq frame-background-mode 'dark)

(setq system-uses-terminfo nil)
(prefer-coding-system 'utf-8)
(use-package multi-term
  :ensure t
  :init
  (setq multi-term-program "/usr/local/bin/zsh"))


;; Show time on status bar
(display-time-mode 1)

;; Show line numbers if activated manually
;; (setq-default display-line-numbers-type 'relative)
(global-set-key (kbd "C-c n t") 'global-display-line-numbers-mode)
;; (add-hook 'prog-mode-hook 'display-line-numbers-mode)

(setq linum-format "%d ")

(use-package linum-relative
  :ensure t)

(setq linum-relative-current-symbol "")

;; (global-linum-mode)
;; Don't show native OS scroll bars for buffers because they're redundant
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

(use-package sublime-themes
  :ensure t)

(use-package lush-theme
  :ensure t)

(use-package color-theme-sanityinc-solarized
  :ensure t)

(use-package color-theme-sanityinc-tomorrow
  :ensure t)

(defadvice load-theme
           ;; Make sure to disable current colors before switching
           (before theme-dont-propagate activate)
           (mapc #'disable-theme custom-enabled-themes))

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(add-to-list 'load-path "~/.emacs.d/themes")

(defun load-dark ()
  (interactive)
  (load-theme 'base16-material-darker t))

(defun load-very-dark ()
  (interactive)
  (load-theme 'sanityinc-tomorrow-bright t))

(defun load-light ()
  (interactive)
  (load-theme 'leuven t))

(load-dark)

(global-set-key (kbd "C-c u l") 'load-light)
(global-set-key (kbd "C-c u d") 'load-dark)
(global-set-key (kbd "C-c u D") 'load-very-dark)

;; Use Ligatures
;;(global-prettify-symbols-mode)
(when (display-graphic-p) (set-face-attribute 'default nil :font "FuraCode Nerd Font"))
(if (memq window-system '(mac ns))
    (set-face-attribute 'default nil :height 140)
  (set-face-attribute 'default nil :height 100))

;; Uncomment the lines below by removing semicolons and play with the
;; values in order to set the width (in characters wide) and height
;; (in lines high) Emacs will have whenever you start it
;; (setq initial-frame-alist '((top . 0) (left . 0) (width . 177) (height . 53)))

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
(blink-cursor-mode 0)

;; full path in title bar
(setq-default frame-title-format "%b (%f)")

;; don't pop up font menu
(global-set-key (kbd "s-t") '(lambda () (interactive)))

;; no bell
(setq ring-bell-function 'ignore)

;; Git Gutter Fring
(use-package git-gutter-fringe+
             :ensure t
             :config
             (global-git-gutter+-mode t))
;;; End UI

;;; Clojure
;; Enable paredit for Clojure
(use-package paredit
             :ensure t)

;; A little more syntax highlighting
(use-package clojure-mode-extra-font-locking
             :ensure t)

;;;;
;; Cider
;;;;
(use-package cider
  :ensure t
  :bind
  (:map cider-mode-map ("C-c c u" . cider-user-ns)))

(use-package clojure-mode
  :ensure t
  :config
  ;; (define-key clojure-mode-map (kbd "C-c C-v") 'cider-start-http-server)
  ;; (define-key clojure-mode-map (kbd "C-M-r") 'cider-refresh)
  ;; (define-key clojure-mode-map (kbd "C-c u") 'cider-user-ns)
  )

;; syntax hilighting for midje
(add-hook 'clojure-mode-hook
          (lambda ()
            (setq inferior-lisp-program "lein repl")
            (font-lock-add-keywords
              nil
              '(("(\\(facts?\\)"
                 (1 font-lock-keyword-face))
                ("(\\(background?\\)"
                 (1 font-lock-keyword-face))))
            (define-clojure-indent (fact 1))
            (define-clojure-indent (facts 1))))

(add-hook 'clojure-mode-hook 'enable-paredit-mode)

;; This is useful for working with camel-case tokens, like names of
;; Java classes (e.g. JavaClassName)
(add-hook 'clojure-mode-hook 'subword-mode)
(add-hook 'clojure-mode-hook 'cider-mode)

(defun cider-turn-on-eldoc-mode ()
  "Turn on eldoc mode in the current buffer."
  (setq-local eldoc-documentation-function 'cider-eldoc)
  (apply 'eldoc-add-command cider-extra-eldoc-commands)
  (eldoc-mode +1))
;; provides minibuffer documentation for the code you're typing into the repl
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)

;; go right to the REPL buffer when it's finished connecting
(setq cider-repl-pop-to-buffer-on-connect t)

;; When there's a cider error, show its buffer and switch to it
(setq cider-show-error-buffer t)
(setq cider-auto-select-error-buffer t)

;; Where to store the cider history.
(setq cider-repl-history-file "~/.emacs.d/cider-history")

;; Wrap when navigating history.
(setq cider-repl-wrap-history t)

;; enable paredit in your REPL
(add-hook 'cider-repl-mode-hook 'paredit-mode)

;; Use clojure mode for other extensions
(add-to-list 'auto-mode-alist '("\\.edn$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.boot$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.cljs.*$" . clojure-mode))
(add-to-list 'auto-mode-alist '("lein-env" . enh-ruby-mode))


;; key bindings
;; these help me out with the way I usually develop web apps
(defun cider-start-http-server ()
  (interactive)
  (cider-load-current-buffer)
  (let ((ns (cider-current-ns)))
    (cider-repl-set-ns ns)
    (cider-interactive-eval (format "(println '(def server (%s/start))) (println 'server)" ns))
    (cider-interactive-eval (format "(def server (%s/start)) (println server)" ns))))


(defun cider-refresh ()
  (interactive)
  (cider-interactive-eval (format "(user/reset)")))

(defun cider-user-ns ()
  (interactive)
  (cider-repl-set-ns "user"))
;;; EndClojure

;;; Elixir/Erlang
(use-package alchemist
  :ensure t
  :config
  ;;(add-hook 'before-save-hook 'elixir-format)
  )

(use-package flycheck-elixir
  :ensure t)
;;; End Elixir/Erlang

;;; Slime (Common Lisp)
; (load (expand-file-name "~/quicklisp/slime-helper.el"))
; (setq inferior-lisp-program "sbcl")
;;; End Slime (Common Lisp)

;;; Geiser (Scheme)

(use-package geiser
             :ensure t)

;;; Racket

(use-package racket-mode
             :ensure t)

;;; SLIME (Lisp)

;;; Borrowed from Portacle
; (load (expand-file-name "~/quicklisp/slime-helper.el"))
; (setq inferior-lisp-program "/usr/local/bin/sbcl")

; (add-hook 'slime-repl-mode-hook (lambda () (display-line-numbers-mode -1)))

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

;;; C/C++
;; (setq c-default-style "linux")

(use-package irony
  :ensure t
  :config
  (add-hook 'irony-mode-hook '(lambda () (flycheck-mode -1)))
  )

(define-key irony-mode-map (kbd "C-c C-c") 'recompile)
(define-key c-mode-map (kbd "C-c C-c") 'recompile)
(define-key c++-mode-map (kbd "C-c C-c") 'recompile)
;;. End C/C++


;;; D lang
(use-package d-mode
  :ensure t)
;;; End D lang
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(beacon-color "#d54e53")
 '(custom-safe-themes
   (quote
    ("0c3b1358ea01895e56d1c0193f72559449462e5952bded28c81a8e09b53f103f" "446cc97923e30dec43f10573ac085e384975d8a0c55159464ea6ef001f4a16ba" default)))
 '(electric-pair-mode t)
 '(fci-rule-color "#424242")
 '(flycheck-color-mode-line-face-to-color (quote mode-line-buffer-id))
 '(frame-background-mode (quote dark))
 '(package-selected-packages
   (quote
    (rubocopfmt flycheck-crystal lsp-crystal crystal-mode lsp-mode company-lsp lsp-ui which-key rspec-mode haml-mode projectile-rails inf-ruby-mode rbenv rbenv-mode yaml-mode lsp-javascript-typescript graphql-mode prettier-js indium wrap-region yafolding xref-js2 writeroom-mode window-numbering web-mode w3m vue-mode use-package tide sublime-themes spotify spacemacs-theme smex smartparens scss-mode ruby-test-mode ruby-end robe rjsx-mode rinari rake racket-mode paredit pacmacs org-bullets olivetti oceanic-theme neotree navigate multi-term mocha magit lush-theme lsp-rust linum-relative key-chord json-mode js2-refactor irony hemisu-theme helm-projectile helm-ag haxe-mode haxe-imports gruvbox-theme go-autocomplete github-theme git-gutter-fringe+ geiser flycheck-rust flycheck-elixir flatui-theme flatui-dark-theme fiplr expand-region exec-path-from-shell evil-surround evil-leader evil-escape evil-commentary evil-collection enh-ruby-mode elpy dracula-theme dashboard d-mode counsel-projectile company-racer company-go color-theme-solarized color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized clojure-mode-extra-font-locking cider better-defaults beacon base16-theme avy all-the-icons alchemist ag add-node-modules-path)))
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#d54e53")
     (40 . "#e78c45")
     (60 . "#e7c547")
     (80 . "#b9ca4a")
     (100 . "#70c0b1")
     (120 . "#7aa6da")
     (140 . "#c397d8")
     (160 . "#d54e53")
     (180 . "#e78c45")
     (200 . "#e7c547")
     (220 . "#b9ca4a")
     (240 . "#70c0b1")
     (260 . "#7aa6da")
     (280 . "#c397d8")
     (300 . "#d54e53")
     (320 . "#e78c45")
     (340 . "#e7c547")
     (360 . "#b9ca4a"))))
 '(vc-annotate-very-old-color nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background nil)))))
