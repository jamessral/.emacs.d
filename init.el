

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


;; (setq evil-want-integration nil)

;; ;;; Evil
;; (use-package evil-leader
;;   :ensure t
;;   :init
;;   :config
;;   (evil-leader/set-leader "<SPC>")
;;   (evil-leader/set-key
;;    "b" 'switch-to-buffer
;;    "<SPC>" 'counsel-M-x
;;    "p" 'projectile-find-file
;;    "S" 'magit-status
;;    "j" 'avy-goto-line
;;    "v" 'evil-window-vsplit
;;    "s" 'evil-window-split
;;    "/" 'evil-search-highlight-persist-remove-all
;;    "w" 'avy-goto-word-0)
;;   (global-evil-leader-mode))

;; (use-package evil
;;   :ensure t
;;   :config
;;   (evil-mode 1)
;;   (key-chord-mode 1)
;;   (key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
;;   (key-chord-define evil-visual-state-map "jk" 'evil-normal-state)
;;   (define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
;;   (define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)
;;   (define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
;;   (define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
;;   (define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
;;   (define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
;;   (define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
;;   (define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
;;   (define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
;;   (define-key evil-normal-state-map (kbd "RET") 'save-buffer)
;;   (define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
;;   (define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
;;   (define-key evil-visual-state-map (kbd "C-u") 'evil-scroll-up)

;;                                         ; Shamelessly stolen from Amir Rajan
;;   (global-set-key [escape] 'evil-exit-emacs-state)

;;   (defun evil-send-string-to-terminal (string)
;;     (unless (display-graphic-p) (send-string-to-terminal string)))

;;   (defun evil-terminal-cursor-change ()
;;     (when (string= (getenv "TERM_PROGRAM") "iTerm.app")
;;       (add-hook 'evil-insert-state-entry-hook (lambda () (evil-send-string-to-terminal "\e]50;CursorShape=1\x7")))
;;       (add-hook 'evil-insert-state-exit-hook  (lambda () (evil-send-string-to-terminal "\e]50;CursorShape=0\x7"))))
;;     (when (and (getenv "TMUX")  (string= (getenv "TERM_PROGRAM") "iTerm.app"))
;;       (add-hook 'evil-insert-state-entry-hook (lambda () (evil-send-string-to-terminal "\ePtmux;\e\e]50;CursorShape=1\x7\e\\")))
;;       (add-hook 'evil-insert-state-exit-hook  (lambda () (evil-send-string-to-terminal "\ePtmux;\e\e]50;CursorShape=0\x7\e\\")))))

;;   (evil-terminal-cursor-change)
;;   )

;; (use-package evil-collection
;;   :after evil
;;   :ensure t
;;   :config
;;   (evil-collection-init))

;; (use-package evil-surround
;;   :ensure t
;;   :config
;;   (global-evil-surround-mode 1))

;; (use-package evil-commentary
;;   :ensure t
;;   :config
;;   (evil-commentary-mode))

;;; Avy mode (vim easymotion-esque)
(use-package avy
             :ensure t)
(global-set-key (kbd "C-:") 'avy-goto-char)
(global-set-key (kbd "C-\\") 'avy-goto-char-2)
(global-set-key (kbd "M-g f") 'avy-goto-line)
(global-set-key (kbd "M-g w") 'avy-goto-word-1)

(use-package helm
  :ensure t
  :config
  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "C-x C-f") 'helm-find-files)
  (global-set-key (kbd "C-x b") 'helm-buffers-list)
  (helm-mode 1))

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

;; Projectile
;; projectile everywhere!
(use-package projectile
  :ensure t
  :config
  (add-hook 'after-init-hook (projectile-mode)))

(use-package counsel-projectile
  :ensure t
  :config
  (add-hook 'after-init-hook (counsel-projectile-mode))
  :bind (("C-c p p" . counsel-projectile-switch-project)
         ("C-c p f" . counsel-projectile-find-file)
         ("C-c p s" . counsel-projectile-ag)))

;; Highlights matching parenthesis
(show-paren-mode 1)

(use-package smartparens
  :ensure t
  :config
  (add-hook 'after-init-hook 'smartparens-global-mode 1))

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
  (global-set-key (kbd "C-<tab>") 'company-complete))

(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-t") 'transpose-chars)

;; Enable paredit for Clojure
(use-package paredit
  :ensure t
  :config
  ;; Use Paredit to allow slurping
  (global-set-key (kbd "C-)") 'paredit-forward-slurp-sexp))

;; Expand Region
(use-package expand-region
             :ensure t)
(global-set-key (kbd "C-=") 'er/expand-region)

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
             )
(yafolding-mode 1)

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
             :ensure t
             )
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
                           (local-set-key (kbd "C-c C-\\ C-t") 'mocha-test-at-point)
                           (evil-leader/set-key "t" 'mocha-test-at-point)
                           (local-set-key (kbd "C-c C-\\ C-f") 'mocha-test-file)
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
                            (local-set-key (kbd "C-c C-t C-t") 'mocha-test-at-point)
                            (local-set-key (kbd "C-c C-t C-f") 'mocha-test-file)
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
(add-to-list 'magic-mode-alist '(".*import Rreact" . rjsx-mode))
(add-to-list 'magic-mode-alist '("^import React" . rjsx-mode))

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
  :ensure t)

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
  :config
  ;; aligns annotation to the right hand side
  (setq company-tooltip-align-annotations t)

  ;; formats the buffer before saving
  (add-hook 'before-save-hook 'tide-format-before-save)

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

;;; Rust
(use-package company-racer
  :ensure t)

(use-package flycheck-rust
  :ensure t)

(use-package rust-mode
  :ensure t
  :config
  (with-eval-after-load 'rust-mode
    (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)))
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


;; Show time on status bar
(display-time-mode 1)

;; Show line numbers if activated manually
(global-set-key (kbd "C-c C-\\ n") 'display-line-numbers-mode)
;;(add-hook 'prog-mode-hook 'display-line-numbers-mode)
;;(setq-default display-line-numbers-type 'relative)

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

(use-package base16-theme
  :ensure t)

(use-package color-theme-sanityinc-solarized
  :ensure t)

(use-package color-theme-solarized
  :ensure t)

(defadvice load-theme
           ;; Make sure to disable current colors before switching
           (before theme-dont-propagate activate)
           (mapc #'disable-theme custom-enabled-themes))

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(add-to-list 'load-path "~/.emacs.d/themes")

(defun load-dark ()
  (interactive)
  (load-theme 'gruvbox-dark-hard t))

(defun load-light ()
  (interactive)
  (load-theme 'base16-atelier-lakeside-light t))

(load-dark)

(global-set-key (kbd "C-c C-\\ l") 'load-light)
(global-set-key (kbd "C-c C-\\ d") 'load-dark)

;; Use Ligatures
;;(global-prettify-symbols-mode)
(when (display-graphic-p) (set-face-attribute 'default nil :font "FuraCode Nerd Font"))
(if (memq window-system '(mac ns))
    (set-face-attribute 'default nil :height 150)
  (set-face-attribute 'default nil :height 150))

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

;; Git Gutter
(use-package git-gutter
             :ensure t
             :config
             (global-git-gutter-mode t))
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
  (:map cider-mode-map ("C-c u" . cider-user-ns)))

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
(load (expand-file-name "~/quicklisp/slime-helper.el"))
(setq inferior-lisp-program "sbcl")
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

(define-key irony-mode-map (kbd "C-c C-m") 'recompile)
(define-key c-mode-map (kbd "C-c C-m") 'recompile)
(define-key c++-mode-map (kbd "C-c C-m") 'recompile)
;;. End C/C++


;;; D lang
(use-package d-mode
  :ensure t)
;;; End D lang

(use-package w3m
  :ensure t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["black" "red3" "ForestGreen" "yellow3" "blue" "magenta3" "DeepSkyBlue" "gray50"])
 '(ansi-term-color-vector
   [unspecified "#22221b" "#ba6236" "#7d9726" "#a5980d" "#36a166" "#5f9182" "#36a166" "#929181"] t)
 '(beacon-mode t)
 '(custom-safe-themes
   (quote
    ("69e7e7069edb56f9ed08c28ccf0db7af8f30134cab6415d5cf38ec5967348a3c" "2a998a3b66a0a6068bcb8b53cd3b519d230dd1527b07232e54c8b9d84061d48d" "fede08d0f23fc0612a8354e0cf800c9ecae47ec8f32c5f29da841fe090dfc450" "a5956ec25b719bf325e847864e16578c61d8af3e8a3d95f60f9040d02497e408" "f27c3fcfb19bf38892bc6e72d0046af7a1ded81f54435f9d4d09b3bff9c52fc1" "196df8815910c1a3422b5f7c1f45a72edfa851f6a1d672b7b727d9551bb7c7ba" "4bf5c18667c48f2979ead0f0bdaaa12c2b52014a6abaa38558a207a65caeb8ad" "986e7e8e428decd5df9e8548a3f3b42afc8176ce6171e69658ae083f3c06211c" "7a1190ad27c73888f8d16142457f59026b01fa654f353c17f997d83565c0fc65" "d80975e30569ad6c5f359521eac1b32626015aed1c8bef139c96418a38dd3b04" "3de3f36a398d2c8a4796360bfce1fa515292e9f76b655bb9a377289a6a80a132" "df21cdadd3f0648e3106338649d9fea510121807c907e2fd15565dde6409d6e9" "cbd8e65d2452dfaed789f79c92d230aa8bdf413601b261dbb1291fb88605110c" "542e6fee85eea8e47243a5647358c344111aa9c04510394720a3108803c8ddd1" "4feee83c4fbbe8b827650d0f9af4ba7da903a5d117d849a3ccee88262805f40d" "85e6bb2425cbfeed2f2b367246ad11a62fb0f6d525c157038a0d0eaaabc1bfee" "f6f5d5adce1f9a764855c9730e4c3ef3f90357313c1cae29e7c191ba1026bc15" "eae831de756bb480240479794e85f1da0789c6f2f7746e5cc999370bbc8d9c8a" "0c3b1358ea01895e56d1c0193f72559449462e5952bded28c81a8e09b53f103f" "5b8eccff13d79fc9b26c544ee20e1b0c499587d6c4bfc38cabe34beaf2c2fc77" "760ce657e710a77bcf6df51d97e51aae2ee7db1fba21bbad07aab0fa0f42f834" "aea30125ef2e48831f46695418677b9d676c3babf43959c8e978c0ad672a7329" "16dd114a84d0aeccc5ad6fd64752a11ea2e841e3853234f19dc02a7b91f5d661" "44961a9303c92926740fc4121829c32abca38ba3a91897a4eab2aa3b7634bed4" "a62f0662e6aa7b05d0b4493a8e245ab31492765561b08192df61c9d1c7e1ddee" "f5f3a6fb685fe5e1587bafd07db3bf25a0655f3ddc579ed9d331b6b19827ea46" "45a8b89e995faa5c69aa79920acff5d7cb14978fbf140cdd53621b09d782edcf" "41eb3fe4c6b80c7ad156a8c52e9dd6093e8856c7bbf2b92cc3a4108ceb385087" "fc7fd2530b82a722ceb5b211f9e732d15ad41d5306c011253a0ba43aaf93dccc" "34ed3e2fa4a1cb2ce7400c7f1a6c8f12931d8021435bad841fdc1192bd1cc7da" "f2dd097452b79276ce522df2f8aeb37f6d90f504529616aa46122d549910e46d" "3380a2766cf0590d50d6366c5a91e976bdc3c413df963a0ab9952314b4577299" "cea3ec09c821b7eaf235882e6555c3ffa2fd23de92459751e18f26ad035d2142" "78c1c89192e172436dbf892bd90562bc89e2cc3811b5f9506226e735a953a9c6" "8be07a2c1b3a7300860c7a65c0ad148be6d127671be04d3d2120f1ac541ac103" "c968804189e0fc963c641f5c9ad64bca431d41af2fb7e1d01a2a6666376f819c" "36282815a2eaab9ba67d7653cf23b1a4e230e4907c7f110eebf3cdf1445d8370" "93268bf5365f22c685550a3cbb8c687a1211e827edc76ce7be3c4bd764054bad" "6145e62774a589c074a31a05dfa5efdf8789cf869104e905956f0cbd7eda9d0e" "aded4ec996e438a5e002439d58f09610b330bbc18f580c83ebaba026bbef6c82" "5a7830712d709a4fc128a7998b7fa963f37e960fd2e8aa75c76f692b36e6cf3c" "dd4628d6c2d1f84ad7908c859797b24cc6239dfe7d71b3363ccdd2b88963f336" "25c06a000382b6239999582dfa2b81cc0649f3897b394a75ad5a670329600b45" "6daa09c8c2c68de3ff1b83694115231faa7e650fdbb668bc76275f0f2ce2a437" "b3bcf1b12ef2a7606c7697d71b934ca0bdd495d52f901e73ce008c4c9825a3aa" "1d079355c721b517fdc9891f0fda927fe3f87288f2e6cc3b8566655a64ca5453" "3e34e9bf818cf6301fcabae2005bba8e61b1caba97d95509c8da78cff5f2ec8e" "cabc32838ccceea97404f6fcb7ce791c6e38491fd19baa0fcfb336dcc5f6e23c" "3cc2385c39257fed66238921602d8104d8fd6266ad88a006d0a4325336f5ee02" "4486ade2acbf630e78658cd6235a5c6801090c2694469a2a2b4b0e12227a64b9" "72a81c54c97b9e5efcc3ea214382615649ebb539cb4f2fe3a46cd12af72c7607" "3d5ef3d7ed58c9ad321f05360ad8a6b24585b9c49abcee67bdcbb0fe583a6950" "b3775ba758e7d31f3bb849e7c9e48ff60929a792961a2d536edec8f68c671ca5" "9b59e147dbbde5e638ea1cde5ec0a358d5f269d27bd2b893a0947c4a867e14c1" "e9776d12e4ccb722a2a732c6e80423331bcb93f02e089ba2a4b02e85de1cf00e" "987b709680284a5858d5fe7e4e428463a20dfabe0a6f2a6146b3b8c7c529f08b" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "15348febfa2266c4def59a08ef2846f6032c0797f001d7b9148f30ace0d08bcf" "718fb4e505b6134cc0eafb7dad709be5ec1ba7a7e8102617d87d3109f56d9615" "a4df5d4a4c343b2712a8ed16bc1488807cd71b25e3108e648d4a26b02bc990b3" "c48551a5fb7b9fc019bf3f61ebf14cf7c9cdca79bcb2a4219195371c02268f11" "0820d191ae80dcadc1802b3499f84c07a09803f2cb90b343678bdb03d225b26b" default)))
 '(fci-rule-color "#f1c40f")
 '(global-undo-tree-mode t)
 '(hl-paren-background-colors (quote ("#2492db" "#95a5a6" nil)))
 '(hl-paren-colors (quote ("#ecf0f1" "#ecf0f1" "#c0392b")))
 '(linum-format " %5i ")
 '(package-selected-packages
   (quote
    (color-theme-solarized color-theme-sanityinc-solarized color-theme-sanity-inc-solarized company-racer company-rust flycheck-rust rust-mode rust gruvbox-theme helm pacmacs irony irony-mode d-mode w3m base16-theme evil-leader spacemacs-theme evil-collection flatui-theme oceanic-theme flatui-dark-theme linum-relative dracula-theme evil-commentary evil-surround navigate evil sublime-themes flycheck-elixir beacon undo-tree add-node-modules-path spotify clojure-mode-extra-font-locking alchemist counsel-projectile tide racket-mode geiser yafolding key-chord all-the-icons smex fiplr ag counsel swiper ivy avy window-numbering flycheck use-package)))
 '(sml/active-background-color "#34495e")
 '(sml/active-foreground-color "#ecf0f1")
 '(sml/inactive-background-color "#dfe4ea")
 '(sml/inactive-foreground-color "#34495e")
 '(vc-annotate-background "#ecf0f1")
 '(vc-annotate-color-map
   (quote
    ((30 . "#e74c3c")
     (60 . "#c0392b")
     (90 . "#e67e22")
     (120 . "#d35400")
     (150 . "#f1c40f")
     (180 . "#d98c10")
     (210 . "#2ecc71")
     (240 . "#27ae60")
     (270 . "#1abc9c")
     (300 . "#16a085")
     (330 . "#2492db")
     (360 . "#0a74b9"))))
 '(vc-annotate-very-old-color "#0a74b9"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background nil)))))
(put 'downcase-region 'disabled nil)
