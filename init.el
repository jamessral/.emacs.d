;; Packages and Setup
;; Turn off mouse interface early in startup to avoid momentary display
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(require 'cl)

;; Define package repositories
(require 'package)
(package-initialize)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("tromey" . "http://tromey.com/elpa/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
             '("gnu" . "http://elpa.gnu.org/packages/") t)
;; (setq package-archives '(
;;                          ("marmalade" . "http://marmalade-repo.org/packages/")
;;                          ("melpa" . "http://melpa-stable.milkbox.net/packages/")))

;; Fix for the warnings

(defvar ido-cur-item nil)
(defvar ido-default-item nil)
(defvar ido-cur-list nil)
(defvar predicate nil)
(defvar inherit-input-method nil)
;; Load and activate emacs packages. Do this first so that the
;; packages are loaded before you start trying to modify them.
;; This also sets the load path.
(package-initialize)
(add-hook 'after-init-hook 'global-company-mode)

;; Download the ELPA archive description if needed.
;; This informs Emacs about the latest versions of all packages, and
;; makes them available for download.
(when (not package-archive-contents)
  (package-refresh-contents))

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-copy-env "GOPATH")
  (exec-path-from-shell-copy-env "NPM_TOKEN")
  (exec-path-from-shell-initialize))

;; Don't wrap lines
(setq-default truncate-lines 1)

(use-package flycheck
  :ensure t)

(add-hook 'flycheck-hook #'global-flycheck-mode)

(use-package ag
  :ensure t)

(use-package git
  :ensure t)
(add-to-list 'load-path "~/.emacs.d/vendor")

;;; Basics
(setq make-backup-files nil) ; stop creating backup~ files
(setq auto-save-default nil) ; stop creating #autosave# files
(use-package exec-path-from-shell
  :ensure t)
;; Show tabs as 4 spaces
(setq tab-width 4)


;;; Editing
;; Customizations relating to editing a buffer.
;; Key binding to use "hippie expand" for text autocompletion
;; http://www.emacswiki.org/emacs/HippieExpand

(use-package flymake
  :ensure t)

;; Get something like the vim '.' repeat
;;(use-package dot-mode
;;  :ensure t)
;;(add-hook 'find-file-hooks 'dot-mode-on)

;; Use subword mode
(global-subword-mode)
;; Fix Org Mode syntax stuff
(setq org-src-fontify-natively t)
;; Use Asci for compile mode (running tests)

(use-package company
  :ensure t
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  (global-set-key (kbd "C-<tab>") 'company-complete)
)

(global-set-key (kbd "M-/") 'hippie-expand)

(global-set-key (kbd "C-t") 'transpose-chars)

;; Use Paredit to allow slurping
(global-set-key (kbd "C-)") 'paredit-forward-slurp-sexp)

;; Expand Region
(use-package expand-region
  :ensure t
  :config
  (global-set-key (kbd "C-@") 'er/expand-region)
)


;; Case sensitive company mode
(setq company-dabbrev-downcase nil)

(use-package aggressive-indent
  :ensure t)

(global-aggressive-indent-mode)

;; Snippets
(use-package yasnippet
  :ensure t)

(yas-global-mode 1)

;; global key to get suggestions for snippets
(global-set-key (kbd "C-x y") 'company-yasnippet)

(with-eval-after-load 'company
  '(add-to-list 'company-backends 'company-yasnippet)
  '(add-to-list 'company-backends 'company-flow)
  '(add-to-list 'company-backends 'company-elm)
  '(add-to-list 'company-backends 'company-tern)
  '(add-to-list 'company-backends 'company-web)
  '(add-to-list 'company-backends 'company-css)
  '(add-to-list 'company-backends 'company-go)
  '(add-to-list 'company-backends 'company-lua)
  '(add-to-list 'company-backends 'company-ac-php-backend)
  '(add-to-list 'company-backends 'company-irony)
  '(add-to-list 'company-backends 'company-ghc)
  '(add-to-list 'company-backends 'company-racer))

(company-quickhelp-mode 1)

;; Lisp-friendly hippie expand
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))

;; Mac key admustments
(setq mac-option-modifier 'control)
(setq mac-command-modifier 'meta)

;; Highlights matching parenthesis
(show-paren-mode 1)

(use-package smartparens
  :ensure t)

;;(use-package smartparens-config
;;q  :ensure t)
(smartparens-global-mode 1)
(electric-pair-mode 1)

;; Highlight current line
(when (window-system) global-hl-line-mode 1)


;; Use Key Chords
(use-package key-chord
  :ensure t
  :init
  (key-chord-mode 1)
)

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
  :ensure t)

(yafolding-mode 1)



;; Add yasnippet support for all company backends
;; https://github.com/syl20bnr/spacemacs/pull/179
;; (defvar company-mode/enable-yas t
;;   "Enable yasnippet for all backends.")

;; (defun company-mode/backend-with-yas (backend)
;;   (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
;;       backend
;;     (append (if (consp backend) backend (list backend))
;;             '(:with company-yasnippet))))

;; (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))


;; (add-hook 'web-mode-hook #'rainbow-delimiters-mode)
;; Rainbow Mode hooks
(add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)
(add-hook 'racket-mode-hook #'rainbow-delimiters-mode)

;; Interactive search key bindings. By default, C-s runs
;; isearch-forward, so this swaps the bindings.
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
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
;; keep track of saved places in ~/.emacs.d/places
(setq save-place-file (concat user-emacs-directory "places"))

;; Emacs can automatically create backup files. This tells Emacs to
;; put all backups in ~/.emacs.d/backups. More info:
;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Backup-Files.html
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory
                                               "backups"))))
(setq auto-save-default nil)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

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

(setq electric-indent-mode nil)

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


;;; Navigation
(use-package windmove
  :ensure t
  :config
  (when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings)))


(use-package window-numbering
  :ensure t)
(window-numbering-mode 1)
;; These customizations make it easier for you to navigate files,
;; switch buffers, and choose options from the minibuffer.


;; "When several buffers visit identically-named files,
;; Emacs must give the buffers distinct names. The usual method
;; for making buffer names unique adds ‘<2>’, ‘<3>’, etc. to the end
;; of the buffer names (all but one of them).
;; The forward naming method includes part of the file's directory
;; name at the beginning of the buffer name
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Uniquify.html
;;(use-package uniquify
;;  :ensure t)
(setq uniquify-buffer-name-style 'forward)


;; Turn on recent file mode so that you can more easily switch to
;; recently edited files when you first start emacs
(setq recentf-save-file (concat user-emacs-directory ".recentf"))
(use-package recentf
  :ensure t)

(recentf-mode 1)


(setq recentf-max-menu-items 40)

;; Use the super fast FZF
(global-set-key (kbd "C-c z") 'fzf)

;; ido-mode allows you to more easily navigate choices. For example,
;; when you want to switch buffers, ido presents you with a list
;; of buffers in the the mini-buffer. As you start to type a buffer's
;; name, ido will narrow down the list of buffers to match the text
;; you've typed in
;; http://www.emacswiki.org/emacs/InteractivelyDoThings
(ido-mode 1)

;; This allows partial matches, e.g. "tl" will match "Tyrion Lannister"
(setq ido-enable-flex-matching t)

;; Turn this behavior off because it's annoying
(setq ido-use-filename-at-point nil)

;; Don't try to match file across all "work" directories; only match files
;; in the current directory displayed in the minibuffer
(setq ido-auto-merge-work-directories-length -1)

;; Includes buffer names of recently open files, even if they're not
;; open now
(setq ido-use-virtual-buffers t)

;; Ace Jump Mode (vim easymotion)
;;(require 'ace-jump-mode)
;;(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)

;; Avy mode (vim easymotion-esque)
(use-package avy
  :ensure t
  :config
  (global-set-key (kbd "C-:") 'avy-goto-char)
  (global-set-key (kbd "C-'") 'avy-goto-char-2)
  (global-set-key (kbd "M-g f") 'avy-goto-line)
  (global-set-key (kbd "M-g w") 'avy-goto-word-1)
)

;; This enables ido in all contexts where it could be useful, not just
;; for selecting buffer and file names
(ido-ubiquitous-mode 1)


;; Ivy
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
(global-set-key "\C-s" 'swiper)
(global-set-key (kbd "C-C C-r") 'ivy-resume)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "C-c k") 'counsel-ag)
(global-set-key (kbd "C-x l") 'counsel-locate)
(define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)

;; Shows a list of buffers
;; (global-set-key (kbd "C-x b") 'helm-buffers-list) ;; Helm


(global-set-key (kbd "C-x C-b") 'ibuffer)

(global-set-key (kbd "C-x f") 'fiplr-find-file)

(setq fiplr-ignored-globs '((directories (".git" ".svn" "node_modules" ".vscode"))
                            (files ("*.jpg" "*.png" "*.zip" "*~" "*.log" ".project"))))
;; Enhances M-x to allow easier execution of commands. Provides
;; a filterable list of possible commands in the minibuffer
;; http://www.emacswiki.org/emacs/Smex
(setq smex-save-file (concat user-emacs-directory ".smex-items"))
(smex-initialize)

;; projectile everywhere!
(projectile-global-mode)
;; Neotree
(global-set-key (kbd "C-, C-n") 'neotree-toggle)
(setq neo-theme (if (display-graphic-p) 'icons 'arrow 'nerd))


;;; Narrowing/Widening
  (enable-command 'narrow-to-region)
  (enable-command 'narrow-to-defun)
  (enable-command 'narrow-to-page)
  (enable-command 'widen)

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
  :init
  (dashboard-setup-startup-hook)
)

(setq dashboard-items '((bookmarks . 5)
                        (projects . 5)))


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
(global-linum-mode 0)
(global-set-key (kbd "C-c C-' n") 'linum-mode)

(setq linum-format "%d ")
;; Don't show native OS scroll bars for buffers because they're redundant
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

;; Color Themes
;; Read http://batsov.com/articles/2012/02/19/color-theming-in-emacs-reloaded/
;; for a great explanation of emacs color themes.
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Custom-Themes.html
;; for a more technical explanation.
;; Powerline must be added before Moe theme
;; (add-to-list 'load-path "~/.emacs.d/elpy/")

;;(require 'powerline)
;;(powerline-default-theme)

(use-package gruvbox-theme
  :ensure t)

(use-package sublime-themes
  :ensure t)

(use-package dracula-theme
  :ensure t)

(defadvice load-theme
    ;; Make sure to disable current colors before switching
    (before theme-dont-propagate activate)
  (mapc #'disable-theme custom-enabled-themes))


(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(add-to-list 'load-path "~/.emacs.d/themes")
(load-theme 'dracula t)

(defun load-dark ()
  (interactive)
  (load-theme 'dracula t))

(defun load-light ()
  (interactive)
  (load-theme 'leuven t)
)

(global-set-key (kbd "C-c C-' l") 'load-light)
(global-set-key (kbd "C-c C-' d") 'load-dark)

;; Use Ligatures
;;(global-prettify-symbols-mode)
(when (display-graphic-p) (set-face-attribute 'default nil :font "RobotoMono Nerd Font Mono"))
(set-face-attribute 'default nil :height 100)

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
  :init
  (global-git-gutter-mode t))


;;; Javascript
;;; Javascript stuff
(use-package web-mode
  :ensure t)

;; Flow Type
(add-hook 'web-mode-hook 'flow-minor-enable-automatically)


;; disable jshint since we prefer eslint checking
;;(setq-default flycheck-disabled-checkers
;;  (append flycheck-disabled-checkers
;;    '(javascript-jshint)))

;; customize flycheck temp file prefix
(setq-default flycheck-temp-prefix ".flycheck")

;;(flycheck-add-mode 'javascript-eslint 'rjsx-mode)
;;(flycheck-add-mode 'javascript-eslint 'web-mode)


;; use local eslint from node_modules before global
;; http://emacs.stackexchange.com/questions/21205/flycheck-with-file-relative-eslint-executable
(defun my/use-eslint-from-node-modules ()
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint (and root
                      (expand-file-name "node_modules/eslint/bin/eslint.js"
                                        root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))

(defun codefalling/reset-eslint-rc ()
    (let ((rc-path (if (projectile-project-p)
                       (concat (projectile-project-root) ".eslintrc"))))
      (if (file-exists-p rc-path)
          (progn
            (message rc-path)
          (setq flycheck-eslintrc rc-path)))))

(defun codefalling/reset-eslint-js ()
    (let ((rc-path (if (projectile-project-p)
                       (concat (projectile-project-root) ".eslintrc.js"))))
      (if (file-exists-p rc-path)
          (progn
            (message rc-path)
          (setq flycheck-eslintrc rc-path)))))

;;(add-hook 'flycheck-mode-hook 'codefalling/reset-eslint-rc)
(add-hook 'flycheck-mode-hook 'codefalling/reset-eslint-js)
(add-hook 'flycheck-mode-hook 'my/use-eslint-from-node-modules)


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


(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))


(use-package scss-mode
  :ensure t)
(add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))
(add-hook 'css-mode-hook (lambda ()
                           (set (make-local-variable 'company-backends) '(company-css))))

;; Better imenu
(add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode))
(add-hook 'js2-mode-hook #'js2-imenu-extras-mode)

(use-package js2-refactor
  :ensure t)

(use-package xref-js2
  :ensure t)

;; Add keybindings to run jest tests
(use-package mocha
  :ensure t)
(add-hook 'js2-mode-hook #'js2-refactor-mode)
(add-hook 'js2-mode-hook (lambda ()
                          (aggressive-indent-mode -1)
                           (local-set-key (kbd "C-c C-' C-t") 'mocha-test-at-point)
                           (local-set-key (kbd "C-c C-' C-f") 'mocha-test-file)))

(js2r-add-keybindings-with-prefix "C-c C-r")
(define-key js2-mode-map (kbd "C-k") #'js2r-kill)

;; js-mode (which js2 is based on) binds "M-." which conflicts with xref, so
;; unbind it.
(define-key js-mode-map (kbd "M-.") nil)

(add-hook 'js2-mode-hook (lambda ()
                           (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))


;; Turn off js2 mode errors & warnings (we lean on eslint/standard)
;; this also affects rjsx mode (yay!)
(setq js2-mode-show-parse-errors nil)
(setq js2-mode-show-strict-warnings nil)
(setq js2-basic-offset 2)

(add-to-list 'auto-mode-alist '("\\.js\\'" . react-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . react-mode))
(add-to-list 'auto-mode-alist '("\\.react.js\\'" . react-mode))
(add-to-list 'auto-mode-alist '("\\.test.js\\'" . react-mode))
(add-to-list 'auto-mode-alist '("\\.jest.js\\'" . react-mode))
(add-to-list 'auto-mode-alist '("\\index.android.js\\'" . react-mode))
(add-to-list 'auto-mode-alist '("\\index.ios.js\\'" . react-mode))
(add-to-list 'magic-mode-alist '("/\\*\\* @jsx React\\.DOM \\*/" . react-mode))
(add-to-list 'magic-mode-alist '("^\\/\\/ @flow" . react-mode))
(add-to-list 'magic-mode-alist '(".*import React" . react-mode))
(add-to-list 'magic-mode-alist '("^import React" . react-mode))

(define-derived-mode react-mode rjsx-mode "React")

(use-package prettier-js
  :ensure t)

(provide 'react-mode)

(add-hook 'react-mode-hook (lambda ()
                             (emmet-mode 0)
                             (linum-mode 1)
                             ;; See https://github.com/CestDiego/emmet-mode/commit/3f2904196e856d31b9c95794d2682c4c7365db23
                             (setq-local emmet-expand-jsx-className? t)
                             ;; Enable js-mode
                             (yas-activate-extra-mode 'js-mode)
                             ;; Force jsx content type
                             (web-mode-set-content-type "jsx")
                             ;;(aggressive-indent-mode -1)
                             ;;(global-aggressive-indent-mode -1)
                             (local-set-key (kbd "C-c C-i") 'aggressive-indent-mode)
                             ;; Don't auto-quote attribute values
                             (local-set-key (kbd "C-c C-t C-t") 'mocha-test-at-point)
                             (local-set-key (kbd "C-c C-t C-f") 'mocha-test-file)
                             (setq-local web-mode-enable-auto-quoting nil)))

(flycheck-add-mode 'javascript-eslint 'react-mode)

;; Turn off aggressive-indent-mode for jsx until I figure out how to fix it
(add-hook 'rjsx-mode-hook (lambda ()
                            (flow-minor-mode t)
                            (aggressive-indent-mode -1))
          (local-set-key (kbd "C-c C-' C-t") 'mocha-test-at-point)
          (local-set-key (kbd "C-c C-' C-f") 'mocha-test-file))

;;(add-hook 'react-mode (lambda ()
;;                            (my/setup-react-mode))))


;;; OrgMode
(use-package org-bullets
  :ensure t)
(add-hook 'org-mode-hook (lambda ()
  (org-bullets-mode 1)))

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


;;; Geiser (Schemes)
(use-package geiser
  :ensure t)

;;; SLIME (Lisp)
;;; Borrowed from Portacle
(load (expand-file-name "~/quicklisp/slime-helper.el"))
(setq inferior-lisp-program "/usr/bin/sbcl")

(add-hook 'slime-repl-mode-hook (lambda () (linum-mode -1)))


;;; Clojure
;; Enable paredit for Clojure
(add-hook 'clojure-mode-hook 'enable-paredit-mode)

;; This is useful for working with camel-case tokens, like names of
;; Java classes (e.g. JavaClassName)
(add-hook 'clojure-mode-hook 'subword-mode)
(add-hook 'clojure-mode-hook 'cider-mode)

;; A little more syntax highlighting
(use-package clojure-mode-extra-font-locking
  :ensure t)

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

;;;;
;; Cider
;;;;
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

(eval-after-load 'cider
  '(progn
     (define-key clojure-mode-map (kbd "C-c C-v") 'cider-start-http-server)
     (define-key clojure-mode-map (kbd "C-M-r") 'cider-refresh)
     (define-key clojure-mode-map (kbd "C-c u") 'cider-user-ns)
     (define-key cider-mode-map (kbd "C-c u") 'cider-user-ns)))


;;; Haskell
;; Haskell Config
(use-package intero
  :ensure t
  :init
  (intero-global-mode 1)
)


;;; Ocaml
(require 'opam-user-setup "~/.emacs.d/opam-user-setup.el")

;;; Rust
;;; Rust config
(use-package racer
  :ensure t)
;; run rustfmt on save
(use-package rust-mode
  :ensure t)
(setq rust-format-on-save t)

(setq rust-racer-src-path "~/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/src")

(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)
(add-hook 'rust-mode-hook #'company-mode)
(add-hook 'rust-mode-hook (lambda ()
  (add-to-list 'company-backends 'company-racer)))

(define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
(setq company-tooltip-align-annotations t)

;;; Golang
(use-package go-autocomplete
  :ensure t)

(use-package company-go
  :ensure t)

(use-package go-autocomplete
  :ensure t)

(use-package company-go
  :ensure t)

(use-package go-mode
  :ensure t
  :init
  ; Use goimports instead of go-fmt
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
  (lambda ()
    (set (make-local-variable 'company-backends) '(company-go))
    (company-mode))
)
;;; C/C++
;;; C/C++ config stuffs
(use-package irony
  :ensure t)

(use-package rtags
  :ensure t)
(defun my-irony-mode-on ()
  ;; avoid enabling irony-mode in modes that inherits c-mode, e.g: php-mode
  (when (member major-mode irony-supported-major-modes)
    (irony-mode 1)))

(add-hook 'c++-mode-hook 'my-irony-mode-on)
(add-hook 'c-mode-hook 'my-irony-mode-on)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

(cmake-ide-setup)


;;; Python
(use-package elpy
  :ensure t)
(elpy-enable)

(defun my/python-mode-hook ()
  (add-to-list 'company-backends 'company-jedi)
  (jedi:setup))

(add-hook 'python-mode-hook 'my/python-mode-hook)


;;; Ruby
(use-package seeing-is-believing
  :ensure t
  :config
  (setq seeing-is-believing-prefix "C-.")
  :init
  (add-hook 'ruby-mode-hook 'seeing-is-believing)
)

(use-package ruby-test-mode
  :ensure t
  :init
  (add-hook 'ruby-mode-hook 'ruby-test-mode)
  (add-hook 'ruby-mode-hook 'rinari-minor-mode)
)



;;; Terminal

(add-hook 'term-mode-hook (lambda () (linum-mode -1)))



;;; Love2D

(defvar love2d-program "/Applications/love.app/Contents/MacOS/love")

(defun love2d-launch-current ()
  (interactive)
  (let ((app-root (locate-dominating-file (buffer-file-name) "main.lua")))
    (if app-root
        (shell-command (format "%s %s &" love2d-program app-root))
      (error "main.lua not found"))))



;;; Feeds

(use-package elfeed
  :ensure t)

;;(use-package elfeed-org
;;  :ensure t
;;  :config
;;  (elfeed-org)
;;  (setq rmh-elfeed-org-files (list "/home/jsral/.emacs.d/feeds/elfeed.org")))


;;; Misc

;; Changes all yes/no questions to y/n type
(fset 'yes-or-no-p 'y-or-n-p)

;; shell scripts
(setq-default sh-basic-offset 2)
(setq-default sh-indentation 2)

;; No need for ~ files when editing
(setq create-lockfiles nil)

;; Go straight to scratch buffer on startup
(setq inhibit-startup-message t)

;; Magit
(use-package magit
  :ensure t)
(global-set-key (kbd "C-x g") 'magit-status)


;; Turn off numbers for eww browser
(add-hook 'eww-mode-hook (lambda () (linum-mode -1)))

;; Turn of linum mode for games
(add-hook 'tetris-mode-hook (lambda () (linum-mode -1)))



;;; Elisp

;; Automatically load paredit when editing a lisp file
;; More at http://www.emacswiki.org/emacs/ParEdit
(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)

;; eldoc-mode shows documentation in the minibuffer when writing code
;; http://www.emacswiki.org/emacs/ElDoc
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)



;;; Godot
;;(require 'godot-gdscript)
;;(require 'company-godot-gdscript)

;; Company mode completions
;;(add-hook 'godot-gdscript-mode-hook
;;          (lambda ()
;;            (make-local-variable 'company-backends)
;;            (add-to-list 'company-backends 'company-godot-gdscript)
;;            (setq-local company-minimum-prefix-length 1)
;;            (setq-local company-async-timeout 10)
;;            (setq-local company-idle-delay 0.2)
;;            (company-mode)))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   (vector "#657b83" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#073642"))
 '(company-quickhelp-color-background "#4F4F4F")
 '(company-quickhelp-color-foreground "#DCDCCC")
 '(custom-safe-themes
   (quote
    ("190a9882bef28d7e944aa610aa68fe1ee34ecea6127239178c7ac848754992df" "a4df5d4a4c343b2712a8ed16bc1488807cd71b25e3108e648d4a26b02bc990b3" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" "c48551a5fb7b9fc019bf3f61ebf14cf7c9cdca79bcb2a4219195371c02268f11" "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" default)))
 '(fci-rule-color "#eee8d5")
 '(global-linum-mode t)
 '(linum-format " %7i ")
 '(nrepl-message-colors
   (quote
    ("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3")))
 '(package-selected-packages
   (quote
    (prettier-js zenburn-theme geiser dracula-theme yaml-mode yafolding xref-js2 window-numbering window-number web-mode use-package tagedit sublime-themes smex smartparens slime seeing-is-believing scss-mode ruby-test-mode rtags rjsx-mode rinari rainbow-delimiters racer projectile paredit org-bullets multi-term mocha magit linum-relative key-chord js2-refactor jedi intero ido-ubiquitous gruvbox-theme go-autocomplete git-gutter git flycheck-rust flycheck-irony flycheck-crystal flow-minor-mode fiplr expand-region exec-path-from-shell emmet-mode elpy elfeed dashboard crystal-mode counsel company-racer company-quickhelp company-go color-theme-sanityinc-solarized cmake-ide clojure-mode-extra-font-locking cider bundler avy autopair atom-one-dark-theme aggressive-indent ag)))
 '(pdf-view-midnight-colors (quote ("#DCDCCC" . "#383838")))
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#dc322f")
     (40 . "#cb4b16")
     (60 . "#b58900")
     (80 . "#859900")
     (100 . "#2aa198")
     (120 . "#268bd2")
     (140 . "#d33682")
     (160 . "#6c71c4")
     (180 . "#dc322f")
     (200 . "#cb4b16")
     (220 . "#b58900")
     (240 . "#859900")
     (260 . "#2aa198")
     (280 . "#268bd2")
     (300 . "#d33682")
     (320 . "#6c71c4")
     (340 . "#dc322f")
     (360 . "#cb4b16"))))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-defun 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'widen 'disabled nil)
