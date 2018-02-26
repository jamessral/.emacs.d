
;;; -*- lexical-binding: t -*-

(defun tangle-init ()
  "If the current buffer is 'init.org' the code-blocks are
   tangled, and the tangled file is compiled."
  (when (equal (buffer-file-name)
           (expand-file-name (concat user-emacs-directory "init.org")))
  ;; Avoid running hooks when tangling.
  (let ((prog-mode-hook nil))
     (org-babel-tangle)
     (byte-compile-file (concat user-emacs-directory "init.el")))))

(add-hook 'after-save-hook 'tangle-init)

(add-hook
  'after-init-hook
  (lambda ()
    (let ((private-file (concat user-emacs-directory "private.el")))
      (when (file-exists-p private-file)
        (load-file private-file)))))

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

;; (setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
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

;; The packages you want installed. You can also install these
;; manually with M-x package-install
;; Add in your own as you wish:
(defvar my-packages
  '(ruby-mode
    rinari
    bundler
    js2-mode
    js2-refactor
    rjsx-mode
    elpy
    web-mode
    autopair
    company
    git
    flycheck
    ;;smartparens
    multi-term
    ;; go-mode
    paredit
    ivy
    avy
    slime
    multiple-cursors
    expand-region
    ido-ubiquitous
    smex
    projectile
    rainbow-delimiters
    tagedit
    magit
    exec-path-from-shell
    aggressive-indent
    color-theme-sanityinc-solarized
    atom-one-dark-theme
    dashboard
    company-quickhelp
    smartparens
    key-chord
    yafolding
    counsel
    swiper
    ag
    xref-js2
    mocha
    rust-mode
    irony
    rtags
    cmake-ide
    seeing-is-believing
    elpy
    window-numbering
    fiplr
    jedi
    racer
    flycheck-rust
    flow-minor-mode
    yaml-mode
    emmet-mode
    ))

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-copy-env "GOPATH")
  (exec-path-from-shell-copy-env "NPM_TOKEN")
  (exec-path-from-shell-initialize))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; Don't wrap lines
(setq-default truncate-lines 1)

(require 'flycheck)
;; turn on flychecking globally
(add-hook 'after-init-hook #'global-flycheck-mode)

(require 'git)
(add-to-list 'load-path "~/.emacs.d/vendor")

(setq make-backup-files nil) ; stop creating backup~ files
(setq auto-save-default nil) ; stop creating #autosave# files

;; Show tabs as 4 spaces
(setq tab-width 4)

;; Customizations relating to editing a buffer.
;; Key binding to use "hippie expand" for text autocompletion
;; http://www.emacswiki.org/emacs/HippieExpand

;; Use Asci for compile mode (running tests)
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region compilation-filter-start (point))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
(global-set-key (kbd "C-<tab>") 'company-complete)


(global-set-key (kbd "M-/") 'hippie-expand)

(global-set-key (kbd "C-t") 'transpose-chars)

;; Use Paredit to allow slurping
(global-set-key (kbd "C-)") 'paredit-forward-slurp-sexp)

;; Expand Region
(require 'expand-region)
(global-set-key (kbd "C-@") 'er/expand-region)

;; Case sensitive company mode
(setq company-dabbrev-downcase nil)

(global-aggressive-indent-mode)

;; Snippets
(require 'yasnippet)
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

(require 'smartparens)
(require 'smartparens-config)
(smartparens-global-mode 1)
(electric-pair-mode 1)

;; Highlight current line
;;(global-linum-mode t)
(when (window-system) global-hl-line-mode 1)


;; Use Key Chords
(require 'key-chord)
(key-chord-mode 1)

;; Multiple Cursors
(require 'multiple-cursors)
(global-set-key (kbd "C-c C-l") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C->") 'mc/mark-next-like-this-word)
(global-set-key (kbd "C-c C-<") 'mc/mark-previous-like-this-word)

;; Folding
(require 'yafolding)
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
(require 'saveplace)
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

;; These customizations make it easier for you to navigate files,
;; switch buffers, and choose options from the minibuffer.


;; "When several buffers visit identically-named files,
;; Emacs must give the buffers distinct names. The usual method
;; for making buffer names unique adds ‘<2>’, ‘<3>’, etc. to the end
;; of the buffer names (all but one of them).
;; The forward naming method includes part of the file's directory
;; name at the beginning of the buffer name
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Uniquify.html
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)


;; Use windmove keys to use shift+arrows for switching windows
(windmove-default-keybindings)
(window-numbering-mode 1)
;; Turn on recent file mode so that you can more easily switch to
;; recently edited files when you first start emacs
(setq recentf-save-file (concat user-emacs-directory ".recentf"))
(require 'recentf)
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
(require 'avy)
(global-set-key (kbd "C-:") 'avy-goto-char)
(global-set-key (kbd "C-'") 'avy-goto-char-2)
(global-set-key (kbd "M-g f") 'avy-goto-line)
(global-set-key (kbd "M-g w") 'avy-goto-word-1)

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

;; These customizations change the way emacs looks and disable/enable
;; some user interface elements. Some useful customizations are
;; commented out, and begin with the line "CUSTOMIZE". These are more
;; a matter of preference and may require some fiddling to match your
;; preferences

;; Turn off the menu bar at the top of each frame because it's distracting
(menu-bar-mode -1)
(tool-bar-mode -1)


;; Show dashboard on startup
(require 'dashboard)
(dashboard-setup-startup-hook)
;; Or if you use use-package
(use-package dashboard
  :config
  (dashboard-setup-startup-hook))

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
(global-linum-mode 1)
(global-set-key (kbd "C-c SPC n") 'global-linum-mode)

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

(defadvice load-theme
    ;; Make sure to disable current colors before switching
    (before theme-dont-propagate activate)
  (mapc #'disable-theme custom-enabled-themes))


(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(add-to-list 'load-path "~/.emacs.d/themes")
(load-theme 'gruvbox-dark-hard t)

(defun load-one-dark ()
  (interactive)
  (load-theme 'gruvbox-dark-hard t))

(defun load-solarized-light ()
  (interactive)
  (load-theme 'sanityinc-solarized-light t))

(global-set-key (kbd "C-c SPC l") 'load-solarized-light)
(global-set-key (kbd "C-c SPC d") 'load-one-dark)

;; Use Ligatures
;; (global-prettify-symbols-mode)
(when (display-graphic-p) (set-face-attribute 'default nil :font "Hasklug Nerd Font"))
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

;;; Javascript stuff

(require 'web-mode)

;; Flow Type
(add-hook 'web-mode-hook 'flow-minor-enable-automatically)


;; disable jshint since we prefer eslint checking
(setq-default flycheck-disabled-checkers
  (append flycheck-disabled-checkers
    '(javascript-jshint)))

;; customize flycheck temp file prefix
(setq-default flycheck-temp-prefix ".flycheck")

(flycheck-add-mode 'javascript-eslint 'rjsx-mode)
(flycheck-add-mode 'javascript-eslint 'web-mode)


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

;; Use regular JS2-mode for test files to help test runner
(add-to-list 'auto-mode-alist '("\\.jest.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.test.js\\'" . js2-mode))

(add-to-list 'auto-mode-alist '("\\.scss\\'" . css-mode))
(add-hook 'css-mode-hook (lambda ()
                           (set (make-local-variable 'company-backends) '(company-css))))

;; Better imenu
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-hook 'js2-mode-hook #'js2-imenu-extras-mode)

(require 'js2-refactor)

(require 'xref-js2)

;; Add keybindings to run jest tests
(require 'mocha)
(add-hook 'js2-mode-hook #'js2-refactor-mode)
(add-hook 'js2-mode-hook (lambda ()
                           (aggressive-indent-mode -1)
                           (local-set-key (kbd "C-c C-t C-t") 'mocha-test-at-point)
                           (local-set-key (kbd "C-c C-t C-f") 'mocha-test-file)))

(js2r-add-keybindings-with-prefix "C-c C-r")
(define-key js2-mode-map (kbd "C-k") #'js2r-kill)

;; js-mode (which js2 is based on) binds "M-." which conflicts with xref, so
;; unbind it.
(define-key js-mode-map (kbd "M-.") nil)

(add-hook 'js2-mode-hook (lambda ()
                           (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))

(add-hook 'js2-mode 'emmet-mode)

;; JS test runner
;; (add-hook 'js2-mode-hook
;;           (lambda ()
;;             (set (make-local-variable 'testing-command)
;;                  (test-javascript))))

(define-derived-mode react-mode web-mode "React")
(add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . rjsx-mode))
(add-to-list 'auto-mode-alist '("\\.react.js\\'" . rjsx-mode))
(add-to-list 'auto-mode-alist '("\\.test.js\\'" . rjsx-mode))
(add-to-list 'auto-mode-alist '("\\.jest.js\\'" . rjsx-mode))
(add-to-list 'auto-mode-alist '("\\index.android.js\\'" . rjsx-mode))
(add-to-list 'auto-mode-alist '("\\index.ios.js\\'" . rjsx-mode))
(add-to-list 'magic-mode-alist '("/\\*\\* @jsx React\\.DOM \\*/" . rjsx-mode))
(add-to-list 'magic-mode-alist '("^\\/\\/ @flow" . rjsx-mode))
(add-to-list 'magic-mode-alist '(".*import React" . rjsx-mode))
(add-to-list 'magic-mode-alist '("^import React" . rjsx-mode))

(provide 'react-mode)

(add-hook 'react-mode-hook (lambda ()
                             (emmet-mode 0)
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
                             (emmet-mode 0)
                             (flow-minor-mode t)
                             ;; See https://github.com/CestDiego/emmet-mode/commit/3f2904196e856d31b9c95794d2682c4c7365db23
                             (setq-local emmet-expand-jsx-className? t)
                            (aggressive-indent-mode -1))
                            (local-set-key (kbd "C-c C-t C-t") 'mocha-test-at-point)
                             (local-set-key (kbd "C-c C-t C-f") 'mocha-test-file))
)
(add-hook 'react-mode (lambda ()
                            (my/setup-react-mode))))

;; Turn off js2 mode errors & warnings (we lean on eslint/standard)
;; this also affects rjsx mode (yay!)
(setq js2-mode-show-parse-errors nil)
(setq js2-mode-show-strict-warnings nil)

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

;;; Borrowed from Portacle
(load (expand-file-name "~/quicklisp/slime-helper.el"))
(setq inferior-lisp-program "/usr/local/bin/sbcl")

(add-hook 'slime-repl-mode-hook (lambda () (linum-mode -1)))

;;; Haskell Config
(require 'intero)
(intero-global-mode 1)

;;; Rust config
;; run rustfmt on save
(require 'rust-mode)
(setq rust-format-on-save t)

(setq rust-racer-src-path "~/.rustup/toolchains/nightly-x86_64-apple-darwin/lib/rustlib/src/rust/src")

(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'rust-mode-hook #'flycheck-rust-setup)
(add-hook 'racer-mode-hook #'eldoc-mode)
(add-hook 'racer-mode-hook #'company-mode)

(define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
(setq company-tooltip-align-annotations t)

;;; C/C++ config stuffs
(require 'irony)
(require 'rtags)
(defun my-irony-mode-on ()
  ;; avoid enabling irony-mode in modes that inherits c-mode, e.g: php-mode
  (when (member major-mode irony-supported-major-modes)
    (irony-mode 1)))

(add-hook 'c++-mode-hook 'my-irony-mode-on)
(add-hook 'c-mode-hook 'my-irony-mode-on)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

(cmake-ide-setup)

(elpy-enable)

(defun my/python-mode-hook ()
  (add-to-list 'company-backends 'company-jedi)
  (jedi:setup))

(add-hook 'python-mode-hook 'my/python-mode-hook)

(require 'seeing-is-believing)

(setq seeing-is-believing-prefix "C-.")
(add-hook 'ruby-mode-hook 'seeing-is-believing)

(autoload 'inf-ruby-minor-mode "inf-ruby" "Run an inferior Ruby process" t)
(add-hook 'ruby-mode-hook 'inf-ruby-minor-mode)

(require 'ruby-test-mode)
(add-hook 'ruby-mode-hook 'ruby-test-mode)
(add-hook 'ruby-mode-hook 'rinari-minor-mode)

(add-hook 'term-mode-hook (lambda () (linum-mode -1)))

(defvar love2d-program "/Applications/love.app/Contents/MacOS/love")

(defun love2d-launch-current ()
  (interactive)
  (let ((app-root (locate-dominating-file (buffer-file-name) "main.lua")))
    (if app-root
        (shell-command (format "%s %s &" love2d-program app-root))
      (error "main.lua not found"))))

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
(global-set-key (kbd "C-x g") 'magit-status)


;; Turn off numbers for eww browser
(add-hook 'eww-mode-hook (lambda () (linum-mode -1)))

;; Turn of linum mode for games
(add-hook 'tetris-mode-hook (lambda () (linum-mode -1)))

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

(require 'godot-gdscript)
(require 'company-godot-gdscript)

;; Company mode completions
(add-hook 'godot-gdscript-mode-hook
          (lambda ()
            (make-local-variable 'company-backends)
            (add-to-list 'company-backends 'company-godot-gdscript)
            (setq-local company-minimum-prefix-length 1)
            (setq-local company-async-timeout 10)
            (setq-local company-idle-delay 0.2)
            (company-mode)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   ["#272822" "#F92672" "#A6E22E" "#E6DB74" "#66D9EF" "#FD5FF0" "#A1EFE4" "#F8F8F2"])
 '(beacon-color "#d54e53")
 '(coffee-tab-width 2)
 '(custom-safe-themes
   (quote
    ("a7e7804313dbf827a441c86a8109ef5b64b03011383322cbdbf646eb02692f76" "0be964eabe93f09be5a943679ced8d98e08fe7a92b01bf24478e56eee7b6b21d" "840db7f67ce92c39deb38f38fbc5a990b8f89b0f47b77b96d98e4bf400ee590a" "7c49651e62ee04b5f6d8fca78bca6f31730d982901fc1e1c651e464fa0ecfea4" "fd24b2c570dbd976e17a63ba515967600acb7d2f9390793859cb82f6a2d5dacd" "c63a789fa2c6597da31f73d62b8e7fad52c9420784e6ec34701ae8e8f00071f6" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "25c06a000382b6239999582dfa2b81cc0649f3897b394a75ad5a670329600b45" "c614d2423075491e6b7f38a4b7ea1c68f31764b9b815e35c9741e9490119efc0" "0961d780bd14561c505986166d167606239af3e2c3117265c9377e9b8204bf96" "15348febfa2266c4def59a08ef2846f6032c0797f001d7b9148f30ace0d08bcf" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "8ed752276957903a270c797c4ab52931199806ccd9f0c3bb77f6f4b9e71b9272" "732b807b0543855541743429c9979ebfb363e27ec91e82f463c91e68c772f6e3" "2e1d19424153d41462ad31144549efa41f55dacda9b76571f73904612b15fd0a" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "6ee6f99dc6219b65f67e04149c79ea316ca4bcd769a9e904030d38908fd7ccf9" "3629b62a41f2e5f84006ff14a2247e679745896b5eaa1d5bcfbc904a3441b0cd" "f6a935e77513ba40014aa8467c35961fdb1fc936fa48407ed437083a7ad932de" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "611e38c2deae6dcda8c5ac9dd903a356c5de5b62477469133c89b2785eb7a14d" "6f11ad991da959fa8de046f7f8271b22d3a97ee7b6eca62c81d5a917790a45d9" "365d9553de0e0d658af60cff7b8f891ca185a2d7ba3fc6d29aadba69f5194c7f" "4182c491b5cc235ba5f27d3c1804fc9f11f51bf56fb6d961f94788be034179ad" "b81bfd85aed18e4341dbf4d461ed42d75ec78820a60ce86730fc17fc949389b2" "98cc377af705c0f2133bb6d340bf0becd08944a588804ee655809da5d8140de6" "7f3ef7724515515443f961ef87fee655750512473b1f5bf890e2dc7e065f240c" "86e2d09ebcfff3b7ec95543bce5a163384579a2bf2e2a81bfba8908b7a0c44df" "2d16a5d1921feb826a6a9b344837c1ab3910f9636022fa6dc1577948694b7d84" "a4c9e536d86666d4494ef7f43c84807162d9bd29b0dfd39bdf2c3d845dcc7b2e" "5310b88333fc64c0cb34a27f42fa55ce371438a55f02ac7a4b93519d148bd03d" "3b0a350918ee819dca209cec62d867678d7dac74f6195f5e3799aa206358a983" "b571f92c9bfaf4a28cb64ae4b4cdbda95241cd62cf07d942be44dc8f46c491f4" "0e0c37ee89f0213ce31205e9ae8bce1f93c9bcd81b1bcda0233061bb02c357a8" "f2057733672d3b119791f5b7d1a778bf8880121f22ea122a21d221b45081f49e" "0eef522d30756a80b28333f05c7eed5721f2ba9b3eaaff244ea4c6f6a1b8ac62" "5673c365c8679addfb44f3d91d6b880c3266766b605c99f2d9b00745202e75f6" "8d3c5e9ba9dcd05020ccebb3cc615e40e7623b267b69314bdb70fe473dd9c7a8" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "84f35ac02435aa65aef82f510756ab21f173624fcb332dd81e3c9f2adaf6b85b" "9b349c5c09056c292d635725cecf587b44780e061c3d2477383d7eb25d4cdd68" "6b1e6953a08acf12843973ec25d69dbfa1a53d869f649dc991a56fbdf0d7eb9e" "363de9fd1194546e7461bdb766793b1442c222376faa8254b8eafaf25afe48dc" "4b3c24a1b13f29c6c6926c194eb8aa76e4ddab7a487cd171043b88ac1f3b4481" "f78de13274781fbb6b01afd43327a4535438ebaeec91d93ebdbba1e3fba34d3c" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" "c7a9a68bd07e38620a5508fef62ec079d274475c8f92d75ed0c33c45fbe306bc" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "74278d14b7d5cf691c4d846a4bbf6e62d32104986f104c1e61f718f9669ec04b" "8b30636c9a903a9fa38c7dcf779da0724a37959967b6e4c714fdc3b3fe0b8653" "5999e12c8070b9090a2a1bbcd02ec28906e150bb2cdce5ace4f965c76cf30476" "770181eda0f652ef9293e8db103a7e5ca629c516ca33dfa4709e2c8a0e7120f3" "1db337246ebc9c083be0d728f8d20913a0f46edc0a00277746ba411c149d7fe5" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "3fd0fda6c3842e59f3a307d01f105cce74e1981c6670bb17588557b4cebfe1a7" "003a9aa9e4acb50001a006cfde61a6c3012d373c4763b48ceb9d523ceba66829" "3eb93cd9a0da0f3e86b5d932ac0e3b5f0f50de7a0b805d4eb1f67782e9eb67a4" "4f5bb895d88b6fe6a983e63429f154b8d939b4a8c581956493783b2515e22d6d" "8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" "0820d191ae80dcadc1802b3499f84c07a09803f2cb90b343678bdb03d225b26b" "ad950f1b1bf65682e390f3547d479fd35d8c66cafa2b8aa28179d78122faa947" "32e3693cd7610599c59997fee36a68e7dd34f21db312a13ff8c7e738675b6dfc" "5ee12d8250b0952deefc88814cf0672327d7ee70b16344372db9460e9a0e3ffc" "52588047a0fe3727e3cd8a90e76d7f078c9bd62c0b246324e557dfa5112e0d0c" "557c283f4f9d461f897b8cac5329f1f39fac785aa684b78949ff329c33f947ec" "9e54a6ac0051987b4296e9276eecc5dfb67fdcd620191ee553f40a9b6d943e78" "7f1263c969f04a8e58f9441f4ba4d7fb1302243355cb9faecb55aec878a06ee9" "1157a4055504672be1df1232bed784ba575c60ab44d8e6c7b3800ae76b42f8bd" "cf08ae4c26cacce2eebff39d129ea0a21c9d7bf70ea9b945588c1c66392578d1" default)))
 '(debug-on-error nil)
 '(fci-rule-color "#eee8d5")
 '(flycheck-color-mode-line-face-to-color (quote mode-line-buffer-id))
 '(frame-background-mode nil)
 '(gdscript-tab-width 4)
 '(gdscript-tabs-mode nil)
 '(global-aggressive-indent-mode t)
 '(intero-global-mode t nil (intero))
 '(jdee-db-active-breakpoint-face-colors (cons "#f0f0f0" "#4078f2"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#f0f0f0" "#50a14f"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#f0f0f0" "#9ca0a4"))
 '(js-indent-level 2)
 '(js-switch-indent-offset 2)
 '(magit-use-overlays nil)
 '(neo-theme (quote nerd) t)
 '(neo-window-width 45)
 '(org-fontify-done-headline t)
 '(org-fontify-quote-and-verse-blocks t)
 '(org-fontify-whole-heading-line t)
 '(package-selected-packages
   (quote
    (dashboard yaml-mode emmet-mode mocha doom ansi darkroom json-mode company-lua company-web fill-column-indicator column-marker recompile-on-save auto-compile zen-mode evil-collection all-the-icons-ivy rtags cmake-ide dante flycheck-elm elm-yasnippets irony-eldoc css-eldoc eldoc-overlay eldoc-extension go-eldoc avy-flycheck toml-mode color-theme-cobalt common-lisp-snippets base16-theme doremi-cmd doremi-mac icicles flatui-theme spacemacs-theme rjsx-mode ag darkokai-theme helm-themes sublime-themes solarized-theme js-doc indium yarn-mode php-eldoc php-auto-yasnippets php-mode elixir-yasnippets material-theme company-quickhelp nodejs-repl xref-js2 smex ido-ubiquitous paredit company-ghc cargo intero racer flycheck-irony company-irony irony go-snippets zerodark-theme pacmacs spotify smartparens company-jedi avy fzf eyebrowse flow-minor-mode wrap-region mu4e-alert rinari ruby-test-mode seeing-is-believing psc-ide flycheck-purescript purescript-mode flymake-ruby atom-one-dark-theme company-racer restclient railscasts-theme molokai-theme key-chord multiple-cursors expand-region yafolding slack utop yasnippet-bundle magit gited git company-go go-autocomplete flymake-rust flymake-lua luarocks lua-mode tao-theme all-the-icons-dired haxe-imports haxe-mode gruvbox-theme meghanada typescript tide stack-mode company-cmake company-c-headers alchemist haskell-tab-indent haskell-emacs xkcd w3m company-erlang window-numbering elpy multi-term elm-mode doom-themes slime use-package fiplr flycheck-rust flycheck-ycmd flycheck-ocaml flycheck-clojure markdown-mode+ god-mode color-theme-sanityinc-solarized evil-leader haskell-mode company-tern relative-line-numbers clojurescript-mode racket-mode omnisharp haml-mode company-flow eslint-fix autopair yasnippet web-mode tagedit scss-mode rustfmt rust-mode ruby-tools ruby-end ruby-dev ruby-block ruby-additional rubocop rsense robe rbenv rainbow-mode rainbow-delimiters projectile powerline popup neotree monokai-theme macrostep jsx-mode javascript highlight-indentation go-mode exec-path-from-shell epc enh-ruby-mode doremi company-inf-ruby color-theme-solarized color-theme-sanityinc-tomorrow color-theme-monokai clojure-mode-extra-font-locking cider)))
 '(server-mode t)
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
 '(vc-annotate-very-old-color nil)
 '(yas-global-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background nil))))
 '(makefile-space ((t nil))))
;; ## added by OPAM user-setup for emacs / base ## 56ab50dc8996d2bb95e7856a6eddb17b ## you can edit, but keep this line
(require 'opam-user-setup "~/.emacs.d/opam-user-setup.el")
;; ## end of OPAM user-setup addition for emacs / base ## keep this line
