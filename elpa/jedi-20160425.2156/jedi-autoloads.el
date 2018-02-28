;;; jedi-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

<<<<<<< HEAD
;;;### (autoloads nil "jedi" "jedi.el" (23180 60224 466549 765000))
=======
;;;### (autoloads nil "jedi" "jedi.el" (23181 45170 976609 937000))
>>>>>>> b39d0218fae6f6e1096f577baf6c61a0073cae73
;;; Generated autoloads from jedi.el

(autoload 'jedi:ac-setup "jedi" "\
Add Jedi AC sources to `ac-sources'.

If auto-completion is all you need, you can call this function instead
of `jedi:setup', like this::

   (add-hook 'python-mode-hook 'jedi:ac-setup)

Note that this function calls `auto-complete-mode' if it is not
already enabled, for people who don't call `global-auto-complete-mode'
in their Emacs configuration.

\(fn)" t nil)

(autoload 'jedi:complete "jedi" "\
Complete code at point.

\(fn &key (EXPAND ac-expand-on-auto-complete))" t nil)

(autoload 'jedi:auto-complete-mode "jedi" "\


\(fn)" nil nil)

(setq jedi:setup-function #'jedi:ac-setup jedi:mode-function #'jedi:auto-complete-mode)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; jedi-autoloads.el ends here
