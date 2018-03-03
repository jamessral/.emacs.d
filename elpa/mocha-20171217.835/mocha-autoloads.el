;;; mocha-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "mocha" "mocha.el" (23195 2103 632845 691000))
;;; Generated autoloads from mocha.el

(autoload 'mocha-test-project "mocha" "\
Test the current project.

\(fn)" t nil)

(autoload 'mocha-debug-project "mocha" "\
Debug the current project.

\(fn)" t nil)

(autoload 'mocha-test-file "mocha" "\
Test the current file.

\(fn)" t nil)

(autoload 'mocha-debug-file "mocha" "\
Debug the current file.

\(fn)" t nil)

(autoload 'mocha-test-at-point "mocha" "\
Test the current innermost 'it' or 'describe' or the file if none is found.

\(fn)" t nil)

(autoload 'mocha-debug-at-point "mocha" "\
Debug the current innermost 'it' or 'describe' or the file if none is found.

\(fn)" t nil)

(autoload 'mocha-toggle-imenu-function "mocha" "\
Toggle the use mocha-specific imenu function.

\(fn)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; mocha-autoloads.el ends here
