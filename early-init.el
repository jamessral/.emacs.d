;;; early-init.el --- early bird  -*- no-byte-compile: t -*-
(setq load-prefer-newer t)
(package-initialize)
(require 'auto-compile)
(auto-compile-on-load-mode)
(auto-compile-on-save-mode)
;;; early-init.el ends here
