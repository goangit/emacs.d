;;; Package --- Summary
;;;
;;; Enable arbitrary customised local configuration, while minimising
;;; upstream merge conflict potential.
;;;
;;;
;;; Commentary:
;;;
;;; Possibly this is not strictly necessary if one understands git
;;; better than I do at this point.
;;;
;;; See init-preload-local.el for pre-loaded local customisations,
;;; which include specification of user's local Lisp directory.
;;;
;;;
;;; Code:

(add-to-list 'package-archives
	     (cons "melpa-stable" "http://stable.melpa.org/packages/") t)

(add-to-list 'package-pinned-packages
             '("ensime" . "melpa-stable"))

(require-package 'ensime)
(use-package ess :init (require 'ess-site))
;;(require-package 'ess) ;; older, probably deprecated
;;(require-package 'jabber)
;;(require-package 'julia-mode)

(require 'init-local-ensime)
(require 'init-local-ess)
(require 'init-local-misc)
(require 'init-local-python)
(require 'init-local-org)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:

(provide 'init-local)
;;; init-local.el ends here
