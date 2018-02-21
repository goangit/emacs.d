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
;;; which include specification of user's local lisp directory.
;;;
;;;
;;; Code:

(require-package 'ess)
;;(require-package 'flycheck-kotlin)
;;(require-package 'jabber)
;;(require-package 'julia-mode)
;;(require-package 'kotlin-mode)

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
