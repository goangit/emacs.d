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

(require 'init-local-packages)
(require 'init-local-polymode)
(require 'init-local-ess)
(require 'init-local-python)
(require 'init-local-org)

;; (require 'init-local-ensime)
;; (require-package 'jabber)
;; (require-package 'julia-mode)
;; (require 'init-local-misc)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:

(provide 'init-local)
;;; init-local.el ends here
