;;;
;;; Enable arbitrary customised local configuration, while minimising
;;; upstream merge conflict potential. Possibly this is not strictly
;;; necessary if one understands git better than I do at this point.
;;;
;;; See init-preload-local.el for pre-loaded local customisations,
;;; which include specification of user's local lisp directory.
;;;

(require-package 'ess)

(require 'ess-init)

(provide 'init-local)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
