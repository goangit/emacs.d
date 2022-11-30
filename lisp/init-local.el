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

(global-linum-mode t)               ;; enable line numbers globally
;; (global-set-key (kbd "M-3") '(lambda () (interactive) (insert "#")))

(add-to-list 'load-path (expand-file-name "lisp/local" user-emacs-directory))

;;(require 'init-local-packages)
;;(require 'init-local-polymode)
(require 'init-local-ess)
(require 'init-local-fortran)
;;(require 'init-local-kotlin)
;;(require 'init-local-python)
;;(require 'init-local-org)

;; (require 'init-local-ensime)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:

(provide 'init-local)
;;; init-local.el ends here
