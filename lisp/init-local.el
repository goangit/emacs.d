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
             '("use-package" . "melpa-stable"))


(eval-when-compile 
  ;; (add-to-list 'load-path "<path where use-package is installed>")
  (require 'use-package)) ;; https://github.com/jwiegley/use-package

(use-package poly-markdown
  :pin melpa-stable
  :ensure t)

(use-package poly-org
  :pin melpa-stable
  :ensure t)

(use-package poly-noweb
  :pin melpa-stable
  :ensure t)

(use-package poly-R
  :pin melpa-stable
  :ensure t)

(use-package ess
  :pin melpa-stable
  :ensure t
  :init (require 'ess-site))

(use-package ensime
  :pin melpa-stable
  :ensure t)


;; (require 'init-local-ensime)
(require 'init-local-ess)
;;(require-package 'jabber)
;;(require-package 'julia-mode)
(require 'init-local-misc)
;;(require 'init-local-python)
(require 'init-local-org)


;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:

(provide 'init-local)
;;; init-local.el ends here
