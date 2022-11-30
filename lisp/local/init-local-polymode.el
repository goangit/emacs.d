;;; Package --- Summary
;;;
;;; Ensime configuration
;;;
;;; Commentary:
;;;
;;; polymode replaces noweb and ess-noweb
;;;
;;; Code:

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


;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:

(provide 'init-local-polymode)
;;; init-local-ensime.el ends here
