;;; Package --- Summary
;;;
;;;
;;; Commentary:
;;;
;;;
;;; Code:

(use-package elpy
  :pin melpa-stable
  :ensure t)

(setq python-shell-interpreter "ipython3"
      python-shell-interpreter-args "--simple-prompt -i")

(elpy-enable)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
(provide 'init-local-python)
