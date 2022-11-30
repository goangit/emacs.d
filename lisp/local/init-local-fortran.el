;;; Package --- Summary
;;;
;;;
;;; Commentary:
;;;
;;;
;;; Code:

(add-to-list 'auto-mode-alist '("\\.mod\\'" . f90-mode))
(add-hook 'fortran-mode-hook 'whitespace-cleanup-mode)

(provide 'init-local-fortran)
