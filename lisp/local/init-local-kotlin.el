;;; Package --- Summary
;;;
;;;
;;; Commentary:
;;;
;;;
;;; Code:

(require-package 'kotlin-mode)

(add-to-list 'auto-mode-alist '("\\.kt\\'" . kotlin-mode))
(add-hook 'kotlin-mode-hook 'whitespace-cleanup-mode)

(provide 'init-local-kotlin)
