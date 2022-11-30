;;; Package --- Summary
;;;
;;;
;;; Commentary:
;;;
;;;
;;; Code:

(defun gl/compare-extension-strings (s1 s2)
  "Alphabetic substring comparison with boolean return required by sort"
  (let ((tst (compare-strings (downcase s1) (string-match "[a-zA-Z]" s1) nil
                              (downcase s2) (string-match "[a-zA-Z]" s2) nil)))
    (when (numberp tst)
      (setq tst (if (< tst 0) t nil) ))
    tst))

(defun gl/add-completion-ignored-extensions (lst)
  "Add LST elements to completion-ignored-extensions, return sorted result"
  (setq completion-ignored-extensions
        (sort (delete-dups (append completion-ignored-extensions lst))
              'gl/compare-extension-strings)))

(provide 'init-local-util)
