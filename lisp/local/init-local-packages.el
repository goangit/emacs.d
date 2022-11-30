;;; Package --- Summary
;;;
;;; Additional local package mgmt.
;;; Specifically, use-package available from this point.
;;;
;;; Commentary: Suspect none of this required since (at least) 2022
;;;
;;; See also: ../init-elpa.el
;;;
;;; Code:


;; (add-to-list 'package-archives
;;              (cons "melpa-stable" "https://stable.melpa.org/packages/") t)

;; (add-to-list 'package-archives
;;              '("elpy" . "https://jorgenschaefer.github.io/packages/"))

;; (add-to-list 'package-pinned-packages
;;              '("use-package" . "melpa-stable"))

;; ;; Bootstrap `use-package' (package.el pre-initialised: init-elpa.el)
;; ;; see also: https://github.com/jwiegley/use-package/issues/219

;; (unless (package-installed-p 'use-package)
;;   (package-refresh-contents)
;;   (package-install 'use-package))

;; (eval-when-compile
;;   ;; (add-to-list 'load-path "<path where use-package is installed>")
;;   (require 'use-package)) ;; https://github.com/jwiegley/use-package


;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:

(provide 'init-local-packages)
;;; init-local-packages.el ends here
