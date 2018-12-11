;;; Package --- Summary
;;;
;;; Ensime configuration
;;;
;;; Commentary:
;;;
;;;
;;; Code:

(use-package ensime
  :pin melpa-stable
  :ensure t)

(setq
 ensime-sbt-command "/usr/share/sbt/bin/sbt"
 sbt:program-name "/usr/share/sbt/bin/sbt")

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:

(provide 'init-local-ensime)
;;; init-local-ensime.el ends here
