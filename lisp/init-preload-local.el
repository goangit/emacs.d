;;; init-preload-local.el is one of two bootstrap files which enable
;;; arbitrary customised local configuration, while minimising
;;; upstream merge conflict potential. Possibly this is not strictly
;;; necessary if one understands git better than I do at this point.
;;;
;;; See also init-local.el for "post-load" local customisations.
;;;

(setq user-local-lisp-directory (expand-file-name "lisp/local" user-emacs-directory))

(add-to-list 'load-path user-local-lisp-directory)

(provide 'init-preload-local)
