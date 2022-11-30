;; NOTE: An OS site-wide file, if present, will be read first. If it
;; loads ess, the following autoloads will be redundant. When possible
;; prefer Melpa version of ESS. Alt: start emacs with --no-site-file

;; (use-package ess
;;   :pin melpa-stable
;;   :ensure t
;;   :init (require 'ess-site))

(require 'ess)
(require 'stan-mode)
(require 'stan-snippets)

(setq ess-plain-first-buffername nil)
(setq ess-local-process-name "R")
(setq ess-ask-for-ess-directory nil)
(setq ess-eval-visibly-p nil)
(setq ansi-color-for-comint-mode 'filter)
(setq comint-move-point-for-output t)
(setq comint-scroll-to-bottom-on-input t)
(setq comint-scroll-to-bottom-on-output t)

(require 'ess-eldoc "ess-eldoc" t)

(autoload 'ess-rdired "ess-rdired"
  "View *R* objects in a dired-like buffer." t)

;; (use-package stan-mode
;;   :pin melpa-stable
;;   :ensure t
;;   :init (require 'stan-mode))

;; (use-package stan-snippets
;;   :pin melpa-stable
;;   :ensure t
;;   :init (require 'stan-snippets))

;; (require 'ess-tracebug "ess-tracebug" t)
;; (add-hook 'ess-post-run-hook 'ess-tracebug t)

(provide 'init-local-ess)
