;; NOTE: An OS site-wide file, if present, will be read first. If it
;; loads ess, the following autoloads will be redundant. When possible
;; prefer Melpa version of ESS. Alt: start emacs with --no-site-file

(autoload 'R "ess-site" "ESS" t)
(autoload 'R-mode "ess-site" "ESS" t)
(autoload 'r-mode "ess-site" "ESS" t)
(autoload 'Rd-mode "ess-site" "ESS" t)
(autoload 'noweb-mode "ess-site" "ESS" t)

(add-to-list 'auto-mode-alist '("\\.R$" . R-mode))
(add-to-list 'auto-mode-alist '("\\.r$" . R-mode))
(add-to-list 'auto-mode-alist '("\\.Rd$" . Rd-mode))
(add-to-list 'auto-mode-alist '("\\.Rnw$" . noweb-mode))

(autoload 'ess-rdired "ess-rdired"
  "View *R* objects in a dired-like buffer." t)

;; unsure about these for now
;;(require 'ac-R)
;;(require 'ess-bugs-d)
;;(require 'ess-jags-d)

(require 'ess-eldoc "ess-eldoc" t)
;; (require 'ess-tracebug "ess-tracebug" t)
;; (add-hook 'ess-post-run-hook 'ess-tracebug t)

(setq ansi-color-for-comint-mode 'filter)
(setq comint-move-point-for-output t)
(setq comint-scroll-to-bottom-on-input t)
(setq comint-scroll-to-bottom-on-output t)
(setq ess-ask-for-ess-directory nil)
(setq ess-eval-visibly-p nil)
(setq ess-local-process-name "R")
(setq ess-plain-first-buffername nil)

(gl/add-completion-ignored-extensions '(".RData" ".Rcheck/"))

(defun my-ess-eval ()
  ;; sensible context-aware evaluation
  (interactive)
  (my-ess-start-R)
  (if (and transient-mark-mode mark-active)
      (call-interactively 'ess-eval-region)
    (call-interactively 'ess-eval-line-and-step)))

(add-hook 'ess-mode-hook
          '(lambda()
             (local-set-key [(shift return)] 'my-ess-eval)))

;; (add-hook 'inferior-ess-mode-hook
;;           '(lambda() ;; perhaps these work by default?
;;              (local-set-key [M-{] 'comint-previous-input)
;;              (local-set-key [M-}] 'comint-next-input)))

(add-hook 'Rnw-mode-hook
          '(lambda()
             (local-set-key [(shift return)] 'my-ess-eval)))


;; ----- options to be considered ---------------------------------------------

;; ;; ESS Mode (.R file)
;;   (define-key ess-mode-map "\C-l" 'ess-eval-line-and-step)
;;   (define-key ess-mode-map "\C-p"
;;              'ess-eval-function-or-paragraph-and-step)
;;   (define-key ess-mode-map "\C-r" 'ess-eval-region)

;; ;; iESS Mode (R console)
;;   (define-key inferior-ess-mode-map "\C-u" 'comint-kill-input)
;;   (define-key inferior-ess-mode-map "\C-w" 'backward-kill-word)
;;   (define-key inferior-ess-mode-map "\C-a" 'comint-bol)
;;   (define-key inferior-ess-mode-map [home] 'comint-bol)

;; ;; Comint Mode (R console)
;;   (define-key comint-mode-map "\C-e" 'comint-show-maximum-output)
;;   (define-key comint-mode-map "\C-r" 'comint-show-output)
;;   (define-key comint-mode-map "\C-o" 'comint-kill-output)

;;   ;; ess-tracebug: requires unbind of M-t (above) for this to work?
;;   (setq ess-tracebug-prefix (kbd "M-t") ) ;; todo: bind M-+
;;   (setq ess-use-tracebug t)
;;
;; ----------------------------------------------------------------------------

(provide 'init-local-ess)
