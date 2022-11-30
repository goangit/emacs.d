
;; old .emacs file in process of being canabalised into new configuration

;; (auto-compression-mode t)

;;(menu-bar-mode -1)

;; (when (fboundp 'scroll-bar-mode)
;;   (scroll-bar-mode -1))
;; (setq redisplay-dont-pause t) ;; speeds up display

;; (defun gl/sorted-copy-set (seq)
;;   (let ((my-seq (copy-sequence seq)))
;;     (sort my-seq ))
;;   )

;; (defadvice completion--file-name-table
;;     (after ignoring-backups-f-n-completion activate)
;;   "Filter out results when they match `completion-ignored-extensions'."
;;   (let ((res ad-return-value))
;;     (if (and (listp res)
;;           (stringp (car res))
;;           (cdr res))         ;; length > 1, don't ignore sole match
;;      (setq ad-return-value
;;               (completion-pcm--filename-try-filter res)))))

;; (setq-default major-mode 'text-mode)


;; (when (string= window-system "x")
;;   (set-face-attribute 'default nil :font "Inconsolata-dz:style=dz")
;;   (modify-frame-parameters nil '((wait-for-wm . nil)))
;;   (setq inhibit-splash-screen t)

;;   ;; (package-initialize) adds elpa-managed packages to load-path
;;   (if (equal 1 (random 2))
;;       (load-theme 'solarized-dark t)
;;     (load-theme 'zenburn t))
;;   )


;; ;; ----- host specific --------------------------------------------------------

;; (when (string= system-name "biome")
;;   ;; start frame size: hard coded, requires knowledge of individual screen
;;   ;; actually, since (left ) appears not to work only (height ) differs
;;   (setq initial-frame-alist
;;      `((left . 15) (top . 0)
;;        (width . 165) (height . 93)))

;;   ;; (add-to-list 'load-path "~/share/emacs/site-lisp/mu4e")

;;   ;; ;; example configuration for mu4e
;;   ;; ;; make sure mu4e is in your load-path
;;   ;; (require 'mu4e)

;;   ;; ;; Only needed if your maildir is _not_ ~/Maildir
;;   ;; ;; (setq mu4e-maildir "/home/user/Maildir")

;;   ;; ;; defaults; if they do not exist yet, mu4e offers create; see docstrings.
;;   ;; (setq mu4e-sent-folder   "/sent")
;;   ;; (setq mu4e-drafts-folder "/drafts")
;;   ;; (setq mu4e-trash-folder  "/trash")

;;   ;; ;; smtp mail setting; same as `gnus'
;;   ;; (setq
;;   ;;  message-send-mail-function   'smtpmail-send-it
;;   ;;  smtpmail-default-smtp-server "localhost"
;;   ;;  smtpmail-smtp-server         "localhost"
;;   ;;  ;;smtpmail-local-domain        "example.com"
;;   ;;  )
;;   )

;; (when (string= system-name "bup")
;;   (autoload 'android-mode "android-mode" "Android Development Mode" t)
;;   (custom-set-variables '(android-mode-sdk-dir "~/opt/android"))
;;   )


;; (when (string= system-name "float")
;;   ;; start frame size: hard coded, requires knowledge of individual screen
;;   ;; actually, since (left ) appears not to work only (height ) differs
;;   (setq initial-frame-alist
;;      `((left . 15) (top . 0)
;;        (width . 165) (height . 53)))

;;   ;; (load-file "~/src/sys/mu4e.el")

;;   ;; (add-to-list 'load-path "~/.emacs.d/spice-mode/")
;;   ;; (autoload 'spice-mode "spice-mode" "Spice/Layla Editing Mode" t)
;;   ;; (setq auto-mode-alist (append (list (cons "\\.sp$" 'spice-mode)
;;   ;;                                       (cons "\\.cir$" 'spice-mode)
;;   ;;                                       (cons "\\.ckt$" 'spice-mode)
;;   ;;                                       (cons "\\.mod$" 'spice-mode)
;;   ;;                                       (cons "\\.cdl$" 'spice-mode)
;;   ;;                                       (cons "\\.chi$" 'spice-mode) ;eldo outpt
;;   ;;                                       (cons "\\.inp$" 'spice-mode))
;;   ;;                                 auto-mode-alist))

;;   ;; Mode Line shows machine and directory, needs some work.
;;   ;; (load-file "~/src/ace/.emacs.mode.line.el")

;;   )

;; (when (string= system-name "doofus")
;;   (autoload 'haskell-mode "haskell-mode" "Haskell Mode" t)
;;   (eval-after-load 'haskell-mode
;;     '(progn
;;        (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
;;        (add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
;;        ;; (add-hook 'haskell-mode-hook
;;        ;;        (define-key haskell-mode-map
;;        ;;          (kbd "C-x C-s") 'haskell-mode-save-buffer))
;;        ))
;;   )


;; (when (string= system-name (or "biome" "bup" "doofus" "float"))

;;   (autoload 'flycheck "flycheck")

;;   ;; (add-to-list 'load-path "~/local/lib/elisp") ;; python/ncl need attention

;;   (let ((ess-path "/usr/share/emacs/site-lisp/ess"));; ARCH/Ubuntu default
;;     (if (file-exists-p ess-path) (add-to-list 'load-path ess-path)
;;       (message "ESS path not found")))

;;   (autoload 'stan-mode "stan-mode" "Stan Mode" t)
;;   (autoload 'stan-snippets "stan-snippets" "Stan Snippets" t)
;;   ;; stan-mode can integrate with flymake (flycheck??), requires stanc
;;   (let ((stancmp (expand-file-name "~/src/ref/github/stan-dev/stan/bin/stanc")))
;;     (if (file-exists-p stancmp) (setq stan-stanc-path stancmp)
;;       (message "stanc not found")))

;;   ;; https://github.com/clojure-emacs/cider#configuration
;;   (add-to-list 'auto-mode-alist '("\\.clj\\'" . clojure-mode))
;;   (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
;;   ;;(add-hook 'cider-mode-hook 'company-mode)      ; autocomplete source files
;;   ;; repl tweaks
;;   (setq nrepl-hide-special-buffers t)
;;   ;;(add-hook 'cider-repl-mode-hook 'company-mode) ; autocomplete
;;   (add-hook 'cider-repl-mode-hook 'paredit-mode)

;;   (autoload 'js "js2-mode")
;;   (add-to-list 'auto-mode-alist '("\\.json$" . js-mode))
;;   (eval-after-load 'js
;;     '(progn
;;        (add-hook 'js-mode-hook 'js2-minor-mode)
;;        (add-hook 'js2-mode-hook 'ac-js2-mode)
;;        (add-hook 'js-mode-hook (lambda () (flycheck-mode t)))
;;        (define-key js-mode-map "{" 'paredit-open-curly)
;;        (define-key js-mode-map "}" 'paredit-close-curly-and-newline)
;;        (setq js2-highlight-level 3) ;; levels [0,4], -1 == off

;;        ;; enable intergration with mozrepl firefox plugin via moz.el:
;;        ;; https://addons.mozilla.org/en-US/firefox/addon/mozrepl/
;;        ;; https://raw.githubusercontent.com/bard/mozrepl/master/chrome/content/moz.el
;;        ;; (autoload 'moz-minor-mode "moz"
;;        ;;                           "Mozilla Minor and Inferior Mozilla Modes" t)
;;        ;; (add-hook 'javascript-mode-hook 'javascript-custom-setup)
;;        ;; (defun javascript-custom-setup () (moz-minor-mode 1))
;;        ))

;;   (setq inferior-lisp-program "sbcl"
;;      lisp-indent-function 'common-lisp-indent-function
;;      slime-complete-symbol-function 'slime-fuzzy-complete-symbol)

;;   ;; SLIME
;;   (load (expand-file-name "~/local/lib/site-lisp/quicklisp/slime-helper.el"))
;;   ;; (add-to-list 'load-path "~/src/ref/github/slime") ; necessary?
;;   (require 'slime-autoloads)
;;   (eval-after-load 'slime
;;     '(progn
;;        (slime-setup '(slime-fancy))
;;        (slime-startup-animation t)))

;;   ;; gnus
;;   (load-file "~/src/sys/gnus-config.el")

;;   (require 'auto-complete)
;;   (add-to-list 'ac-dictionary-directories "~/src/sys/emacs/ac-dict")
;;   (require 'auto-complete-config)
;;   (ac-config-default)
;;   (setq global-auto-complete-mode t)
;;   (setq ac-use-menu-map t)
;;   (define-key ac-menu-map "\C-n" 'ac-next)
;;   (define-key ac-menu-map "\C-p" 'ac-previous)
;;   (setq ac-auto-start t)
;;   (setq ac-delay 0.01)
;;   ;; (setq ac-auto-show-menu nil)
;;   (setq ac-show-menu-immediately-on-auto-complete t)
;;   )

;; (when (string= system-name "katabatic.ice.internal")
;;   ;; seems emacs 21 does not have this default keybinding
;;   (global-set-key "\M-g \M-g" 'goto-line)
;;   )


;; ;; ---- smtp ------------------------------------------------------------------

;; ;; use message-mode, default in emacs24?
;; (setq mail-user-agent 'message-user-agent)

;; ;; use the default input method in message mode
;; ;;(add-hook 'message-mode-hook 'toggle-input-method)

;; ;; add Cc and Bcc headers to the message buffer
;; (setq message-default-mail-headers "Cc: \nBcc: \n")

;; ;; setup below successfully authenticates with webfaction server
;; ;; hangs emacs while sending ... perhaps try gnus message mode, mu4e?

;; ;; perhaps smtpmail-default-smtp-server is unncessary, here?
;; ;; needs specified before (require 'smtpmail), apparently
;; (setq smtpmail-default-smtp-server "smtp.webfaction.com")
;; (autoload 'smtpmail "smtpmail")
;; (setq smtpmail-smtp-server "smtp.webfaction.com")
;; (setq send-mail-function 'smtpmail-send-it)
;; (setq message-send-mail-function 'smtpmail-send-it)
;; (setq smtpmail-smtp-service 587)
;; ;(setq smtpmail-smtp-user "twominds") ;; filters .authinfo valid users
;; ;; default From: message header value, webfaction restrictions apply!
;; (setq smtpmail-local-domain "twominds.org")
;; (setq user-mail-address (concat "me@" smtpmail-local-domain))
;;       (setq user-full-name "Me")
;; (setq smtpmail-debug-info nil)




;; (when (> emacs-major-version 23)

;;   ;; temporary, intention to move out to bbdb-conf.el once gnus configured
;;   (autoload 'bbdb         "bbdb-com" "Insidious Big Brother Database" t)
;;   (autoload 'bbdb-name    "bbdb-com" "Insidious Big Brother Database" t)
;;   (autoload 'bbdb-company "bbdb-com" "Insidious Big Brother Database" t)
;;   (autoload 'bbdb-net     "bbdb-com" "Insidious Big Brother Database" t)
;;   (autoload 'bbdb-notes   "bbdb-com" "Insidious Big Brother Database" t)
;;   ;; (autoload 'bbdb-insinuate-vm       "bbdb-vm"    "Hook BBDB into VM")
;;   ;; (autoload 'bbdb-insinuate-rmail    "bbdb-rmail" "Hook BBDB into RMAIL")
;;   ;; (autoload 'bbdb-insinuate-mh       "bbdb-mhe"   "Hook BBDB into MH-E")
;;   ;; (autoload 'bbdb-insinuate-gnus     "bbdb-gnus"  "Hook BBDB into GNUS")
;;   ;; (autoload 'bbdb-insinuate-sendmail "bbdb"       "Hook BBDB into sendmail")

;;   (eval-after-load 'bbdb
;;     '(progn
;;        (setq bbdb-file "~/src/sys/.bbdb")
;;        (setq bbdb-check-zip-codes-p nil)
;;        (setq bbdb-default-area-code nil)
;;        (setq bbdb-default-country "Australia")
;;        (setq bbdb-north-american-phone-numbers-p nil)
;;        (add-hook 'smtpmail-setup-hook 'bbdb-mail-aliases)
;;        ;; (load-file "~/src/sys/bbdb-conf.el")
;;        ))

;;  (add-to-list 'auto-mode-alist
;;            '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))
;;  (autoload 'org "org" "Org Mode" t)
;;  (eval-after-load 'org '(load-file "~/src/sys/org/org-mode.el"))
;;  )


;; ;; ----- EasyPG ---------------------------------------------------------------

;; (autoload 'epa-file "epa-file")
;; ;; (epa-file-enable)
;; ;; (global-set-key "\M-g p" 'epa-file-disable) ;; plain
;; ;; (global-set-key "\M-g c" 'epa-file-enable)  ;; crypt

;; ;; ----- NCL ------------------------------------------------------------------
;; ;;
;; ;; Fork of the official (NCAR) ncl.el by YYR, available here:
;; ;;       https://github.com/yyr/ncl-mode/downloads
;; ;;
;; ;; See ~/opt/site-lisp/ncl-mode/README.org for details.
;; ;; (load "~/local/lib/elisp/ncl-mode/ncl-mode-load.el")

;; ;; ----- fortran --------------------------------------------------------------

;; ;; defaults: fortran-mode: *.f,*.F,*.for; f90-mode: *.f90,*.f95,*.f03,*.f08
;; ;; The following are supplementary; eg. Chapman 2003 source code uses these.
;; (add-to-list 'auto-mode-alist '("\\.FOR\\'" . fortran-mode))
;; (add-to-list 'auto-mode-alist '("\\.F90\\'" . f90-mode))


;; ;; ----- tramp ----------------------------------------------------------------

;; (setq tramp-default-method "ssh")

;; ;; 2010-10-24: exploring connect via .ssh/config (although possibly
;; ;; this too is not required, if local machine is in remote known_host
;; ;; and local id_rsa.pub is in remote authorized keys)


;; ;; ----- ERC -----------------------------------------------------------------

;; (autoload 'erc "erc" "ERC Mode" t)
;; (eval-after-load 'erc
;;     (global-set-key "\C-cef"
;;                  (lambda () (interactive)
;;                    (erc :server "irc.freenode.net" :port "6667"
;;                         :nick "NO3")))) ;; CaCO3


;; ;; ----- keymap ---------------------------------------------------------------

;; (global-unset-key (kbd "M-t")) ;; sacrifice transpose-words keybind

;; (global-set-key "\C-cw" 'compare-windows)
;; (global-set-key "\C-x\C-b" 'buffer-menu) ;; 'buffer-list


;; (defun swap-window-buffer ()
;;   "Swap buffers between windows. When more than two windows,
;; repeated invocations move selected buffer appear to each window
;; successively."
;;   (interactive)
;;   (cond ((one-window-p) (display-buffer (other-buffer)))
;;         ((let* ((buffer-a (current-buffer))
;;                 (window-b (cadr (window-list)))
;;                 (buffer-b (window-buffer window-b)))
;;            (set-window-buffer window-b buffer-a)
;;            (switch-to-buffer buffer-b)
;;            (other-window 1)))))

;; (global-set-key [M-tab] 'swap-window-buffer) ;; fails on Ubuntu/Windows
;; (global-set-key [C-tab] 'other-window)


;; (defun toggle-current-window-dedication ()
;;   ;; Pause = dedicate window => nothing will supplant say, *R*.
;;   (interactive)
;;   (let* ((window    (selected-window))
;;       (dedicated (window-dedicated-p window)))
;;     (set-window-dedicated-p window (not dedicated))
;;     (message "Window %sdedicated to %s"
;;           (if dedicated "no longer " "")
;;           (buffer-name))))

;; (global-set-key [pause] 'toggle-current-window-dedication)


;; ;; Replace ^M macro
;; (fset 'replace-ctrlms ;; Replace all ^M chars in the current buffer
;;       [escape ?< escape ?% ?\C-q ?\C-m return ?\C-q ?\C-j return ?!])

;; (global-set-key "\C-cm" 'replace-ctrlms)


;; ;; (defun replace-single-comment-hash ()
;; ;;   "Replace single hash character instances with double hash."
;; ;;   (interactive)
;; ;;   (save-excursion
;; ;;     (beginning-of-buffer)
;; ;;     (while (re-search-forward "^\\s*\\(#\\)[^#]\\.*$" nil t)
;; ;;       (replace-match \,(make-string 2 ?#) nil nil))
;; ;;     )
;; ;;   )
;; ;; (global-set-key (kbd "M-#") 'double-hashes)

;; ----- eof ------------------------------------------------------------------
