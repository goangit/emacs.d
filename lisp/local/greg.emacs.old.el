
;; old .emacs file in process of being canabalised into new configura

(auto-compression-mode t)
;;(electric-pair-mode t)
;;(global-font-lock-mode t)
;;(menu-bar-mode -1)
(tool-bar-mode -1)

;; (when (fboundp 'scroll-bar-mode)
;;   (scroll-bar-mode -1))
;; (setq redisplay-dont-pause t) ;; speeds up display
;; (fset 'yes-or-no-p 'y-or-n-p)
;; (global-auto-revert-mode t)
;; ;; (set-default 'line-spacing 0)

;; (setq completion-ignored-extensions
;;       '(".abs" ".aux" ".BAK" ".bak" ".CKP" ".class" ".elc" ".fasl" ".gz" ".hi"
;;      ".imp" ".mx" ".nc" ".o" ".press" ".ps" ".RData" ".Rcheck" ".so" ".tgz"
;;      ".toc" ".u" ))

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

;; ;; ----- package management ---------------------------------------------------

;; (when (> emacs-major-version 23)

;;   (defvar elpa-packages
;;     '(
;;       ac-js2
;;       auto-complete
;;       cider
;;       clojure-mode
;;       css-mode
;;       ;;dash
;;       ;;dash-at-point
;;       ;;ensime
;;       flycheck
;;       js2-mode
;;       ;;ghc
;;       magit
;;       org
;;       paredit
;;       solarized-theme
;;       stan-mode
;;       stan-snippets
;;       web-mode
;;       zenburn-theme
;;       ))

;;   (require 'package)

;;   (setq package-archives
;;      '(;; ("gnu"       . "http://elpa.gnu.org/packages/")
;;        ;; ("marmalade" . "http://marmalade-repo.org/packages/")
;;        ("org"   . "http://orgmode.org/elpa/")
;;        ("melpa" . "http://melpa.milkbox.net/packages/")))

;;   (package-initialize)

;;   (when (not package-archive-contents)
;;     (package-refresh-contents))

;;   (mapc (lambda (p)
;;        (unless (package-installed-p p) (package-install p)))
;;      elpa-packages)
;;   )


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


;; ;; ---- ielm ------------------------------------------------------------------

;; (defun ielm-auto-complete ()
;;   "Enables `auto-complete' support in \\[ielm]."
;;   (setq ac-sources '(ac-source-functions
;;                      ac-source-variables
;;                      ac-source-features
;;                      ac-source-symbols
;;                      ac-source-words-in-same-mode-buffers))
;;   (add-to-list 'ac-modes 'inferior-emacs-lisp-mode)
;;   (auto-complete-mode 1))

;; (add-hook 'ielm-mode-hook 'ielm-auto-complete)
;; (add-hook 'ielm-mode-hook (lambda () (paredit-mode +1)))

;; ;; (defadvice ielm-eval-input (after ielm-paredit activate)
;; ;;   "Begin each IELM prompt with a ParEdit parenthesis pair."
;; ;;   (paredit-open-round))
;; ;; this is almost good, but needs one-key cancel

;; ;; ---- paredit ---------------------------------------------------------------

;; (autoload 'paredit-mode "paredit"
;;      "Minor mode for pseudo-structurally editing Lisp code." t)
;; (add-hook 'emacs-lisp-mode-hook       (lambda () (paredit-mode +1)))
;; (add-hook 'lisp-mode-hook             (lambda () (paredit-mode +1)))
;; (add-hook 'lisp-interaction-mode-hook (lambda () (paredit-mode +1)))
;; (add-hook 'clojure-mode-hook          (lambda () (paredit-mode +1)))


;; (defun my-paredit-nonlisp ()
;;   "Turn on paredit mode for non-lisps."
;;   (interactive)
;;   (set (make-local-variable 'paredit-space-for-delimiter-predicates)
;;        '((lambda (endp delimiter) nil)))
;;   (paredit-mode 1))

;; ;; perhaps consider autopair instead?
;; (add-hook 'R-mode-hook 'my-paredit-nonlisp)
;; (add-hook 'ess-mode-hook 'my-paredit-nonlisp)
;; (add-hook 'iess-mode-hook 'my-paredit-nonlisp)
;; (add-hook 'js-mode-hook 'my-paredit-nonlisp)


;; ;; ----- NCL ------------------------------------------------------------------
;; ;;
;; ;; Fork of the official (NCAR) ncl.el by YYR, available here:
;; ;;       https://github.com/yyr/ncl-mode/downloads
;; ;;
;; ;; See ~/opt/site-lisp/ncl-mode/README.org for details.
;; ;; (load "~/local/lib/elisp/ncl-mode/ncl-mode-load.el")

;; ;; ---- python-----------------------------------------------------------------

;; ;; (load-file "python-init.el")


;; ;; ----- fortran --------------------------------------------------------------

;; ;; defaults: fortran-mode: *.f,*.F,*.for; f90-mode: *.f90,*.f95,*.f03,*.f08
;; ;; The following are supplementary; eg. Chapman 2003 source code uses these.
;; (add-to-list 'auto-mode-alist '("\\.FOR\\'" . fortran-mode))
;; (add-to-list 'auto-mode-alist '("\\.F90\\'" . f90-mode))

;; (autoload 'fortpy "fortpy" "Fortran Autocompletion" t)

;; ;; todo: need mode-hook here?

;; ----- ess ------------------------------------------------------------------

;; (load-library "ess-init")


;; ;; ----- ido: interactively do things -----------------------------------------

;; (when (> emacs-major-version 21)       ;; GNU Emacs since v22
;;   (ido-mode t)
;;   ;; (ido-mode 'both)                     ;; buffers and files
;;   (setq
;;    ido-everywhere t
;;    ido-case-fold  t                    ;; case-insensitive
;;    ido-confirm-unique-completion nil   ;; wait on RET, even unique completion
;;    ido-create-new-buffer 'always
;;    ido-enable-flex-matching t          ;; don't try to be too smart
;;    ido-enable-last-directory-history t ;; remember last used dirs
;;    ido-enable-prefix nil
;;    ido-ignore-extensions t             ;; use completion-ignored-extensions
;;    ido-file-extensions-order
;;    '("\\*eshell\\*" ".emacs" ".R" ".clj") ;; this idea probably sucks
;;    ido-ignore-buffers
;;    '("\\` " ".*Completion" "^session\.*" "^\\*[^(e?shell|R)]")
;;    ido-max-prospects 8                 ;; don't spam minibuffer
;;    ido-max-work-directory-list 30      ;; should be enough
;;    ido-max-work-file-list      50      ;; remember many
;;    ido-save-directory-list-file "~/.emacs.d/ido.last"
;;    ido-use-filename-at-point 'guess    ;; (can be annoying)
;;    ido-use-url-at-point nil            ;; (can be annoying)
;;    ido-work-directory-list '("~/src")
;;    ido-work-directory-list-ignore-regexps '("\\.Rcheck$")
;;    ido-auto-merge-delay-time 1.2       ;; default 0.7
;;    )
;;   (setq confirm-nonexistent-file-or-buffer nil)
;;   )

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


;; ;; ----- Custom Variables -----------------------------------------------------

;; (custom-set-variables
;;  ;; custom-set-variables was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(blink-cursor-mode nil)
;;  '(column-number-mode t)
;;  '(cursor-type (quote (bar . 1)) t)
;;  '(global-hl-line-mode t)
;;  '(send-mail-function nil)
;;  '(show-paren-mode t)
;;  '(uniquify-buffer-name-style (quote forward) nil (uniquify))
;; )


;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  )

;; (provide 'greg.emacs.old)


;; ----- eof ------------------------------------------------------------------
