;;; Package --- Summary
;;;
;;;
;;; Commentary:
;;;
;;;
;;; Code:

;; elpy requires: pip install elpy jedi flake8 yapf autopep8

(use-package elpy
  :pin melpa
  :ensure t)

(use-package pyenv-mode
  :pin melpa
  :ensure t)

(use-package ein
  :pin melpa
  :ensure t)

(defalias 'workon 'pyvenv-workon)

(setenv "IPY_TEST_SIMPLE_PROMPT" "1")

(add-hook 'elpy-mode-hook (lambda () (highlight-indentation-mode -1)))
(add-hook 'inferior-python-mode-hook 'visual-line-mode)

(define-key elpy-mode-map (kbd "C-x C-n") 'elpy-shell-send-group-and-step)

(setq elpy-rpc-python-command "python"
      python-shell-interpreter "jupyter"
      ;; python-shell-interpreter "ipython"
      ;; elpy-django-server-ipaddr "10.1.1.100" ;; default 127.0.0.1
      elpy-django-server-port "0:8000"
      python-shell-completion-native-enable nil
      )

(cond ((equalp python-shell-interpreter "python")
       (setq python-shell-interpreter-args "-i"))
      ((equalp python-shell-interpreter "ipython")
       (setq python-shell-interpreter-args "-i --simple-prompt"))
      ((equalp python-shell-interpreter "jupyter")
       (setq python-shell-interpreter-args "console --simple-prompt"
             python-shell-prompt-detect-failure-warning nil)
       (add-to-list 'python-shell-completion-native-disabled-interpreters "jupyter")))


(defun discover-virtualenv-package-paths (virtualenv)
  "Discover paths to site-packages using VIRTUALENV python-version."
  (let* ((venv-base-lib (concat (file-name-as-directory virtualenv) "lib"))
         (venv-python-version (car (reverse (directory-files venv-base-lib))))
         (venv-python-version-directory
          (concat (file-name-as-directory venv-base-lib)
                  (file-name-as-directory venv-python-version))))
    (mapcar (lambda (test-directory)
              (let ((venv-package-directory
                     (concat venv-python-version-directory
                             (file-name-as-directory test-directory))))
                (if (file-directory-p venv-package-directory)
                    venv-package-directory)))
            '("site-packages" "lib-dynload"))
    )
  )


(defun reset-pythonpath (pythonpath)
  "Backup PYTHONPATH, replace with VIRTUALENV appropriate alternative."
  (setq pythonpath-backup pythonpath)
  (let ((virtualenv (getenv "VIRTUAL_ENV")))
    (when virtualenv
      (add-hook 'kill-emacs-hook (lambda () (setenv "PYTHONPATH" pythonpath-backup)))
      (setenv "PYTHONPATH"
              (mapconcat 'identity
                         (discover-virtualenv-package-paths virtualenv) ":"))))
  )


(reset-pythonpath (getenv "PYTHONPATH"))

;;(setq elpy-modules (delq 'elpy-module-company elpy-modules))
(elpy-enable)
(pyenv-mode)


;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:

(provide 'init-local-python)
