;;; Package --- Summary
;;;
;;; Configure Org-Mode for daily work planning.
;;;
;;;
;;; Commentary:
;;;
;;; configuration below adapted from http://doc.norang.ca/org-mode.html
;;;
;;;
;;; Todo: pare down to minimal set of genuine useful functionality.
;;;
;;; Greg Lee 2015-09-09.
;;;
;;; Revised: 2018-01-26.
;;;
;;; Code:


;; custom latex export of org files
(with-eval-after-load 'ox-latex
  (add-to-list 'org-latex-classes
               '("org-plain-latex"
                 "\\documentclass{article}
           [NO-DEFAULT-PACKAGES]
           [PACKAGES]
           [EXTRA]"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))


;; older:

(setq org-startup-indented t)   ;; indent mode = minimum stars
(setq org-log-done 'time)	;; add timestamps to completed items
(setq org-log-done 'note)	;; and notes as well

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

(setq org-stuck-projects (quote ("" nil nil "")))


;; ----- Agenda Files ---------------------------------------------------------

(setq org-agenda-files '("~/src/org" "~/src/med/anums"))

(defun gl/org-agenda-symlinks ()
  "list fqfn targets of *.org symlinks from org-agenda-files directories"
  (require 'find-lisp)
  (let ((osyms  () ))
    (dolist (oad org-agenda-files)
      (dolist (oaf (find-lisp-find-files oad "\\.org$"))
        (when (file-symlink-p oaf)
          (push (file-truename (file-symlink-p oaf)) osyms))))
    (nreverse osyms))) ;; placeholder for further processing

;; (gl/org-agenda-symlinks)

;; append parent directories for *.org symlinks to org-agenda-files
;; (append org-agenda-files
;;      (mapcar #'file-name-directory
;;              (gl/org-agenda-symlinks)))
;;
;; now: modify org-git.sync to accept org-agenda-files arguments and
;;         call this using call-process-shell-command:
;;
;; (call-process-shell-command "./org-git.sync" nil nil nil
;;                          (append org-agenda-files
;;                                  (mapcar #'file-name-directory
;;                                          (gl/org-agenda-symlinks))))
;;
;; alt: implement commit in elisp and use run-with-timer
;;
(defun gl/git-commit (dirlst)
  "run git commit on arbitrary directory list"
  (dolist (tgt dirlst)
    (progn
      (shell-command (concat "git ls-files --deleted -z " tgt
                             " | xargs -0 git rm >/dev/null 2>&1"))
      (shell-command (concat "git add " tgt " >/dev/null 2>&1"))
      (shell-command (concat "pushd ~/src; " "git commit -m '"
                             (format-time-string "%Y-%m-%d %T") "'" tgt
                             "; popd"))
      ))
  )

(defun gl/git-commit-org-agenda-files ()
  "Run git commit on directories implicated by org-agenda-files."
  (gl/git-commit (append org-agenda-files
                         (mapcar #'file-name-directory
                                 (gl/org-agenda-symlinks))))
  )

;; ;; run once
;; (gl/git-commit-org-agenda-files)

;; automate: every 30 minutes: todo: needs work
;; (run-with-timer 0 (* 30 60) 'gl/git-commit-org-agenda-files)

;; ----- call process examples -----------------------------------------------

;; (call-process "/bin/bash" nil t nil "-c" "ls -t ~/src/org"
;;            (gl/org-agenda-symlinks))
;;
;; ;; equivalent to "script.sh arg1 arg" in a shell.
;; (call-process-shell-command "./org-git.sync" nil nil nil "arg1" "arg2")
;;
;;
;; ----- Keybindings ----------------------------------------------------------

;; Custom Key Bindings

(global-set-key (kbd "<f12>")     'org-agenda)
(global-set-key (kbd "<f5>")      'bh/org-todo)
(global-set-key (kbd "<S-f5>")    'bh/widen)
(global-set-key (kbd "<f7>")      'bh/set-truncate-lines)
(global-set-key (kbd "<f8>")      'org-cycle-agenda-files)
(global-set-key (kbd "<f9> <f9>") 'bh/show-org-agenda)
(global-set-key (kbd "<f9> b")    'bbdb)
(global-set-key (kbd "<f9> c")    'calendar)
(global-set-key (kbd "<f9> f")    'boxquote-insert-file)
(global-set-key (kbd "<f9> g")    'gnus)
(global-set-key (kbd "<f9> h")    'bh/hide-other)
(global-set-key (kbd "<f9> n")    'bh/toggle-next-task-display)
(global-set-key (kbd "<f9> w")    'widen)

(global-set-key (kbd "<f9> I")    'bh/punch-in)
(global-set-key (kbd "<f9> O")    'bh/punch-out)

(global-set-key (kbd "<f9> o")    'bh/make-org-scratch)

(global-set-key (kbd "<f9> r")    'boxquote-region)
(global-set-key (kbd "<f9> s")    'bh/switch-to-scratch)

(global-set-key (kbd "<f9> t")    'bh/insert-inactive-timestamp)
(global-set-key (kbd "<f9> T")    'bh/toggle-insert-inactive-timestamp)

(global-set-key (kbd "<f9> v")    'visible-mode)
(global-set-key (kbd "<f9> l")    'org-toggle-link-display)
(global-set-key (kbd "<f9> SPC")  'bh/clock-in-last-task)
(global-set-key (kbd "C-<f9>")    'previous-buffer)
(global-set-key (kbd "M-<f9>")    'org-toggle-inline-images)
(global-set-key (kbd "C-x n r")   'narrow-to-region)
(global-set-key (kbd "C-<f10>")   'next-buffer)
(global-set-key (kbd "<f11>")     'org-clock-goto)
(global-set-key (kbd "C-<f11>")   'org-clock-in)
(global-set-key (kbd "C-s-<f12>") 'bh/save-then-publish)
(global-set-key (kbd "C-c c")     'org-capture)

(defun bh/hide-other ()
  (interactive)
  (save-excursion
    (org-back-to-heading 'invisible-ok)
    (hide-other)
    (org-cycle)
    (org-cycle)
    (org-cycle)))

(defun bh/set-truncate-lines ()
  "Toggle value of truncate-lines and refresh window display."
  (interactive)
  (setq truncate-lines (not truncate-lines))
  ;; now refresh window display (an idiom from simple.el):
  (save-excursion
    (set-window-start (selected-window)
                      (window-start (selected-window)))))

(defun bh/make-org-scratch ()
  (interactive)
  (find-file "/tmp/publish/scratch.org")
  (gnus-make-directory "/tmp/publish"))

(defun bh/switch-to-scratch ()
  (interactive)
  (switch-to-buffer "*scratch*"))

;; ----- todo -----------------------------------------------------------------

(setq org-use-fast-todo-selection t)

;; update todo status without attendant processing
(setq org-treat-S-cursor-todo-selection-as-state-change nil)

(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
              (sequence "WAIT(w@/!)" "HOLD(h@/!)" "|"
                        "CANC(c@/!)" "PHONE" "MEETING"))))

(setq org-todo-keyword-faces
      (quote (("TODO"    :foreground "red" :weight bold)
              ("NEXT"    :foreground "light blue" :weight bold)
              ("DONE"    :foreground "forest green" :weight bold)
              ("WAIT"    :foreground "orange" :weight bold)
              ("HOLD"    :foreground "magenta" :weight bold)
              ("CANC"    :foreground "forest green" :weight bold)
              ("MEETING" :foreground "forest green" :weight bold)
              ("PHONE"   :foreground "forest green" :weight bold))))

(setq org-todo-state-tags-triggers
      (quote (("CANC" ("CANC" . t))
              ("WAIT" ("WAIT" . t))
              ("HOLD" ("WAIT") ("HOLD" . t))
              (done ("WAIT") ("HOLD"))
              ("TODO" ("WAIT") ("CANC") ("HOLD"))
              ("NEXT" ("WAIT") ("CANC") ("HOLD"))
              ("DONE" ("WAIT") ("CANC") ("HOLD")))))

(setq org-directory "~/src/org")
(setq org-default-notes-file "~/src/org/refile.org")

;; I use C-c c to start capture mode
(global-set-key (kbd "C-c c") 'org-capture)

;; Capture templates for: TODO ctasks, Notes, appointments,
;; phone calls, meetings, and org-protocol

(setq org-capture-templates
      (quote
       (("t" "todo" entry (file "~/src/org/refile.org")
         "* TODO %?\n %U\n %a\n" :clock-in t :clock-resume t)
        ("r" "respond" entry (file "~/src/org/refile.org")
         "* NEXT Respond to %:from on %:subject\nSCHEDULED: %t\n %U\n %a\n"
         :clock-in t :clock-resume t :immediate-finish t)
        ("n" "note" entry
         (file "~/src/org/refile.org")
         "* %? :NOTE:\n %U\n %a\n" :clock-in t :clock-resume t)
        ("j" "Journal" entry
         (file+datetree "~/src/org/diary.org")
         "* %?\n %U\n" :clock-in t :clock-resume t)
        ("w" "org-protocol" entry (file "~/src/org/refile.org")
         "* TODO Review %c\n %U\n" :immediate-finish t)
        ("m" "Meeting" entry (file "~/src/org/refile.org")
         "* MEETING with %? :MEETING:\n %U" :clock-in t :clock-resume t)
        ("p" "Phone call" entry (file "~/src/org/refile.org")
         "* PHONE %? :PHONE:\n %U" :clock-in t :clock-resume t)
        ("h" "Habit" entry (file "~/src/org/refile.org")
         "* NEXT %?\n %U\n %a\n SCHEDULED:
%(format-time-string \"<%Y-%m-%d %a .+1d/3d>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n"))))


;; Remove empty LOGBOOK drawers on clock out
(defun bh/remove-empty-drawer-on-clock-out ()
  (interactive)
  (save-excursion
    (beginning-of-line 0)
    (org-remove-empty-drawer-at "LOGBOOK" (point))))

(add-hook 'org-clock-out-hook
          'bh/remove-empty-drawer-on-clock-out 'append)

;; ----- Refiling -------------------------------------------------------------

;; include this and any agenda-contributing file, to 9 levels deep
(setq org-refile-targets (quote ((nil :maxlevel . 9)
                                 (org-agenda-files :maxlevel . 9))))

; Use full outline paths for refile targets - file directly with IDO
(setq org-refile-use-outline-path t)

; Targets complete directly with IDO
(setq org-outline-path-complete-in-steps nil)

; Allow refile to create parent tasks with confirmation
(setq org-refile-allow-creating-parent-nodes (quote confirm))

; Use IDO for both buffer and file completion and ido-everywhere to t
(setq org-completion-use-ido t)
(setq ido-everywhere t)
(setq ido-max-directory-size 100000)
(ido-mode (quote both))

; Use the current window when visiting files and buffers with ido
(setq ido-default-file-method 'selected-window)
(setq ido-default-buffer-method 'selected-window)

; Use the current window for indirect buffer display
(setq org-indirect-buffer-display 'current-window)

;;;; Refile settings
; Exclude DONE state tasks from refile targets
(defun bh/verify-refile-target ()
  "Exclude todo keywords with a done state from refile targets"
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))

(setq org-refile-target-verify-function 'bh/verify-refile-target)

;; ----- Clocking -------------------------------------------------------------
;;
;; Resume clocking task when emacs is restarted
(org-clock-persistence-insinuate)
;; use discrete minute intervals (no rounding) increments
(setq org-time-stamp-rounding-minutes (quote (1 1)))
;; Show lot of clocking history so it's easy to pick items off the C-F11 list
(setq org-clock-history-length 23)
;; Resume clocking task on clock-in if the clock is open
(setq org-clock-in-resume t)
;; Change tasks to NEXT when clocking in
(setq org-clock-in-switch-to-state 'bh/clock-in-to-next)
;; Separate drawers for clocking and logs
(setq org-drawers (quote ("PROPERTIES" "LOGBOOK")))
;; Save clock data and state changes and notes in the LOGBOOK drawer
(setq org-clock-into-drawer t)
;; Sometimes change tasks quickly, remove clocked tasks with 0:00 duration
(setq org-clock-out-remove-zero-time-clocks t)
;; Clock out when moving task to a done state
(setq org-clock-out-when-done t)
;; Save running clock and all clock history on exit, load on startup
(setq org-clock-persist t)
;; Do not prompt to resume an active clock
(setq org-clock-persist-query-resume nil)
;; Enable auto clock resolution for finding open clocks
(setq org-clock-auto-clock-resolution (quote when-no-clock-is-running))
;; Include current clocking task in clock reports
(setq org-clock-report-include-clocking-task t)

(setq bh/keep-clock-running nil)

(defun bh/clock-in-to-next (kw)
  "Switch a task from TODO to NEXT when clocking in.
Skips capture tasks, projects, and subprojects.
Switch projects and subprojects from NEXT back to TODO"
  (when (not (and (boundp 'org-capture-mode) org-capture-mode))
    (cond
     ((and (member (org-get-todo-state) (list "TODO"))
           (bh/is-task-p))
      "NEXT")
     ((and (member (org-get-todo-state) (list "NEXT"))
           (bh/is-project-p))
      "TODO"))))

(defun bh/find-project-task ()
  "Move point to the parent (project) task if any"
  (save-restriction
    (widen)
    (let ((parent-task (save-excursion
                         (org-back-to-heading 'invisible-ok) (point))))
      (while (org-up-heading-safe)
        (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
          (setq parent-task (point))))
      (goto-char parent-task)
      parent-task)))

(defun bh/punch-in (arg)
  "Start continuous clocking and set the default task to the
selected task.  If no task is selected set the Organization task
as the default task."
  (interactive "p")
  (setq bh/keep-clock-running t)
  (if (equal major-mode 'org-agenda-mode)
      ;;
      ;; in the agenda
      ;;
      (let* ((marker (org-get-at-bol 'org-hd-marker))
             (tags (org-with-point-at marker (org-get-tags-at))))
        (if (and (eq arg 4) tags)
            (org-agenda-clock-in '(16))
          (bh/clock-in-organization-task-as-default)))
    ;;
    ;; not in the agenda
    ;;
    (save-restriction
      (widen)
      ;; Find the tags on the current task
      (if (and (equal major-mode 'org-mode)
               (not (org-before-first-heading-p)) (eq arg 4))
          (org-clock-in '(16))
        (bh/clock-in-organization-task-as-default)))))

(defun bh/punch-out ()
  (interactive)
  (setq bh/keep-clock-running nil)
  (when (org-clock-is-active)
    (org-clock-out))
  (org-agenda-remove-restriction-lock))

(defun bh/clock-in-default-task ()
  (save-excursion
    (org-with-point-at org-clock-default-task
      (org-clock-in))))

(defun bh/clock-in-parent-task ()
  "Move point to the parent (project) task if any and clock in"
  (let ((parent-task))
    (save-excursion
      (save-restriction
        (widen)
        (while (and (not parent-task) (org-up-heading-safe))
          (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
            (setq parent-task (point))))
        (if parent-task
            (org-with-point-at parent-task
              (org-clock-in))
          (when bh/keep-clock-running
            (bh/clock-in-default-task)))))))

;; (defvar bh/organization-task-id "eb155a82-92b2-4f25-a3c6-0304591af2f9")
(defvar bh/organization-task-id "fb155a82-92b2-4f25-a3c6-0304591af2f9")

(defun bh/clock-in-organization-task-as-default ()
  (interactive)
  (org-with-point-at (org-id-find bh/organization-task-id 'marker)
    (org-clock-in '(16))))

;; identify timing gaps (unstopped clocks, etc)
(setq org-agenda-clock-consistency-checks
      (quote (:max-duration "4:00"
              :min-duration 0
              :max-gap 0
              :gap-ok-around ("4:00"))))

(defun bh/clock-out-maybe ()
  (when (and bh/keep-clock-running
             (not org-clock-clocking-in)
             (marker-buffer org-clock-default-task)
             (not org-clock-resolving-clocks-due-to-idleness))
    (bh/clock-in-parent-task)))

(add-hook 'org-clock-out-hook 'bh/clock-out-maybe 'append)

;; deprecated
;; (require 'org-id)
;; (defun bh/clock-in-task-by-id (id)
;;   "Clock in a task by id"
;;   (org-with-point-at (org-id-find id 'marker)
;;     (org-clock-in nil)))

(defun bh/clock-in-last-task (arg)
  "Clock in the interrupted task if there is one
Skip the default task and get the next one.
A prefix arg forces clock in of the default task."
  (interactive "p")
  (let ((clock-in-to-task
         (cond
          ((eq arg 4) org-clock-default-task)
          ((and (org-clock-is-active)
                (equal org-clock-default-task (cadr org-clock-history)))
           (caddr org-clock-history))
          ((org-clock-is-active) (cadr org-clock-history))
          ((equal org-clock-default-task (car org-clock-history)) (cadr org-clock-history))
          (t (car org-clock-history)))))
    (widen)
    (org-with-point-at clock-in-to-task
      (org-clock-in nil))))


;; ----- Agenda ---------------------------------------------------------------
;;
;; Keep Agenda fast, showing only today's date
;; (setq org-agenda-span 'day)

;; Do not dim blocked tasks
(setq org-agenda-dim-blocked-tasks nil)

;; Compact the block agenda view
(setq org-agenda-compact-blocks t)

;; Custom agenda command definitions
(setq org-agenda-custom-commands
      '(("N" "Notes" tags "NOTE"
         ((org-agenda-overriding-header "Notes")
          (org-tags-match-list-sublevels t)))
        ("h" "Habits" tags-todo "STYLE=\"habit\""
         ((org-agenda-overriding-header "Habits")
          (org-agenda-sorting-strategy
           '(todo-state-down effort-up category-keep))))
        (" " "Agenda"
         ((agenda "" nil)
          (tags "REFILE"
                ((org-agenda-overriding-header "Tasks to Refile")
                 (org-tags-match-list-sublevels nil)))
          (tags-todo "-CANC/!"
                     ((org-agenda-overriding-header "Stuck Projects")
                      (org-agenda-skip-function
                       'bh/skip-non-stuck-projects)
                      (org-agenda-sorting-strategy
                       '(category-keep))))
          (tags-todo "-HOLD-CANC/!"
                     ((org-agenda-overriding-header "Projects")
                      (org-agenda-skip-function 'bh/skip-non-projects)
                      (org-tags-match-list-sublevels 'indented)
                      (org-agenda-sorting-strategy
                       '(category-keep))))
          (tags-todo "-CANC/!NEXT"
                     ((org-agenda-overriding-header
                       (concat "Project Next Tasks"
                               (if bh/hide-scheduled-and-waiting-next-tasks
                                   ""
                                 " (including WAIT and SCHEDULED tasks)")))
                      (org-agenda-skip-function
                       'bh/skip-projects-and-habits-and-single-tasks)
                      (org-tags-match-list-sublevels t)
                      (org-agenda-todo-ignore-scheduled
                       bh/hide-scheduled-and-waiting-next-tasks)
                      (org-agenda-todo-ignore-deadlines
                       bh/hide-scheduled-and-waiting-next-tasks)
                      (org-agenda-todo-ignore-with-date
                       bh/hide-scheduled-and-waiting-next-tasks)
                      (org-agenda-sorting-strategy
                       '(todo-state-down effort-up category-keep))))
          (tags-todo "-REFILE-CANC-WAIT-HOLD/!"
                     ((org-agenda-overriding-header
                       (concat "Project Subtasks"
                               (if bh/hide-scheduled-and-waiting-next-tasks
                                   ""
                                 " (including WAIT and SCHEDULED tasks)")))
                      (org-agenda-skip-function
                       'bh/skip-non-project-tasks)
                      (org-agenda-todo-ignore-scheduled
                       bh/hide-scheduled-and-waiting-next-tasks)
                      (org-agenda-todo-ignore-deadlines
                       bh/hide-scheduled-and-waiting-next-tasks)
                      (org-agenda-todo-ignore-with-date
                       bh/hide-scheduled-and-waiting-next-tasks)
                      (org-agenda-sorting-strategy
                       '(category-keep))))
          (tags-todo "-REFILE-CANC-WAIT-HOLD/!"
                     ((org-agenda-overriding-header
                       (concat "Standalone Tasks"
                               (if bh/hide-scheduled-and-waiting-next-tasks
                                   ""
                                 " (including WAIT and SCHEDULED tasks)")))
                      (org-agenda-skip-function
                       'bh/skip-project-tasks)
                      (org-agenda-todo-ignore-scheduled
                       bh/hide-scheduled-and-waiting-next-tasks)
                      (org-agenda-todo-ignore-deadlines
                       bh/hide-scheduled-and-waiting-next-tasks)
                      (org-agenda-todo-ignore-with-date
                       bh/hide-scheduled-and-waiting-next-tasks)
                      (org-agenda-sorting-strategy
                       '(category-keep))))
          (tags-todo "-CANC+WAIT|HOLD/!"
                     ((org-agenda-overriding-header
                       "Waiting and Postponed Tasks")
                      (org-agenda-skip-function 'bh/skip-stuck-projects)
                      (org-tags-match-list-sublevels nil)
                      (org-agenda-todo-ignore-scheduled t)
                      (org-agenda-todo-ignore-deadlines t)))
          (tags "-REFILE/"
                ((org-agenda-overriding-header "Tasks to Archive")
                 (org-agenda-skip-function 'bh/skip-non-archivable-tasks)
                 (org-tags-match-list-sublevels nil))))
         nil)))

(defun bh/org-auto-exclude-function (tag)
  "Automatic task exclusion in the agenda with / RET"
  (and (cond
        ((string= tag "hold") t)
        ((string= tag "pers") t))
       (concat "-" tag)))

(setq org-agenda-auto-exclude-function 'bh/org-auto-exclude-function)


;; ----- Reporting ------------------------------------------------------------
;;
;; Agenda clock report parameters
(setq org-agenda-clockreport-parameter-plist
      (quote (:link t :maxlevel 5 :fileskip0 t :compact t :narrow 80)))

;; Set default column view headings: Task Effort Clock_Summary
(setq org-columns-default-format
      "%80ITEM(Task) %10Effort(Effort){:} %10CLOCKSUM")

;; global Effort estimate values
;; global STYLE property values for completion
(setq org-global-properties
      (quote (("Effort_ALL" .
               "0:15 0:30 0:45 1:00 2:00 3:00 4:00 5:00 6:00 0:00")
              ("STYLE_ALL" . "habit"))))

;; Agenda log mode items to display (closed and state changes by default)
(setq org-agenda-log-mode-items (quote (closed state)))

;; ----- Tags -----------------------------------------------------------------
;;
;; fast select tags
(setq org-tag-alist (quote ((:startgroup)
                            ("@errand" . ?e)
                            ("@office" . ?o)
                            ("@home" . ?H)
                            (:endgroup)
                            ("WAIT" . ?w)
                            ("HOLD" . ?h)
                            ("PERS" . ?P)
                            ("WORK" . ?W)
                            ("ORG" . ?O)
                            ("CANDID" . ?N)
                            ("crypt" . ?E)
                            ("NOTE" . ?n)
                            ("CANC" . ?c)
                            ("FLAG" . ??))))

; Allow setting single tags without the menu
(setq org-fast-tag-selection-single-key (quote expert))

; For tag searches ignore tasks with scheduled and deadline dates
(setq org-agenda-tags-todo-honor-ignore-options t)


;; ----- Phone Calls ----------------------------------------------------------
;;
;; moved to .emacs
;; (require 'bbdb)
;; (require 'bbdb-com)

;; not in keybindings above!!
(global-set-key (kbd "<f9> p") 'bh/phone-call)

;;
;; Phone capture template handling with BBDB lookup
;; Adapted from code by Gregory J. Grubbs
(defun bh/phone-call ()
  "Return name and company info for caller from bbdb lookup"
  (interactive)
  (let* (name rec caller)
    (setq name (completing-read "Who is calling? "
                                (bbdb-hashtable)
                                'bbdb-completion-predicate
                                'confirm))
    (when (> (length name) 0)
      ;; Something supplied, lookup bbdb
      (setq rec
            (or (first
                 (or (bbdb-search (bbdb-records) name nil nil)
                     (bbdb-search (bbdb-records) nil name nil)))
                name)))

    ;; Build bbdb link if bbdb record exists, else return name
    (setq caller (cond ((and rec (vectorp rec))
                        (let ((name (bbdb-record-name rec))
                              (company (bbdb-record-company rec)))
                          (concat "[[bbdb:"
                                  name "]["
                                  name "]]"
                                  (when company
                                    (concat " - " company)))))
                       (rec)
                       (t "NameOfCaller")))
    (insert caller)))

;; ----- Archiving ------------------------------------------------------------
;;
(setq org-archive-mark-done nil)
(setq org-archive-location "%s_archive::* Archived Tasks")

(defun bh/skip-non-archivable-tasks ()
  "Skip trees that are not available for archiving"
  (save-restriction
    (widen)
    ;; Consider only tasks with done todo headings as archivable candidates
    (let ((next-headline
           (save-excursion (or (outline-next-heading) (point-max))))
          (subtree-end (save-excursion (org-end-of-subtree t))))
      (if (member (org-get-todo-state) org-todo-keywords-1)
          (if (member (org-get-todo-state) org-done-keywords)
              (let* ((daynr (string-to-int
                             (format-time-string "%d" (current-time))))
                     (a-month-ago (* 60 60 24 (+ daynr 1)))
                     (last-month
                      (format-time-string "%Y-%m-"
                                          (time-subtract
                                           (current-time)
                                           (seconds-to-time a-month-ago))))
                     (this-month (format-time-string "%Y-%m-" (current-time)))
                     (subtree-is-current
                      (save-excursion
                        (forward-line 1)
                        (and (< (point) subtree-end)
                             (re-search-forward
                              (concat last-month "\\|" this-month)
                              subtree-end t)))))
                (if subtree-is-current
                    subtree-end ;; date in current or previous month: skip
                  nil))  ;; available to archive
            (or subtree-end (point-max)))
        next-headline))))


;; ----- Publishing -----------------------------------------------------------

;; (setq org-alphabetical-lists t)

;; ;; Explicitly load required exporters
;; (require 'ox-html)
;; (require 'ox-latex)
;; (require 'ox-ascii)

;; ;; Org-babl setup
;; (setq org-ditaa-jar-path "~/java/ditaa0_6b.jar")
;; (setq org-plantuml-jar-path "~/java/plantuml.jar")

;; (add-hook 'org-babel-after-execute-hook 'bh/display-inline-images 'append)

;; ; Make babel results blocks lowercase
;; (setq org-babel-results-keyword "results")

;; (defun bh/display-inline-images ()
;;   (condition-case nil
;;       (org-display-inline-images)
;;     (error nil)))

;; (org-babel-do-load-languages
;;  (quote org-babel-load-languages)
;;  (quote ((emacs-lisp . t)
;;          (dot . t)
;;          (ditaa . t)
;;          (R . t)
;;          (python . t)
;;          (ruby . t)
;;          (gnuplot . t)
;;          (clojure . t)
;;          (sh . t)
;;          (ledger . t)
;;          (org . t)
;;          (plantuml . t)
;;          (latex . t))))

;; ; Do not prompt to confirm evaluation
;; ; This may be dangerous - make sure you understand the consequences
;; ; of setting this -- see the docstring for details
;; (setq org-confirm-babel-evaluate nil)

;; ; Use fundamental mode when editing plantuml blocks with C-c '
;; (add-to-list 'org-src-lang-modes (quote ("plantuml" . fundamental)))

;; ;; Don't enable this because it breaks access to emacs from my Android phone
;; (setq org-startup-with-inline-images nil)

;; ;; Ditaa template
;; #+begin_src ditaa :file some_filename.png :cmdline -r -s 0.8
;;   <context of ditaa source goes here>
;; #+end_src

;; ; experimenting with docbook exports - not finished
;; (setq org-export-docbook-xsl-fo-proc-command "fop %s %s")
;; (setq org-export-docbook-xslt-proc-command "xsltproc --output %s /usr/share/xml/docbook/stylesheet/nwalsh/fo/docbook.xsl %s")
;; ;
;; ; Inline images in HTML instead of producting links to the image
;; (setq org-html-inline-images t)
;; ; Do not use sub or superscripts - I currently don't need this functionality in my documents
;; (setq org-export-with-sub-superscripts nil)
;; ; Use org.css from the norang website for export document stylesheets
;; (setq org-html-head-extra "<link rel=\"stylesheet\" href=\"http://doc.norang.ca/org.css\" type=\"text/css\" />")
;; (setq org-html-head-include-default-style nil)
;; ; Do not generate internal css formatting for HTML exports
;; (setq org-export-htmlize-output-type (quote css))
;; ; Export with LaTeX fragments
;; (setq org-export-with-LaTeX-fragments t)
;; ; Increase default number of headings to export
;; (setq org-export-headline-levels 6)

;; ; List of projects
;; ; norang       - http://www.norang.ca/
;; ; doc          - http://doc.norang.ca/
;; ; org-mode-doc - http://doc.norang.ca/org-mode.html and associated files
;; ; org          - miscellaneous todo lists for publishing
;; (setq org-publish-project-alist
;;       ;
;;       ; http://www.norang.ca/  (norang website)
;;       ; norang-org are the org-files that generate the content
;;       ; norang-extra are images and css files that need to be included
;;       ; norang is the top-level project that gets published
;;       (quote (("norang-org"
;;                :base-directory "~/git/www.norang.ca"
;;                :publishing-directory "/ssh:www-data@www:~/www.norang.ca/htdocs"
;;                :recursive t
;;                :table-of-contents nil
;;                :base-extension "org"
;;                :publishing-function org-html-publish-to-html
;;                :style-include-default nil
;;                :section-numbers nil
;;                :table-of-contents nil
;;                :html-head "<link rel=\"stylesheet\" href=\"norang.css\" type=\"text/css\" />"
;;                :author-info nil
;;                :creator-info nil)
;;               ("norang-extra"
;;                :base-directory "~/git/www.norang.ca/"
;;                :publishing-directory "/ssh:www-data@www:~/www.norang.ca/htdocs"
;;                :base-extension "css\\|pdf\\|png\\|jpg\\|gif"
;;                :publishing-function org-publish-attachment
;;                :recursive t
;;                :author nil)
;;               ("norang"
;;                :components ("norang-org" "norang-extra"))
;;               ;
;;               ; http://doc.norang.ca/  (norang website)
;;               ; doc-org are the org-files that generate the content
;;               ; doc-extra are images and css files that need to be included
;;               ; doc is the top-level project that gets published
;;               ("doc-org"
;;                :base-directory "~/git/doc.norang.ca/"
;;                :publishing-directory "/ssh:www-data@www:~/doc.norang.ca/htdocs"
;;                :recursive nil
;;                :section-numbers nil
;;                :table-of-contents nil
;;                :base-extension "org"
;;                :publishing-function (org-html-publish-to-html org-org-publish-to-org)
;;                :style-include-default nil
;;                :html-head "<link rel=\"stylesheet\" href=\"/org.css\" type=\"text/css\" />"
;;                :author-info nil
;;                :creator-info nil)
;;               ("doc-extra"
;;                :base-directory "~/git/doc.norang.ca/"
;;                :publishing-directory "/ssh:www-data@www:~/doc.norang.ca/htdocs"
;;                :base-extension "css\\|pdf\\|png\\|jpg\\|gif"
;;                :publishing-function org-publish-attachment
;;                :recursive nil
;;                :author nil)
;;               ("doc"
;;                :components ("doc-org" "doc-extra"))
;;               ("doc-private-org"
;;                :base-directory "~/git/doc.norang.ca/private"
;;                :publishing-directory "/ssh:www-data@www:~/doc.norang.ca/htdocs/private"
;;                :recursive nil
;;                :section-numbers nil
;;                :table-of-contents nil
;;                :base-extension "org"
;;                :publishing-function (org-html-publish-to-html org-org-publish-to-org)
;;                :style-include-default nil
;;                :html-head "<link rel=\"stylesheet\" href=\"/org.css\" type=\"text/css\" />"
;;                :auto-sitemap t
;;                :sitemap-filename "index.html"
;;                :sitemap-title "Norang Private Documents"
;;                :sitemap-style "tree"
;;                :author-info nil
;;                :creator-info nil)
;;               ("doc-private-extra"
;;                :base-directory "~/git/doc.norang.ca/private"
;;                :publishing-directory "/ssh:www-data@www:~/doc.norang.ca/htdocs/private"
;;                :base-extension "css\\|pdf\\|png\\|jpg\\|gif"
;;                :publishing-function org-publish-attachment
;;                :recursive nil
;;                :author nil)
;;               ("doc-private"
;;                :components ("doc-private-org" "doc-private-extra"))
;;               ;
;;               ; Miscellaneous pages for other websites
;;               ; org are the org-files that generate the content
;;               ("org-org"
;;                :base-directory "~/git/org/"
;;                :publishing-directory "/ssh:www-data@www:~/org"
;;                :recursive t
;;                :section-numbers nil
;;                :table-of-contents nil
;;                :base-extension "org"
;;                :publishing-function org-html-publish-to-html
;;                :style-include-default nil
;;                :html-head "<link rel=\"stylesheet\" href=\"/org.css\" type=\"text/css\" />"
;;                :author-info nil
;;                :creator-info nil)
;;               ;
;;               ; http://doc.norang.ca/  (norang website)
;;               ; org-mode-doc-org this document
;;               ; org-mode-doc-extra are images and css files that need to be included
;;               ; org-mode-doc is the top-level project that gets published
;;               ; This uses the same target directory as the 'doc' project
;;               ("org-mode-doc-org"
;;                :base-directory "~/git/org-mode-doc/"
;;                :publishing-directory "/ssh:www-data@www:~/doc.norang.ca/htdocs"
;;                :recursive t
;;                :section-numbers nil
;;                :table-of-contents nil
;;                :base-extension "org"
;;                :publishing-function (org-html-publish-to-html)
;;                :plain-source t
;;                :htmlized-source t
;;                :style-include-default nil
;;                :html-head "<link rel=\"stylesheet\" href=\"/org.css\" type=\"text/css\" />"
;;                :author-info nil
;;                :creator-info nil)
;;               ("org-mode-doc-extra"
;;                :base-directory "~/git/org-mode-doc/"
;;                :publishing-directory "/ssh:www-data@www:~/doc.norang.ca/htdocs"
;;                :base-extension "css\\|pdf\\|png\\|jpg\\|gif\\|org"
;;                :publishing-function org-publish-attachment
;;                :recursive t
;;                :author nil)
;;               ("org-mode-doc"
;;                :components ("org-mode-doc-org" "org-mode-doc-extra"))
;;               ;
;;               ; http://doc.norang.ca/  (norang website)
;;               ; org-mode-doc-org this document
;;               ; org-mode-doc-extra are images and css files that need to be included
;;               ; org-mode-doc is the top-level project that gets published
;;               ; This uses the same target directory as the 'doc' project
;;               ("tmp-org"
;;                :base-directory "/tmp/publish/"
;;                :publishing-directory "/ssh:www-data@www:~/www.norang.ca/htdocs/tmp"
;;                :recursive t
;;                :section-numbers nil
;;                :table-of-contents nil
;;                :base-extension "org"
;;                :publishing-function (org-html-publish-to-html org-org-publish-to-org)
;;                :html-head "<link rel=\"stylesheet\" href=\"http://doc.norang.ca/org.css\" type=\"text/css\" />"
;;                :plain-source t
;;                :htmlized-source t
;;                :style-include-default nil
;;                :auto-sitemap t
;;                :sitemap-filename "index.html"
;;                :sitemap-title "Test Publishing Area"
;;                :sitemap-style "tree"
;;                :author-info t
;;                :creator-info t)
;;               ("tmp-extra"
;;                :base-directory "/tmp/publish/"
;;                :publishing-directory "/ssh:www-data@www:~/www.norang.ca/htdocs/tmp"
;;                :base-extension "css\\|pdf\\|png\\|jpg\\|gif"
;;                :publishing-function org-publish-attachment
;;                :recursive t
;;                :author nil)
;;               ("tmp"
;;                :components ("tmp-org" "tmp-extra")))))

;; ; I'm lazy and don't want to remember the name of the project to publish when I modify
;; ; a file that is part of a project.  So this function saves the file, and publishes
;; ; the project that includes this file
;; ;
;; ; It's bound to C-S-F12 so I just edit and hit C-S-F12 when I'm done and move on to the next thing
;; (defun bh/save-then-publish (&optional force)
;;   (interactive "P")
;;   (save-buffer)
;;   (org-save-all-org-buffers)
;;   (let ((org-html-head-extra)
;;         (org-html-validation-link "<a href=\"http://validator.w3.org/check?uri=referer\">Validate XHTML 1.0</a>"))
;;     (org-publish-current-project force)))

;; (global-set-key (kbd "C-s-<f12>") 'bh/save-then-publish)


;; Misellaneous settings

;; (setq org-latex-listings t)

;; (setq org-html-xml-declaration
;;       (quote
;;        (("html" . "")
;;      ("was-html" . "<?xml version=\"1.0\" encoding=\"%s\"?>")
;;      ("php" . "<?php echo \"<?xml version=\\\"1.0\\\" encoding=\\\"%s\\\" ?>\"; ?>"))))

;; (setq org-export-allow-BIND t)

;; ----- Reminders ------------------------------------------------------------
;;
;; Erase all reminders and rebuilt reminders for today from the agenda
(defun bh/org-agenda-to-appt ()
  (interactive)
  (setq appt-time-msg-list nil)
  (org-agenda-to-appt))

;; Rebuild the reminders everytime the agenda is displayed
(add-hook 'org-finalize-agenda-hook 'bh/org-agenda-to-appt 'append)

;; This is at the end of my .emacs - so appointments are set up when
;; Emacs starts
(bh/org-agenda-to-appt)

;; Activate appointments so we get notifications
(appt-activate t)

;; If we leave Emacs running overnight - reset the appointments one
;; minute after midnight
(run-at-time "24:01" nil 'bh/org-agenda-to-appt)

;; ----- Productivity Tools ---------------------------------------------------
;;
;; http://doc.norang.ca/org-mode.html#ProductivityTools
;;; Enable abbrev-mode
(add-hook 'org-mode-hook (lambda () (abbrev-mode 1)))

;; Skeletons
;;
;; sblk - Generic block #+begin_FOO .. #+end_FOO
(define-skeleton skel-org-block
  "Insert an org block, querying for type."
  "Type: "
  "#+begin_" str "\n"
  _ - \n
  "#+end_" str "\n")

(define-abbrev org-mode-abbrev-table "sblk" "" 'skel-org-block)

;; ;; splantuml - PlantUML Source block
;; (define-skeleton skel-org-block-plantuml
;;   "Insert a org plantuml block, querying for filename."
;;   "File (no extension): "
;;   "#+begin_src plantuml :file " str ".png :cache yes\n"
;;   _ - \n
;;   "#+end_src\n")

;; (define-abbrev org-mode-abbrev-table "splantuml" "" 'skel-org-block-plantuml)

;; (define-skeleton skel-org-block-plantuml-activity
;;   "Insert a org plantuml block, querying for filename."
;;   "File (no extension): "
;;   "#+begin_src plantuml :file " str "-act.png :cache yes :tangle " str "-act.txt\n"
;;   (bh/plantuml-reset-counters)
;;   "@startuml\n"
;;   "skinparam activity {\n"
;;   "BackgroundColor<<New>> Cyan\n"
;;   "}\n\n"
;;   "title " str " - \n"
;;   "note left: " str "\n"
;;   "(*) --> \"" str "\"\n"
;;   "--> (*)\n"
;;   _ - \n
;;   "@enduml\n"
;;   "#+end_src\n")

;; (defvar bh/plantuml-if-count 0)

;; (defun bh/plantuml-if ()
;;   (incf bh/plantuml-if-count)
;;   (number-to-string bh/plantuml-if-count))

;; (defvar bh/plantuml-loop-count 0)

;; (defun bh/plantuml-loop ()
;;   (incf bh/plantuml-loop-count)
;;   (number-to-string bh/plantuml-loop-count))

;; (defun bh/plantuml-reset-counters ()
;;   (setq bh/plantuml-if-count 0
;;         bh/plantuml-loop-count 0)
;;   "")

;; (define-abbrev org-mode-abbrev-table "sact" "" 'skel-org-block-plantuml-activity)

;; (define-skeleton skel-org-block-plantuml-activity-if
;;   "Insert a org plantuml block activity if statement"
;;   ""
;;   "if \"\" then\n"
;;   "  -> [condition] ==IF" (setq ifn (bh/plantuml-if)) "==\n"
;;   "  --> ==IF" ifn "M1==\n"
;;   "  -left-> ==IF" ifn "M2==\n"
;;   "else\n"
;;   "end if\n"
;;   "--> ==IF" ifn "M2==")

;; (define-abbrev org-mode-abbrev-table "sif" "" 'skel-org-block-plantuml-activity-if)

;; (define-skeleton skel-org-block-plantuml-activity-for
;;   "Insert a org plantuml block activity for statement"
;;   "Loop for each: "
;;   "--> ==LOOP" (setq loopn (bh/plantuml-loop)) "==\n"
;;   "note left: Loop" loopn ": For each " str "\n"
;;   "--> ==ENDLOOP" loopn "==\n"
;;   "note left: Loop" loopn ": End for each " str "\n" )

;; (define-abbrev org-mode-abbrev-table "sfor" "" 'skel-org-block-plantuml-activity-for)

;; (define-skeleton skel-org-block-plantuml-sequence
;;   "Insert a org plantuml activity diagram block, querying for filename."
;;   "File appends (no extension): "
;;   "#+begin_src plantuml :file " str "-seq.png :cache yes :tangle " str "-seq.txt\n"
;;   "@startuml\n"
;;   "title " str " - \n"
;;   "actor CSR as \"Customer Service Representative\"\n"
;;   "participant CSMO as \"CSM Online\"\n"
;;   "participant CSMU as \"CSM Unix\"\n"
;;   "participant NRIS\n"
;;   "actor Customer"
;;   _ - \n
;;   "@enduml\n"
;;   "#+end_src\n")

;; (define-abbrev org-mode-abbrev-table "sseq" "" 'skel-org-block-plantuml-sequence)

;; ;; sdot - Graphviz DOT block
;; (define-skeleton skel-org-block-dot
;;   "Insert a org graphviz dot block, querying for filename."
;;   "File (no extension): "
;;   "#+begin_src dot :file " str ".png :cache yes :cmdline -Kdot -Tpng\n"
;;   "graph G {\n"
;;   _ - \n
;;   "}\n"
;;   "#+end_src\n")

;; (define-abbrev org-mode-abbrev-table "sdot" "" 'skel-org-block-dot)

;; sditaa - Ditaa source block
(define-skeleton skel-org-block-ditaa
  "Insert a org ditaa block, querying for filename."
  "File (no extension): "
  "#+begin_src ditaa :file " str ".png :cache yes\n"
  _ - \n
  "#+end_src\n")

(define-abbrev org-mode-abbrev-table "sditaa" "" 'skel-org-block-ditaa)

;; selisp - Emacs Lisp source block
(define-skeleton skel-org-block-elisp
  "Insert a org emacs-lisp block"
  ""
  "#+begin_src emacs-lisp\n"
  _ - \n
  "#+end_src\n")

(define-abbrev org-mode-abbrev-table "selisp" "" 'skel-org-block-elisp)

;; 18.2.1 Narrowing

(defun bh/org-todo (arg)
  (interactive "p")
  (if (equal arg 4)
      (save-restriction
        (bh/narrow-to-org-subtree)
        (org-show-todo-tree nil))
    (bh/narrow-to-org-subtree)
    (org-show-todo-tree nil)))


(defun bh/widen ()
  (interactive)
  (if (equal major-mode 'org-agenda-mode)
      (progn
        (org-agenda-remove-restriction-lock)
        (when org-agenda-sticky
          (org-agenda-redo)))
    (widen)))


(add-hook
 'org-agenda-mode-hook
 '(lambda ()
    (org-defkey org-agenda-mode-map "W"
                (lambda ()
                  (interactive)
                  (setq bh/hide-scheduled-and-waiting-next-tasks t)
                  (bh/widen))))
 'append)


(defun bh/restrict-to-file-or-follow (arg)
  "Set agenda restriction to 'file or with argument invoke follow mode.
I don't use follow mode very often but I restrict to file all the time
so change the default 'F' binding in the agenda to allow both"
  (interactive "p")
  (if (equal arg 4)
      (org-agenda-follow-mode)
    (widen)
    (bh/set-agenda-restriction-lock 4)
    (org-agenda-redo)
    (beginning-of-buffer)))

(add-hook 'org-agenda-mode-hook
          '(lambda ()
             (org-defkey org-agenda-mode-map "F"
                         'bh/restrict-to-file-or-follow))
          'append)


(defun bh/narrow-to-org-subtree ()
  (widen)
  (org-narrow-to-subtree)
  (save-restriction
    (org-agenda-set-restriction-lock)))


(defun bh/narrow-to-subtree ()
  (interactive)
  (if (equal major-mode 'org-agenda-mode)
      (progn
        (org-with-point-at (org-get-at-bol 'org-hd-marker)
          (bh/narrow-to-org-subtree))
        (when org-agenda-sticky
          (org-agenda-redo)))
    (bh/narrow-to-org-subtree)))

(add-hook 'org-agenda-mode-hook
          '(lambda ()
             (org-defkey org-agenda-mode-map "N" 'bh/narrow-to-subtree))
          'append)


(defun bh/narrow-up-one-org-level ()
  (widen)
  (save-excursion
    (outline-up-heading 1 'invisible-ok)
    (bh/narrow-to-org-subtree)))


(defun bh/get-pom-from-agenda-restriction-or-point ()
  (or (org-get-at-bol 'org-hd-marker)
      (and (marker-position org-agenda-restrict-begin)
           org-agenda-restrict-begin)
      (and (equal major-mode 'org-mode) (point))
      org-clock-marker))


(defun bh/narrow-up-one-level ()
  (interactive)
  (if (equal major-mode 'org-agenda-mode)
      (org-with-point-at (bh/get-pom-from-agenda-restriction-or-point)
        (bh/narrow-up-one-org-level))
    (bh/narrow-up-one-org-level)))


(add-hook 'org-agenda-mode-hook
          '(lambda ()
             (org-defkey org-agenda-mode-map "U" 'bh/narrow-up-one-level))
          'append)


(defun bh/narrow-to-org-project ()
  (widen)
  (save-excursion
    (bh/find-project-task)
    (bh/narrow-to-org-subtree)))


(defun bh/narrow-to-project ()
  (interactive)
  (if (equal major-mode 'org-agenda-mode)
      (progn
        (org-with-point-at (bh/get-pom-from-agenda-restriction-or-point)
          (bh/narrow-to-org-project)
          (save-excursion
            (bh/find-project-task)
            (org-agenda-set-restriction-lock)))
        (org-agenda-redo)
        (beginning-of-buffer))
    (bh/narrow-to-org-project)
    (save-restriction
      (org-agenda-set-restriction-lock))))

(add-hook 'org-agenda-mode-hook
          '(lambda ()
             (org-defkey org-agenda-mode-map "P" 'bh/narrow-to-project))
          'append)


(defvar bh/project-list nil)

(defun bh/view-next-project ()
  (interactive)
  (let (num-project-left current-project)
    (unless (marker-position org-agenda-restrict-begin)
      (goto-char (point-min))
                                        ; Clear all of the existing markers on the list
      (while bh/project-list
        (set-marker (pop bh/project-list) nil))
      (re-search-forward "Tasks to Refile")
      (forward-visible-line 1))

    ;; Build a new project marker list
    (unless bh/project-list
      (while (< (point) (point-max))
        (while (and (< (point) (point-max))
                    (or (not (org-get-at-bol 'org-hd-marker))
                        (org-with-point-at (org-get-at-bol 'org-hd-marker)
                          (or (not (bh/is-project-p))
                              (bh/is-project-subtree-p)))))
          (forward-visible-line 1))
        (when (< (point) (point-max))
          (add-to-list 'bh/project-list
                       (copy-marker (org-get-at-bol 'org-hd-marker))
                       'append))
        (forward-visible-line 1)))

    ;; Pop off the first marker on the list and display
    (setq current-project (pop bh/project-list))
    (when current-project
      (org-with-point-at current-project
        (setq bh/hide-scheduled-and-waiting-next-tasks nil)
        (bh/narrow-to-project))
                                        ; Remove the marker
      (setq current-project nil)
      (org-agenda-redo)
      (beginning-of-buffer)
      (setq num-projects-left (length bh/project-list))
      (if (> num-projects-left 0)
          (message "%s projects left to view" num-projects-left)
        (beginning-of-buffer)
        (setq bh/hide-scheduled-and-waiting-next-tasks t)
        (error "All projects viewed.")))))


(add-hook 'org-agenda-mode-hook
          '(lambda ()
             (org-defkey org-agenda-mode-map "V" 'bh/view-next-project))
          'append)

(setq org-show-entry-below (quote ((default))))


;; 18.2.2 Limit Agenda to Subtree

(add-hook 'org-agenda-mode-hook
          '(lambda ()
             (org-defkey org-agenda-mode-map "\C-c\C-x<"
                         'bh/set-agenda-restriction-lock))
          'append)

(defun bh/set-agenda-restriction-lock (arg)
  "Set restriction lock to current task subtree or file if prefix
is specified"
  (interactive "p")
  (let* ((pom (bh/get-pom-from-agenda-restriction-or-point))
         (tags (org-with-point-at pom (org-get-tags-at))))
    (let ((restriction-type (if (equal arg 4) 'file 'subtree)))
      (save-restriction
        (cond
         ((and (equal major-mode 'org-agenda-mode) pom)
          (org-with-point-at pom
            (org-agenda-set-restriction-lock restriction-type))
          (org-agenda-redo))
         ((and (equal major-mode 'org-mode) (org-before-first-heading-p))
          (org-agenda-set-restriction-lock 'file))
         (pom
          (org-with-point-at pom
            (org-agenda-set-restriction-lock restriction-type))))))))

;; Limit restriction lock highlighting to the headline only
(setq org-agenda-restriction-lock-highlight-subtree nil)


;; 18.2.3 Limit Agenda to File

;; Always highlight the current agenda line
(add-hook 'org-agenda-mode-hook
          '(lambda () (hl-line-mode 1))
          'append)

;; 18.3.2 Keep Timestamped Tasks Visible on Global Todo Lists

;; Keep tasks with dates on the global todo lists
(setq org-agenda-todo-ignore-with-date nil)

;; Keep tasks with deadlines on the global todo lists
(setq org-agenda-todo-ignore-deadlines nil)

;; Keep tasks with scheduled dates on the global todo lists
(setq org-agenda-todo-ignore-scheduled nil)

;; Keep tasks with timestamps on the global todo lists
(setq org-agenda-todo-ignore-timestamp nil)

;; Remove completed deadline tasks from the agenda view
(setq org-agenda-skip-deadline-if-done t)

;; Remove completed scheduled tasks from the agenda view
(setq org-agenda-skip-scheduled-if-done t)

;; Remove completed items from search results
(setq org-agenda-skip-timestamp-if-done t)


;; 18.3.3 Use Diary for Holidays and Appointments

(setq org-agenda-include-diary nil)
(setq org-agenda-diary-file "~/src/org/diary.org")

(setq org-agenda-insert-diary-extract-time t)

;; Include agenda archive files when searching for things
(setq org-agenda-text-search-extra-files (quote (agenda-archives)))

;; 18.3.5 Agenda View Tweaks

;; Show all future entries for repeating tasks
(setq org-agenda-repeating-timestamp-show-all t)

;; Show all agenda dates - even if they are empty
(setq org-agenda-show-all-dates t)

;; Sorting order for tasks on the agenda
(setq org-agenda-sorting-strategy
      (quote ((agenda habit-down time-up
                      user-defined-up effort-up category-keep)
              (todo category-up effort-up)
              (tags category-up effort-up)
              (search category-up))))

;; Start the weekly agenda on Monday
(setq org-agenda-start-on-weekday 1)

;; Enable time grid display and current time marker
(setq org-agenda-time-grid (quote ((daily today remove-match)
                                   #("----------------" 0 16 (org-heading t))
                                   (0900 1100 1300 1500 1700))))

;; Display tags farther right
(setq org-agenda-tags-column -102)

;;
;; Agenda sorting functions
;;
(setq org-agenda-cmp-user-defined 'bh/agenda-sort)

(defun bh/agenda-sort (a b)
  "Sorting strategy for agenda items.
Late deadlines first, then scheduled, then non-late deadlines"
  (let (result num-a num-b)
    (cond
     ;; time specific items already sorted by org-agenda-sorting-strategy

     ; non-deadline and non-scheduled items next
     ((bh/agenda-sort-test 'bh/is-not-scheduled-or-deadline a b))

     ; deadlines for today next
     ((bh/agenda-sort-test 'bh/is-due-deadline a b))

     ; late deadlines next
     ((bh/agenda-sort-test-num 'bh/is-late-deadline '> a b))

     ; scheduled items for today next
     ((bh/agenda-sort-test 'bh/is-scheduled-today a b))

     ; late scheduled items next
     ((bh/agenda-sort-test-num 'bh/is-scheduled-late '> a b))

     ; pending deadlines last
     ((bh/agenda-sort-test-num 'bh/is-pending-deadline '< a b))

     ; finally default to unsorted
     (t (setq result nil)))
    result))

(defmacro bh/agenda-sort-test (fn a b)
  "Test for agenda sort"
  `(cond
    ;; if both match leave them unsorted
    ((and (apply ,fn (list ,a))
          (apply ,fn (list ,b)))
     (setq result nil))
    ;; if a matches put a first
    ((apply ,fn (list ,a))
     (setq result -1))
    ;; otherwise if b matches put b first
    ((apply ,fn (list ,b))
     (setq result 1))
    ;; if none match leave them unsorted
    (t nil)))

(defmacro bh/agenda-sort-test-num (fn compfn a b)
  `(cond
    ((apply ,fn (list ,a))
     (setq num-a (string-to-number (match-string 1 ,a)))
     (if (apply ,fn (list ,b))
         (progn
           (setq num-b (string-to-number (match-string 1 ,b)))
           (setq result (if (apply ,compfn (list num-a num-b))
                            -1
                          1)))
       (setq result -1)))
    ((apply ,fn (list ,b))
     (setq result 1))
    (t nil)))

(defun bh/is-not-scheduled-or-deadline (date-str)
  (and (not (bh/is-deadline date-str))
       (not (bh/is-scheduled date-str))))

(defun bh/is-due-deadline (date-str)
  (string-match "Deadline:" date-str))

(defun bh/is-late-deadline (date-str)
  (string-match "\\([0-9]*\\) d\. ago:" date-str))

(defun bh/is-pending-deadline (date-str)
  (string-match "In \\([^-]*\\)d\.:" date-str))

(defun bh/is-deadline (date-str)
  (or (bh/is-due-deadline date-str)
      (bh/is-late-deadline date-str)
      (bh/is-pending-deadline date-str)))

(defun bh/is-scheduled (date-str)
  (or (bh/is-scheduled-today date-str)
      (bh/is-scheduled-late date-str)))

(defun bh/is-scheduled-today (date-str)
  (string-match "Scheduled:" date-str))

(defun bh/is-scheduled-late (date-str)
  (string-match "Sched\.\\(.*\\)x:" date-str))


;; 18.3.6 Sticky Agendas

(setq org-agenda-sticky t)


;; ;; 18.4 Checklist Handling

;; (add-to-list 'load-path
;;              (expand-file-name "~/local/lib/elisp/org-mode/contrib/lisp"))

;; (require 'org-checklist)


;; ;; disallow DONE status for tasks with open subtasks
;; ;; NB: repeating tasks with :NOBLOCKING: t are exempt
;; (setq org-enforce-todo-dependencies t)

;; ;; hide blank lines between headings
;; (setq org-cycle-separator-lines 0)

;; ;; prevent creation of blanks before headings, list item blanks ok
;; (setq org-blank-before-new-entry (quote ((heading)
;;                                          (plain-list-item . auto))))

;; ;; Add new tasks quickly
;; (setq org-insert-heading-respect-content nil)

;; ;; float notes to task top
;; (setq org-reverse-note-order nil)

;; 18.7.6 Search and Show

(setq org-show-following-heading t)
(setq org-show-hierarchy-above t)
(setq org-show-siblings (quote ((default))))

;; 18.7.7 Editing and Special Keys
(setq org-special-ctrl-a/e t)
(setq org-special-ctrl-k t)
(setq org-yank-adjusted-subtrees t)


;; 18.8 Attachments

;; generate unique attachment ids
(setq org-id-method (quote uuidgen))

;; 18.9 Deadlines
(setq org-deadline-warning-days 30)

(setq org-table-export-default-format "orgtbl-to-csv")

;; 18.11 Minimise Emacs Frames
(setq org-link-frame-setup (quote ((vm . vm-visit-folder)
                                   (gnus . org-gnus-no-new-news)
                                   (file . find-file))))

;; Use the current window for C-c ' source editing
(setq org-src-window-setup 'current-window)


;; 18.12 Logging
(setq org-log-done (quote time))
(setq org-log-into-drawer t)
(setq org-log-state-notes-insert-after-drawers nil)

(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
              (sequence "WAIT(w@/!)" "HOLD(h@/!)" "|"
                        "CANC(c@/!)" "PHONE" "MEETING"))))


;; 18.13 Time Limits
;; Need a sound file you actually *have* bucco
;; (setq org-clock-sound "/usr/local/lib/tngchime.wav")

;; 18.14 Habit Tracking

(require 'org-habit) ;; suspect this should be enabled by default, isn't

; Enable habit tracking (and a bunch of other modules)
(setq org-modules (quote (org-bbdb
                          org-bibtex
                          org-crypt
                          org-gnus
                          org-id
                          org-info
                          org-jsinfo
                          org-habit
                          org-inlinetask
                          org-irc
                          org-mew
                          org-mhe
                          org-protocol
                          org-rmail
                          org-vm
                          org-wl
                          org-w3m)))

; position the habit graph on the agenda to the right of the default
(setq org-habit-graph-column 50)

(run-at-time "06:00" 86400 '(lambda () (setq org-habit-show-habits t)))

;; 18.16 Auto Revert (Buffer) Mode
(global-auto-revert-mode t)


;; 18.17 Encryption
(require 'org-crypt)
; Encrypt all entries before saving
(org-crypt-use-before-save-magic)
(setq org-tags-exclude-from-inheritance (quote ("crypt")))
; GPG key to use for encryption
;;(setq org-crypt-key "F0B66B40")

(setq org-crypt-disable-auto-save nil)


;; 18.18 Speed Commands

(setq org-use-speed-commands t)
(setq org-speed-commands-user
      (quote (("0" . ignore)
              ("1" . ignore)
              ("2" . ignore)
              ("3" . ignore)
              ("4" . ignore)
              ("5" . ignore)
              ("6" . ignore)
              ("7" . ignore)
              ("8" . ignore)
              ("9" . ignore)

              ("a" . ignore)
              ("d" . ignore)
              ("h" . bh/hide-other)
              ("i" progn
               (forward-char 1)
               (call-interactively 'org-insert-heading-respect-content))
              ("k" . org-kill-note-or-show-branches)
              ("l" . ignore)
              ("m" . ignore)
              ("q" . bh/show-org-agenda)
              ("r" . ignore)
              ("s" . org-save-all-org-buffers)
              ("w" . org-refile)
              ("x" . ignore)
              ("y" . ignore)
              ("z" . org-add-note)

              ("A" . ignore)
              ("B" . ignore)
              ("E" . ignore)
              ("F" . bh/restrict-to-file-or-follow)
              ("G" . ignore)
              ("H" . ignore)
              ("J" . org-clock-goto)
              ("K" . ignore)
              ("L" . ignore)
              ("M" . ignore)
              ("N" . bh/narrow-to-org-subtree)
              ("P" . bh/narrow-to-org-project)
              ("Q" . ignore)
              ("R" . ignore)
              ("S" . ignore)
              ("T" . bh/org-todo)
              ("U" . bh/narrow-up-one-org-level)
              ("V" . ignore)
              ("W" . bh/widen)
              ("X" . ignore)
              ("Y" . ignore)
              ("Z" . ignore))))

(defun bh/show-org-agenda ()
  (interactive)
  (if org-agenda-sticky
      (switch-to-buffer "*Org Agenda( )*")
    (switch-to-buffer "*Org Agenda*"))
  (delete-other-windows))

;; 18.19 Org-protocol
(require 'org-protocol)

;; unsure this is required, and if so, may belong in .emacs?
;; (setq require-final-newline t)


;; 18.21 Insert Inactive Timestamps and Exclude from Export

(defvar bh/insert-inactive-timestamp t)

(defun bh/toggle-insert-inactive-timestamp ()
  (interactive)
  (setq bh/insert-inactive-timestamp (not bh/insert-inactive-timestamp))
  (message "Heading timestamps are %s"
           (if bh/insert-inactive-timestamp "ON" "OFF")))

(defun bh/insert-inactive-timestamp ()
  (interactive)
  (org-insert-time-stamp nil t t nil nil nil))

(defun bh/insert-heading-inactive-timestamp ()
  (save-excursion
    (when bh/insert-inactive-timestamp
      (org-return)
      (org-cycle)
      (bh/insert-inactive-timestamp))))

(add-hook 'org-insert-heading-hook
          'bh/insert-heading-inactive-timestamp 'append)

(setq org-export-with-timestamps nil)


;; 18.22 Return follows link
;; (implies point required on previous line to insert newline)
(setq org-return-follows-link t)


;; 18.23


;; 18.24 Meeting Notes
;;
;; available in the kill buffer, ready for other applications
;;
(defun bh/prepare-meeting-notes ()
  "Prepare meeting notes for email Take selected region and
   convert tabs to spaces, mark TODOs with leading >>>, and copy
   to kill ring for pasting"
  (interactive)
  (let (prefix)
    (save-excursion
      (save-restriction
        (narrow-to-region (region-beginning) (region-end))
        (untabify (point-min) (point-max))
        (goto-char (point-min))
        (while
            (re-search-forward "^\\( *-\\\) \\(TODO\\|DONE\\): " (point-max) t)
          (replace-match (concat (make-string (length (match-string 1)) ?>)
                                 " " (match-string 2) ": ")))
        (goto-char (point-min))
        (kill-ring-save (point-min) (point-max))))))


;; 18.27 Prefer Future Dates?
(setq org-read-date-prefer-future nil)
;; (setq org-read-date-prefer-future 'time) ;; default tomorrow for past dates

;; 18.28 Auto-change list bullets
(setq org-list-demote-modify-bullet
      (quote (("+" . "-")
              ("*" . "-")
              ("1." . "-")
              ("1)" . "-")
              ("A)" . "-")
              ("B)" . "-")
              ("a)" . "-")
              ("b)" . "-")
              ("A." . "-")
              ("B." . "-")
              ("a." . "-")
              ("b." . "-"))))

;; 18.29 Remove indent from agenda tags view
(setq org-tags-match-list-sublevels t)


;; 18.31 Persistent Agenda Filters
(setq org-agenda-persistent-filter t)


;; 18.33 Mail links open compose-mail
(setq org-link-mailto-program (quote (compose-mail "%a" "%s")))


;; 18.35 Smex M-x IDO-completion

;; (add-to-list 'load-path (expand-file-name "~/.emacs.d"))
;; (require 'smex)
;; (smex-initialize)

;; (global-set-key (kbd "M-x") 'smex)
;; (global-set-key (kbd "C-x x") 'smex)
;; (global-set-key (kbd "M-X") 'smex-major-mode-commands)


;;
;; 18.36 Bookmark handling
;;
(global-set-key (kbd "<C-f6>")
                '(lambda () (interactive) (bookmark-set "SAVED")))
(global-set-key (kbd "<f6>")
                '(lambda () (interactive) (bookmark-jump "SAVED")))


;; 18.37 org-mime
;; (require 'org-mime)


;; 18.38 Remove Multi-State log details from Agenda
(setq org-agenda-skip-additional-timestamps-same-entry t)


;; 18.39 Drop Old Style Table References
(setq org-table-use-standard-references (quote from))

;; 18.40 System file application settings (consistent apps)
(setq org-file-apps
      (quote ((auto-mode . emacs)
              ("\\.mm\\'" . system)
              ("\\.x?html?\\'" . system)
              ("\\.pdf\\'" . system))))


;; 18.41 Replace current window with Agenda
(setq org-agenda-window-setup 'current-window)

;; 18.42 Delete IDs when cloning
(setq org-clone-delete-id t)

;; 18.43 Cycle plain lists
(setq org-cycle-include-plain-lists t)

;; 18.44 Show Source Block Syntax Highlights
(setq org-src-fontify-natively t)

;; 18.45 Insert Structure Template Blocks
(setq org-structure-template-alist
      (quote
       (("s" "#+begin_src ?\n\n#+end_src" "<src lang=\"?\">\n\n</src>")
        ("e" "#+begin_example\n?\n#+end_example" "<example>\n?\n</example>")
        ("q" "#+begin_quote\n?\n#+end_quote" "<quote>\n?\n</quote>")
        ("v" "#+begin_verse\n?\n#+end_verse" "<verse>\n?\n</verse>")
        ("c" "#+begin_center\n?\n#+end_center" "<center>\n?\n</center>")
        ("l" "#+begin_latex\n?\n#+end_latex"
         "<literal style=\"latex\">\n?\n</literal>")
        ("L" "#+latex: " "<literal style=\"latex\">?</literal>")
        ("h" "#+begin_html\n?\n#+end_html"
         "<literal style=\"html\">\n?\n</literal>")
        ("H" "#+html: " "<literal style=\"html\">?</literal>")
        ("a" "#+begin_ascii\n?\n#+end_ascii")
        ("A" "#+ascii: ")
        ("i" "#+index: ?" "#+index: ?")
        ("I" "#+include %file ?" "<include file=%file markup=\"?\">"))))


;; 18.46 NEXT is for Tasks (not Projects)
(defun bh/mark-next-parent-tasks-todo ()
  "Visit each parent task and change NEXT states to TODO"
  (let ((mystate (or (and (fboundp 'org-state)
                          state)
                     (nth 2 (org-heading-components)))))
    (when mystate
      (save-excursion
        (while (org-up-heading-safe)
          (when (member (nth 2 (org-heading-components)) (list "NEXT"))
            (org-todo "TODO")))))))

(add-hook 'org-after-todo-state-change-hook
          'bh/mark-next-parent-tasks-todo 'append)
(add-hook 'org-clock-in-hook 'bh/mark-next-parent-tasks-todo 'append)

;; 18.47 Startup folded
(setq org-startup-folded t)

;; 18.48 Allow alphabetical list entries
(setq org-alphabetical-lists t)
;; a. item one
;; b. item two

;; 18.49 Orgstruct mode for (gnus) mail
(add-hook 'message-mode-hook 'orgstruct++-mode 'append)
(add-hook 'message-mode-hook 'turn-on-auto-fill 'append)
(add-hook 'message-mode-hook 'bbdb-define-all-aliases 'append)
(add-hook 'message-mode-hook 'orgtbl-mode 'append)
(add-hook 'message-mode-hook 'turn-on-flyspell 'append)
(add-hook 'message-mode-hook '(lambda () (setq fill-column 72)) 'append)


;; 18.50 Flyspell everything

;; flyspell mode for spell checking everywhere
(add-hook 'org-mode-hook 'turn-on-flyspell 'append)

;; Disable keys in org-mode
;;    C-c [
;;    C-c ]
;;    C-c ;
;;    C-c C-x C-q  cancelling the clock (we never want this)
(add-hook 'org-mode-hook
          '(lambda ()
             ;; Undefine C-c [ and C-c ] since this breaks my
             ;; org-agenda files when directories are include It
             ;; expands the files in the directories individually
             (org-defkey org-mode-map "\C-c[" 'undefined)
             (org-defkey org-mode-map "\C-c]" 'undefined)
             (org-defkey org-mode-map "\C-c;" 'undefined)
             (org-defkey org-mode-map "\C-c\C-x\C-q" 'undefined))
          'append)

(add-hook 'org-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c M-o") 'bh/mail-subtree))
          'append)

(defun bh/mail-subtree ()
  (interactive)
  (org-mark-subtree)
  (org-mime-subtree))


;; 18.51 Preserve Source block indentation
(setq org-src-preserve-indentation nil)
(setq org-edit-src-content-indentation 0)

;; 18.52 Prevent edit of invisible text
(setq org-catch-invisible-edits 'error)

;; UTF-8 default encoding
(setq org-export-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-charset-priority 'unicode)
(setq default-process-coding-system '(utf-8-unix . utf-8-unix))

;; ;; 18.54 Clock Duration in Hours
;; (setq org-time-clocksum-format
;;       '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t))

;; 18.55 Create unique Task ID when linking: allows flexible, mobile links
(setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)

;; ----- Automated GITup ------------------------------------------------------
;;
;; supply set of freshly saved org files
(run-at-time "00:59" 1800 'org-save-all-org-buffers)
;; for cron job commit at 01,31 past the hour using ~/src/sys/org-git.sync

;; ----- helper functions -----------------------------------------------------
;;
(defun bh/is-project-p ()
  "Any task with a todo keyword subtask"
  (save-restriction
    (widen)
    (let ((has-subtask)
          (subtree-end (save-excursion (org-end-of-subtree t)))
          (is-a-task (member (nth 2 (org-heading-components))
                             org-todo-keywords-1)))
      (save-excursion
        (forward-line 1)
        (while (and (not has-subtask)
                    (< (point) subtree-end)
                    (re-search-forward "^\*+ " subtree-end t))
          (when (member (org-get-todo-state) org-todo-keywords-1)
            (setq has-subtask t))))
      (and is-a-task has-subtask))))

(defun bh/is-project-subtree-p ()
  "Any task with a todo keyword that is in a project subtree.
Callers of this function already widen the buffer view."
  (let ((task (save-excursion (org-back-to-heading 'invisible-ok)
                              (point))))
    (save-excursion
      (bh/find-project-task)
      (if (equal (point) task)
          nil
        t))))

(defun bh/is-task-p ()
  "Any task with a todo keyword and no subtask"
  (save-restriction
    (widen)
    (let ((has-subtask)
          (subtree-end (save-excursion (org-end-of-subtree t)))
          (is-a-task (member (nth 2 (org-heading-components))
                             org-todo-keywords-1)))
      (save-excursion
        (forward-line 1)
        (while (and (not has-subtask)
                    (< (point) subtree-end)
                    (re-search-forward "^\*+ " subtree-end t))
          (when (member (org-get-todo-state) org-todo-keywords-1)
            (setq has-subtask t))))
      (and is-a-task (not has-subtask)))))

(defun bh/is-subproject-p ()
  "Any task which is a subtask of another project"
  (let ((is-subproject)
        (is-a-task (member (nth 2 (org-heading-components))
                           org-todo-keywords-1)))
    (save-excursion
      (while (and (not is-subproject) (org-up-heading-safe))
        (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
          (setq is-subproject t))))
    (and is-a-task is-subproject)))

(defun bh/list-sublevels-for-projects-indented ()
  "Set org-tags-match-list-sublevels so when restricted to a
subtree we list all subtasks.  This is normally used by skipping
functions where this variable is already local to the agenda."
  (if (marker-buffer org-agenda-restrict-begin)
      (setq org-tags-match-list-sublevels 'indented)
    (setq org-tags-match-list-sublevels nil))
  nil)

(defun bh/list-sublevels-for-projects ()
  "Set org-tags-match-list-sublevels so when restricted to a
subtree we list all subtasks.  This is normally used by skipping
functions where this variable is already local to the agenda."
  (if (marker-buffer org-agenda-restrict-begin)
      (setq org-tags-match-list-sublevels t)
    (setq org-tags-match-list-sublevels nil))
  nil)

(defvar bh/hide-scheduled-and-waiting-next-tasks t)

(defun bh/toggle-next-task-display ()
  (interactive)
  (setq bh/hide-scheduled-and-waiting-next-tasks
        (not bh/hide-scheduled-and-waiting-next-tasks))
  (when  (equal major-mode 'org-agenda-mode)
    (org-agenda-redo))
  (message "%s WAIT and SCHEDULED NEXT Tasks"
           (if bh/hide-scheduled-and-waiting-next-tasks "Hide" "Show")))

(defun bh/skip-stuck-projects ()
  "Skip trees that are not stuck projects"
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading)
                                             (point-max)))))
      (if (bh/is-project-p)
          (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
                 (has-next ))
            (save-excursion
              (forward-line 1)
              (while (and (not has-next) (< (point) subtree-end)
                          (re-search-forward "^\\*+ NEXT " subtree-end t))
                (unless (member "WAIT" (org-get-tags-at))
                  (setq has-next t))))
            (if has-next
                nil
              next-headline)) ; a stuck project, has subtasks but no next task
        nil))))

(defun bh/skip-non-stuck-projects ()
  "Skip trees that are not stuck projects"
  ;; (bh/list-sublevels-for-projects-indented)
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading)
                                             (point-max)))))
      (if (bh/is-project-p)
          (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
                 (has-next ))
            (save-excursion
              (forward-line 1)
              (while (and (not has-next) (< (point) subtree-end)
                          (re-search-forward "^\\*+ NEXT " subtree-end t))
                (unless (member "WAIT" (org-get-tags-at))
                  (setq has-next t))))
            (if has-next
                next-headline
              nil)) ; a stuck project, has subtasks but no next task
        next-headline))))

(defun bh/skip-non-projects ()
  "Skip trees that are not projects"
  ;; (bh/list-sublevels-for-projects-indented)
  (if (save-excursion (bh/skip-non-stuck-projects))
      (save-restriction
        (widen)
        (let ((subtree-end (save-excursion (org-end-of-subtree t))))
          (cond
           ((bh/is-project-p)
            nil)
           ((and (bh/is-project-subtree-p) (not (bh/is-task-p)))
            nil)
           (t
            subtree-end))))
    (save-excursion (org-end-of-subtree t))))

(defun bh/skip-project-trees-and-habits ()
  "Skip trees that are projects"
  (save-restriction
    (widen)
    (let ((subtree-end (save-excursion (org-end-of-subtree t))))
      (cond
       ((bh/is-project-p)
        subtree-end)
       ((org-is-habit-p)
        subtree-end)
       (t
        nil)))))

(defun bh/skip-projects-and-habits-and-single-tasks ()
  "Skip trees that are projects, tasks that are habits, single
non-project tasks"
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading)
                                             (point-max)))))
      (cond
       ((org-is-habit-p)
        next-headline)
       ((and bh/hide-scheduled-and-waiting-next-tasks
             (member "WAIT" (org-get-tags-at)))
        next-headline)
       ((bh/is-project-p)
        next-headline)
       ((and (bh/is-task-p) (not (bh/is-project-subtree-p)))
        next-headline)
       (t
        nil)))))

(defun bh/skip-project-tasks-maybe ()
  "Show tasks related to the current restriction.
When restricted to a project, skip project and sub project tasks,
habits, NEXT tasks, and loose tasks.  When not restricted, skip
project and sub-project tasks, habits, and project related
tasks."
  (save-restriction
    (widen)
    (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
           (next-headline (save-excursion (or (outline-next-heading)
                                              (point-max))))
           (limit-to-project (marker-buffer org-agenda-restrict-begin)))
      (cond
       ((bh/is-project-p)
        next-headline)
       ((org-is-habit-p)
        subtree-end)
       ((and (not limit-to-project)
             (bh/is-project-subtree-p))
        subtree-end)
       ((and limit-to-project
             (bh/is-project-subtree-p)
             (member (org-get-todo-state) (list "NEXT")))
        subtree-end)
       (t
        nil)))))

(defun bh/skip-project-tasks ()
  "Show non-project tasks.
Skip project and sub-project tasks, habits, and project related tasks."
  (save-restriction
    (widen)
    (let* ((subtree-end (save-excursion (org-end-of-subtree t))))
      (cond
       ((bh/is-project-p)
        subtree-end)
       ((org-is-habit-p)
        subtree-end)
       ((bh/is-project-subtree-p)
        subtree-end)
       (t
        nil)))))

(defun bh/skip-non-project-tasks ()
  "Show project tasks.
Skip project and sub-project tasks, habits, and loose non-project tasks."
  (save-restriction
    (widen)
    (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
           (next-headline (save-excursion (or (outline-next-heading)
                                              (point-max)))))
      (cond
       ((bh/is-project-p)
        next-headline)
       ((org-is-habit-p)
        subtree-end)
       ((and (bh/is-project-subtree-p)
             (member (org-get-todo-state) (list "NEXT")))
        subtree-end)
       ((not (bh/is-project-subtree-p))
        subtree-end)
       (t
        nil)))))

(defun bh/skip-projects-and-habits ()
  "Skip trees that are projects and tasks that are habits"
  (save-restriction
    (widen)
    (let ((subtree-end (save-excursion (org-end-of-subtree t))))
      (cond
       ((bh/is-project-p)
        subtree-end)
       ((org-is-habit-p)
        subtree-end)
       (t
        nil)))))

(defun bh/skip-non-subprojects ()
  "Skip trees that are not projects"
  (let ((next-headline (save-excursion (outline-next-heading))))
    (if (bh/is-subproject-p)
        nil
      next-headline)))

;; Local Variables:
;; coding: utf-8
;; End:

(provide 'init-local-org)
;;; init-local-org.el ends here
