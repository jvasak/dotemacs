;; Org mode stuff, not in global config
(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

(require 'org-install)

(setq org-drawers '("PROPERTIES" "CLOCK" "LOGBOOK"))
(setq org-log-into-drawer t)
;; (setq org-clock-into-drawer t)


;;
;; Customize variables
;;
(setq org-agenda-files (quote ("~/Dropbox/gtd/gtd.org"
                               "~/Dropbox/gtd/incoming.org")))
(setq org-tags-column -88)
(setq org-agenda-tags-column -100)
(setq org-use-fast-todo-selection t)
(setq org-use-tag-inheritance t)
(setq org-agenda-span 1)
(setq org-agenda-repeating-timestamp-show-all t)
(setq org-agenda-show-all-dates t)
(setq org-agenda-skip-deadline-if-done t)
(setq org-agenda-skip-scheduled-if-done t)
(setq org-agenda-start-on-weekday nil)
(setq org-agenda-todo-ignore-deadlines t)
(setq org-agenda-todo-ignore-scheduled t)
(setq org-agenda-todo-ignore-with-date t)
(setq org-deadline-warning-days 7)
(setq org-fast-tag-selection-single-key nil)
;;(setq org-log-done 'time)
;;(setq org-log-done 'note)
(setq org-reverse-note-order nil)
;;(setq org-tags-match-list-sublevels nil)
(setq org-time-stamp-rounding-minutes '(0 5))
(setq org-hide-leading-stars 1)
(setq org-startup-folded 'content)
(setq org-agenda-include-diary nil)
(setq org-show-hierarchy-above t)
(setq org-agenda-start-with-follow-mode nil)
(setq org-agenda-start-with-clockreport-mode t)

;;
;; Clock settings
;;
;; From: http://doc.norang.ca/org-mode.html#sec-7.1
;;
;; Resume clocking tasks when emacs is restarted
(setq org-clock-persist 'history)
(setq org-clock-persistence-insinuate)
;;
;; Yes it's long... but more is better ;)
;;(setq org-clock-history-length 10)
;; Resume clocking task on clock-in if the clock is open
;;(setq org-clock-in-resume t)
;; Sometimes I change tasks I'm clocking quickly - this removes clocked tasks with 0:00 duration
;; (setq org-clock-out-remove-zero-time-clocks t)
;; Don't clock out when moving task to a done state
;;(setq org-clock-out-when-done nil)
;; Agenda log mode items to display (clock time only by default)
;;(setq org-agenda-log-mode-items (quote (clock)))
;; Agenda clock report parameters (no links, 2 levels deep)
;;(setq org-agenda-clockreport-parameter-plist (quote (:link nil :maxlevel 2)))

;; Custom key bindings
(global-set-key (kbd "<f12>")   'org-agenda)
(global-set-key (kbd "<S-f12>") 'org-capture)
(global-set-key (kbd "<f5>")    'org-clock-in)
(global-set-key (kbd "<f6>")    'org-clock-goto)
(global-set-key (kbd "<f7>")    'org-clock-out)


;; Define tags
(setq org-tag-alist '((:startgroup . nil)
                      ("@work" . ?w)
                      ("@computer" . ?c)
                      ("@out" . ?o)
                      (:endgroup . nil)
                      ("NEXT" . ?N)
                      ("SCHED" . ?S)
                      (:newline)
                      ("call" . ?p)
                      ("email" . ?e)
                      ("notes" . ?n)
                      ("finance" . ?f)))


;; Define keywords and state transitions
(setq org-todo-keywords (quote ((sequence "TODO(t)" "STARTED(s)" "|" "DONE(d!/!)")
                                (sequence "WAITING(W)" "SOMEDAY(S)" "PROJECT(P)" "|" "CANCELLED(C)"))))

(setq org-todo-keyword-faces (quote (("TODO" :foreground "red" :weight bold)
 ("STARTED" :foreground "blue" :weight bold)
 ("DONE" :foreground "forest green" :weight bold)
 ("WAITING" :foreground "orange" :weight bold)
 ("SOMEDAY" :foreground "magenta" :weight bold)
 ("CANCELLED" :foreground "forest green" :weight bold)
 ("PROJECT" :foreground "dark cyan" :weight bold))))

;;(setq org-todo-state-tags-triggers
;;      (quote (("CANCELLED" ("CANCELLED" . t))
;;              ("WAITING" ("WAITING" . t) ("NEXT"))
;;              ("SOMEDAY" ("WAITING" . t))
;;              (done ("NEXT") ("WAITING"))
;;              ("TODO" ("WAITING") ("CANCELLED"))
;;              ("STARTED" ("WAITING"))
;;              ("PROJECT" ("CANCELLED") ("PROJECT" . t)))))

(setq org-todo-state-tags-triggers
      (quote (("WAITING" ("NEXT"))
              ("SOMEDAY" ("NEXT"))
              (done ("NEXT")))))

;; Change task state to STARTED when clocking in
(setq org-clock-in-switch-to-state "STARTED")

;; Useful to find projects with no next action defined
(setq org-stuck-projects (quote ("/PROJECT" ("WAITING") ("NEXT" "SCHED") "")))


;; Agenda column format
(setq org-columns-default-format
      "%38ITEM(Details) %TAGS(Context) %7TODO(To Do) %5Effort(Est.){:} %6CLOCKSUM{Total}")

;; Agenda Effort estimation
(setq org-global-properties
      '(("Effort_ALL" . "0 0:30 1:00 2:00 4:00 6:00 8:00 16:00 24:00 32:00 40:00 80:00")))


(add-hook 'org-agenda-mode-hook 'hl-line-mode)

(setq org-directory "~/Dropbox/gtd/")
(setq org-mobile-directory "~/Dropbox/MobileOrg")
(setq org-mobile-inbox-for-pull "~/Dropbox/gtd/incoming.org")

(setq org-default-notes-file (concat org-directory "gtd.org"))

;; templates for TODO tasks
(setq org-capture-templates
      '(
        ("t" "Todo" entry (file+headline "~/Dropbox/gtd/gtd.org" "Unfiled")
         "* TODO %? \n %i")
        ("a" "Active Todo" entry (file+headline "~/Dropbox/gtd/gtd.org" "Unfiled")
         "* TODO %? \n %i" ":clock-in")
        ))

;;
;; Refile definition
;;
; Use IDO for target completion
(setq org-completion-use-ido t)

; Targets include this file and any file contributing to the agenda - up to 5 levels deep
(setq org-refile-targets (quote ((org-agenda-files :maxlevel . 5) 
                                 (nil :maxlevel . 5))))

; Targets start with the file name - allows creating level 1 tasks
(setq org-refile-use-outline-path (quote file))

; Targets complete in steps so we start with filename, TAB shows the next level of targets etc 
(setq org-outline-path-complete-in-steps t)


;;
;; Agenda views
;;
(setq org-agenda-custom-commands
      '(

        ("w" "Standard Agenda View"
         ((agenda "" ((org-deadline-warning-days 0)))
          (tags-todo "NEXT")
          (tags-todo "-NEXT+TODO=\"TODO\"|-NEXT+TODO=\"STARTED\"")
          (tags-todo "TODO=\"WAITING\"")
          )
         ((org-agenda-files '("~/Dropbox/gtd/gtd.org"))))

        ("r" "Refile items" tags "REFILE" ((org-agenda-todo-ignore-with-date nil)))

        )

      )

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . nil)
   (R . t)
   (ditaa . t)
   (dot . t)))

(defun my-org-confirm-babel-evaluate (lang body)
  (not (string= lang "dot"))) ; don't ask for dot
(setq org-confirm-babel-evaluate 'my-org-confirm-babel-evaluate)


;; ditaa
(setq org-ditaa-jar-path "~/bin/ditaa0_6b.jar")


;;
;; User contrib to reset checklists on recrring tasks
;;
(load "~/Dropbox/gtd/org-checklist.el")
(load "~/Dropbox/gtd/htmlize.el")

;; Publishing set up:
;;     website - publish to cleopatra
;;     web-dev - publish to local path (testing)

(setq org-publish-project-alist
      '(("orgfiles"
         :base-directory "~/cleo/content/"
         :base-extension "org"
         :publishing-directory "/ssh:jvasak@cleopatra:~/public_html/"
         :publishing-function org-publish-org-to-html
         :headline-levels 3
         :section-numbers t
         :table-of-contents t
;;         :style "<link rel=\"stylesheet\" href=\"../other/mystyle.css\" type=\"text/css\"/>"
         :auto-preamble t
         :auto-postamble t)
        ("images"
         :base-directory "~/cleo/images/"
         :base-extension "jpg\\|gif\\|png"
         :publishing-directory "/ssh:jvasak@cleopatra:~/public_html/images/"
         :publishing-function org-publish-attachment)
        ("other"
         :base-directory "~/cleo/other/"
         :base-extension "css\\|el\\|pdf\\|rpm\\|js"
         :publishing-directory "/ssh:jvasak@cleopatra:~/public_html/other/"
         :publishing-function org-publish-attachment)
        ("website" :components ("orgfiles" "images" "other"))

        ("org-dev"
         :base-directory "~/cleo/content/"
         :base-extension "org"
         :publishing-directory "~/public_html/"
         :publishing-function org-publish-org-to-html
         :headline-levels 3
         :section-numbers t
         :table-of-contents t
;;         :style "<link rel=\"stylesheet\" href=\"../other/mystyle.css\" type=\"text/css\"/>"
         :auto-preamble t
         :auto-postamble t)
        ("img-dev"
         :base-directory "~/cleo/images/"
         :base-extension "jpg\\|gif\\|png"
         :publishing-directory "~/public_html/images/"
         :publishing-function org-publish-attachment)
        ("oth-dev"
         :base-directory "~/cleo/other/"
         :base-extension "css\\|el\\|pdf\\|rpm\\|js"
         :publishing-directory "~/public_html/other/"
         :publishing-function org-publish-attachment)
        ("web-dev" :components ("org-dev" "img-dev" "oth-dev"))))
