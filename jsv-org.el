;; Org mode stuff, not in global config
(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))

(require 'org-install)

;; (require 'org-export-generic)
(require 'org-checklist)
(require 'htmlize)

(setq org-directory "~/Dropbox/gtd/")

(setq org-default-notes-file   (format "%s/%s" org-directory "gtd.org"))
(setq org-agenda-files        '("~/Dropbox/gtd/gtd.org"))

;; Custom key bindings
(global-set-key (kbd "<f2>") (lambda () (interactive) (find-file org-default-notes-file)))
(global-set-key (kbd "<f12>")   'org-agenda)
(global-set-key (kbd "<f11>")   'org-capture)
(global-set-key (kbd "<f5>")    'org-clock-in)
(global-set-key (kbd "<f6>")    'org-clock-goto)
(global-set-key (kbd "<f7>")    'org-clock-out)
(global-set-key "\C-cl"         'org-store-link)

(setq org-drawers '("PROPERTIES" "CLOCK" "LOGBOOK"))
(setq org-log-into-drawer t)
(setq org-clock-into-drawer t)

;;
;; Customize variables
;;
(setq org-tags-column -88)
(setq org-agenda-tags-column -100)
(setq org-use-fast-todo-selection t)
(setq org-use-tag-inheritance t)
(setq org-agenda-span 3)
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
(setq org-clock-history-length 10)
;; Resume clocking task on clock-in if the clock is open
(setq org-clock-in-resume t)
;; Sometimes I change tasks I'm clocking quickly - this removes clocked tasks with 0:00 duration
;; (setq org-clock-out-remove-zero-time-clocks t)
;; Don't clock out when moving task to a done state
;;(setq org-clock-out-when-done nil)
;; Agenda log mode items to display (clock time only by default)
;;(setq org-agenda-log-mode-items (quote (clock)))
;; Agenda clock report parameters (no links, 2 levels deep)
;;(setq org-agenda-clockreport-parameter-plist (quote (:link nil :maxlevel 2)))

;; Define tags
(setq org-tag-alist '((:startgroup . nil)
                      ("ssrp"  . ?s)
                      ("rtt"   . ?r)
                      ("dhs"   . ?d)
                      ("cp5"   . ?5)
                      ("vista" . ?v)
                      (:endgroup . nil)
                      ("jira"  . ?j)
                      ("call"  . ?c)
                      ("email" . ?e)
                      ("notes" . ?n)))


;; Define keywords and state transitions
(setq org-todo-keywords (quote ((sequence "TODO(t)" "STARTED(s)" "|" "DONE(d!/!)")
                                (sequence "WAITING(W)" "SOMEDAY(S)" "PROJECT(P)" "|" "CANCELLED(C)"))))

(setq org-todo-keyword-faces (quote (
                                     ("TODO" :foreground "red" :weight bold)
                                     ("STARTED" :foreground "blue" :weight bold)
                                     ("DONE" :foreground "forest green" :weight bold)
                                     ("WAITING" :foreground "orange" :weight bold)
                                     ("SOMEDAY" :foreground "magenta" :weight bold)
                                     ("CANCELLED" :foreground "forest green" :weight bold)
                                     ("PROJECT" :foreground "dark cyan" :weight bold)
                                     )))

;; Change task state to STARTED when clocking in
(setq org-clock-in-switch-to-state "STARTED")

;; Useful to find projects with no next action defined
(setq org-stuck-projects 
      '("+PROJECT/-DONE" ("TODO" "STARTED") () ""))

;; Agenda column format
(setq org-columns-default-format
      "%38ITEM(Details) %TAGS(Context) %7TODO(To Do) %5Effort(Est.){:} %6CLOCKSUM{Total}")

;; Agenda Effort estimation
(setq org-global-properties
      '(("Effort_ALL" . "0 0:30 1:00 2:00 4:00 6:00 8:00 16:00 24:00 32:00 40:00 80:00")))

(add-hook 'org-agenda-mode-hook 'hl-line-mode)

;; templates for TODO tasks
(setq org-capture-templates
      '(
        ("t" "Todo"        entry (file "~/Dropbox/gtd/gtd.org")
         "* TODO %? \n %i")
        ("a" "Active Todo" entry (file "~/Dropbox/gtd/gtd.org")
         "* TODO %? \n %i" ":clock-in")
        )
      )

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

(setq org-agenda-custom-commands
      '(
        ("w" "Agenda and Task Lists for Work"
         ((agenda "" 
                  ((org-agenda-ndays 7)
                   (org-agenda-start-on-weekday 0)
                   ;; (org-agenda-time-grid nil)
                   (org-agenda-show-log t)))
          (todo "STARTED"
                ((org-agenda-sorting-strategy '(tag-up priority-down))
                 (org-agenda-todo-keyword-format "")))
          (todo "TODO"
                ((org-agenda-sorting-strategy '(tag-up priority-down))
                 (org-agenda-todo-keyword-format "")))
          (todo "PROJECT"
                ((org-agenda-todo-keyword-format "")))
          (todo "WAITING")
          (todo "SOMEDAY"))
         )
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

(require 'org-latex)
(setq org-export-latex-listings t)

(setenv "PATH" (concat "/usr/local/texlive/2012/bin/x86_64-linux:"
                       (getenv "PATH")))

;; Originally taken from Bruno Tavernier: http://thread.gmane.org/gmane.emacs.orgmode/31150/focus=31432
;; but adapted to use latexmk 4.20 or higher.
(defun my-auto-tex-cmd ()
  "When exporting from .org with latex, automatically run latex,
     pdflatex, or xelatex as appropriate, using latexmk."
  (let ((texcmd)))
  ;; default command: oldstyle latex via dvi
  (setq texcmd "latexmk -dvi -pdfps -quiet %f")
  ;; pdflatex -> .pdf
  (if (string-match "LATEX_CMD: pdflatex" (buffer-string))
      (setq texcmd "latexmk -pdf -quiet %f"))
  ;; xelatex -> .pdf
  (if (string-match "LATEX_CMD: xelatex" (buffer-string))
      (setq texcmd "latexmk -pdflatex=xelatex -pdf -quiet %f"))
  ;; LaTeX compilation command
  (setq org-latex-to-pdf-process (list texcmd)))

(add-hook 'org-export-latex-after-initial-vars-hook 'my-auto-tex-cmd)


;; Specify default packages to be included in every tex file, whether pdflatex or xelatex
(setq org-export-latex-packages-alist
      '(("" "graphicx" t)
        ("" "longtable" nil)
        ("" "float" nil)))

(defun my-auto-tex-parameters ()
      "Automatically select the tex packages to include."
      ;; default packages for ordinary latex or pdflatex export
      (setq org-export-latex-default-packages-alist
            '(("AUTO" "inputenc" t)
              ("T1"   "fontenc"   t)
              (""     "fixltx2e"  nil)
              (""     "wrapfig"   nil)
              (""     "soul"      t)
              (""     "textcomp"  t)
              (""     "marvosym"  t)
              (""     "wasysym"   t)
              (""     "latexsym"  t)
              (""     "amssymb"   t)
              (""     "hyperref"  nil)))

      ;; Packages to include when xelatex is used
      (if (string-match "LATEX_CMD: xelatex" (buffer-string))
          (setq org-export-latex-default-packages-alist
                '(("" "fontspec" t)
                  ("" "xunicode" t)
                  ("" "url" t)
                  ("" "rotating" t)
                  ("american" "babel" t)
                  ("babel" "csquotes" t)
                  ("" "listings" nil)
                  ("" "soul" t)
                  ("xetex" "hyperref" nil)
                  )))



      (if (string-match "LATEX_CMD: xelatex" (buffer-string))
          (setq org-export-latex-classes
                (cons '("article"
                        "\\documentclass[11pt,oneside]{article}"
                        ("\\section{%s}" . "\\section*{%s}")
                        ("\\subsection{%s}" . "\\subsection*{%s}")
                        ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                        ("\\paragraph{%s}" . "\\paragraph*{%s}")
                        ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
                      org-export-latex-classes))))

(add-hook 'org-export-latex-after-initial-vars-hook 'my-auto-tex-parameters)


(add-to-list 'org-export-latex-classes
          '("org-article"
             "\\documentclass{org-article}
             [NO-DEFAULT-PACKAGES]
             [PACKAGES]
             [EXTRA]"
             ("\\section{%s}" . "\\section*{%s}")
             ("\\subsection{%s}" . "\\subsection*{%s}")
             ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
             ("\\paragraph{%s}" . "\\paragraph*{%s}")
             ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

