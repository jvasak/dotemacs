;; For testing
;; (setq user-emacs-directory "~/emacs")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interface setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq inhibit-splash-screen t)

;; http://www.emacswiki.org/emacs/EnablingFontLock
(if (fboundp 'global-font-lock-mode)
    (global-font-lock-mode 1)        ; GNU Emacs
  (setq font-lock-auto-fontify t))   ; XEmacs

;; I HATE line wrapping, so try disabling by default
(set-default 'truncate-lines t)

(setq x-select-enable-clipboard t)
(setq transient-mode-mark t)

;; some reasonable frame size/font defaults
;; (setq default-frame-alist
;;       '((width . 120)
;;	(height . 74)
;;	;;(alpha 85 50)
;;	(font . "Liberation Mono-7:regular")))

;; (set-face-attribute 'default nil :font "Liberation Mono" :height 75)

;; http://stackoverflow.com/a/3669681/5840
(setq frame-title-format
      (list (format "%s %%S: %%j " (system-name))
	    '(buffer-file-name "%f" (dired-directory dired-directory "%b"))))

(defun v-resize (key)
  "interactively resize the window"
  (interactive "cHit +/- to enlarge/shrink")
  (cond
   ((eq key (string-to-char "+"))
    (enlarge-window 1)
    (call-interactively 'v-resize))
   ((eq key (string-to-char "-"))
    (enlarge-window -1)
    (call-interactively 'v-resize))
   (t (push key unread-command-events))))
(global-set-key "\C-c+" 'v-resize)

;; Set up line number options
(require 'linum)
(setq linum-format "%4d ")

(global-linum-mode 1)
(setq linum-mode-inhibit-modes-list '(eshell-mode
				      shell-mode
				      magit-mode
				      dired-mode
				      twittering-mode
				      erc-mode
				      jabber-roster-mode
				      jabber-chat-mode
				      weechat-mode
				      gnus-group-mode
				      gnus-summary-mode
				      gnus-article-mode))

(defadvice linum-on (around linum-on-inhibit-for-modes)
  "Stop the load of linum-mode for some major modes."
  (unless (member major-mode linum-mode-inhibit-modes-list)
    ad-do-it))

(ad-activate 'linum-on)

(global-set-key (kbd "<f9>")   'linum-mode)
(global-set-key (kbd "C-<f9>") 'auto-fill-mode)
(global-set-key (kbd "S-<f9>") 'toggle-truncate-lines)
(global-set-key (kbd "M-<f9>") 'whitespace-mode)
(global-set-key (kbd "S-C-<f9>") 'global-hl-line-mode)

(global-set-key "\C-xg" 'goto-line)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package Setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq el-get-dir
      (concat (file-name-as-directory user-emacs-directory) "el-get/"))
(setq el-get-user-package-directory
      (concat (file-name-as-directory user-emacs-directory) "el-get-init-files/"))
(setq el-get-github-default-url-type 'https)

;; (setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
;;                          ("marmalade" . "http://marmalade-repo.org/packages/")
;;                          ("melpa" . "http://melpa.milkbox.net/packages/")))

(add-to-list 'load-path (concat el-get-dir "el-get"))

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (let (el-get-master-branch)
      (goto-char (point-max))
      (eval-print-last-sexp))))

;;(add-to-list 'el-get-recipe-path "~/emacs/el-get-user/recipes")

(setq
 el-get-sources
 '(el-get
   emacs-w3m
   weechat
   asciidoc
   haskell-mode
   powerline

   (:name markdown-mode-css
	  :type git
	  :url "https://github.com/michaelamie/markdown-mode-css")

   (:name markdown-mode
	  :after (progn
		   ;; (defun jsv/setup-markdown-mode ()
		   ;;   (visual-line-mode t))
		   ;; (add-hook ‘markdown-mode-hook ’jsv/setup-markdown-mode)
		   (defvar markdown-css-dir (expand-file-name "el-get/markdown-mode-css/css" user-emacs-directory))
		   (setq markdown-css-path (expand-file-name "github.css" markdown-css-dir))
		   (setq markdown-command "pandoc --smart -f markdown -t html")))

   (:name doc-mode
	  :after (progn
		   (add-to-list 'auto-mode-alist '("\\.adoc$" . doc-mode))))

   (:name pandoc-mode
	  :after (progn
		   (add-hook 'markdown-mode-hook 'turn-on-pandoc)))
   (:name auctex
	  :after (progn
		   (defun tweak-latex-formatting ()
		     (setq LaTeX-item-indent 0)
		     (add-to-list 'LaTeX-verbatim-environments "lstlisting")
		     (TeX-normal-mode))
		   (add-hook 'LaTeX-mode-hook 'tweak-latex-formatting)))

   ;; (:name cyberpunk-theme
   ;;	  :type git
   ;;	  :url "https://github.com/n3mo/cyberpunk-theme.el.git"
   ;;	  :load "cyberpunk-theme.el"
   ;;	  :after (progn
   ;;		   (load-theme 'cyberpunk t)))

   ;; (:name tangotango-theme
   ;;	  :type git
   ;;	  :url "https://github.com/juba/color-theme-tangotango.git"
   ;;	  :load "tangotango-theme.el"
   ;;	  :after (progn
   ;;		   (load-theme 'tangotango t)))

   (:name moe-theme
	  :type git
	  :url "https://github.com/kuanyui/moe-theme.el"
	  :load "moe-theme.el"
	  :after (progn
		   (defun my-moe-setup ()
		     (setq moe-theme-mode-line-color 'orange)
		     (powerline-moe-theme)
		     (load-theme 'moe-dark t)
		     (if window-system
			 (progn
			   (set-background-color "#000011")
			   (set-foreground-color "#c6c6c6")
			   )
		       (progn
			 (set-background-color "color-16")
			 (set-foreground-color "white")
			 ))
		     )
		   (if (daemonp)
		       (add-hook 'after-make-frame-functions
				 (lambda (frame)
				   (with-selected-frame frame
				     (my-moe-setup))))
		     (my-moe-setup))	   
		   ))

   (:name magit
	  :after (progn
		   (setq magit-repo-dirs '("~/src/vrisys"
					   "~/src/common-cpp"
					   "~/src/vri-core"
					   "~/src/uis"))
		   (setq magit-repo-dirs-depth 1)

		   ;; full screen magit-status
		   (defadvice magit-status (around magit-fullscreen activate)
		     (window-configuration-to-register :magit-fullscreen)
		     ad-do-it
		     (delete-other-windows))

		   ;; restore previously hidden windows
		   (defadvice magit-quit-window (around magit-restore-screen activate)
		     ad-do-it
		     (jump-to-register :magit-fullscreen))

		   (global-set-key (kbd "<f8>") 'magit-status)))

   (:name deft
	  :after (progn
		   (if (file-exists-p "~/Dropbox/")
		       (setq deft-directory "~/Dropbox/notes")
		     (setq deft-directory "~/notes"))
		   (setq deft-extension "txt")
		   (setq deft-text-mode 'org-mode)
		   (setq deft-use-filename-as-title t)
		   (global-set-key (kbd "<f1>") 'deft)))

   (:name org-mode
	  :after (progn
		   (load-file (concat (file-name-as-directory user-emacs-directory) "jsv-org.el"))))

   (:name twittering-mode
	  :after (progn
		   (setq twittering-use-master-password t)))

   ;; (:name rainbow-delimiters
   ;;	  :features "rainbow-delimiters"
   ;;	  :after (progn
   ;;		   (global-rainbow-delimiters-mode)))

   (:name jedi
	  :after (progn
		   (setq jedi:setup-keys t)
		   (add-hook 'python-mode-hook 'jedi:setup)))

   (:name projectile
	  :after (progn
		   (projectile-global-mode)))

   ;; (:name smex
   ;;	  :after (progn
   ;;		   (global-set-key (kbd "M-x") 'smex)))

   (:name helm
	  :after (progn
		   (helm-mode t)
		   (global-set-key "\C-xb" 'helm-mini)))

   helm-ls-git

   (:name go-mode
	  :type git
	  :url "https://github.com/dominikh/go-mode.el"
	  :load "go-mode.el"
	  :after (progn
		   (setq gofmt-command "~/go/bin/gofmt")
		   (add-hook 'before-save-hook 'gofmt-before-save)
		   ))

   (:name fic-mode
	  :after (progn
		   (require 'fic-mode)
		   (add-hook 'prog-mode-hook 'fic-mode)))
   ))

(setq my-packages
      (mapcar 'el-get-source-name el-get-sources))

(el-get-cleanup my-packages)
(package-initialize)
(el-get 'sync my-packages)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Printing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq ps-printer-name "copier")
(setq ps-paper-type 'letter)
(setq ps-landscape-mode t)
(setq ps-number-of-columns 2)
(setq ps-line-number t)
(setq ps-line-number-start 10)
(setq ps-line-number-step 10)

(global-set-key (kbd "<print>")   'ps-spool-buffer-with-faces)
(global-set-key (kbd "S-<print>") 'ps-spool-region-with-faces)
(global-set-key (kbd "C-<print>") 'ps-despool)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C/C++ programming
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load-file (concat (file-name-as-directory user-emacs-directory) "vista-c-style.el"))
(require 'vista-c-style)
(add-hook 'c-mode-common-hook 'vista-set-c-style)
(add-hook 'c-mode-common-hook 'vista-make-newline-indent)

(setq compilation-window-height 30)

;; from enberg on #emacs
;; if the compilation has a zero exit code,
;; the windows disappears after ten seconds
;; otherwise it stays
(setq compilation-finish-function
      (lambda (buf str)
	(unless (string-match "exited abnormally" str)
	  ;;no errors, make the compilation window go away in a few seconds
	  (run-at-time
	   "1 sec" nil 'delete-windows-on
	   (get-buffer-create "*compilation*"))
	  (message "No Compilation Errors!"))))

;; http://stackoverflow.com/questions/3311577/when-using-two-frames-in-emacs-how-do-i-prevent-the-compilation-buffer-from-sho
(setq-default display-buffer-reuse-frames t)

(global-set-key (kbd "C-0") 'c++-mode)

(global-set-key "\C-xp" 'compile)

;; gtags/global support
;; (setq load-path (cons "/home/jvasak/src/global-6.2.8" load-path))
;; (autoload 'gtags-mode "gtags" "" t)
;; (setq gtags-suggested-key-mapping t)

;; (add-hook 'c-mode-common-hook '(lambda ()
;;                                  (gtags-mode 1)))

;; (setq gtags-mode-hook
;;       '(lambda ()
;;          (setq gtags-suggested-key-mapping t)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Buffer management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; iswitchb niceness
;; (iswitchb-mode 1)
;; (setq iswitchb-default-method 'maybe-frame)
;; (defun iswitchb-local-keys ()
;;   (mapc (lambda (K)
;; 	  (let* ((key (car K)) (fun (cdr K)))
;; 	    (define-key iswitchb-mode-map (edmacro-parse-keys key) fun)))
;; 	'(("<right>" . iswitchb-next-match)
;; 	  ("<left>"  . iswitchb-prev-match)
;; 	  ("<up>"    . ignore             )
;; 	  ("<down>"  . ignore             ))))
;; (add-hook 'iswitchb-define-mode-map-hook 'iswitchb-local-keys)

;; Clean up the current emacs session, when needed
;; look at (require 'midnight) at some point, too
(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer
	(delq (current-buffer)
	      (remove-if-not 'buffer-file-name (buffer-list)))))

;; quick revert buffer
(defun my-revert-buffer ()
  "Reverts buffer nicely for VC changes"
  (interactive)
  (revert-buffer t t t)
  )
(global-set-key (kbd "<f4>") 'my-revert-buffer)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GPG setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(epa-file-enable)
(setenv (concat "GPG_AGENT_INFO" nil))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; One-time stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'auto-mode-alist '("\\.zsh$" . shell-script-mode))
;;(electric-indent-mode +1)
(electric-pair-mode +1)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-default nil)
 '(blink-cursor-mode nil)
 '(c-echo-syntactic-information-p t)
 '(column-number-mode t)
 '(compilation-scroll-output (quote first-error))
 '(delete-selection-mode t)
 '(global-font-lock-mode t nil (font-lock))
 '(make-backup-files nil)
 '(menu-bar-mode nil)
 '(mouse-wheel-scroll-amount (quote (1 ((shift) . 1) ((control)))))
 '(region ((((class color) (background dark)) (:background "#BFAB91"))))
 '(tool-bar-mode nil))

;; Default to google-chrome browser
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "google-chrome")
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
