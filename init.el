;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Library Paths
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'load-path "~/.emacs.d")

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; Interface stuff
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load-library "interface")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Init elpa, el-get
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))
(load-library "el-helper")

(setq 
 el-get-sources
 '((:name twilight-anti-bright-theme
          :after (progn
                   (load-theme 'twilight-anti-bright t)))

   (:name deft
          :after (progn
                   (if (file-exists-p "~/Dropbox/")
                       (setq deft-directory "~/Dropbox/notes")
                     (setq deft-directory "~/notes"))
                   (setq deft-extension "org")
                   (setq deft-text-mode 'org-mode)
                   (setq deft-use-filename-as-title t)
                   (global-set-key (kbd "<f1>") 'deft)))
   
   (:name magit				; git meet emacs, and a binding
          :after (progn
                   (global-set-key (kbd "<f8>") 'magit-status)))

   (:name rainbow-delimiters
          :after (progn
                   (add-hook 'c-mode-hook 'rainbow-delimiters-mode)
                   (add-hook 'c++-mode-hook 'rainbow-delimiters-mode)
                   (setq-default frame-background-mode 'dark)))
   
   (:name javascript-mode
          :type http
          :url "http://www.karllandstrom.se/downloads/emacs/javascript.el"
          :load "javascript.el"
          :features javascript-mode)

   (:name json-mode
          :type git
          :url "https://github.com/joshwnj/json-mode"
          :load "json-mode.el"
          :compile ("json-mode.el")
          :features json-mode)

   (:name twittering-mode
          :after (progn
                   (setq twittering-use-master-password t)))

   (:name asciidoc
          :type git
          :url "https://github.com/metaperl/asciidoc-el"
          :load "asciidoc.el"
          :compile ("asciidoc.el")
          :features asciidoc)

   (:name doc-mode
          :after (progn
                   (autoload 'doc-mode "doc-mode" nil t)
                   (add-to-list 'auto-mode-alist '("\\.adoc$" . doc-mode))
                   (add-hook 'doc-mode-hook
                             '(lambda ()
                                (turn-on-auto-fill)
                                (require 'asciidoc)))))

   (:name markdown-mode
          :after (progn
                   (add-to-list 'auto-mode-alist '("\\.markdown" . markdown-mode))))

   (:name org-mode
          :after (progn
                   (add-to-list 'load-path "~/.emacs.d/el-get/org-mode/contrib/oldexp/")
                   (load-library "jsv-org")))

   (:name ack
          :after (progn
                   (setq ack-guess-type t)
                   (add-to-list 'ack-mode-type-map '((python-mode) . "python"))))

   (:name mmm-mode
          :after (progn
                   (require 'mmm-auto)
                   (setq mmm-global-mode 'maybe)))

   (:name mmm-mako
          :type git
          :url "https://github.com/emacsmirror/mmm-mako.git"
          :load "mmm-mako.el"
          :compile ("mmm-mako.el")
          :after (progn
                   (add-to-list 'auto-mode-alist '("\\.mak\\'" . html-mode))
                   (mmm-add-mode-ext-class 'html-mode "\\.mak\\'" 'mako)
                   (global-set-key "\M-p"  'mmm-parse-buffer)))

   (:name haskell-mode
          :after (progn
                   (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
                   (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
                   (add-to-list 'completion-ignored-extensions ".hi")))
   ))

; list all packages you want installed
(setq my-el-get-packages
      (append
       '(el-get
         minimap
         tempo-c-cpp
         auctex
         )
       (mapcar 'el-get-source-name el-get-sources)))

(el-get 'sync my-el-get-packages)


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; Printing
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load-library "printers")

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; C/C++ programming
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load-library "c-cpp-compile")

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; Buffer management
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load-library "buffer-man")

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; GPG setup
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(epa-file-enable)
(setenv (concat "GPG_AGENT_INFO" nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; One-time stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
 '(custom-safe-themes (quote ("159bb8f86836ea30261ece64ac695dc490e871d57107016c09f286146f0dae64" default)))
 '(delete-selection-mode t)
 '(global-font-lock-mode t nil (font-lock))
 '(indent-tabs-mode nil)
 '(line-number-mode t)
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
