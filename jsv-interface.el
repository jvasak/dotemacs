(setq inhibit-splash-screen t)

;; http://www.emacswiki.org/emacs/EnablingFontLock
(if (fboundp 'global-font-lock-mode)
    (global-font-lock-mode 1)        ; GNU Emacs
  (setq font-lock-auto-fontify t))   ; XEmacs

;; I HATE line wrapping, so try disabling by default
(set-default 'truncate-lines t)

(setq x-select-enable-clipboard t)
(setq transient-mode-mark t)

;; lower alpha when frame is deselected
;; (set-frame-parameter (selected-frame) 'alpha '(85 50))

;; some reasonable frame size/font defaults
(setq default-frame-alist
      '((width . 120)
	(height . 74) 
	;;(alpha 85 50) 
	(font . "Liberation Mono-7:regular")))

;; set up pleasing color theme
(require 'color-theme)
(color-theme-initialize)

(require 'color-theme-solarized)
;;(color-theme-solarized-dark)
(color-theme-calm-forest)
(global-set-key (kbd "C-<kp-0>") 'color-theme-solarized-dark)

(global-set-key (kbd "C-<kp-1>") 'color-theme-calm-forest)
(global-set-key (kbd "C-<kp-2>") 'color-theme-charcoal-black)
(global-set-key (kbd "C-<kp-3>") 'color-theme-blackboard)
(global-set-key (kbd "C-<kp-4>") 'color-theme-gtk-ide)
(global-set-key (kbd "C-<kp-5>") 'color-theme-kingsajz)
(global-set-key (kbd "C-<kp-6>") 'color-theme-gnome2)
(global-set-key (kbd "C-<kp-7>") 'color-theme-subtle-hacker)
(global-set-key (kbd "C-<kp-8>") 'color-theme-jsc-dark)
(global-set-key (kbd "C-<kp-9>") 'color-theme-kingsajz)

(if (file-exists-p "~/emacs/tomorrow-theme")
    (progn
      (add-to-list 'load-path "~/emacs/tomorrow-theme/GNU Emacs/")
      (require 'color-theme-tomorrow)
      (global-set-key (kbd "C-<kp-9>") 'color-theme-tomorrow-night-eighties))
  )

;;(if window-system
;;    (color-theme-greiner)
;;    (color-theme-taylor))

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
