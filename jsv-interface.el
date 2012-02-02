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
(set-frame-parameter (selected-frame) 'alpha '(85 50))

;; some reasonable frame size/font defaults
(setq default-frame-alist
      '((width . 120)
	(height . 74) 
	(alpha 85 50) 
	(font . "Liberation Mono-10:regular")))

;; set up pleasing color theme
(require 'color-theme)
(color-theme-initialize)

(color-theme-jsc-dark)

(global-set-key (kbd "C-<kp-1>") 'color-theme-calm-forest)
(global-set-key (kbd "C-<kp-2>") 'color-theme-blackboard)
(global-set-key (kbd "C-<kp-3>") 'color-theme-jsc-dark)
(global-set-key (kbd "C-<kp-4>") 'color-theme-subtle-hacker)
(global-set-key (kbd "C-<kp-5>") 'color-theme-robin-hood)
(global-set-key (kbd "C-<kp-6>") 'color-theme-greiner)
(global-set-key (kbd "C-<kp-7>") 'color-theme-taylor)
(global-set-key (kbd "C-<kp-8>") 'color-theme-taming-mr-arneson)

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
