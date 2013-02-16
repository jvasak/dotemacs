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
;; 	(height . 74) 
;; 	;;(alpha 85 50) 
;; 	(font . "Liberation Mono-7:regular")))

(set-face-attribute 'default nil :font "Liberation Mono" :height 75)

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

(global-set-key "\C-xg" 'goto-line)
