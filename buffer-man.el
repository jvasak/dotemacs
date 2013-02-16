;; iswitchd niceness
(iswitchb-mode 1)
(defun iswitchb-local-keys ()
  (mapc (lambda (K) 
          (let* ((key (car K)) (fun (cdr K)))
            (define-key iswitchb-mode-map (edmacro-parse-keys key) fun)))
        '(("<right>" . iswitchb-next-match)
          ("<left>"  . iswitchb-prev-match)
          ("<up>"    . ignore             )
          ("<down>"  . ignore             ))))
(add-hook 'iswitchb-define-mode-map-hook 'iswitchb-local-keys)

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
