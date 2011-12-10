;; Set up GIT bindings
(require 'magit)
(require 'magit-svn)
(global-set-key (kbd "<f8>") 'magit-status)

;; Set up SVN bindings
(require 'psvn)
(global-set-key (kbd "C-<f8>") 'svn-status)

;; quick revert buffer
(defun my-revert-buffer ()
  "Reverts buffer nicely for VC changes"
  (interactive)
  (revert-buffer t t t)
  )
(global-set-key (kbd "<f4>") 'my-revert-buffer)
