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

(global-set-key (kbd "<f9>") 'linum-mode)
