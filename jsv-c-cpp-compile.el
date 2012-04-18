;; https://github.com/genehack/smart-tab
;; (require 'smart-tab)
;; (global-smart-tab-mode 1)

(require 'vista-c-style)
(add-hook 'c-mode-common-hook 'vista-set-c-style)
(add-hook 'c-mode-common-hook 'vista-make-newline-indent)

(require 'rainbow-delimiters)
(add-hook 'c-mode-hook 'rainbow-delimiters-mode)
(add-hook 'c++-mode-hook 'rainbow-delimiters-mode)
(setq-default frame-background-mode 'dark)

;; c/c++ tempo stuff
(require 'tempo-c-cpp)

;;mode-compile
;; (autoload 'mode-compile "mode-compile"
;;   "Command to compile current buffer file based on the major mode" t)
;; (global-set-key "\C-cc" 'mode-compile)
;; (autoload 'mode-compile-kill "mode-compile"
;;   "Command to kill a compilation launched by `mode-compile'" t)
;; (global-set-key "\C-ck" 'mode-compile-kill)

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


(global-set-key (kbd "C-<f9>") 'auto-fill-mode)
(global-set-key (kbd "S-<f9>") 'toggle-truncate-lines)
(global-set-key (kbd "C-0") 'c++-mode)

(global-set-key "\C-xp" 'compile)
;; (global-set-key "\C-xr" 'recompile)
(global-set-key "\C-xg" 'goto-line)
