;;; https://sites.google.com/site/steveyegge2/effective-emacs
;;; M-x describe-bindings|describe-key|apropos|info
;;; REGEX
;;   M-x query-replace-regexp | list-matching-lines
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;; Package init

(setq load-path (cons "~/.emacs.d/ext" load-path))

(require 'package) ;; You might already have this line

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

(package-initialize)

(load "ivy.el")
(load "window.el")

;;(load "helm.el")

;;(helm-mode 1)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;; unscroll  Writing GNU Emacs extension CH1 & CH2

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;; Global Config

(global-set-key "\M-?" 'help-command)
(global-set-key "\C-h" 'delete-backward-char)

(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)

(global-set-key "\C-c\C-k" 'kill-region)


(put 'eval-expression 'disabled nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;; Searching and Modifying Buffers  Writing GNU Emac CH4

(defvar insert-time-format "%X"
  "*Format for \\[insert-time] (c.f. 'format-time-string').")

(defvar insert-date-format "%x"
  "*Format for \\[insert-date\ (c.f. 'format-time-string').")

(defun insert-time ()
  "Insert the current time according to insert-time-format"
  (interactive "*")
  (insert (format-time-string insert-time-format
			      (current-time))))

(defun insert-date ()
  "Insert the current date according to insert-date-format"
  (interactive "*")
  (insert (format-time-string insert-date-format
			      (current-time))))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (find-file-in-project helm-descbinds counsel-projectile projectile swiper-helm counsel ivy helm))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
