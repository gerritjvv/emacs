;;; https://sites.google.com/site/steveyegge2/effective-emacs
;;; M-x describe-bindings|describe-key|apropos|info
;;; REGEX
;;   M-x query-replace-regexp | list-matching-lines
;;

(defun my-filter (condp lst)
    (delq nil
          (mapcar (lambda (x) (and (funcall condp x) x)) lst)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;; Package init

(setq load-path (cons "~/.emacs.d/ext" load-path))

;;(require 'package) ;; You might already have this line

;;(add-to-list 'package-archives
 ;;            '("melpa" . "https://melpa.org/packages/"))

;;(package-initialize)

(defvar cask-paths '("/usr/local/share/emacs/site-lisp/cask/cask.el"
                         "~/.cask/cask.el"))

(require 'cask (car (my-filter 'file-exists-p cask-paths)))

(cask-initialize)

(require 'pallet)
(pallet-mode t)

(load "ivy.el")
(load "window.el")

(require 'dash)

;;(load "helm.el")

;;;;;;;;;; copy shell paths
;;; https://github.com/purcell/exec-path-from-shell

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))


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
    (ivy-rtags find-file-in-project counsel-projectile projectile counsel ivy))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
