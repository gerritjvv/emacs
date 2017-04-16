;;; https://sites.google.com/site/steveyegge2/effective-emacs
;;; M-x describe-bindings|describe-key|apropos|info
;;; 



;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(setq load-prefer-newer t)

(defun my-filter (condp lst)
    (delq nil
          (mapcar (lambda (x) (and (funcall condp x) x)) lst)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;; Package init

(setq load-path (cons "~/.emacs.d/ext" load-path))

(defvar cask-paths '("/usr/local/share/emacs/site-lisp/cask/cask.el"
                         "~/.cask/cask.el"))

(require 'cask (car (my-filter 'file-exists-p cask-paths)))

(cask-initialize)

(require 'pallet)
(pallet-mode t)

(load "my-ivy")
(load "my-window")
(load "my-git")
(load "my-clojure")

(global-auto-revert-mode 1)
(global-company-mode)

(company-quickhelp-mode 1)

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
 '(custom-safe-themes
   (quote
    ("f5512c02e0a6887e987a816918b7a684d558716262ac7ee2dd0437ab913eaec6" default)))
 '(package-selected-packages
   (quote
    (company-quickhelp smartparens cider undo-tree magit use-package ag dumb-jump counsel-gtags ggtags company-emacs-eclim eclim zenburn-theme epc which-key ivy-rtags find-file-in-project counsel-projectile projectile counsel ivy)))
 '(projectile-mode t nil (projectile))
 '(which-key-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
