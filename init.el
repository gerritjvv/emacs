;;; https://sites.google.com/site/steveyegge2/effective-emacs
;;; M-x describe-bindings|describe-key|apropos|info
;;; 



;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.

(require 'package)

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

(require 'company)

(add-hook 'after-init-hook 'global-company-mode)

(require 'flycheck)
(global-flycheck-mode)


(load "my-ivy")
(load "my-window")
(load "my-git")
(load "my-org")

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

(require 'god-mode)

(global-set-key (kbd "<f12>") 'god-mode)
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
    ("4980e5ddaae985e4bae004280bd343721271ebb28f22b3e3b2427443e748cd3f" "f5512c02e0a6887e987a816918b7a684d558716262ac7ee2dd0437ab913eaec6" default)))
 '(package-selected-packages
   (quote
    (better-shell shell-pop god-mode org-projectile noctilux-theme org-pomodoro dashboard projectile-speedbar flycheck-cask flycheck-clojure meghanada memory-usage all-the-icons-dired all-the-icons neotree groovy-mode company-quickhelp smartparens cider undo-tree magit use-package ag dumb-jump counsel-gtags ggtags zenburn-theme epc which-key ivy-rtags find-file-in-project counsel-projectile projectile counsel ivy)))
 '(projectile-mode t nil (projectile))
 '(which-key-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
