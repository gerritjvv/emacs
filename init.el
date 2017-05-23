;;; https://sites.google.com/site/steveyegge2/effective-emacs
;;; M-x describe-bindings|describe-key|apropos|info
;;;



;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.

;;; Code:

(require 'package)

(package-initialize)

(setq load-prefer-newer t)

(defun my-filter (condp lst)
    (delq nil
          (mapcar (lambda (x) (and (funcall condp x) x)) lst)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;; Package init

(setq load-path (append '("~/.emacs.d/ext" "~/.emacs.d/ext/treemacs") load-path))

(defvar cask-paths '("/usr/local/share/emacs/site-lisp/cask/cask.el"
                         "~/.cask/cask.el"))

(require 'cask (car (my-filter 'file-exists-p cask-paths)))

(cask-initialize)

(require 'pallet)
(pallet-mode t)

(require 'company)

(add-hook 'after-init-hook 'global-company-mode)

(require 'editorconfig)
(editorconfig-mode 1)

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(require 'saveplace)
(setq-default save-place t)

(show-paren-mode 1)

;;https://github.com/technomancy/better-defaults/blob/master/better-defaults.el
(setq save-interprogram-paste-before-kill t
      auto-save-visited-file-name t ;; don't create separate autosave files
      make-backup-files nil
      create-lockfiles  nil
      apropos-do-all t
      mouse-yank-at-point t
      require-final-newline t
      load-prefer-newer t
      ediff-window-setup-function 'ediff-setup-windows-plain
      save-place-file (concat user-emacs-directory "places")

      ;;backup files are turned off, keeping this if its turned on again
      backup-directory-alist `(("." . ,(concat user-emacs-directory
					       "backups"))))

(defun full-auto-save ()
  "Save all buffers on `auto-save-hook`.
See https://www.emacswiki.org/emacs/AutoSave"
  (interactive)
  (save-excursion
    (dolist (buf (buffer-list))
      (set-buffer buf)
      (if (and (buffer-file-name) (buffer-modified-p))
          (basic-save-buffer)))))
(add-hook 'auto-save-hook 'full-auto-save)

(load "treemacs")

(load "my-ivy")
(load "my-window")
(load "my-git")
(load "my-org")
(load "my-java-scala")

(load "my-cheatsheet")
(load "my-style")
(load "my-sql")
(load "my-elfeed")

(global-auto-revert-mode 1)
(global-company-mode)

(company-quickhelp-mode 1)

(require 'flycheck)

(global-flycheck-mode)

(require 'eldoc)
(global-eldoc-mode t)


(when (eq system-type 'darwin)
  (require 'ls-lisp)
  (setq ls-lisp-use-insert-directory-program nil))

(setq exec-path-from-shell-check-startup-files nil)

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
(global-set-key "\C-c." 'find-function-at-point)
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
  "Insert the current time according to insert-time-format."
  (interactive "*")
  (insert (format-time-string insert-time-format
			      (current-time))))

(defun insert-date ()
  "Insert the current date according to insert-date-format."
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
 '(elfeed-feeds nil)
 '(package-selected-packages
   (quote
    (comment-dwim-2 persp-mode-projectile-bridge persp-projectile perspective flycheck-demjsonlint json-mode groovy-imports grails-mode neotree elfeed-goodies elfeed-org elfeed sqlup-mode pdf-tools ivy-hydra ivy-rich java-snippets github-browse-file github-search javadoc-lookup cssh editorconfig clojure-cheatsheet cheatsheet better-shell shell-pop god-mode org-projectile noctilux-theme org-pomodoro dashboard projectile-speedbar flycheck-cask flycheck-clojure meghanada memory-usage all-the-icons-dired all-the-icons groovy-mode company-quickhelp smartparens cider undo-tree magit use-package ag dumb-jump counsel-gtags ggtags zenburn-theme epc which-key ivy-rtags find-file-in-project counsel-projectile projectile counsel ivy)))
 '(projectile-mode t nil (projectile))
 '(which-key-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
