;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; windowing movement functions

(desktop-save-mode 1)

(require 'ace-window)

(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))

(defvar aw-dispatch-alist
'((?x aw-delete-window " Ace - Delete Window")
    (?m aw-swap-window " Ace - Swap Window")
    (?n aw-flip-window)
    (?c aw-split-window-fair " Ace - Split Fair Window")
    (?v aw-split-window-vert " Ace - Split Vert Window")
    (?b aw-split-window-horz " Ace - Split Horz Window")
    (?i delete-other-windows " Ace - Maximize Window")
    (?o delete-other-windows))
"List of actions for `aw-dispatch-default'.")


(global-set-key (kbd "C-x C-n") 'ace-window)

(defun other-window-backward (&optional n)
  "Select the previous window."
  (interactive "p")
  (other-window (- (or n 1))))

(global-set-key "\C-x\C-p" 'other-window-backward)

(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

(defalias 'scroll-ahead 'scroll-up)
(defalias 'scroll-behind 'scroll-down)

(defun scroll-n-lines-ahead (&optional n)
  "Scroll ahead N lines (1 by default)."
  (interactive "P")
  (scroll-ahead (prefix-numeric-value n)))

(defun scroll-n-lines-behind (&optional n)
  "Scroll behind N lines (1 by default)."
  (interactive "P")
  (scroll-behind (prefix-numeric-value n)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; Scrolling

(global-set-key "\C-x\C-q" 'quoted-insert)
(global-set-key "\C-q" 'scroll-n-lines-behind)
(global-set-key "\C-z" 'scroll-n-lines-ahead)

(defun read-only-if-symlink ()
  (if (file-symlink-p buffer-file-name)
      (progn
	(setq buffer-read-only t)
	(message "File is a symlink"))))


(add-hook 'find-file-hooks 'read-only-if-symlink)

(defadvice switch-to-buffer
    (before existing-buffer
	    activate compile)
    "When interactive switch to existing buffers only,
     unless given a prefix argument"
    (interactive
     (list (read-buffer "Switch to buffer: "
			(other-buffer)
			(null current-prefix-arg)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;; unscroll  Writing GNU Emacs extension


(defvar unscroll-point (make-marker)
  "Text position for next call to 'unscroll'.")

(defvar unscroll-window-start (make-marker)
  "Window start for next call to 'unscroll'.")

(defvar unscroll-hscroll nil
  "Hscroll for next call to 'unscroll'.")

(put 'scroll-up 'unscrollable t)
(put 'scroll-down 'unscrollable t)
(put 'scroll-right 'unscrollable t)
(put 'scroll-left 'unscrollable t)

(defun remember-for-unscroll (&rest r)
  (if (not (get last-command 'unscrollable))
      (progn
	(set-marker unscroll-point (point))
	(set-marker unscroll-window-start (window-start))
	(setq unscroll-hscroll (window-hscroll)))))

(advice-add 'scroll-up :before #'remember-for-unscroll)
(advice-add 'scroll-down :before #'remember-for-unscroll)
(advice-add 'scroll-left :before #'remrember-for-unscroll)
(advice-add 'scroll-right :before #'remrember-for-unscroll)

(defun unscroll ()
  "Jump to location specified by 'unscroll-to'"
  (interactive)
  (if unscroll-point
      (goto-char unscroll-point))

  (if unscroll-hscroll
      (set-window-hscroll nil unscroll-hscroll)))

(global-set-key (kbd "C-c <up>") 'enlarge-window)                    ;; make window taller
(global-set-key (kbd "C-c <down>") 'shrink-window)                   ;; make window shorter
(global-set-key (kbd "C-c <right>") 'enlarge-window-horizontally)    ;; make window wider
(global-set-key (kbd "C-c <left>") 'shrink-window-horizontally)      ;; make window less wide

(when (and (string-equal system-type "darwin") (member "Iosevka" (font-family-list)))
  (set-default-font "Iosevka"))


(require 'dumb-jump)

(use-package dumb-jump
  :bind (("M-g o" . dumb-jump-go-other-window)
         ("M-g j" . dumb-jump-go)
         ("M-g x" . dumb-jump-go-prefer-external)
         ("M-g z" . dumb-jump-go-prefer-external-other-window))
  :config (setq dumb-jump-selector 'ivy
		dumb-jump-aggressive nil
		dumb-jump-prefer-searcher 'rg) ;; (setq dumb-jump-selector 'helm)
  :ensure)


(use-package undo-tree
  :bind (:map undo-tree-map
              ("C-x u" . undo-tree-visualize)
	      ("C-/" . undo-tree-undo)
              ("s-Z" . undo-tree-redo)
              ("C-x v" . undo-tree-visualize))
  :config (global-undo-tree-mode t))
