;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Commentry:

;;;;;;;;;; Java and Scala dev
;;;;;;;;;; https://github.com/mopemope/meghanada-emacs



;;; Code:
(require 'flycheck)
;; (advice-add 'c-indent-line :after #'my-print-syntax-context)
;; (advice-remove 'c-indent-line #'my-print-syntax-context)

(require 'javadoc-lookup)
(require 'cc-mode)
(require 'eldoc)
(require 'ivy)
(require 'yasnippet)
(require 'java-snippets)
(require 'comment-dwim-2)

(global-set-key (kbd "M-;") 'comment-dwim-2)

(setq javadoc-lookup-completing-read-function 'ivy-completing-read)


(defun my--java-new-class ()
  "Create a new java class file."
  (interactive)
  (let* ((buff (call-interactively 'find-file)))
    (with-current-buffer buff
      (widen)
      (goto-char (point-min))

      ;;insert package
      (insert "pa")
      (call-interactively 'yas-expand)
      (goto-char (point-max))
      (newline 2)

      ;;inset class
      (beginning-of-line)
      (insert "cla")
      (call-interactively 'yas-expand)

      (call-interactively 'yas-exit-all-snippets)

      (goto-char (point-min))
      (search-forward "{")
      (newline 2))))

(use-package meghanada
  :bind (
	 :map java-mode-map
	      ("C-c C-j i" . meghanada-import-all)
	      ("C-c C-j o" . meghanada-optimize-import)
	      ("C-c C-j l" . meghanada-local-variable)
	      ("C-c C-j r" . meghanada-run-task)
	      ("C-c C-j j" . javadoc-lookup)
	      ("C-c C-j n c" . my--java-new-class)
	      ("C-c C-j c f" . meghanada-compile-file)
	      ("C-c C-j c p" . meghanada-project-compile)
	      ("C-c C-j t c" . meghanada-run-junit-class)
	      ("C-c C-j t t" . meghanada-run-junit-test-case)
	      ("M-." . meghanada-jump-declaration)
	      ("M-," . meghanada-back-jump)))

(add-hook 'java-mode-hook
	  (lambda ()
	    (editorconfig-mode t)      ;;reads the .editorconfig files and applies format, only to new files
	    (meghanada-mode t)
	    (line-number-mode t)
	    (yas-minor-mode-on)
	    (eldoc-mode t)
	    (flycheck-mode t)
	    (c-toggle-auto-newline t) ;;automatically insert new lines where
	    (hs-hide-level 2)
	    (linum-mode t)
	    (add-hook 'before-save-hook 'delete-trailing-whitespace)

	    ))

(add-hook 'c-mode-common-hook
  (lambda()
    (local-set-key (kbd "C-c <right>") 'hs-show-block)
    (local-set-key (kbd "C-c <left>")  'hs-hide-block)
    (local-set-key (kbd "C-c <up>")    'hs-hide-all)
    (local-set-key (kbd "C-c <down>")  'hs-show-all)
    (hs-minor-mode t)))

(add-hook 'groovy-mode-hook
	  (lambda ()
	    (gradle-mode t)))



(provide 'my-java-scala)

;;; my-java-scala.el ends here
