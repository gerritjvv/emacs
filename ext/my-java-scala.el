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

(setq javadoc-lookup-completing-read-function 'ivy-completing-read)

(use-package meghanada
  :bind (
	 :map java-mode-map
	      ("C-c C-j i" . meghanada-import-all)
	      ("C-c C-j o" . meghanada-optimize-import)
	      ("C-c C-j l" . meghanada-local-variable)
	      ("C-c C-j j" . javadoc-lookup)
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
	    (eldoc-mode t)
	    (c-toggle-auto-newline 't) ;;automatically insert new lines where
	    (add-hook 'before-save-hook 'delete-trailing-whitespace)

	    ))

(add-hook 'groovy-mode-hook
	  (lambda ()
	    (gradle-mode t)))



					;spaces_around_operators = true
					;spaces_around_brackets = none

					;curly_bracket_next_line = true
					;indent_brace_style = Allman
					;continuation_indent_size = 8

;; (add-hook 'editorconfig-custom-hooks
;; 	  '(lambda (props)
;;props is a hashmap
;; 	     (message (format " PROPS %s" props))))



(provide 'my-java-scala)

;;; my-java-scala.el ends here
