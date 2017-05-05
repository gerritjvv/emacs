;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Commentry:

;;;;;;;;;; Java and Scala dev
;;;;;;;;;; https://github.com/mopemope/meghanada-emacs



;;; Code:


(require 'flycheck)
;; (advice-add 'c-indent-line :after #'my-print-syntax-context)
;; (advice-remove 'c-indent-line #'my-print-syntax-context)


(require 'meghanada)
  (add-hook 'java-mode-hook
	    (lambda ()
	       (editorconfig-mode t)      ;;reads the .editorconfig files and applies format, only to new files
               (meghanada-mode t)

               (c-toggle-auto-newline 't) ;;automatically insert new lines where
               (add-hook 'before-save-hook 'delete-trailing-whitespace)))

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
