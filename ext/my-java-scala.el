;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; Java and Scala dev
;;;;;;;;;; http://ensime.org/editors/emacs/install/



;;; Code:

(require 'editorconfig)


;;c-indent-line is called after hitting RET
;; (defun my-print-syntax-context (&rest args)
  ;; (message (format "Syntax context %s" args)))

;; (advice-add 'c-indent-line :after #'my-print-syntax-context)
;; (advice-remove 'c-indent-line #'my-print-syntax-context)


(require 'meghanada)
  (add-hook 'java-mode-hook
	    (lambda ()
	       (editorconfig-mode t)      ;;reads the .editorconfig files and applies format, only to new files
               (meghanada-mode t)
               (c-toggle-auto-newline 't) ;;automatically insert new lines where required
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
