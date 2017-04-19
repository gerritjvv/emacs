;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; Java and Scala dev
;;;;;;;;;; http://ensime.org/editors/emacs/install/



;;; Code:

(require 'meghanada)
(add-hook 'java-mode-hook
          (lambda ()
            ;; meghanada-mode on
            (meghanada-mode t)
            (add-hook 'before-save-hook 'meghanada-code-beautify-before-save)))

(provide 'my-java-scala)

;;; my-java-scala.el ends here


