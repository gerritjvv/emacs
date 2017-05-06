;;; package ---- my-style.el

;;; Commentry:

;;; Code:

;;; bds == allman style

;; (setq my-java-style
;;       '(bsd
;; 	(c-tab-always-indent        . t)
;;     (c-comment-only-line-offset . 4)
;;     (c-hanging-braces-alist     . ((substatement-open (before after))
;;                                    (brace-list-open (before after))
;; 				   (ineline-open (before after))))
;;     (c-hanging-colons-alist     . ((member-init-intro before)
;;                                    (inher-intro)
;;                                    (case-label after)
;;                                    (label after)
;;                                    (access-label after)))
;;     (c-cleanup-list             . (scope-operator
;;                                    empty-defun-braces
;;                                    defun-close-semi))
;;     (c-offsets-alist            . ((arglist-close . c-lineup-arglist)
;;                                    (substatement-open . 0)
;; 				   (inline-open     . 0)
;;                                    (case-label        . 4)
;;                                    (block-open        . 0)
;;                                    (knr-argdecl-intro . -)))
;;     (c-require-final-newline    . f)
;;     (c-echo-syntactic-information-p . t)))

(require 'editorconfig)

(editorconfig-mode 1)

(defun tuple (s)
  (list s s))

(setq brace-style-alist
  (list
    (tuple    'bsd)
    (tuple    'gnu)
    (tuple    'k&r)
    (tuple    'whitesmith)
    (tuple    'stroustrup)
    (tuple    'ellemtel)
    (tuple    'linux)
    (tuple    'python)
    (tuple    'java)))

(defun parse-brace-style (style)
  "Return the correct symbol from the STYLE string or if not supported use BSD."
  (car
   (alist-get
    (if (string-or-null-p style) (intern (downcase style)) style)
    brace-style-alist
    '(bsd))))


(defun my-create-style-from-config (cfg)
  "Create a style list from CFG which should be the editorconfig hash."
  `(,(format "%s" (parse-brace-style (gethash 'indent_brace_style cfg "bsd")))


    (c-tab-always-indent        . t)

    (c-cleanup-list             . (scope-operator
				   empty-defun-braces
				   defun-close-semi))

    (c-require-final-newline d. ,(gethash 'insert_final_newline cfg 't))

    (c-echo-syntactic-information-p . t)))


(defun my-projectile-name-or-default (default)
  "Get the current projectile name or return the DEFAULT value."
  (or (projectile-project-name)
      default))

(defvar my-mode-style nil
  "Store the current style selected from eidtorconfig-custom-hooks.
Used in my-set-style to set the current 'c-set-style'.")

(defun my-set-style ()
  "Set 'c-set-style' to 'my-mode-style' if not void."
  (if (not (null my-mode-style))
      (progn
	(c-toggle-auto-newline 1)
	(c-set-style my-mode-style))))

(add-hook 'c-mode-common-hook 'my-set-style)


;;; Issue with when to load all the set style code,
;;; we only get the editor config hook but c-mode is not yet enabled
;;;  running this in the c-mode-common-hook gives usues

;; on editor config load, create a c-style and add
(add-hook 'editorconfig-custom-hooks
	  (lambda (cfg)
	    (warn (format "In editor-cusomt-hook"))
	    (let ((pname (my-projectile-name-or-default "user")))
	      (warn (format "Compare pname[%s] == my-mode-style[%s]" pname my-mode-style))
	      (if (not (string= pname my-mode-style))
		  (progn
		    (warn "Changing ctyle")
		    (c-add-style pname
				 (my-create-style-from-config cfg))

		    (setq my-mode-style pname)

		    (warn (format "Set my-mode-style to %s" my-mode-style))

		    (if (derived-mode-p 'c-mode 'java-mode 'c++-mode)
			(my-set-style)))))))



;;(c-add-style "PERSONAL" my-java-style)


(provide 'my-style)
;;; my-style.el ends here
