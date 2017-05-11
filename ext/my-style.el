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
(require 'cc-mode)
(require 'editorconfig)
(require 'projectile)

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

;;see https://github.com/emacs-mirror/emacs/blob/master/lisp/progmodes/cc-vars.el#L656
(defvar my--hanging-braces-alist-nextline
  '((inexpr-class-open (before after))
    (inexpr-class-close before)
    (class-open  (before after))
    (inclass     (before after))
    (defun-block-intro (before after)))
  "Used if curly_bracket_nextline is t.
Sets the class open, innner class method and if { on new line")


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

    (c-hanging-braces-alist     (if (gethash 'curly_bracket_next_line cfg false)
				    (append
				     my--hanging-braces-alist-nextline
				     c-hanging-braces-alist)
				  c-hanging-braces-alist))

    (c-basic-offset             (gethash 'indent_size cfg 4))
    (c-tab-always-indent        . t)

    (c-cleanup-list             . (scope-operator))

    (c-require-final-newline d. ,(gethash 'insert_final_newline cfg 't))

    (c-echo-syntactic-information-p . t)))


(defun my-projectile-name-or-default (default)
  "Get the current projectile name or return the DEFAULT value."
  (or (projectile-project-name)
      default))

(defvar my-editorconfig nil
  "Store the editorconfig hash from the 'editorconfig-custom-hooks.")

(defvar my-mode-style nil
  "Store the current style selected from eidtorconfig-custom-hooks.
Used in my-set-style to set the current 'c-set-style'.")

(defun get-my-editorconfig ()
  "Return 'myeditorconfig or load 'editorconfig-get-properties.
This function sets my-editorconfig."
  (if (null my-editorconfig)
      (progn
	(let (cfg (editorconfig-get-properties))
	  (setq my-editorconfig cfg)
	  cfg))
    my-editorconfig))

(defun my-set-c-style ()
  "Read the editorconfig for the project and set c-style accordingly.
The style is changed only if 'c-indentation-style is not already equal,
to the project name.  The style is only created once and 'c-style-alist,
is used to check."
  (let ((pname (my-projectile-name-or-default "user")))

    ;only change cstyle if the current indentation style != pname
    (if (not (string= pname c-indentation-style))
	(progn
           ;set pname as c-style-alist item if not exist, otherwise reuse existing
	  (if (not (assoc pname c-style-alist))
	      (let ((cfg (get-my-editorconfig)))
		(c-add-style pname
			     (my-create-style-from-config cfg))))

	  (c-toggle-auto-newline 1)
	  (c-set-style pname)))))

(add-hook 'c-mode-common-hook 'my-set-c-style)

;; on editor config load save to 'my-editorconfig
(add-hook 'editorconfig-custom-hooks
	  (lambda (cfg)
	    (setq my-editorconfig cfg)))



;;(c-add-style "PERSONAL" my-java-style)


(provide 'my-style)
;;; my-style.el ends here
