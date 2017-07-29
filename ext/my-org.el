;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; Org mode settings

;;; Code:
;;; from https://blog.aaronbieber.com/2016/09/24/an-agenda-for-life-with-org-mode.html
;;; http://sachachua.com/blog/2015/02/learn-take-notes-efficiently-org-mode/

;;; Ord drill see http://orgmode.org/worg/org-contrib/org-drill.html

(require 'find-lisp)
(require 'dash)
(require 'org)
(require 'cl)
(require 'org-drill)

(setq org-agenda-files (find-lisp-find-files "~/Dropbox/Org/" "\\.org$"))

(setq org-return-follows-link t)



(global-set-key (kbd "C-c c") 'org-capture)

(setq org-directory "~/Dropbox/Org")

(setq org-refile-targets (-map (lambda (x) `(,x :maxlevel . 1)) (org-agenda-files)))

(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/Dropbox/Org/tasksheet.org" "Tasks")
	 "* TODO %?\n  %i\n  %a")
	("n" "ShellCodersNotes" entry
	 (file+headline (concat org-directory "/readingnotes/shellcoders.org") "Drill Notes")
	 "* %^{Note heading} :drill:\n %t\n %^{Longer notes (may be empty)} \n** Answer \n%^{The definition}")))

(provide 'my-org)
;;; my-org.el file ends here
