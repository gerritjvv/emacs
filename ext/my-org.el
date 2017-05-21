;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; Org mode settings

;;; Code:
;;; from https://blog.aaronbieber.com/2016/09/24/an-agenda-for-life-with-org-mode.html
;;; http://sachachua.com/blog/2015/02/learn-take-notes-efficiently-org-mode/

(require 'find-lisp)
(require 'dash)
(require 'org)


(setq org-agenda-files (find-lisp-find-files "~/Dropbox/Org/" "\\.org$"))

(setq org-return-follows-link t)



(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c t") (lambda (&rest args)
				(interactive "p")
				(if (util-line-contains-opt-p "[]" "[ ]" "[X]" "[x]" "[-]")
				    (org-toggle-checkbox)
				  (org-todo))))

(setq org-refile-targets (-map (lambda (x) `(,x :maxlevel . 1)) (org-agenda-files)))

(setq org-default-notes-file "~/Dropbox/Org/todo.org")

(setq org-agenda-custom-commands
      '(("d" "Daily agenda and all TODOs"
         ((tags "PRIORITY=\"A\""
                ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                 (org-agenda-overriding-header "High-priority unfinished tasks:")))
          (agenda "" ((org-agenda-ndays 14)))
          (alltodo ""
                   ((org-agenda-skip-function '(or (air-org-skip-subtree-if-habit)
                                                   (air-org-skip-subtree-if-priority ?A)
                                                   (org-agenda-skip-if nil '(scheduled deadline))))
                    (org-agenda-overriding-header "ALL normal priority tasks:"))))
         ((org-agenda-compact-blocks t)))))


(defun org-agenda-show-unscheduled (&optional arg)
  (interactive "P")
  (org-agenda arg "d"))

(global-set-key (kbd "<f6>") 'org-agenda-show-unscheduled)

(defun air-org-skip-subtree-if-habit ()
  "Skip an agenda entry if it has a STYLE property equal to \"habit\"."
  (let ((subtree-end (save-excursion (org-end-of-subtree t))))
    (if (string= (org-entry-get nil "STYLE") "habit")
        subtree-end
      nil)))


(defun air-org-skip-subtree-if-priority (priority)
  "Skip an agenda subtree if it has a priority of PRIORITY.

PRIORITY may be one of the characters ?A, ?B, or ?C."
  (let ((subtree-end (save-excursion (org-end-of-subtree t)))
        (pri-value (* 1000 (- org-lowest-priority priority)))
        (pri-current (org-get-priority (thing-at-point 'line t))))
    (if (= pri-value pri-current)
        subtree-end
      nil)))

(provide 'my-org)
;;; my-org.el file ends here
