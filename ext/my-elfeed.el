

;;; Commentary:
;;; http://pragmaticemacs.com/emacs/read-your-rss-feeds-in-emacs-with-elfeed/

;;; Code:

(setq elfeed-db-directory "~/Dropbox/Org/elfeeddb")


(defun elfeed-mark-all-as-read ()
      (interactive)
      (mark-whole-buffer)
      (elfeed-search-untag-all-unread))


;;functions to support syncing .elfeed between machines
;;makes sure elfeed reads index from disk before launching
(defun my--elfeed-load-db-and-open ()
  "Wrapper to load the elfeed db from disk before opening"
  (interactive)
  (elfeed-db-load)
  (elfeed)
  (elfeed-search-update--force)
  (elfeed-update))

;;write to disk when quiting
(defun my--elfeed-save-db-and-bury ()
  "Wrapper to save the elfeed db to disk before burying buffer"
  (interactive)
  (elfeed-db-save)
  (quit-window))



(defalias 'elfeed-toggle-star
  (elfeed-expose #'elfeed-search-toggle-all 'star))

(use-package elfeed
  :ensure t
  :bind (:map elfeed-search-mode-map
	      ("q" . my--elfeed-save-db-and-bury)
	      ("Q" . my--elfeed-save-db-and-bury)
	      ("m" . elfeed-toggle-star)
	      ("M" . elfeed-toggle-star)
	      )
  )

(global-set-key (kbd "C-c e") 'my--elfeed-load-db-and-open)

(use-package elfeed-goodies
  :ensure t
  :config
  (elfeed-goodies/setup))


(use-package elfeed-org
  :ensure t
  :config
  (elfeed-org)
  (setq rmh-elfeed-org-files (list "~/Dropbox/Org/elfeed.org")))


;; (defun my--display-current-entry-feed ()
;;   (interactive)
;;   (let ((entry (elfeed-search-selected t)))
;;     (when entry
;;       (let ((feed (elfeed-entry-feed entry)))
;;         (message "%s" (elfeed-feed-url feed))))))


;; (defun my--unsubscribe-current-feed ()
;;   (interactive)
;;   (let ((entry (elfeed-search-selected t)))
;;     (when entry
;;       (let* ((feed (elfeed-entry-feed entry))
;;              (url (elfeed-feed-url feed)))
;;         (setf elfeed-feeds
;;               (cl-remove url elfeed-feeds
;;                          :test #'equal
;;                          :key (lambda (e) (if (listp e) (car e) e)))))
;;       (customize-save-variable 'elfeed-feeds elfeed-feeds))))

(provide 'my-elfeed)
;;; my-elfeed.el ends here
