;;; package --- summary

;;; Commentary:
;;; All things performance related while running Emacs.

;;; Code:

(setq garbage-collection-messages 't)
(defun my-minibuffer-setup-hook ()
  "Set gc threshold to never run."
  (setq gc-cons-threshold most-positive-fixnum))

(defun my-minibuffer-exit-hook ()
  "Set gc threshold back to 8Kb."
  (setq gc-cons-threshold 800000))

(add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)

(provide 'my-perf)
;;; my-perf.el ends here
