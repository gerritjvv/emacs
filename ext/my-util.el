
;;; Code:

(defun util-line-contains-opt-p (&rest args)
  "Search for an occurance of each ARGS."
  (util-line-contains-p (regexp-opt args)))

(defun util-line-contains-p (regex)
  "Search a line from the begning for the REGEX supplied."
  (save-excursion
    (move-beginning-of-line nil)
    (search-forward-regexp regex (line-end-position) 'f 1)))

(provide 'my-util)
;;;; util ends here
