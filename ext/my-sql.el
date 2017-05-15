
;;; Code:

(require 'sql)
(require 'sqlup-mode)

(add-hook 'sql-mode-hook 'sqlup-mode)
;; Capitalize keywords in an interactive session (e.g. psql)
(add-hook 'sql-interactive-mode-hook 'sqlup-mode)

(provide 'my-sql)

;;; my-sql.el ends here
