
;; Comentary:
;;;  Shift key bindings for custom contextual work
;;; Code:

(require 'counsel-projectile)

(global-set-key (kbd "s-f") 'counsel-projectile-find-file)
(global-set-key (kbd "s-b") 'counsel-projectile-switch-to-buffer)

(provide 'my-shiftkeys.el)
;;; my-shiftkeys.el ends here
