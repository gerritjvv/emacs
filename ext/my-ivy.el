;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; http://oremacs.com/swiper/#introduction
;;;;;;;;;;
;;;;;;;;;; projectile http://projectile.readthedocs.io/en/latest/configuration/
;;;
;;;; http://projectile.readthedocs.io/en/latest/usage/#ignoring-files


;;; Code:

(require 'ivy)
(require 'counsel)
(require 'swiper)
(require 'projectile)
(require 'counsel-projectile)
(require 'imenu-anywhere)
(require 'magit)
(require 'ivy-rich)

(counsel-projectile-on)

(projectile-mode 1)
(ivy-mode 1)


(defun my--ivy-rich-switch-buffer-transformer (str)
  "Transform STR to more readable format.

Currently the transformed format is

| Buffer name |  Project"
  (let ((buf (get-buffer str)))
    (cond (buf (with-current-buffer buf
                 (let* ((indicator  (ivy-rich-switch-buffer-indicators))
			(buf-name   (ivy-rich-switch-buffer-buffer-name))
			(project    (ivy-rich-switch-buffer-project))
                        (path       (ivy-rich-switch-buffer-path project)))
                   (ivy-rich-switch-buffer-format `(,buf-name ,project ,path)))))
          ((and (eq ivy-virtual-abbreviate 'full)
                ivy-rich-switch-buffer-align-virtual-buffer)
           (ivy-rich-switch-buffer-virtual-buffer))
          (t str))))

(ivy-set-display-transformer 'ivy-switch-buffer 'my--ivy-rich-switch-buffer-transformer)

(setq ivy-use-virtual-buffers t
      ivy-count-format "(%d/%d) "
      ivy-virtual-abbreviate 'name
      ivy-rich-switch-buffer-align-virtual-buffer t
      ivy-rich-abbreviate-paths t)

(setq projectile-completion-system 'ivy)

(setq magit-completing-read-function 'ivy-completing-read)

(defmacro projectile-aware (p-f f)
  "If in project run P-F otherwise run F."
  `(lambda (&optional args)
    (interactive)
    (if (projectile-project-p)
	(apply ,p-f args)
      (apply ,f args))))

(global-set-key (kbd "C-.") #'ivy-imenu-anywhere)

(global-set-key (kbd "<f3>") 'ansi-term)

(global-set-key (kbd "C-s") 'swiper)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "M-y") 'counsel-yank-pop)
(global-set-key (kbd "C-x b") 'ivy-switch-buffer)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)

(global-set-key (kbd "<f2> f") 'counsel-describe-function)
(global-set-key (kbd "<f2> v") 'counsel-describe-variable)
(global-set-key (kbd "<f2> l") 'counsel-find-library)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)

(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c j") 'counsel-git-grep) ;; prefer counsel-ag
(global-set-key (kbd "C-c k") (projectile-aware 'counsel-projectile-ag 'counsel-ag))
(global-set-key (kbd "C-x l") 'counsel-locate)
(global-set-key (kbd "C-S-o") 'counsel-rhythmbox)

(global-set-key (kbd "C-c C-r") 'ivy-resume)


;;;;;;;;; projectile specific

(setq projectile-sort-order 'recentf)

(setq projectile-mode-line '(:eval (format "P[%s]" (projectile-project-name))))

(setq projectile-globally-ignored-files (append '(".ensime"
						  ".gitignore"
						  ".bintray"
						  ".travis.yml"
						  ".mode"
						  ".cask")
						projectile-globally-ignored-files))

(setq projectile-globally-ignored-directories (append '(".ensime_cache.d"
							"bin"
							".gradle"
							".gradle-old"
							".metadata"
							".cask"
							"elpa")
						      projectile-globally-ignored-directories))


(setq projectile-globally-ignored-file-suffixes '(".class" ".jar" ".dat" ".rpm" ".deb" ".bin" ".tar" ".tar.gz" ".gz" ".bz2" ".lzo" ".zip" ".el~" ".el#" ".cache" ".el.swp" ".png" ".jpg" ".gif"
 ".java#" ".org#"))

;;(setq projectile-switch-project-action )

(provide 'my-ivy)
;;; my-ivy.el ends here
