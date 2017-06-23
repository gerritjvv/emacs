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
(require 'perspective)
(require 'persp-projectile)
(require 'counsel-projectile)
(require 'imenu-anywhere)
(require 'magit)
(require 'ivy-hydra)

(counsel-projectile-on)

(persp-mode 1)
(projectile-mode 1)
(ivy-mode 1)




(setq ivy-use-virtual-buffers t
      ivy-count-format "(%d/%d) "
      ivy-virtual-abbreviate 'name)

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

;;(global-set-key (kbd "") 'projectile-persp-switch-project)

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
							"elfeeddb"
							".cask"
							"jdt.ls-java-project"
							"elpa")
						      projectile-globally-ignored-directories))


(setq projectile-globally-ignored-file-suffixes '(".class" ".jar" ".dat" ".rpm" ".deb" ".bin" ".tar" ".tar.gz" ".gz" ".bz2" ".lzo" ".zip" ".el~" ".el#" ".cache" ".el.swp" ".png" ".jpg" ".gif"
						  ".swo" ".swp" ".back" ".bak" ".eld"
 ".java#" ".org#"))

;;(setq projectile-switch-project-action )

(provide 'my-ivy)
;;; my-ivy.el ends here
