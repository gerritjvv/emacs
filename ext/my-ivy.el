;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; http://oremacs.com/swiper/#introduction
;;;;;;;;;;
;;;;;;;;;; projectile http://projectile.readthedocs.io/en/latest/configuration/
;;;
;;;; http://projectile.readthedocs.io/en/latest/usage/#ignoring-files

(require 'ivy)
(require 'counsel)
(require 'swiper)
(require 'projectile)
(require 'counsel-projectile)
(require 'imenu-anywhere)

(counsel-projectile-on)

(projectile-mode 1)
(ivy-mode 1)


(setq ivy-use-virtual-buffers t)
(setq ivy-count-format "(%d/%d) ")

(setq projectile-completion-system 'ivy)

(setq magit-completing-read-function 'ivy-completing-read)


;;(setq ivy-re-builders-alist
  ;;    '((ivy-switch-buffer . ivy--regex-plus)
    ;;    (t . ivy--regex-fuzzy)))

(global-set-key (kbd "C-.") #'ivy-imenu-anywhere)

(global-set-key (kbd "C-s") 'swiper)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "M-y") 'counsel-yank-pop)
(global-set-key (kbd "C-x b") 'ivy-switch-buffer)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)

(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)

(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(global-set-key (kbd "C-c k") 'counsel-ag)
(global-set-key (kbd "C-x l") 'counsel-locate)
(global-set-key (kbd "C-S-o") 'counsel-rhythmbox)

(global-set-key (kbd "C-c C-r") 'ivy-resume)

