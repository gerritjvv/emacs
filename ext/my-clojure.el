(require 'clojure-mode)
(require 'smartparens)
(require 'cider)


(add-hook 'clojure-mode-hook #'smartparens-strict-mode)

(add-hook 'clojure-mode-hook #'aggressive-indent-mode)

