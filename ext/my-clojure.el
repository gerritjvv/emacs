(require 'clojure-mode)
(require 'smartparens)
(require 'cider)


(add-hook 'clojure-mode-hook #'smartparens-strict-mode)

(add-hook 'clojure-mode-hook #'aggressive-indent-mode)

(add-hook 'cider-repl-mode-hook #'eldoc-mode)

add-hook 'cider-repl-mode-hook #'cider-company-enable-fuzzy-completion)
(add-hook 'cider-mode-hook #'cider-company-enable-fuzzy-completion)
