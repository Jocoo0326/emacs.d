;; code snippet
(use-package yasnippet
  :ensure t
  :config (progn
	    (require 'yasnippet)
	    (yas-global-mode 1)))

(use-package yasnippet-snippets
  :ensure t)

(provide 'init-yasnippet)
