;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;; (package-initialize)
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))

(require 'init-package)

(require 'init-use-package)

(require 'init-restclient)

(require 'init-evil)

(require 'init-company)

(require 'init-appearance)

(require 'init-quick-edit)

(require 'init-yasnippet)

(require 'init-swiper)

(require 'init-frame)

(require 'init-help)

(require 'init-paredit)

(require 'init-multiple-cursors)

(require 'init-translate-plugin)

(require 'init-expand-region)

(require 'init-ace-jump)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (ace-jump-mode yasnippet which-key use-package try swiper solarized-theme restclient paredit multiple-cursors material-theme mark-multiple expand-region evil company arjen-grey-theme))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
