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

(require 'init-multiple-cursors)

(require 'init-translate-plugin)

(require 'init-expand-region)

(require 'init-ace-jump)

(require 'init-cnfonts)

(require 'init-helm-configuration)

(require 'init-projectile)

(require 'init-magit)

(require 'init-ggtags)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-check-signature nil)
 '(package-selected-packages
   (quote
    (ggtags helm-gtags undo-tree magit async bind-key dash epl helm-core ivy pkg-info popup s powerline projectile helm-projectile spaceline helm cnfonts ace-jump-mode expand-region multiple-cursors swiper yasnippet-snippets yasnippet nyan-mode spacemacs-theme company evil which-key try use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
