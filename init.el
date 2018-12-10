;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;; (package-initialize)
(require 'package)
(setq package-archives '(("gnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
			 ("melpa" . "http://mirros.tuna.tsinghua.edu.cn/elpa/melpa/")))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(org-babel-load-file (concat user-emacs-directory "config.org"))


;; ;; (require 'init-evil)

;; (require 'init-company)

;; (require 'init-appearance)

;; (require 'init-quick-edit)

;; (require 'init-frame)

;; (require 'init-help)

;; (require 'init-multiple-cursors)

;; (require 'init-translate-plugin)

;; (require 'init-expand-region)

;; (require 'init-ace-jump)

;; (require 'init-cnfonts)

;; (require 'init-helm-configuration)

;; (require 'init-projectile)

;; (require 'init-magit)

;; (require 'init-ggtags)

;; (require 'init-neotree)
