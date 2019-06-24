(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(show-paren-mode t)
(electric-pair-mode t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-startup-message t)

(use-package magit
  :ensure t
  :bind (("C-x g" . 'magit-status)))

(use-package expand-region
  :ensure t
  :bind (("C-=" . er/expand-region)))

(use-package swiper
  :ensure t
  :bind (("C-s" . 'swiper)))

(use-package company
  :ensure t
  :config
  (global-company-mode t))

(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode t)
  (use-package yasnippet-snippets :ensure t))

(use-package iedit
  :ensure t)

(use-package projectile
  :ensure t
  :bind-keymap
  ("C-c p" . projectile-command-map))

(use-package helm
  :ensure t
  :config
  (use-package helm-gtags
    :init
    (setenv "GTAGSLIBPATH" "/usr/include")
    :ensure t
    :bind (("C-]" . 'helm-gtags-dwim)
	   ("C-o" . 'helm-gtags-pop-stack)
	   ("C-<f12>" . 'helm-gtags-parse-file))
    :hook ((c-mode . helm-gtags-mode)
	   (c-mode . linum-mode)))
  (use-package helm-projectile
    :ensure t
    :bind (("C-c ss" . 'helm-projectile-ag)))
  :bind (("M-x" . 'helm-M-x)
	 ("C-x C-f" . 'helm-find-files)))

(use-package flycheck
  :ensure t)
