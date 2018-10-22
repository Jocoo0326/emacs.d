;; disable startup page and all bars
(setq inhibit-startup-message t)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;; set color theme
(use-package spacemacs-theme 
  :ensure t
  :defer t
  :init (load-theme 'spacemacs-dark t))

;; set region face in windows
(when (string-equal system-type "windows-nt")
  (set-face-attribute 'region nil :background "#FFF" :foreground "#93A1A1"))

;; set default font
(cond ((string-equal system-type "darwin")
       (set-default-font "Monaco-13"))
      ((or (string-equal system-type "gnu/linux")
	   (string-equal system-type "windows-nt"))
       (set-default-font "Source Code Pro-14")))

;; show line number
(global-linum-mode t)

(use-package nyan-mode
  :ensure t
  :config
  (nyan-mode 1))

(provide 'init-appearance)
