;; disable startup page and all bars
(setq inhibit-startup-message t)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;; set color theme
(use-package solarized-theme 
  :ensure t
  :config
  (progn 
    (load-theme 'solarized-light t)
    (set-face-attribute 'region nil :background "#FFF" :foreground "#93A1A1")))

;; set default font
(cond ((string-equal system-type "darwin")
       (set-default-font "Monaco-13"))
      ((or (string-equal system-type "gnu/linux")
	   (string-equal system-type "windows-nt"))
       (set-default-font "YaHei Consolas Hybrid-14")))

;; show line number
(global-linum-mode t)

(provide 'init-appearance)
