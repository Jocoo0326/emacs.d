;; disable startup page and all bars
(setq inhibit-startup-message t)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;; set color theme
(use-package material-theme
  :ensure t
  :config (load-theme 'material t))

;; set default font
(set-default-font "YaHei Consolas Hybrid-13")
;; (set-default-font "Monaco-13")

;; show line number
(global-linum-mode t)

(provide 'init-appearance)
