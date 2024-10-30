;; miscellaneous
(electric-pair-mode t)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(if (file-exists-p custom-file)
    (load custom-file))
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq inhibit-startup-screen t)
(set-frame-font "fira code-14")

;; custom use package
(defmacro -> (&rest BODY)
  "thread first"
  (pcase BODY ;;destructuring
    (`(,x (,f . ,args) . ,rest) `(-> (,f ,x ,@args) ,@rest))
    (`(,x ,f . ,rest) `(-> (,f ,x) ,@rest))
    (_ (car BODY))))
(defvar-local jocoo/package-content-refreshed nil)
(defun jocoo/use-package (package)
  (unless (package-installed-p package)
    (unless jocoo/package-content-refreshed
      (package-refresh-contents t)
      (setq-local jocoo/package-content-refreshed t))
    (package-install package)))
(defun jocoo/use-theme (theme)
  (let ((theme-package (-> theme
			   (symbol-name)
			   (concat "-theme")
			   (intern))))
    (jocoo/use-package theme-package)
    (load-theme theme)))

;; theme
(jocoo/use-theme 'dracula)
;; multiple cursors
(jocoo/use-package 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/mark-lines)
(global-set-key (kbd "C-c C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-previous-like-this)
;; editing stuff
(jocoo/use-package 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

(global-set-key (kbd "C-c edf")
		(lambda ()
		  (interactive)
		  (find-file user-init-file)))
(setq duplicate-line-final-position 1)
(global-set-key (kbd "C-c C-;") 'duplicate-line)
(global-set-key (kbd "C-c J") 'join-line)

;; magit
(jocoo/use-package 'magit)
(global-set-key (kbd "C-x g") 'magit-status)
