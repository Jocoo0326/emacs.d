;; miscellaneous
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(electric-pair-mode t)
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(if (file-exists-p custom-file)
    (load custom-file))
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq inhibit-startup-screen t)
;; (set-frame-font "fira code-13")
(set-frame-font "monaco-13")
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(ido-mode)

;; custom use package
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/") t)
(package-initialize)
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

;; theme
(defun jocoo/use-theme (theme)
  (let ((theme-package (-> theme
			   (symbol-name)
			   (concat "-theme")
			   (intern))))
    (jocoo/use-package theme-package)
    (load-theme theme)))

;; (jocoo/use-theme 'dracula)
;; (jocoo/use-theme 'gruber-darker)
;; (jocoo/use-package 'doom-themes)
(load-theme 'jocoo-material t)

;; multiple cursors
(jocoo/use-package 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/mark-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-;") 'mc/mark-all-like-this)
;; editing stuff
(jocoo/use-package 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

(global-set-key (kbd "C-c c") 'compile)
(global-set-key (kbd "C-c edf")
		(lambda ()
		  (interactive)
		  (find-file user-init-file)))
(setq duplicate-line-final-position 1)
(global-set-key (kbd "C-c C-;") 'duplicate-line)
(global-set-key (kbd "C-c J") 'join-line)
(defun jocoo/mark-line ()
  (interactive)
  (move-beginning-of-line 1)
  (set-mark-command nil)
  (move-end-of-line 1)
  (next-line)
  (move-beginning-of-line 1))
(global-set-key (kbd "C-c C-l") 'jocoo/mark-line)
(setq ring-bell-function 'ignore)

;; magit
(jocoo/use-package 'magit)
(global-set-key (kbd "C-x g") 'magit-status)

(mapc (lambda (b) (global-set-key (kbd (car b))
                                  `(lambda () (interactive)
                                     (when-let ((w (window-in-direction ',(cdr b))))
                                       (select-window w)))))
      '(("S-<left>" . left) ("S-<right>" . right)
        ("S-<up>" . up) ("S-<down>" . down)))

;; restclient
;; (jocoo/use-package 'restclient)
;; (add-to-list 'auto-mode-alist '("\\.rest\\'" . restclient-mode))

(jocoo/use-package 'realgud)
(setq realgud:gdb-command-name "gdb --fullname")
