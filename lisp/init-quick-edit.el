;; insert new line above current line
(defun my/insert-new-line-before-current (times)
  (interactive "p")
  (move-beginning-of-line 1)
  (newline times)
  (previous-line times)
  (indent-for-tab-command))
(global-set-key (kbd "C-S-o") 'my/insert-new-line-before-current)

;; insert new line below current line
(defun my/insert-new-line-below-current (times)
  (interactive "P")
  (move-end-of-line 1)
  (newline times)
  (indent-for-tab-command))
(global-set-key (kbd "C-o") 'my/insert-new-line-below-current)

;; delete word under cursor
(defun my/delete-word-under-cursor ()
  (interactive)
  (backward-word)
  (kill-word 1))
(global-set-key (kbd "C-c c i w") 'my/delete-word-under-cursor)

(global-set-key (kbd "C-]") 'ffap)

;; enable paren mode
(show-paren-mode)
(electric-pair-mode)

(provide 'init-quick-edit)
