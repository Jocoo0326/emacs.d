(defun show-last-command ()
  (interactive)
  (eval-expression 'last-command))

(global-set-key (kbd "C-c C-l") 'show-last-command)

(provide 'init-help)
