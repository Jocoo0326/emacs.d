(defun show-last-command ()
  "Print the command name latest executed."
  (interactive)
  (eval-expression 'last-command))
(global-set-key (kbd "C-c C-l") 'show-last-command)

;; Putting all files in one directory
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(provide 'init-help)
