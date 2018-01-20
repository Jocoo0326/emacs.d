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

;; copy word
(defun copy-word (arg)
  (interactive "p")
  (let ((count (or arg 1)) (beg) (end))
    (if (= count 1)
	(let ((bnd (bounds-of-thing-at-point 'word)))
	  (setq beg (car bnd)
		end (cdr bnd)))
      (save-excursion
	(setq beg (point))
	(forward-word count)
	(setq end (point))))
    (copy-region-as-kill beg end)
    (message "word%s copied." (if (> count 1) "s" ""))))

(global-set-key (kbd "C-c w") 'copy-word)

;; copy line
(defun copy-line (arg)
  (interactive "p")
  (let ((beg (line-beginning-position))
	(end (line-end-position arg)))
    (when mark-active
      (if (> (point) (mark))
	  (setq beg (save-excursion (goto-char (mark)) (line-beginning-position)))
	(setq end (save-excursion (goto-char (mark)) (line-end-position)))))
    (if (eq last-command 'copy-line)
	(kill-append (buffer-substring beg end) (< end beg))
      (kill-ring-save beg end)))
  (kill-append "\n" nil)
  (beginning-of-line (or (and arg (1+ arg)) 2))
  (if (and arg (not (= 1 arg))) (message "%d lines copied" arg)))
(global-set-key (kbd "C-c l") 'copy-line)

;; find file at position
(global-set-key (kbd "C-]") 'ffap)

;; enable paren mode
(show-paren-mode)
(electric-pair-mode)

(provide 'init-quick-edit)
