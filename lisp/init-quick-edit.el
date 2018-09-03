;; insert new line above current line
(defun jocoo/insert-new-line-before-current (times)
  (interactive "p")
  (move-beginning-of-line 1)
  (newline times)
  (previous-line times)
  (indent-for-tab-command))
(global-set-key (kbd "C-S-o") 'jocoo/insert-new-line-before-current)

;; insert new line below current line
(defun jocoo/insert-new-line-below-current (times)
  (interactive "P")
  (move-end-of-line 1)
  (newline times)
  (indent-for-tab-command))
(global-set-key (kbd "C-o") 'jocoo/insert-new-line-below-current)

;;------------------------------------------------------------------------------------------
;; copy/delete chars words lines paragraphs
;;------------------------------------------------------------------------------------------

;; operate region macro
(defmacro jocoo/region-operate (op-name unit op)
  `(defun ,(intern (concat "jocoo/" op-name "-" unit "-under")) (arg)
     (interactive "p")
     (let ((count (or arg 1)) (beg) (end) (bound))
       (setq bound (bounds-of-thing-at-point (quote ,(intern unit))))
       (setq beg (car bound))
       (save-excursion
	 (goto-char beg)
	 (,(intern (concat "forward-" unit)) count)
	 (setq end (point)))
       (,op beg end)
       (message ,(concat op-name " " unit "%s") (if (> count 1) "s" "")))))

;; char operation
(jocoo/region-operate "copy" "char" copy-region-as-kill)
(jocoo/region-operate "delete" "char" kill-region)
(global-set-key (kbd "C-c c c") 'jocoo/copy-char-under)
(global-set-key (kbd "C-c d c") 'jocoo/delete-char-under)

;; word operation
(jocoo/region-operate "copy" "word" copy-region-as-kill)
(jocoo/region-operate "delete" "word" kill-region)
(global-set-key (kbd "C-c c w") 'jocoo/copy-word-under)
(global-set-key (kbd "C-c d w") 'jocoo/delete-word-under)

;; line operation
(jocoo/region-operate "copy" "paragraph" copy-region-as-kill)
(jocoo/region-operate "delete" "paragraph" kill-region)
(global-set-key (kbd "C-c c p") 'jocoo/copy-paragraph-under)
(global-set-key (kbd "C-c d p") 'jocoo/delete-paragraph-under)

;; paragraph operation
(jocoo/region-operate "copy" "paragraph" copy-region-as-kill)
(jocoo/region-operate "delete" "paragraph" kill-region)
(global-set-key (kbd "C-c c p") 'jocoo/copy-paragraph-under)
(global-set-key (kbd "C-c d p") 'jocoo/delete-paragraph-under)
(global-set-key (kbd "C-c v p") 'mark-paragraph)

;;------------------------------------------------------------------------------------------
;; zap [up] to char operations
;;------------------------------------------------------------------------------------------

(global-set-key (kbd "C-c z c") 'zap-to-char)
(defun jocoo/zap-up-to-char (args char)
  (interactive "p\ncZap up to char:")
  (zap-to-char args char)
  (insert char)
  (forward-char -1))
(global-set-key (kbd "C-c z u") 'jocoo/zap-up-to-char)

;;------------------------------------------------------------------------------------------

;; find file at position
(global-set-key (kbd "C-]") 'ffap)

;; delete region when typing
(pending-delete-mode t)

;; enable paren mode
(show-paren-mode)
(electric-pair-mode)

(provide 'init-quick-edit)
