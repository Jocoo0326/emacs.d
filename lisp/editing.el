(defun jocoo/change-line (beginning end)
  "change region or current line"
  (interactive "r")
  (if (use-region-p)
      (kill-region beginning end)
    (progn
      (move-beginning-of-line 1)
      (kill-line)
      (indent-for-tab-command)
      )))

(global-set-key (kbd "C-c c l") 'jocoo/change-line)

(defun jocoo/indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))

(defun jocoo/new-defun-at-point ()
  (interactive)
  (let* ((bounds (bounds-of-thing-at-point 'sexp))
         (begin (car
                 (end (cdr bounds))
                 (defun-name (buffer-substring begin end))
                 (cursor-placeholder "|"))
                (beginning-of-defun)
                (insert (format "(defun %s ()" defun-name))
                (newline-and-indent)
                (insert cursor-placeholder)
                (newline-and-indent)
                (insert ")")
                (newline 2)
                (search-backward cursor-placeholder)
                (forward-char 1)
                (delete-char -1)
                (jocoo/indent-buffer)))))
(global-set-key (kbd "C-c <tab>") 'jocoo/indent-buffer)

(defun jocoo/insert-new-line-before-current (times)
  (interactive "p")
  (move-beginning-of-line 1)
  (newline times)
  (previous-line times)
  (indent-for-tab-command))
(global-set-key (kbd "C-S-o") 'jocoo/insert-new-line-before-current)

(defun jocoo/insert-new-line-below-current (times)
  (interactive "p")
  (move-end-of-line 1)
  (newline times)
  (indent-for-tab-command))
(global-set-key (kbd "C-c o") 'jocoo/insert-new-line-below-current)

(defun jocoo/join-next-line ()
  (interactive)
  (next-line)
  (join-line))
(global-set-key (kbd "C-c J") 'jocoo/join-next-line)
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
(jocoo/region-operate "copy" "line" copy-region-as-kill)
(jocoo/region-operate "delete" "line" kill-region)
(global-set-key (kbd "C-c c l") 'jocoo/copy-line-under)
(global-set-key (kbd "C-c d l") 'jocoo/delete-line-under)

;; paragraph operation
(jocoo/region-operate "copy" "paragraph" copy-region-as-kill)
(jocoo/region-operate "delete" "paragraph" kill-region)
(global-set-key (kbd "C-c c p") 'jocoo/copy-paragraph-under)
(global-set-key (kbd "C-c d p") 'jocoo/delete-paragraph-under)
(global-set-key (kbd "C-c v p") 'mark-paragraph)

(global-set-key (kbd "C-S-n")
                (lambda ()
                  (interactive)
                  (ignore-errors (next-line 5))))

(global-set-key (kbd "C-S-p")
                (lambda ()
                  (interactive)
                  (ignore-errors (previous-line 5))))

(global-set-key (kbd "C-S-f")
                (lambda ()
                  (interactive)
                  (ignore-errors (forward-char 5))))

(global-set-key (kbd "C-S-b")
                (lambda ()
                  (interactive)
                  (ignore-errors (backward-char 5))))

(defun jocoo/delete-current-buffer-file ()
  "Removes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer)))
    (if (not (and filename (file-exists-p filename)))
        (ido-kill-buffer)
      (when (yes-or-no-p (format "Are you sure to remove this file '%s'?" filename))
        (delete-file filename)
        (kill-buffer buffer)
        (message "File '%s' successfully removed." filename)))))
(global-set-key (kbd "C-x C-k") 'jocoo/delete-current-buffer-file)

(defun jocoo/rename-current-buffer-file ()
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully rename to '%s'"
                   name (file-name-nondirectory new-name)))))))
(global-set-key (kbd "C-c rf") 'jocoo/rename-current-buffer-file)

(defun jocoo/copy-buffer-file-path ()
  (interactive)
  (let ((filepath (buffer-file-name)))
    (if (not (and filepath (file-exists-p filepath)))
        (message "no file connect to this buffer.")
      (kill-new filepath)
      (message "'%s' copyed." filepath))))

(global-set-key (kbd "C-c b f p") 'jocoo/copy-buffer-file-path)

(defun jocoo/move-line-down ()
  (interactive)
  (let ((col (current-column)))
    (save-excursion
      (forward-line)
      (transpose-lines 1))
    (forward-line)
    (move-to-column col)))
(global-set-key (kbd "<C-S-down>") 'jocoo/move-line-down)

(defun jocoo/move-line-up ()
  (interactive)
  (let ((col (current-column)))
    (save-excursion
      (forward-line)
      (transpose-lines -1))
    (forward-line -2)
    (move-to-column col)))
(global-set-key (kbd "<C-S-up>") 'jocoo/move-line-up)

(defun jocoo/repeat-current-line ()
  (interactive)
  (let* ((line-bounds (bounds-of-thing-at-point 'line))
         (line-begin (car line-bounds))
         (line-end (cdr line-bounds))
         (point (point)))
    (move-beginning-of-line 1)
    (insert (buffer-substring line-begin line-end))
    (goto-char point)
    (next-line)))
(global-set-key (kbd "C-c ;") 'jocoo/repeat-current-line)

(defun jocoo/mark-line (args)
  "mark thing at point"
  (interactive "P")
  (when (region-active-p)
    (move-end-of-line 1)
    (set-mark (point))
    (move-beginning-of-line 1)))

;; marking
(global-set-key (kbd "C-c m s") 'er/mark-symbol)
(global-set-key (kbd "C-c m l") 'jocoo/mark-line)
(global-set-key (kbd "C-c m q") 'er/mark-inside-quotes)
(global-set-key (kbd "C-c m Q") 'er/mark-outside-quotes)
(global-set-key (kbd "C-c m p") 'er/mark-inside-pairs)
(global-set-key (kbd "C-c m P") 'er/mark-outside-pairs)
(global-set-key (kbd "C-c m f") 'mark-defun)

;; zap
(global-set-key (kbd "M-z") 'zap-up-to-char)

(defun jocoo/mark-to-char (args char)
  "mark to char(included)"
  (interactive (list (prefix-numeric-value current-prefix-arg)
		     (read-char "char: " t)))
  (when (not (use-region-p))
    (save-excursion
      (search-forward (char-to-string char) (buffer-end 1) t args)
      (exchange-point-and-mark))))
(global-set-key (kbd "C-c m t c") 'jocoo/mark-to-char)
