;;; init-translate-plugin.el --- translate word or region handy

(require 'url)
(require 'json)

(defconst translate-url "https://openapi.youdao.com/api")

(defconst translate-app-id "444bd5ef4ab9c9c6")

(defconst translate-app-key "iKqWEawK04HyIIcq5PcPzFWe0cO2sLTh")

(defun get-sign (query salt)
  (let* ((result (concat translate-app-id query salt translate-app-key)))
    (md5 result)))

(defun get-form-data (query)
  (let ((salt (number-to-string (random 999999999))) (form-data-list))
    (setq form-data-list (list (cons "from" "auto")
			       (cons "to" "zh-CHS")
			       (cons "q" query)
			       (cons "salt" salt)
			       (cons "appKey" translate-app-id)
			       (cons "sign" (get-sign query salt))))
    (mapconcat (lambda (arg)
		 (concat (url-hexify-string (car arg))
			 "="
			 (url-hexify-string (cdr arg))))
	       form-data-list
	       "&")))

(defun translate-handler (src)
  (let ((url-request-method "POST")
	(url-request-extra-headers
	 '(("Content-Type" . "application/x-www-form-urlencoded")))
	(url-request-data (get-form-data src)))
    (url-retrieve translate-url 'switch-to-url-buffer)))

(defun switch-to-url-buffer (status)
  (when (buffer-live-p (current-buffer))
    (with-current-buffer (current-buffer)
      (while (and (not (looking-at "^{\\(.*\\)}$"))
		  (eq (forward-line) 0)))
      (message (parse-translate-response (buffer-substring (point) (point-max))))
      (kill-buffer (current-buffer)))))

(defun just-show-buffer (status)
  (switch-to-buffer (current-buffer)))

(defun parse-translate-response (buffer-string)
  (let ((res))
    (setq res (cdr (assoc 'translation
		     (json-read-from-string buffer-string))))
    (message (decode-coding-string (string-join res) 'utf-8))
    ))

(defun jocoo/get-string-in-region (region-start region-end)
  (interactive "r")
    (let ((str (buffer-substring-no-properties region-start region-end)))
      (setq str (replace-regexp-in-string "^[\s\*\\#]*" "" str))
      (setq str (replace-regexp-in-string "\n" " " str))
      (prin1 str)
      ))

(defun jocoo/translate-word-or-region (start end)
  (interactive "r")
  (let ((string-to-be-translated))
    (if (region-active-p)
	(setq string-to-be-translated (jocoo/get-string-in-region start end))
      (setq string-to-be-translated (word-at-point)))
    (translate-handler string-to-be-translated)))

;; (global-set-key (kbd "C-c C-t") 'jocoo/translate-word-or-region)

(provide 'translate)
