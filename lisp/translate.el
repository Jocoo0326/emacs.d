;;; init-translate-plugin.el --- translate word or region handy

(require 'url)
(require 'json)

(defconst translate-url "https://fanyi-api.baidu.com/api/trans/vip/translate")

(defconst translate-app-id "20170526000049218")

(defconst translate-app-key "pLGTiromt1BVdSx2kJ76")

(defun get-sign (query salt)
  (let* ((result (concat translate-app-id query salt translate-app-key)))
    (md5 result)))

(defun get-form-data (query)
  (let ((salt (number-to-string (random 999999999))) (form-data-list))
    (setq form-data-list (list (cons "from" "auto")
			       (cons "to" "zh")
			       (cons "q" query)
			       (cons "salt" salt)
			       (cons "appid" translate-app-id)
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

(defun jocoo/get-string-in-region (region-start region-end)
  (interactive "r")
  (buffer-substring-no-properties region-start region-end))

(defun jocoo/translate-word-or-region (start end)
  (interactive "r")
  (let ((string-to-be-translated))
    (if (region-active-p)
	(setq string-to-be-translated (jocoo/get-string-in-region start end))
      (setq string-to-be-translated (word-at-point)))
    (translate-handler string-to-be-translated)))

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
  (let ((json-object-type 'alist))
    (mapconcat #'(lambda (arg)
		  (cdr (assoc 'dst arg)))
	       (cdr
		(assoc 'trans_result
		       (json-read-from-string buffer-string)))
	       "\n")))

(global-set-key (kbd "C-c C-t") 'jocoo/translate-word-or-region)

(provide 'translate)
