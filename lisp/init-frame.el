;; set startup frame size
(add-hook 'after-init-hook
	  '(lambda ()
	     (when window-system (set-frame-size (selected-frame) 140 35))))

(provide 'init-frame)
