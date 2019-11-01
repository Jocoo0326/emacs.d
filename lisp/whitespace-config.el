(defun jocoo/set-up-whitespace-handling ()
  (interactive)
  (whitespace-mode 1)
  (add-to-list 'write-file-functions 'delete-trailing-whitespace))

(add-hook 'whitespace-mode-hook
          '(lambda ()
             (progn
               (set-face-attribute 'whitespace-space nil :background nil :foreground "gray35")
               (set-face-attribute 'whitespace-indentation nil :background nil :foreground "gray35"))))

(custom-set-variables
 '(whitespace-style (quote (face tabs spaces trailing space-before-tab
                                 newline indentation empty space-after-tab
                                 space-mark tab-mark))))

(setq whitespace-display-mappings
      ;; all numbers are Unicode codepoint in decimal. ⁖ (insert-char 182 1)
      '(
        (space-mark 32 [183] [46]) ; 32 SPACE 「 」, 183 MIDDLE DOT 「·」, 46 FULL STOP 「.」
        (newline-mark 10 [180 10]) ; 10 LINE FEED
        (tab-mark 9 [9655 9] [92 9]) ; 9 TAB, 9655 WHITE RIGHT-POINTING TRIANGLE 「▷」
        ))

(let ((whitespace-enabled-modes
       '(
         tuareg-mode-hook
         c++-mode-hook
         c-mode-hook
         emacs-lisp-mode
	 lisp-interaction-mode
         java-mode-hook
         lua-mode-hook
         rust-mode-hook
         scala-mode-hook
         markdown-mode-hook
         js2-mode-hook
         haskell-mode-hook
         python-mode-hook
         erlang-mode-hook
         asm-mode-hook
         nasm-mode-hook
         go-mode-hook
         nim-mode-hook
         makefile-gmake-mode
         )))
  (dolist (mode whitespace-enabled-modes)
    (add-hook mode 'jocoo/set-up-whitespace-handling)))
