(defun describe-function-primitive (name)   
  (interactive "sInput Function name:")
  (flet ((first-line (text)
					 (if text
						 (substring text 0 (string-match "\n" text))
					   "")))
	(let ((funclist (list)))
	  (mapatoms 
	   (lambda (x)
		 (and (fboundp x)                     ; does x name a function?
			  (not (commandp (symbol-function x))) ; is it non-interactive?
			  (subrp (symbol-function x))          ; is it built-in?
			  (string-match name (system-name x))
			  (add-to-list 'funclist 
						   (concat (symbol-name x) " - " (first-line (documentation x))
								   "\n")))))
	  (dolist (item (sort funclist 'string<))
		(insert item)))))






