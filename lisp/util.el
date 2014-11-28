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
			  (string-match name (symbol-name x))
			  (add-to-list 'funclist 
						   (concat (symbol-name x) " - " (first-line (documentation x))
								   "\n")))))
	  (with-output-to-temp-buffer "output"
		(dolist (item (sort funclist 'string<))
		  (print item))))))
