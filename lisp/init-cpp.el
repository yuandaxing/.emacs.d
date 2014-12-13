(require 'compile)
(add-hook 'c-mode-hook
		  (lambda ()
			(unless (file-exists-p "Makefile")
			  (set (make-local-variable 'compile-command)
				   ;; emulate make's .c.o implicit pattern rule, but with
				   ;; different defaults for the CC, CPPFLAGS, and CFLAGS
				   ;; variables:
				   ;; $(CC) -c -o $@ $(CPPFLAGS) $(CFLAGS) $<
				   (let ((file (file-name-nondirectory buffer-file-name)))
					 (format "%s -c -o %s.o %s %s %s"
							 (or (getenv "CC") "gcc")
							 (file-name-sans-extension file)
							 (or (getenv "CPPFLAGS") "-DDEBUG=9")
							 (or (getenv "CFLAGS") "-ansi -pedantic -Wall -g")
							 file))))))

(provide 'init-cpp)
