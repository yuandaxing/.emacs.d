;;; code:
  (defun hh-save-macro (name)
    "save a macro. Take a name as argument
     and save the last defined macro under
     this name at the end of your .emacs"
     (interactive "SName of the macro :")  ; ask for the name of the macro
     (kmacro-name-last-macro name)         ; use this name for the macro
     (find-file "~/.emacs.d/lisp/init-macro.el")            ; open ~/.emacs or other user init file
     (goto-char (point-max))               ; go to the end of the .emacs
     (newline)                             ; insert a newline
     (insert-kbd-macro name)               ; copy the macro
     (newline)                             ; insert a newline
     (switch-to-buffer nil))
                                        ; return to the initial buffer
(fset 'clear-buff "\^xh\^x\^k")

(fset 'diff-number-set
      (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([134217830 23 25 24 111 134217788 19 25 return 24 111] 0 "%d")) arg)))
(provide 'init-macro)
;;;

(fset 'write-hostname
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([104 111 115 116 110 97 109 101 21 134217786 40 115 101 116 113 32 120 32 40 43 32 49 32 120 return 46 108 109 46 99 111 109 10] 0 "%d")) arg)))

(fset 'put-curly-bracket-down
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([19 123 return 2 10 14 1 1] 0 "%d")) arg)))

(fset 'hh-format-sql
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ("()," 0 "%d")) arg)))
