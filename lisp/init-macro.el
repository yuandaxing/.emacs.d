(fset 'move-brace-down
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ("{
" 0 "%d")) arg)))
(provide 'init-macro)
