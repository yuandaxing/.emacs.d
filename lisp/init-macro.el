(fset 'move-brace-down
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ("{
" 0 "%d")) arg)))


(fset 'clear-buff "\^xh\^x\^k")
(provide 'init-macro)
