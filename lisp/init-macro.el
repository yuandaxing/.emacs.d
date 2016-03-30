;;; code:
(fset 'clear-buff "\^xh\^x\^k")

(fset 'diff-number-set
      (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([134217830 23 25 24 111 134217788 19 25 return 24 111] 0 "%d")) arg)))
(provide 'init-macro)
;;;
