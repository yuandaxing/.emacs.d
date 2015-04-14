(require-package 'tramp)

(require 'tramp)

(set-default 'tramp-default-method "scpx")
(set-default 'tramp-shell-prompt-pattern "\\(?:^\\|\r\\)[^]#$%>\n]*#?[]#$%>].* *\\(^[\\[[0-9;]*[a-zA-Z] *\\)*")
;; (add-to-list 'tramp-default-proxies-alist
;;              '(nil "ambition" "/ssh:%h:/home/ambition/"))

(provide 'init-tramp)
