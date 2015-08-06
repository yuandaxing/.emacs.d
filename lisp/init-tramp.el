(use-package tramp
  :ensure t
  :config
  (progn
    (set-default 'tramp-default-method "scpx")
    (set-default 'tramp-shell-prompt-pattern "\\(?:^\\|\r\\)[^]#$%>\n]*#?[]#$%>].* *\\(^[\\[[0-9;]*[a-zA-Z] *\\)*")
    ))
(provide 'init-tramp)
