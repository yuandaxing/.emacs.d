(require 'doc-view)
(add-hook 'doc-view-mode-hook
  (lambda ()
    (linum-mode -1)
  ))

(provide 'init-docview)
