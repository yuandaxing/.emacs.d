(use-package django-mode
  :ensure t
  :config
  (progn
    (add-to-list 'auto-mode-alist '("\\.html$" . django-html-mode))
    ))

(provide 'init-web)
