;; (use-package django-mode
;;   :ensure t
;;   :config
;;   (progn
;;     (add-to-list 'auto-mode-alist '("\\.html$" . django-html-mode))
;;     ))

(use-package web-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html\\.twig\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  )

(provide 'init-web)
