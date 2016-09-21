(use-package flycheck
  :ensure t
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode)
  (diminish 'flycheck-mode)
  (set-default 'flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  )

(add-hook 'c++-mode-hook (lambda () (flycheck-mode -1)))


(provide 'init-fly)
