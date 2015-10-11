(use-package jedi
  :ensure t
  :config
  (progn
    (add-hook 'python-mode-hook 'jedi:setup)
    (setq jedi:complete-on-dot t)))
(provide 'init-python)
