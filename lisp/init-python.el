(use-package jedi
  :ensure t
  :config
  (progn
    (add-hook 'python-mode-hook 'jedi:setup)
    (setq jedi:complete-on-dot t)))
(use-package python-mode
  :ensure t
  :config
  (progn
    (require 'python-mode)
    (add-to-list 'auto-mode-alist '("\\.py$" . python-mode))))
(setq py-split-windows-on-execute-p t)
(provide 'init-python)
