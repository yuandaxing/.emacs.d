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
    (setq py-split-windows-on-execute-p t)
    (add-to-list 'auto-mode-alist '("\\.py$" . python-mode))))

(provide 'init-python)
