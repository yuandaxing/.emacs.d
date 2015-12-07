(use-package recentf
  :ensure t
  :config
  (progn
    (recentf-mode 1)
    (setq recentf-max-menu-items 500)
    ))
(provide 'init-recentf)
