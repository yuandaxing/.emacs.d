(use-package recentf
  :ensure t
  :config
  (progn
    (recentf-mode 1)
    (setq recentf-max-menu-items 1000
          recentf-max-saved-items 1000
          recentf-max-menu-items 15)
    ))
(provide 'init-recentf)
