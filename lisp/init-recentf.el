(use-package recentf
  :ensure t
  :config
  (progn
    (recentf-mode 1)
    (setq recentf-max-menu-items 1000
          recentf-max-saved-items 1000
          recentf-max-menu-items 15)
    ))
;(run-at-time (current-time) 300 'recentf-save-list)
(provide 'init-recentf)
