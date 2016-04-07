(use-package ido-vertical-mode
                 :ensure t
                 :config
                 (progn
                   (require 'ido-vertical-mode)
                   (ido-vertical-mode 1)
                   (setq ido-vertical-define-keys 'C-n-and-C-p-only)))
(provide 'init-ido)
