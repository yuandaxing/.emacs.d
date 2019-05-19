(use-package eclim
  :ensure t
  :config
  (progn
    (require 'auto-complete-config)
    (set-default 'eclim-eclipse-dirs '("/home/ydx/install/eclipse/eclipse/"))
    (set-default 'eclim-executable "/home/ydx/.p2/pool/plugins/org.eclim_2.8.0/bin/eclim")
    (set-default 'eclimd-executable "/home/ydx/.p2/pool/plugins/org.eclim_2.8.0/bin/eclimd")
    (set-default 'eclim-print-debug-messages t)
    (setq help-at-pt-display-when-idle t)
    (require 'ac-emacs-eclim)
    (ac-emacs-eclim-config)
    (setq eclimd-autostart t)
    (defun my-java-mode-hook ()
      (progn
        (eclim-mode t)
        (setq c-basic-offset 2)))
    (add-hook 'java-mode-hook 'my-java-mode-hook)))


(provide 'init-java)
