(defun hh-makefile ()
  (progn
    (whitespace-cleanup-mode 0)
    (setq tab-width 4)))
(use-package blank-mode
  :ensure t
  :config
  (progn
    (add-hook 'makefile-mode-hook 'blank-mode)
    (add-hook 'makefile-mode-hook 'hh-makefile)
    (add-hook 'makefile-mode-hook 'key-bind-hook)))
(provide 'init-makefile)
