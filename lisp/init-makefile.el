(require-package 'blank-mode)

(defun sanityinc/makefile ()
  (progn
    (whitespace-cleanup-mode 0)
    (setq tab-width 4)))

(add-hook 'makefile-mode-hook 'blank-mode)
(add-hook 'makefile-mode-hook 'sanityinc/makefile)
(provide 'init-makefile)
