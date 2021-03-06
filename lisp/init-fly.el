(use-package flycheck
  :ensure t
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode)
  (diminish 'flycheck-mode)
  (set-default 'flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  (add-hook 'c++-mode-hook (lambda ()
                             (progn (setq flycheck-gcc-language-standard "c++11")
                                    (setq flycheck-clang-language-standard "c++11" ))))
  )
(provide 'init-fly)
