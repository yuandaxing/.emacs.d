(use-package flycheck
  :ensure t
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode)
  (add-hook 'c++-mode-hook (lambda ()
                             (progn (setq flycheck-gcc-language-standard "c++11")
                                    (setq flycheck-clang-language-standard "c++11" ))))
  (diminish 'flycheck-mode)
  (set-default 'flycheck-disabled-checkers '(emacs-lisp-checkdoc)))
(defun disable-flycheck-mode ()
  (interactive)
  (global-flycheck-mode -1))
(add-hook 'python-mode-hook 'disable-flycheck-mode)
(provide 'init-fly)
