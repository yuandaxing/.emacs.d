(use-package solarized-theme
  :ensure t
  :config
  (progn
    (require 'solarized-theme)
    (load-theme 'solarized-light t)))


(add-hook 'shell-mode-hook
               (lambda ()
                (font-lock-add-keywords nil
                 '(("\\<\\(FIXME\\|错误\\|error\\):" 1 font-lock-warning-face t)))))


(provide 'init-theme)
