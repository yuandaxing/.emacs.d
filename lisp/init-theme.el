(use-package solarized-theme
  :ensure t
  :config
  (progn
    (require 'solarized-theme)
    (load-theme 'solarized-light t)))


(add-hook 'shell-mode-hook
               (lambda ()
                (font-lock-add-keywords nil
                                        '(("\\<\\(FIXME\\|错误\\|error\\|FAILED\\|Error\\|fail\\)" 1 font-lock-warning-face t)))
                (font-lock-add-keywords nil
                                        '(("\\<\\(OK\\|PASSED\\|pass\\)" 1 font-lock-keyword-face t)))
                ))
(add-hook 'eshell-mode-hook
                             (lambda ()
                (font-lock-add-keywords nil
                                        '(("\\<\\(FIXME\\|错误\\|error\\|FAILED\\|Error\\|fail\\)" 1 font-lock-warning-face t)))
                (font-lock-add-keywords nil
                                        '(("\\<\\(OK\\|PASSED\\|pass\\)" 1 font-lock-keyword-face t)))
                ))

(provide 'init-theme)
