(use-package engine-mode
  :ensure t
  :config
  (progn
    (engine-mode 1)
    (defengine google
      "http://www.google.com/search?ie=utf-8&oe=utf-8&q=%s"
      :keybinding "g")
    (defengine github
      "https://github.com/search?ref=simplesearch&q=%s"
      :keybinding "t")))
(provide 'init-engine)
