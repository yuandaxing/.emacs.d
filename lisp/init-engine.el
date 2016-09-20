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
      :keybinding "t")
    (defengine bing
      "http://cn.bing.com/search?q=%s"
      :keybinding "b")
    (defengine stackoverflow
      "http://stackoverflow.com/search?q=%s"
      :keybinding "s")
    )
  )
(provide 'init-engine)
