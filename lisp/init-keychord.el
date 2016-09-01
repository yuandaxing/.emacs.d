(use-package ace-jump-mode
  :ensure t
  :config)
(use-package helm-projectile
  :ensure t
  :init
  (require 'helm-projectile)
  :bind (
         ("C-c h g" . helm-projectile-ag)
         ("C-c h a" . ff-get-other-file)
         ("C-c h b" . helm-projectile-ag)
     ))
(use-package hydra
  :ensure t)

(use-package key-chord
  :ensure t
  :config
  (progn
    (setq key-chord-two-keys-delay 0.5)
    (key-chord-mode 1)
    (key-chord-define-global "jj" 'ace-jump-word-mode)
    (key-chord-define-global "jl" 'ace-jump-line-mode)
    (key-chord-define-global "jk" 'ace-jump-char-mode)
    (key-chord-define-global ",," 'helm-buffers-list)
    (key-chord-define-global ",." 'helm-projectile-find-file)
    (key-chord-define-global "uu" 'undo-tree-visualize)
    (key-chord-define-global "xx" 'helm-M-x)
    (key-chord-define-global "yy"
                             (defhydra handy(nil)
                               "easy"
                               ("a" org-agenda "agenda")
                               ("s" search-snippet "search snippet")
                               ("b" search-ambition "search ambition")
                               ("k" describe-key "describe-key")
                               ("f" describe-function "describe-function")
                               ("p" async-make "make project")
                               ("c" compile "compile c or c++ code")
                               ("," ido-switch-buffer "switch buffer")
                               ("m" man "man")
                               ("o" other-window "other windows")
                               ("x" helm-M-x "helm-M-x")
                               ("+" text-scale-increase "increase text size")
                               ("-" text-scale-decrease "decrease text size")
                               ("q" nil "quit")))
    ))

(provide 'init-keychord)
