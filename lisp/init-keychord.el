(use-package ace-jump-mode
  :ensure t
  :config)
(use-package helm-projectile
  :ensure t
  :init
  (require 'helm-projectile)
  :config
  (progn
    (setq projectile-globally-ignored-files (append projectile-globally-ignored-files
                                                    '("GTAGS" "GPATH" "GSYMS" "GRTAGS" "makefile" "*\\.pyc"))))
  :bind (
         ("C-c h g" . helm-projectile-grep)
         ("C-c h a" . ff-get-other-file)
     ))
(use-package hydra
  :ensure t)

(use-package key-chord
  :ensure t
  :config
  (progn
    (setq key-chord-two-keys-delay 0.3)
    (key-chord-mode 1)
    (key-chord-define-global "jj" 'ace-jump-word-mode)
    (key-chord-define-global "jl" 'ace-jump-line-mode)
    (key-chord-define-global "jk" 'ace-jump-char-mode)
    (key-chord-define-global ",," 'ido-switch-buffer)
    (key-chord-define-global ",." 'helm-projectile-find-file)
    (key-chord-define-global "uu" 'undo-tree-visualize)
    (key-chord-define-global "xx" 'smex)
    (key-chord-define-global "yy"
                             (defhydra handy(nil)
                               "easy"
                               ("a" org-agenda "agenda")
                               ("s" search-snippet "search snippet")
                               ("m" search-ambition "search ambition")
                               ("k" describe-key "describe-key")
                               ("f" describe-function "describe-function")
                               ("c" compile "compile c or c++ code")
                               ("," ido-switch-buffer "switch buffer")
                               ("m" man "man")
                               ("o" other-window "other windows")
                               ("x" smex "smex")
                               ("+" text-scale-increase "increase text size")
                               ("-" text-scale-decrease "decrease text size")
                               ("q" nil "quit")))
    ))

(provide 'init-keychord)
