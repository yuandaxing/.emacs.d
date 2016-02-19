(use-package ace-jump-mode
  :ensure t
  :config
  (key-chord-mode 1))
(use-package helm-projectile
  :ensure t
  :init
  (require 'helm-projectile)
  :config
  (progn
    (setq projectile-globally-ignored-files (append projectile-globally-ignored-files
                                                    '("GTAGS" "GPATH" "GSYMS" "GRTAGS" "makefile" "*\\.pyc"))))
  :bind (
         ("C-c h g" . helm-projectile-grep)))
(use-package hydra
  :ensure t)

(use-package key-chord
  :ensure t
  :config
  (progn
    (key-chord-mode +1)
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
                               ("+" text-scale-increase "increase text size")
                               ("-" text-scale-decrease "decrease text size")
                               ("k" describe-key "describe-key")
                               ("f" describe-function "describe-function")
                               ("m" man "man")
                               ("." repeat)
                               ("n" next-line)
                               ("p" previous-line)
                               ("v" scroll-up) ("V" scroll-down) ("x" smex)
                               ("q" nil)))
    ))

(provide 'init-keychord)
