(require-package 'key-chord)
(key-chord-mode 1)
(require-package 'ace-jump-mode)
(key-chord-define-global "jj" 'ace-jump-word-mode)
(key-chord-define-global "jl" 'ace-jump-line-mode)
(key-chord-define-global "jk" 'ace-jump-char-mode)
(key-chord-define-global "oo" 'ido-switch-buffer)
(key-chord-define-global "uu" 'undo-tree-visualize)
(key-chord-define-global "xx" 'smex)
(key-chord-define-global "yy" 'browse-kill-ring)
(key-chord-define-global "ff" 'projectile-find-file)

;; (defvar key-chord-tips '("Press <jj> quickly to jump to the beginning of a visible word."
;;                          "Press <jl> quickly to jump to a visible line."
;;                          "Press <jk> quickly to jump to a visible character."
;;                          "Press <JJ> quickly to switch to previous buffer."
;;                          "Press <uu> quickly to visualize the undo tree."
;;                          "Press <xx> quickly to execute extended command."
;;                          "Press <yy> quickly to browse the kill ring."))

;; (setq prelude-tips (append prelude-tips key-chord-tips))

(key-chord-mode +1)

(provide 'init-keychord)
