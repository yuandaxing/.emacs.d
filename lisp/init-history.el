(require 'savehist)
(setq savehist-file (expand-file-name "savehist" user-emacs-directory))
(savehist-mode +1)
(setq savehist-save-minibuffer-history +1)
(setq savehist-additional-variables
      '(kill-ring
        search-ring
        regexp-search-ring
        helm-ag--helm-history))
(provide 'init-history)
