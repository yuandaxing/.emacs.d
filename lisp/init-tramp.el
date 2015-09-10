(use-package tramp
  :ensure t
  :config
  (progn
    (set-default 'tramp-default-method "sshx")
    (set-default 'tramp-default-user "yuandx")
    (set-default 'tramp-verbose 3) ;default settings
    (set-default 'tramp-shell-prompt-pattern "\\(?:^\\|\r\\)[^]#$%>\n]*#?[]#$%>].* *\\(^[\\[[0-9;]*[a-zA-Z] *\\)*")
    (setq tramp-ssh-controlmaster-options
          (concat
           "-o ControlPath=/tmp/ssh-controlPath-%%r@%%h:%%p"
           " -o ControlMaster=auto -o ControlPersist=yes"
           ))
    ))

(defun hh-find-alternative-file-with-sudo ()
  (interactive)
  (let ((fname (or buffer-file-name
                   dired-directory)))
    (when fname
      (if (string-match "^/sudo:root@localhost:" fname)
          (setq fname (replace-regexp-in-string
                       "^/sudo:root@localhost:" ""
                       fname))
        (setq fname (concat "/sudo:root@localhost:" fname)))
      (find-alternate-file fname))))
(eval-after-load 'tramp '(setenv "SHELL" "/bin/bash"))
(provide 'init-tramp)
