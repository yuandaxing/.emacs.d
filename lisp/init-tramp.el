(use-package tramp
  :ensure t
  :config
  (progn
    (setq projectile-mode-line " Projectile")
    (set-default 'tramp-default-method "sshx")
    (set-default 'tramp-default-user "dxyuan")
    (setq tramp-auto-save-directory "/tmp/tramp/")
    (setq tramp-chunksize 2000)
    (setq password-cache-expiry nil)
    ;(set-default 'tramp-shell-prompt-pattern "\\(?:^\\|\r\\)[^]#$%>\n]*#?[]#$%>].* *\\(^[\\[[0-9;]*[a-zA-Z] *\\)*")
    ;; (setq tramp-ssh-controlmaster-options
    ;;       (concat
    ;;        "-o ControlPath=/tmp/ssh-controlPath-%%r@%%h:%%p"
    ;;        " -o ControlMaster=auto -o ControlPersist=yes"
    ;;        ))
    (setq tramp-verbose 6)
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
(provide 'init-tramp)
