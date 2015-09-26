(progn
  (setq backup-directory-alist '(("." . (expand-file-name "backups" user-emacs-directory))))
  (setq delete-old-versions -1)
  (setq version-control t)
  (setq vc-make-backup-files t)
  (setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))
  (setq sentence-end-double-space nil))
