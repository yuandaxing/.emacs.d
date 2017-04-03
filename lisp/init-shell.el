(require 'shell)
(require 'extend-shell)
(set-default 'shell-file-name "/bin/bash")
(defun comint-write-history-on-exit (process event)
  (comint-write-input-ring)
  (let ((buf (process-buffer process)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (insert (format "\nProcess %s %s" process event))))))

(defun turn-on-comint-history ()
  (let ((process (get-buffer-process (current-buffer))))
    (when process
      (setq comint-input-ring-file-name
            (format "~/Dropbox/emacs/history/inferior-%s-%s--history"
                    (system-name)
                    (process-name process)
                    ))
      (make-directory (file-name-directory comint-input-ring-file-name) t)
      (comint-read-input-ring)
      (set-process-sentinel process
                            #'comint-write-history-on-exit))))
(defun mapc-buffers (fn)
  (mapc (lambda (buffer)
          (with-current-buffer buffer
            (funcall fn)))
        (buffer-list)))

(defun comint-write-input-ring-all-buffers ()
  (mapc-buffers 'comint-write-input-ring))
(add-hook 'kill-emacs-hook 'comint-write-input-ring-all-buffers)
(add-hook 'shell-mode-hook #'(lambda ()
                               (add-hook 'kill-buffer-hook 'comint-write-input-ring :local t)))
(add-hook 'shell-mode-hook 'turn-on-comint-history)
(defun shell1 (&optional buffer)
  (interactive
   (list
    (and current-prefix-arg
         (prog1
             (read-buffer "Shell buffer: "
                          (if (and (eq major-mode 'shell-mode)
                                   (null (get-buffer-process (current-buffer))))
                              (buffer-name)
                            (get-buffer-create "*shell*")))
           (if (file-remote-p default-directory)
               (setq default-directory
                     (expand-file-name
                      (read-directory-name
                       "Default directory: " default-directory default-directory
                       t nil))))))))
  (setq buffer (if (or buffer (not (derived-mode-p 'shell-mode))
                       (comint-check-proc (current-buffer)))
                   (get-buffer-create (or buffer "*shell*"))
                 (current-buffer)))
  (if (and (called-interactively-p 'any)
           (file-remote-p default-directory)
           (null explicit-shell-file-name)
           (null (getenv "ESHELL")))
      (with-current-buffer buffer
        (set (make-local-variable 'explicit-shell-file-name)
             (file-remote-p
              (expand-file-name
               (read-file-name
                "Remote shell path: " default-directory shell-file-name
                t shell-file-name))
              'localname))))
  (switch-to-buffer buffer)
  (unless (comint-check-proc buffer)
    (let* ((prog (or explicit-shell-file-name
                     (getenv "ESHELL") shell-file-name))
           (name (file-name-nondirectory prog))
           (startfile (concat "~/.emacs_" name))
           (xargs-name (intern-soft (concat "explicit-" name "-args"))))
      (unless (file-exists-p startfile)
        (setq startfile (concat user-emacs-directory "init_" name ".sh")))
      (apply 'make-comint-in-buffer "shell" buffer prog
             (if (file-exists-p startfile) startfile)
             (if (and xargs-name (boundp xargs-name))
                 (symbol-value xargs-name)
               '("-i")))
      (shell-mode)))
  buffer)
(defun execute-below-eshell-return ()
  (interactive)
  (save-some-buffers t nil)
  (progn
    (dolist (w (window-list nil nil nil))
      (if (string= (buffer-name (window-buffer w))
                   "\*eshell\*")
          (delete-window w)))
    (while (window-in-direction 'below)
      (delete-window (window-in-direction 'below)))
    (while (window-in-direction 'above)
      (delete-window (window-in-direction 'above)))
    (let ((buf-name (buffer-name)))
      (split-window nil nil 'above)
      (eshell)
      (goto-char (point-max))
      (helm-eshell-history)
      (eshell-send-input)
      (switch-to-buffer-other-window buf-name))))
(require 'helm-misc)
(defvar hh-input-source
  (helm-build-dummy-source "dummy history"
    :action 'helm-comint-input-ring-action
    ))
(defvar hh-perfect-source nil)
(setq hh-perfect-source '(helm-source-comint-input-ring
                          hh-input-source))
(defvar helm-comint-input-ring2-history nil)

(defun helm-comint-input-ring2 ()
  "Preconfigured `helm' that provide completion of `comint' history."
  (interactive)
  (when (derived-mode-p 'comint-mode)
    (helm :sources hh-perfect-source
          :input (buffer-substring-no-properties (comint-line-beginning-position)
                                                 (point-at-eol))
          :buffer "*helm comint history*"
          :history 'helm-comint-input-ring2-history)))
(add-to-list 'savehist-additional-variables 'helm-comint-input-ring2-history)
(defun create-or-get-shell-below (name)
  (save-some-buffers t nil)
  (progn
    (dolist (w (window-list nil nil nil))
      (if (string= (buffer-name (window-buffer w))
                   name)
          (delete-window w)))
    (while (window-in-direction 'below)
      (delete-window (window-in-direction 'below)))
    (while (window-in-direction 'above)
      (delete-window (window-in-direction 'above)))
    (let ((buf-name (buffer-name)))
      (split-window nil nil 'above)
      (shell1 name)
      (switch-to-buffer-other-window buf-name)
      )))
(defvar current-shell-name nil)
(add-to-list 'savehist-additional-variables 'current-shell-name)
(defun execute-below-shell-return (shell-name)
  (interactive "sShell Name:")
  (save-some-buffers t nil)
  (hh-setting-env)
  (if shell-name
      (setq current-shell-name shell-name))
  (progn
    (dolist (w (window-list nil nil nil))
      (if (string= (buffer-name (window-buffer w))
                   shell-name)
          (delete-window w)))
    (while (window-in-direction 'below)
      (delete-window (window-in-direction 'below)))
    (while (window-in-direction 'above)
      (delete-window (window-in-direction 'above)))
    (let ((buf-name (buffer-name)))
      (split-window nil nil 'above)
      (shell1 shell-name)
      (goto-char (point-max))
      (helm-comint-input-ring2)
      (comint-send-input)
      (switch-to-buffer-other-window buf-name))))
(defun hh-execute-below-shell-return (prefix)
  (interactive "p")
  (if (= prefix 1)
      (execute-below-shell-return (if current-shell-name
                                      current-shell-name
                                    "*shell"))
    (call-interactively 'execute-below-shell-return)))
(global-set-key (kbd "C-c h e") 'execute-below-eshell-return)
(global-set-key (kbd "C-c h s") 'hh-execute-below-shell-return)
(add-hook 'shell-mode-hook #'(lambda ()
                               (local-set-key (kbd "C-c C-l") 'helm-comint-input-ring2)))
(setq  comint-input-ring-size 20000)
(provide 'init-shell)
