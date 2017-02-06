(require 'shell)
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
            (format "~/.emacs.d/common-history/inferior-%s-history"
                    (process-name process)))
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
  "Run an inferior shell, with I/O through BUFFER (which defaults to `*shell*').
Interactively, a prefix arg means to prompt for BUFFER.
If `default-directory' is a remote file name, it is also prompted
to change if called with a prefix arg.

If BUFFER exists but shell process is not running, make new shell.
If BUFFER exists and shell process is running, just switch to BUFFER.
Program used comes from variable `explicit-shell-file-name',
 or (if that is nil) from the ESHELL environment variable,
 or (if that is nil) from `shell-file-name'.
If a file `~/.emacs_SHELLNAME' exists, or `~/.emacs.d/init_SHELLNAME.sh',
it is given as initial input (but this may be lost, due to a timing
error, if the shell discards input when it starts up).
The buffer is put in Shell mode, giving commands for sending input
and controlling the subjobs of the shell.  See `shell-mode'.
See also the variable `shell-prompt-pattern'.

To specify a coding system for converting non-ASCII characters
in the input and output to the shell, use \\[universal-coding-system-argument]
before \\[shell].  You can also specify this with \\[set-buffer-process-coding-system]
in the shell buffer, after you start the shell.
The default comes from `process-coding-system-alist' and
`default-process-coding-system'.

The shell file name (sans directories) is used to make a symbol name
such as `explicit-csh-args'.  If that symbol is a variable,
its value is used as a list of arguments when invoking the shell.
Otherwise, one argument `-i' is passed to the shell.

\(Type \\[describe-mode] in the shell buffer for a list of commands.)"
  (interactive
   (list
    (and current-prefix-arg
         (prog1
             (read-buffer "Shell buffer: "
                          ;; If the current buffer is an inactive
                          ;; shell buffer, use it as the default.
                          (if (and (eq major-mode 'shell-mode)
                                   (null (get-buffer-process (current-buffer))))
                              (buffer-name)
                            (get-buffer-create "*shell*")))
           (if (file-remote-p default-directory)
               ;; It must be possible to declare a local default-directory.
               ;; FIXME: This can't be right: it changes the default-directory
               ;; of the current-buffer rather than of the *shell* buffer.
               (setq default-directory
                     (expand-file-name
                      (read-directory-name
                       "Default directory: " default-directory default-directory
                       t nil))))))))
  (setq buffer (if (or buffer (not (derived-mode-p 'shell-mode))
                       (comint-check-proc (current-buffer)))
                   (get-buffer-create (or buffer "*shell*"))
                 ;; If the current buffer is a dead shell buffer, use it.
                 (current-buffer)))

  ;; On remote hosts, the local `shell-file-name' might be useless.
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

  ;; The buffer's window must be correctly set when we call comint (so
  ;; that comint sets the COLUMNS env var properly).
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
(defun execute-below-shell-return ()
  (interactive)
  (save-some-buffers t nil)
  (progn
    (dolist (w (window-list nil nil nil))
      (if (string= (buffer-name (window-buffer w))
                   "\*shell\*")
          (delete-window w)))
    (while (window-in-direction 'below)
      (delete-window (window-in-direction 'below)))
    (while (window-in-direction 'above)
      (delete-window (window-in-direction 'above)))
    (let ((buf-name (buffer-name)))
      (split-window nil nil 'above)
      (shell1 "*shell*")
      (goto-char (point-max))
      (helm-comint-input-ring)
      (comint-send-input)
      (switch-to-buffer-other-window buf-name))))
(global-set-key (kbd "C-c h e") 'execute-below-eshell-return)
(global-set-key (kbd "C-c h s") 'execute-below-shell-return)
(setq  comint-input-ring-size 2000)
(provide 'init-shell)
