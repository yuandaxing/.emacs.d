(defvar current-buffer nil)
(defvar current-directory nil)
(defvar current-filename nil)

(defvar  name-variable-map
  '(
    ("##curdir##" . current-directory)
    ("##curfilename##" .   current-filename)
    ))

(defun hh-extend-parse-command (&optional NO-NEWLINE ARTIFICIAL)
  (message (comint-get-old-input))
  (let ((str (comint-get-old-input)))
    (dolist (item name-variable-map)
      (let ((name (car item))
            (value (symbol-value (cdr item))))
        (setq str (replace-regexp-in-string name value str))
        )))
  )

(defun hh-setting-env ()
  (setq
   current-filename (file-name-nondirectory (buffer-file-name))
   current-directory (file-name-directory (buffer-file-name))
   current-buffer (buffer-name)))

(advice-add 'comint-send-input :before #'hh-extend-parse-command)
(provide 'extend-shell)
