(defvar current-buffer nil)
(defvar current-directory nil)
(defvar current-filename nil)
(defvar current-sans-filename nil)
(defvar current-user-host nil)


(defvar  name-variable-map
  '(
    ("##curdir##" . current-directory)
    ("##curfilename##" .   current-filename)
    ("##user@host##". current-user-host)
    ("##curbuffer##". current-buffer)
    ("##cursansname##". current-sans-filename)
    ))
(require 'comint)
(defun comint-simple-send-around (simle-function proc string)
  (dolist (item name-variable-map)
    (let ((name (car item))
          (value (symbol-value (cdr item))))
      (setq string (replace-regexp-in-string name value string))
      ))
  (message '%s' string)
  (funcall simle-function proc string)
  )
(require 'tramp)
(defun hh-setting-env ()
  (if (buffer-file-name)
      (let* ((local-filename (buffer-file-name)))
        (if (file-remote-p (buffer-file-name))
            (progn
              (setq current-user-host (format "%s@%s" tramp-current-user tramp-current-host)
                    )
              (setq local-filename (replace-regexp-in-string  (file-remote-p (buffer-file-name)) "" (buffer-file-name)))))
        (setq
         current-filename (file-name-nondirectory local-filename)
         current-directory (file-name-directory local-filename)
         current-sans-filename (file-name-sans-extension current-filename)
         )))
  (setq current-buffer (buffer-name))
  )

(advice-add #'comint-simple-send :around #'comint-simple-send-around)
(provide 'extend-shell)
