(require-package 'key-chord)
(key-chord-mode 1)

(require-package 'iy-go-to-char)
(key-chord-define-global "fg" 'iy-go-to-char)
(key-chord-define-global "df" 'iy-go-to-char-backward)
(provide 'init-keychord)
