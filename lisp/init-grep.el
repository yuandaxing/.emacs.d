(setq-default grep-highlight-matches t
              grep-scroll-output t)

(when *is-a-mac*
  (setq-default locate-command "mdfind"))

(when (executable-find "ag")
  (require-package 'ag)
  (require-package 'wgrep-ag)
  (setq-default ag-highlight-search t)
  (global-set-key (kbd "M-?") 'ag-project))

(add-hook 'after-init-hook
          (lambda ()
            (progn
              (add-to-list 'grep-files-aliases '("h" . "*.[ch] *.cpp *hpp"))
              (add-to-list 'grep-files-aliases '("cc" . "*.h *.cc *.cpp *.hpp *.cpp"))
              (add-to-list 'grep-files-aliases '("cpp" . "*.h *.cc *.cpp *.hpp *.cpp")))))

(provide 'init-grep)
