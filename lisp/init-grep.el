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
            (add-to-list 'grep-files-aliases '("h" . "*.cpp *.h *.cc *.c *.hpp"))
            (add-to-list 'grep-files-aliases '("cc" . "*.cpp *.h *.cc *.c *.hpp" ))
            (add-to-list 'grep-files-aliases '("cpp" . "*.cpp *.h *.cc *.hpp"))
            ))


(provide 'init-grep)
