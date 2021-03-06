(setq-default grep-highlight-matches t
              grep-scroll-output t)

;; (when (executable-find "ag")
;;   (require 'ag)
;;   (require 'wgrep-ag)
;;   (setq-default ag-highlight-search t)
;;   (global-set-key (kbd "M-?") 'ag-project))

(add-hook 'after-init-hook
          (lambda ()
            (progn
              (add-to-list 'grep-files-aliases '("h" . "*.[ch] *.cpp *hpp"))
              (add-to-list 'grep-files-aliases '("cc" . "*.h *.cc *.cpp *.hpp *.cpp"))
              (add-to-list 'grep-files-aliases '("cpp" . "*.h *.cc *.cpp *.hpp *.cpp")))))
(provide 'init-grep)
