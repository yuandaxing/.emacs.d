(require-package 'tramp)

(require 'tramp)

(set-default 'tramp-default-method "scpx")

(defun s1()
  (interactive)
  (find-file "/yuandx@10.0.2.140:~/mv_code/trunk/"))

(provide 'init-tramp)
