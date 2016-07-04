;;; package --- Summary

;;; Commentary
;;; Code
;;;
(require 'yasnippet)
;;; Code
(defun my/autoinsert-yas-expand (str)
  "insert str into the buffer then expand "
  (progn
    (goto-char (point-min))
    (insert str)
    (yas-expand)))

(use-package autoinsert
  :ensure t
  :config
  (auto-insert-mode 1)

  (setq
   auto-insert 'other
   auto-insert-alist
   '((("\\.\\([Hh]\\|hh\\|hpp\\)\\'" . "C / C++ header") . [
                                                            (lambda () (my/autoinsert-yas-expand "head"))])
     (("\\.\\([C]\\|cc\\|cpp\\)\\'" . "C++ source") . [
                                                       (lambda () (my/autoinsert-yas-expand "aut"))]))
   auto-insert-query nil)
  )
(provide 'init-auto-insert)
