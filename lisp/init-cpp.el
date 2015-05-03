(require 'compile)

(add-hook 'c-mode-hook
          (lambda ()
            (unless (file-exists-p "Makefile")
              (set (make-local-variable 'compile-command)
                   ;; emulate make's .c.o implicit pattern rule, but with
                   ;; different defaults for the CC, CPPFLAGS, and CFLAGS
                   ;; variables:
                   ;; $(CC) -c -o $@ $(CPPFLAGS) $(CFLAGS) $<
                   (let ((file (file-name-nondirectory buffer-file-name)))
                     (format "%s -c -o %s.o %s %s %s"
                             (or (getenv "CC") "gcc")
                             (file-name-sans-extension file)
                             (or (getenv "CPPFLAGS") "-DDEBUG=9")
                             (or (getenv "CFLAGS") "-ansi -pedantic -Wall -g")
                             file))))))

(defun c++-mode-hook-setting()
  (c-set-offset 'substatement-open 0)
  (c-set-offset 'innamespace 0)
  (set-default 'c-basic-offset 2)
  (setq ff-search-directories '("../include/*" "../src" "." "../../src" "../../include/*"))
  )

(add-hook 'c++-mode-hook 'c++-mode-hook-setting)
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(require-package 'helm)
(require-package 'helm-gtags)

(setq
 helm-gtags-ignore-case t
 helm-gtags-auto-update t
 helm-gtags-use-input-at-cursor t
 helm-gtags-pulse-at-cursor t
 helm-gtags-prefix-key "\C-cg"
 helm-gtags-suggested-key-mapping t
 )
(require 'helm-gtags)
;; Enable helm-gtags-mode
(define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-dwim)
(define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)
(add-hook 'c++-mode-hook 'key-bind-hook )
(add-hook 'c++-mode-hook 'helm-gtags-mode)


(provide 'init-cpp)
