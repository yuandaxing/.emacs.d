(use-package yasnippet
  :ensure t
  :config
  (progn
    (require 'yasnippet)
    (yas-global-mode 1)
    (add-to-list 'yas-snippet-dirs
                 (expand-file-name "snippets" user-emacs-directory))
    (setq yas-wrap-around-region t)
    (diminish 'yas-minor-mode)))
(use-package company-c-headers
  :ensure t)
(use-package company
  :ensure t
  :config
  (progn
    (add-hook 'after-init-hook 'global-company-mode)
    (setq company-backends (delete 'company-semantic company-backends))
    (add-to-list 'company-backends 'company-c-headers)
    (define-key company-active-map (kbd "C-o") 'company-show-doc-buffer)
    (define-key company-active-map (kbd "C-l") 'company-show-location)
    (define-key company-active-map (kbd "C-w") nil)
    (define-key company-active-map (kbd "C-h") nil)
    (add-to-list 'company-c-headers-path-system "/usr/include/c++/4.9/")
    (require 'cc-mode)
    (define-key c-mode-map  [(tab)] 'company-complete)
    (define-key c++-mode-map  [(tab)] 'company-complete)))

;; (use-package auto-complete
;;   :ensure t
;;   :config
;;   (progn
;;     (require 'auto-complete-config)
;;     (global-auto-complete-mode t)
;;     (diminish 'auto-complete-mode)
;;     (setq-default ac-expand-on-auto-complete nil)
;;     (setq-default ac-dwim nil) ; To get pop-ups with docs even if a word is uniquely completed
;;     (setq tab-always-indent 'complete)  ;; use 't when auto-complete is disabled
;;     (add-to-list 'completion-styles 'initials t)
;;     (setq completion-cycle-threshold 5)
;;     (setq c-tab-always-indent nil
;;           c-insert-tab-function 'indent-for-tab-command)
;;     (set-default 'ac-sources
;;                  '(ac-source-imenu
;;                    ac-source-dictionary
;;                    ac-source-yasnippet
;;                    ac-source-words-in-buffer
;;                    ac-source-words-in-same-mode-buffers
;;                    ac-source-words-in-all-buffer))
;;     (dolist (mode '(magit-log-edit-mode
;;                     log-edit-mode org-mode text-mode haml-mode
;;                     git-commit-mode
;;                     sass-mode yaml-mode csv-mode espresso-mode haskell-mode
;;                     html-mode nxml-mode sh-mode smarty-mode clojure-mode
;;                     lisp-mode textile-mode markdown-mode tuareg-mode
;;                     js3-mode css-mode less-css-mode sql-mode
;;                     sql-interactive-mode
;;                     inferior-emacs-lisp-mode))
;;       (add-to-list 'ac-modes mode))
;;     ))

;; (defun hh-auto-complete-at-point ()
;;   (when (and (not (minibufferp))
;; 	     (fboundp 'auto-complete-mode)
;; 	     auto-complete-mode)
;;     (auto-complete)))

(defun hh-never-indent ()
  (set (make-local-variable 'indent-line-function) (lambda () 'noindent)))

;; (defun set-auto-complete-as-completion-at-point-function ()
;;   (setq completion-at-point-functions
;;         (cons 'hh-auto-complete-at-point
;;               (remove 'hh-auto-complete-at-point completion-at-point-functions))))

;; (add-hook 'auto-complete-mode-hook 'set-auto-complete-as-completion-at-point-function)

;; Exclude very large buffers from dabbrev
(defun hh-dabbrev-friend-buffer (other-buffer)
  (< (buffer-size other-buffer) (* 1 1024 1024)))

(setq dabbrev-friend-buffer-function 'hh-dabbrev-friend-buffer)

(setq abbrev-file-name  (expand-file-name "abbrev/defs.el" user-emacs-directory))
(setq-default abbrev-mode t)
(setq save-abbrevs t)
(quietly-read-abbrev-file)
(provide 'init-auto-complete)
