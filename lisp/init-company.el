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
(defun check-expansion ()
  (save-excursion
    (if (looking-at "\\_>") t
      (backward-char 1)
      (if (looking-at "\\.") t
        (backward-char 1)
        (if (looking-at "->") t nil)))))
(defun do-yas-expand ()
  (let ((yas-expand 'return-nil))
    (yas-expand)))
;; (use-package company-irony
;;   :ensure t)
;; (use-package company-irony-c-headers
;;   :ensure t)
(use-package company
  :ensure t
  :config
  (progn
    (setq company-backends '())
    (add-to-list 'company-backends '(company-abbrev company-yasnippet company-files))
    (define-key company-active-map (kbd "C-o") 'company-show-doc-buffer)
    (define-key company-active-map (kbd "C-l") 'company-show-location)
    (define-key company-active-map (kbd "M-o") 'company-show-doc-buffer)
    (define-key company-active-map (kbd "M-l") 'company-show-location)
    (define-key company-active-map (kbd "M-m") 'company-complete-selection)
    (define-key company-active-map (kbd "C-w") nil)
    (define-key company-active-map (kbd "C-h") nil)
      (setq company-idle-delay              0.1
        company-minimum-prefix-length   2
        company-show-numbers            t
        company-tooltip-limit           20
        company-dabbrev-downcase        nil
      )
    (require 'cc-mode)
    (add-hook 'after-init-hook 'global-company-mode))
  (global-set-key (kbd  "C-:")  'company-complete))
(defun tab-indent-or-complete ()
  (interactive)
  (if (minibufferp)
      (minibuffer-complete)
    (if (or (not yas-minor-mode)
            (null (do-yas-expand)))
        (if (check-expansion)
            (company-complete-common)
          (indent-for-tab-command)))))
(global-set-key [tab] 'tab-indent-or-complete)
;; Exclude very large buffers from dabbrev
(defun hh-dabbrev-friend-buffer (other-buffer)
  (< (buffer-size other-buffer) (* 1 1024 1024)))
(setq dabbrev-friend-buffer-function 'hh-dabbrev-friend-buffer)
(setq abbrev-file-name  (expand-file-name "abbrev/defs.el" user-emacs-directory))
(setq-default abbrev-mode t)
(setq save-abbrevs t)
(abbrev-mode t)
(quietly-read-abbrev-file)

(provide 'init-company)
