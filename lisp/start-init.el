(setq enable-local-variables :safe)
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(set-frame-font "DejaVu Sans Mono-14" nil t)
(require 'init-elpa)
(require 'better-defaults)
(require 'init-history)
(require 'init-utils)
                                        ;(require 'init-auto-complete)
(require 'init-company)
(require 'init-editing-utils)
(require 'init-hippie)
(require 'init-grep)
(require 'init-org)
(require 'init-cpp)
(require 'init-makefile)
(require 'init-windows)
(require 'init-tramp)
(require 'init-email)
;(require 'init-docview)
(require 'init-desktop)
;(require 'init-term)
(require 'init-keychord)
(require 'init-theme)
(require 'init-macro)
(require 'init-fly)
(require 'init-easy)
(require 'init-engine)
;(cua-mode t)
;(require 'init-ido)
(require 'init-web)
(require 'init-python)
(require 'init-recentf)
(require 'init-auto-insert)
(require 'init-compile-run)
(use-package paredit
  :ensure t
  :config
  (progn
    (add-hook 'emacs-lisp-mode-hook
              (lambda ()
                (paredit-mode 1)
                (diminish 'paredit-mode)))))
                                        ;(cua-selection-mode t)                  ; for rectangles, CUA is nice
(require 'init-calendar)
(require 'init-shell)
(use-package multiple-cursors
  :ensure t
  :config
  (progn 
    (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
    (global-set-key (kbd "C->") 'mc/mark-next-like-this)
    (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
    (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)))

;(require 'init-php)

(use-package visual-regexp
  :ensure t
  :config 
  )
                                        ;(defvar vr-current-thing-at-point)
                                        ;(defun vr-thing-at-point-minibuffer-setup ()
                                        ;  (when (and vr-current-thing-at-point
                                        ;             (equal vr--in-minibuffer 'vr--minibuffer-regexp))
                                        ;    (insert vr-current-thing-at-point)))
                                        ;(add-hook 'minibuffer-setup-hook 'vr-thing-at-point-minibuffer-setup)

                                        ;(defun vr-thing-at-point ()
                                        ;  (interactive)
                                        ;  (let ((vr-current-thing-at-point (thing-at-point 'symbol t)))
                                        ;    (call-interactively 'vr/replace)))
(progn 
  (define-key global-map (kbd "C-c r") 'vr/replace)
                                        ; (define-key global-map (kbd "C-c q") 'vr-thing-at-point)
  (define-key global-map (kbd "C-c q") 'vr/query-replace)
  (define-key global-map (kbd "C-c m") 'vr/mc-mark))

(progn
  (defadvice switch-to-buffer (before save-buffer-now activate)
    (when buffer-file-name (save-buffer)))
  (defadvice other-window (before other-window-now activate)
    (when buffer-file-name (save-buffer)))
  (defadvice windmove-up (before other-window-now activate)
    (when buffer-file-name (save-buffer)))
  (defadvice windmove-down (before other-window-now activate)
    (when buffer-file-name (save-buffer)))
  (defadvice windmove-left (before other-window-now activate)
    (when buffer-file-name (save-buffer)))
  (defadvice windmove-right (before other-window-now activate)
    (when buffer-file-name (save-buffer)))
  (setq default-tab-width 4)
  (setq tab-width 4)
  (setq tab-stop-list ())
  (loop for x downfrom 40 to 1 do
        (setq tab-stop-list (cons (* x 4) tab-stop-list))))

;; (use-package session
;;   :ensure t
;;   :config
;;   (progn
;;     (require 'session)
;;     (setq session-save-print-spec '(t nil 40000))
;;     (add-hook 'after-init-hook 'session-initialize)))


(defun before-magit (&optional directory cache)
  (save-some-buffers t nil))
(use-package magit
  :ensure t
  :config
  (require 'magit)
  (advice-add 'magit-status :before #'before-magit)
  )

(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown$\\'" . markdown-mode))
(setq inhibit-startup-screen t)
(setq inhibit-startup-message t)
(setq user-full-name "yuandaxing"
      user-email-address "yuandaxing1@360.cn")
(setq-default initial-scratch-message 
              (concat ";; Happy hacking " (or user-login-name "")
                      " - enjoy!!!\n\n"))

(diminish 'keys-bind-minor-mode)
;(add-to-list 'load-path "~/Dropbox/emacs/settings")
;(require 'mysql)
;; (use-package xclip
;;   :ensure t
;;   :config
;;   (progn
;;     (require 'xclip)
;;     (xclip-mode 1)))
(require 'init-java)
(require '360-aicomplete)
(provide 'start-init)
;;;start-init.el ends here




