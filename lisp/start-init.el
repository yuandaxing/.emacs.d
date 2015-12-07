;;;happy hackiing emacs
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'better-defaults)
(require 'init-utils)
(require 'init-elpa)
(require 'init-auto-complete)
(require-package 'diminish)
(require 'init-editing-utils)
(require 'init-hippie)
(require 'init-grep)
(require 'init-org)
(require 'init-cpp)
(require 'init-makefile)
(require 'init-windows)
(require 'init-tramp)
(require 'init-email)
(require 'init-docview)
(require 'init-desktop)
(require 'init-term)
(require 'init-keychord)
(require 'init-theme)
(require 'init-macro)
(require 'init-fly)
(require 'init-easy)
(require 'init-history)
(require 'init-engine)
(require 'init-ido)
(require 'init-web)
(require 'init-python)
(require 'init-recentf)
(use-package paredit
  :ensure t
  :config
  (progn
    (add-hook 'emacs-lisp-mode-hook
              (lambda ()
                (paredit-mode 1)
                (diminish 'paredit-mode)))))
(cua-selection-mode t)                  ; for rectangles, CUA is nice
(use-package multiple-cursors
  :ensure t
  :config
  (progn 
    (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
    (global-set-key (kbd "C->") 'mc/mark-next-like-this)
    (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
    (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)))

(use-package visual-regexp
  :ensure t
  :config
  (progn 
    (define-key global-map (kbd "C-c r") 'vr/replace)
    (define-key global-map (kbd "C-c q") 'vr/query-replace)
    (define-key global-map (kbd "C-c m") 'vr/mc-mark)))

(progn
  ;; automatically save buffers associated with files on buffer switch
  ;; and on windows switch
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

(use-package session
  :ensure t
  :config
  (add-hook 'after-init-hook 'session-initialize))

(use-package magit
  :ensure t
  :config
  (require 'magit)
  )

;; (add-hook 'erc-mode-hook
;; 		  '(lambda ()
;; 			 (erc :server "irc.freenode.net" :port 6665 :full-name "Daxing Yuan" :nick "dayua" :password "e215758")
;; 			 (setq erc-hide-list '("JOIN" "PART" "QUIT"))))

(autoload 'markdown-mode "markdown-mode"
    "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(setq inhibit-startup-screen t)
(setq inhibit-startup-message t)
(setq user-full-name "yuandaxing"
      user-email-address "yuandx@mvad.com")
(setq-default initial-scratch-message 
              (concat ";; Happy hacking " (or user-login-name "")
                      " - enjoy!!!\n\n"))

(diminish 'keys-bind-minor-mode)
(provide 'start-init)
;;;start-init.el ends here




