(defconst *is-a-mac* (eq system-type 'darwin))
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
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
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(require 'init-term)
(require 'init-keychord)
(require 'init-theme)
(require 'init-macro)
(require 'init-fly)
(require 'init-easy)
(require 'init-history)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(ido-enable-flex-matching t)
 '(ido-mode (quote both) nil (ido))
 '(line-number-mode t)
 '(menu-bar-mode nil)
 '(tool-bar-mode nil))
(custom-set-faces
 '(ac-candidate-face ((t (:family "DejaVu Sans Mono")))))

(require-package 'multiple-cursors)
(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(require-package 'visual-regexp)
(require 'visual-regexp)
(define-key global-map (kbd "C-c r") 'vr/replace)
(define-key global-map (kbd "C-c q") 'vr/query-replace)
;; if you use multiple-cursors, this is for you:
(define-key global-map (kbd "C-c m") 'vr/mc-mark)
;(global-set-key (kbd "<f5>") 'redraw-display)

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
(show-paren-mode t)
(global-linum-mode t)
(setq indent-tabs-mode nil)
(setq default-tab-width 4)
(setq tab-width 4)
(setq tab-stop-list ())
(loop for x downfrom 40 to 1 do
      (setq tab-stop-list (cons (* x 4) tab-stop-list)))
(defun xah-cut-line-or-region ()
  "Cut the current line, or current text selection."
  (interactive)
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (kill-region (line-beginning-position) (line-beginning-position 2)) ) )

(defun xah-copy-line-or-region ()
  "Copy current line, or current text selection."
  (interactive)
  (if (region-active-p)
      (kill-ring-save (region-beginning) (region-end))
    (kill-ring-save (line-beginning-position) (line-beginning-position 2)) ) )
(global-set-key (kbd "<f2>") 'xah-cut-line-or-region) ; cut
(global-set-key (kbd "<f3>") 'xah-copy-line-or-region) ; copy
(global-set-key (kbd "<f4>") 'yank) ; paste
(require-package 'session)
(require 'session)
(add-hook 'after-init-hook 'session-initialize)
(global-unset-key (kbd "C-z"))
(global-set-key (kbd "C-z") 'undo)


(setq ido-separator "\n")
(set-face-attribute 'default nil :height 120)
;(require-package 'magit)
;(add-to-list 'load-path (expand-file-name "elpa/magit-20150116.517" user-emacs-directory))
;(require 'magit)

;set uniquify to make buffer name uniq 
;(require-package 'uniquify)
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; (set-fontset-font "fontset-default"
;; 				  'gb18030 '("Microsoft YaHei" .
;; 							 "unicode-bmp"))
(setq ac-disable-faces nil)
(put 'set-goal-column 'disabled nil)
(add-hook 'erc-mode-hook
		  '(lambda ()
			 (erc :server "irc.freenode.net" :port 6667 :full-name "Daxing Yuan" :nick "dayua")
			 (setq erc-hide-list '("JOIN" "PART" "QUIT"))))
(autoload 'markdown-mode "markdown-mode"
    "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(setq inhibit-startup-screen t)
(setq inhibit-startup-message t)
(setq user-full-name "yuandaxing"
      user-email-address "yuandx@mvad.com")
(setq-default initial-scratch-message 
              (concat ";; Happy hacking " (or user-login-name "") " - Emacs ♥ you!\n\n"))
(require-package 'diminish)
(require 'diminish)
(diminish 'keys-bind-minor-mode "M")

(provide 'start-init)
;;;start-init.el ends here

