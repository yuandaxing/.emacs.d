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
(require-package 'yasnippet)
(require 'yasnippet)
(yas-global-mode 1)
(require 'init-windows)
(require 'init-tramp)
(require 'init-email)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(set-default 'tramp-shell-prompt-pattern "\\(?:^\\|\r\\)[^]#$%>\n]*#?[]#$%>].* *\\(^[\\[[0-9;]*[a-zA-Z] *\\)*")
;(add-to-list 'comint-output-filter-functions 'ansi-color-process-output)
;; (global-set-key (kbd "C-c ; p") 'yas-expand)
;; ;; default hotkey `C-c & C-s` is still valid
;; (global-set-key (kbd "C-c ; i") 'yas-insert-snippet)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(custom-enabled-themes (quote (tango-dark)))
 '(ido-enable-flex-matching t)
 '(ido-mode (quote both) nil (ido))
 '(line-number-mode t)
 '(menu-bar-mode nil)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ac-candidate-face ((t (:family "DejaVu Sans Mono")))))
;(windmove-default-keybindings)         ; shifted arrow keys
;; (add-to-list 'load-path "~/.emacs.d/elpa/ace-jump-mode-20140207.530/")
;; (add-to-list 'load-path "~/.emacs.d/user-plugin/")
;; (add-to-list 'load-path "~/.emacs.d/elpa/web-mode-20140711.1339/")
;; (autoload
;;   'ace-jump-mode
;;   "ace-jump-mode"
;;   "Emacs quick move minor mode"
;;   t)
;; (define-key global-map (kbd "C-c C-SPC") 'ace-jump-mode)
;; ;could not work with auto complete
;; ;(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode)) ; 
;; (autoload
;;   'ace-jump-mode-pop-mark
;;   "ace-jump-mode"
;;   "Ace jump back:-)"
;;   t)
;; (eval-after-load "ace-jump-mode"
;;   '(ace-jump-mode-enable-mark-sync))
;; (define-key global-map (kbd "C-x SPC") 'ace-jump-mode-pop-mark)
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
;(add-hook 'python-mode-hook 'jedi:setup)
;(setq jedi:complete-on-dot t)

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
(require-package 'magit)
(require 'magit)

;set uniquify to make buffer name uniq 
;(require-package 'uniquify)
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(set-fontset-font "fontset-default"
				  'gb18030 '("Microsoft YaHei" .
							 "unicode-bmp"))
;这是微软雅黑字体，如果没有乱码，设置成功^-^
;(require 'flycheck)
(setq ac-disable-faces nil)
(put 'set-goal-column 'disabled nil)

(add-hook 'erc-mode-hook
		  '(lambda ()
;			 (erc :server "irc.freenode.net" :port 6667 :full-name "Daxing Yuan" :nick "dayua")
			 (setq erc-hide-list '("JOIN" "PART" "QUIT"))))
(autoload 'markdown-mode "markdown-mode"
    "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(provide 'start-init)
