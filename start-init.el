(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))

;(require 'init-elpa)
;(package-require 'yasnippet)
(require 'yasnippet)
(yas-global-mode 1)
(global-set-key (kbd "C-c ; p") 'yas-expand)
;; default hotkey `C-c & C-s` is still valid
(global-set-key (kbd "C-c ; i") 'yas-insert-snippet)

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
(add-to-list 'load-path "~/.emacs.d/elpa/ace-jump-mode-20140207.530/")
(add-to-list 'load-path "~/.emacs.d/user-plugin/")
(add-to-list 'load-path "~/.emacs.d/elpa/web-mode-20140711.1339/")
(autoload
  'ace-jump-mode
  "ace-jump-mode"
  "Emacs quick move minor mode"
  t)
(define-key global-map (kbd "C-c C-SPC") 'ace-jump-mode)
;could not work with auto complete
;(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode)) ; 
(autoload
  'ace-jump-mode-pop-mark
  "ace-jump-mode"
  "Ace jump back:-)"
  t)
(eval-after-load "ace-jump-mode"
  '(ace-jump-mode-enable-mark-sync))
(define-key global-map (kbd "C-x SPC") 'ace-jump-mode-pop-mark)
(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(add-to-list 'load-path "/home/dayua/.emacs.d/elpa/visual-regexp-20140311.724/") ;; if the files are not already in the load path
(require 'visual-regexp)
(define-key global-map (kbd "C-c r") 'vr/replace)
(define-key global-map (kbd "C-c q") 'vr/query-replace)
;; if you use multiple-cursors, this is for you:
(define-key global-map (kbd "C-c m") 'vr/mc-mark)
;(add-hook 'python-mode-hook 'jedi:setup)
;(setq jedi:complete-on-dot t)

(global-set-key (kbd "<f5>") 'redraw-display)

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
;(linum-mode t)
;(require 'tabkey2)
;(tabkey2-mode t)
;(require 'smart-tab)
;(setq smart-tab-mode t)
(global-linum-mode t)
;(global-smart-tab-mode t)
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
(require 'session)
(add-hook 'after-init-hook 'session-initialize)
(global-unset-key (kbd "C-z"))
(global-set-key (kbd "C-z") 'undo)
; repeat last command, C-x Esc Esc
;ace jump multiple then jump back C-x space
;C-S <BackSpace> kill-whole-line
;C-<UP>|<DOWN>  move by paragraph
;setting for html

(setq ido-separator "\n")
(set-face-attribute 'default nil :height 120)
(add-to-list 'load-path "~/.emacs.d/elpa/magit-20140720.358")
(eval-after-load 'info
  '(progn (info-initialize)
          (add-to-list 'Info-directory-list  "~/.emacs.d/elpa/magit-20140720.358")))
(require 'magit)

;set uniquify to make buffer name uniq 
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
;(setenv "GIT_ASKPASS" "git-gui--askpass")
;; (autoload 'auto-make-header "header2")
;; (add-hook 'python-mode-hook 'auto-make-header)
;; (add-hook 'c-mode-common-hook 'auto-make-header)

(set-fontset-font "fontset-default"
				  'gb18030 '("Microsoft YaHei" .
							 "unicode-bmp"))
;这是微软雅黑字体，如果没有乱码，设置成功^-^
;(require 'flycheck)
(setq ac-disable-faces nil)
(put 'set-goal-column 'disabled nil)
(require 'smex)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(require 'projectile)
(setq projectile-indexing-method 'native)
(projectile-global-mode)
(setq projectile-enable-caching t)
(global-set-key [f5] 'projectile-find-file)
(add-hook 'erc-mode-hook
		  '(lambda ()
;			 (erc :server "irc.freenode.net" :port 6667 :full-name "Daxing Yuan" :nick "dayua")
			 (setq erc-hide-list '("JOIN" "PART" "QUIT"))))
(autoload 'markdown-mode "markdown-mode"
    "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(defconst *is-a-mac* (eq system-type 'darwin))
(add-to-list 'load-path "~/.emacs.d/lisp")
(provide 'start-init)
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
