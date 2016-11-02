(require-package 'unfill)
(when (fboundp 'electric-pair-mode)
  (electric-pair-mode))
                                        ;(global-set-key (kbd "<return>") 'electric-indent-just-newline)
(global-set-key (kbd "C-j") 'newline-and-indent)

;; Some basic preferences

(setq-default
 blink-cursor-interval 0.4
 bookmark-default-file (expand-file-name ".bookmarks.el" user-emacs-directory)
 buffers-menu-max-size 30
 case-fold-search t
 column-number-mode t
 delete-selection-mode t
 ediff-split-window-function 'split-window-horizontally
 ediff-window-setup-function 'ediff-setup-windows-plain
 indent-tabs-mode nil
 mouse-yank-at-point t
 save-interprogram-paste-before-kill t
 scroll-preserve-screen-position 'always
 set-mark-command-repeat-pop t
 tooltip-delay 1.5
 truncate-lines nil
 truncate-partial-width-windows nil
 visible-bell t)

(global-auto-revert-mode)
(setq global-auto-revert-non-file-buffers t
      auto-revert-verbose nil)
(transient-mark-mode t)


(defun hh-no-trailing-whitespace ()
  "Turn off display of trailing whitespace in this buffer."
  (setq show-trailing-whitespace nil))

;; But don't show trailing whitespace in SQLi, inf-ruby etc.
(dolist (hook '(special-mode-hook
                eww-mode-hook
                term-mode-hook
                comint-mode-hook
                compilation-mode-hook
                twittering-mode-hook
                minibuffer-setup-hook))
  (add-hook hook #'hh-no-trailing-whitespace))
(use-package whitespace-cleanup-mode
  :ensure t
  :config
  (progn
    (global-whitespace-cleanup-mode t)
    (diminish 'whitespace-cleanup-mode)))

(when (eval-when-compile (string< "24.3.1" emacs-version))
  ;; https://github.com/purcell/emacs.d/issues/138
  (after-load 'subword
    (diminish 'subword-mode)))



(when (fboundp 'global-prettify-symbols-mode)
  (global-prettify-symbols-mode))


(use-package undo-tree
  :ensure t
  :config
  (progn
    (global-undo-tree-mode)
    (diminish 'undo-tree-mode)))

(use-package highlight-symbol
  :ensure t
  :config
  (progn
    (dolist (hook '(prog-mode-hook html-mode-hook css-mode-hook))
      (add-hook hook 'highlight-symbol-mode)
      (add-hook hook 'highlight-symbol-nav-mode))
    (diminish 'highlight-symbol-mode)))


(autoload 'zap-up-to-char "misc" "Kill up to, but not including ARGth occurrence of CHAR.")
(global-set-key (kbd "M-Z") 'zap-up-to-char)
(global-set-key (kbd "C-t") 'transpose-chars)


(require-package 'browse-kill-ring)



;; Don't disable narrowing commands

(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)

;; Expand region

(use-package expand-region
  :ensure t
  :config
  (global-set-key (kbd "C-=") 'er/expand-region)
  )

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)



;; Handy key bindings

(progn
  (global-set-key (kbd "C-.") 'set-mark-command)
  )


(use-package multiple-cursors
  :ensure t
  :config
  (progn
    (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
    (global-set-key (kbd "C->") 'mc/mark-next-like-this)
    (global-set-key (kbd "C-+") 'mc/mark-next-like-this)
    (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
    ;; From active region to multiple cursors:
    (global-set-key (kbd "C-c c r") 'set-rectangular-region-anchor)
    (global-set-key (kbd "C-c c c") 'mc/edit-lines)
    (global-set-key (kbd "C-c c e") 'mc/edit-ends-of-lines)
    (global-set-key (kbd "C-c c a") 'mc/edit-beginnings-of-lines)))


(global-unset-key [M-left])
(global-unset-key [M-right])

(defun kill-back-to-indentation ()
  "Kill from point back to the first non-whitespace character on the line."
  (interactive)
  (let ((prev-pos (point)))
    (back-to-indentation)
    (kill-region (point) prev-pos)))

(global-set-key (kbd "C-M-<backspace>") 'kill-back-to-indentation)

(require-package 'page-break-lines)
(global-page-break-lines-mode)
(diminish 'page-break-lines-mode)


;; Fill column indicator
(when (eval-when-compile (> emacs-major-version 23))
  (require-package 'fill-column-indicator)
  (defun hh-prog-mode-fci-settings ()
    (turn-on-fci-mode)
    (when show-trailing-whitespace
      (set (make-local-variable 'whitespace-style) '(face trailing))
      (whitespace-mode 1)))

  ;;(add-hook 'prog-mode-hook 'hh-prog-mode-fci-settings)

  (defun hh-fci-enabled-p ()
    (and (boundp 'fci-mode) fci-mode))

  (defvar hh-fci-mode-suppressed nil)
  (defadvice popup-create (before suppress-fci-mode activate)
    "Suspend fci-mode while popups are visible"
    (let ((fci-enabled (hh-fci-enabled-p)))
      (when fci-enabled
        (set (make-local-variable 'hh-fci-mode-suppressed) fci-enabled)
        (turn-off-fci-mode))))
  (defadvice popup-delete (after restore-fci-mode activate)
    "Restore fci-mode when all popups have closed"
    (when (and hh-fci-mode-suppressed
               (null popup-instances))
      (setq hh-fci-mode-suppressed nil)
      (turn-on-fci-mode)))

  ;; Regenerate fci-mode line images after switching themes
  (defadvice enable-theme (after recompute-fci-face activate)
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (hh-fci-enabled-p)
          (turn-on-fci-mode))))))



;; Shift lines up and down with M-up and M-down. When paredit is enabled,
;; it will use those keybindings. For this reason, you might prefer to
;; use M-S-up and M-S-down, which will work even in lisp modes.

(require-package 'move-dup)
(global-set-key [M-up] 'md/move-lines-up)
(global-set-key [M-down] 'md/move-lines-down)
(global-set-key [M-S-up] 'md/move-lines-up)
(global-set-key [M-S-down] 'md/move-lines-down)

(global-set-key (kbd "C-c p") 'md/duplicate-down)
(global-set-key (kbd "C-c P") 'md/duplicate-up)


;; Fix backward-up-list to understand quotes, see http://bit.ly/h7mdIL

(defun backward-up-sexp (arg)
  "Jump up to the start of the ARG'th enclosing sexp."
  (interactive "p")
  (let ((ppss (syntax-ppss)))
    (cond ((elt ppss 3)
           (goto-char (elt ppss 8))
           (backward-up-sexp (1- arg)))
          ((backward-up-list arg)))))

(global-set-key [remap backward-up-list] 'backward-up-sexp) ; C-M-u, C-M-up





(defun hh-open-line-with-reindent (n)
  "A version of `open-line' which reindents the start and end positions.
If there is a fill prefix and/or a `left-margin', insert them
on the new line if the line would have been blank.
With arg N, insert N newlines."
  (interactive "*p")
  (let* ((do-fill-prefix (and fill-prefix (bolp)))
         (do-left-margin (and (bolp) (> (current-left-margin) 0)))
         (loc (point-marker))
         ;; Don't expand an abbrev before point.
         (abbrev-mode nil))
    (delete-horizontal-space t)
    (newline n)
    (indent-according-to-mode)
    (when (eolp)
      (delete-horizontal-space t))
    (goto-char loc)
    (while (> n 0)
      (cond ((bolp)
             (if do-left-margin (indent-to (current-left-margin)))
             (if do-fill-prefix (insert-and-inherit fill-prefix))))
      (forward-line 1)
      (setq n (1- n)))
    (goto-char loc)
    (end-of-line)
    (indent-according-to-mode)))


(global-set-key (kbd "C-o") 'hh-open-line-with-reindent)


;; Random line sorting

(defun sort-lines-random (beg end)
  "Sort lines in region randomly."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (let ;; To make `end-of-line' and etc. to ignore fields.
          ((inhibit-field-text-motion t))
        (sort-subr nil 'forward-line 'end-of-line nil nil
                   (lambda (s1 s2) (eq (random 2) 0)))))))

(require-package 'highlight-escape-sequences)
(hes-mode)


(use-package which-key
  :ensure t
  :config
  (progn 
    (which-key-mode 1)
    (diminish 'which-key-mode)))
                                        ;bind some key according to effective emacs
(defun kill-region-or-backward-word ()
  "If the region is active and non-empty, call `kill-region'.
     Otherwise, call `backward-kill-word'."
  (interactive)
  (call-interactively
   (if (use-region-p) 'kill-region 'backward-kill-word)))
(global-set-key (kbd "C-w") 'kill-backward-word)
(setq frame-title-format "Emacs - %f")
(use-package smex
  :ensure t
  :config
  (progn
    (require 'smex)
    (global-set-key (kbd "M-x") 'smex)
    (global-set-key (kbd "M-X") 'smex-major-mode-commands)))
(use-package projectile
  :ensure t
  :config
  (progn 
    (require 'projectile)
    (setq projectile-indexing-method 'native)
    (projectile-global-mode)
    (setq projectile-enable-caching t)))

(defun hh-new-scrath (buf)
  "open a buffer, if it doesn't exist, open a new one"
  (interactive "sBuffer name: ")
  (switch-to-buffer
   (get-buffer-create (concat "*" buf "*"))))

(defvar keys-bind-minor-mode-map (make-keymap) "my-keys-minor-mode keymap.")
(define-key keys-bind-minor-mode-map (kbd "C-h") 'delete-backward-char)
(define-key keys-bind-minor-mode-map (kbd "C-w") 'backward-kill-word)
(define-key keys-bind-minor-mode-map (kbd "C-x C-k") 'kill-region)

(define-minor-mode keys-bind-minor-mode
  "A minor mode so that my key settings override annoying major modes."
  t " my-keys" 'keys-bind-minor-mode-map)

(defun key-bind-hook()
  (progn 
    (keys-bind-minor-mode 1)
    (subword-mode 1)
    ))
(dolist (hook '(magit-log-edit-mode-hook
                log-edit-mode-hook org-mode-hook text-mode-hook haml-mode-hook
                git-commit-mode-hook
                ido-minibuffer-setup-hook sass-mode-hook yaml-mode-hook csv-mode-hook espresso-mode-hook haskell-mode-hook
                html-mode-hook nxml-mode-hook sh-mode-hook smarty-mode-hook clojure-mode-hook
                lisp-mode-hook textile-mode-hook markdown-mode-hook tuareg-mode-hook
                js3-mode-hook css-mode-hook less-css-mode-hook sql-mode-hook
                sql-interactive-mode-hook c++-mode-hook org-mode-hook
                inferior-emacs-lisp-mode-hook
                helm-mode-hook ido-setup-hook minibuffer-setup-hook helm-minibuffer-set-up-hook))
  (add-hook hook 'key-bind-hook))
                                        ;(define-key minibuffer-local-map (kbd "C-w") 'backward-kill-word)


(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(defun smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.
Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.
If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

(global-set-key [remap move-beginning-of-line]
                'smarter-move-beginning-of-line)

(fset 'yes-or-no-p 'y-or-n-p)

(defun hh-smart-open-line-above ()
  "Insert an empty line above the current line.
Position the cursor at it's beginning, according to the current mode."
  (interactive)
  (move-beginning-of-line nil)
  (newline-and-indent)
  (forward-line -1)
  (indent-according-to-mode))

(defun hh-smart-open-line (arg)
  "Insert an empty line after the current line.
Position the cursor at its beginning, according to the current mode.

With a prefix ARG open line above the current line."
  (interactive "P")
  (if arg
      (hh-smart-open-line-above)
    (progn
      (move-end-of-line nil)
      (newline-and-indent))))
(global-set-key (kbd "S-<return>") 'hh-smart-open-line)
(defun hh-comint-clear-buffer ()
  (interactive)
  (let ((comint-buffer-maximum-size 0))
    (comint-truncate-buffer)))

(use-package smart-mode-line
  :ensure t
  :config
  (progn
    (setq sml/no-confirm-load-theme t)
    (setq-default
     mode-line-format
     '("%e"
       mode-line-front-space
       mode-line-mule-info
       mode-line-client
       mode-line-modified
       mode-line-remote
       mode-line-frame-identification
       mode-line-buffer-identification
       "   "
       mode-line-position
       (vc-mode vc-mode)
       "  "
       mode-line-modes
       mode-line-misc-info
       mode-line-end-spaces))
    (smart-mode-line-enable)))

(defun hh-copy-file-name-to-clipboard (arg)
  "Copy the current buffer file name to the clipboard."
  (interactive "P")
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (let ((text (if arg
                    filename
                  (file-name-nondirectory filename)
                  )))
      (when text
        (kill-new text)
        (message "Copied buffer file name '%s' to the clipboard." text)))))

(defun hh-indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))
(defun hh-insert-date ()
  "Insert a time-stamp according to locale's date and time format."
  (interactive)
  (insert (format-time-string "%c" (current-time))))
(defun hh-untabify-buffer ()
  (interactive)
  (untabify (point-min) (point-max)))

(defun hh-eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))
(global-set-key (kbd "C-c e") 'hh-eval-and-replace)

;; (defun xterm-title-update ()
;;   (interactive)
;;   (send-string-to-terminal (concat "\033]1; " (buffer-name) "\007"))
;;   (if buffer-file-name
;;       (send-string-to-terminal (concat "\033]2; " (buffer-file-name) "\007"))
;;     (send-string-to-terminal (concat "\033]2; " (buffer-name) "\007"))))
;; (add-hook 'post-command-hook 'xterm-title-update)


;; (defvar shell-minor-mode-map (make-keymap) "shell-minor-mode keymap.")
;; (define-key keys-bind-minor-mode-map (kbd "C-c C-l") 'helm-comint-input-ring)

;; (define-minor-mode shell-minor-mode
;;   "A minor mode so that my key settings override annoying major modes."
;;   t "minor shell mode" 'keys-bind-minor-mode-map)

(add-hook 'shell-mode-hook #'(lambda ()
                               (local-set-key (kbd "C-c C-l") 'helm-comint-input-ring)))

(provide 'init-editing-utils)
