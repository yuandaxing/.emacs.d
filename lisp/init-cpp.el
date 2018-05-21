(setq-default scroll-margin 1
              scroll-conservatively 100000
              scroll-up-aggressively 0.01
              scroll-down-aggressively 0.01)
(setq-default auto-window-vscroll nil)
(setq-default scroll-step 1)
(use-package helm
  :ensure t
  :diminish helm-mode
  :config
  (progn
    (require 'helm-config)
    (require 'helm)
    (helm-mode)
    (eval-after-load "helm-grep"
      '(progn
         (setq helm-grep-ignored-files (append helm-grep-ignored-files (list "*.pyc" "*.exe" "GTAGS"  "GPATH" "GSYMS" "GRTAGS")))
         (setq helm-grep-ignored-directories (append helm-grep-ignored-directories
                                                     (list ".git" "elpa" "data")))))
    (setq helm-candidate-number-limit 100)
    (setq helm-input-idle-delay 0.01
          helm-M-x-requires-pattern nil
          helm-recentf-fuzzy-match t
          helm-buffers-fuzzy-matching t
          helm-M-x-fuzzy-match t
          helm-semantic-fuzzy-match t
          helm-imenu-fuzzy-match t
          helm-apropos-fuzzy-match t
          helm-lisp-fuzzy-completion t
          helm-completion-in-region-fuzzy-match t
          helm-split-window-default-side 'below
          helm-split-window-in-side-p t
          helm-buffer-max-length nil
          eshell-history-size  10000
          helm-eshell-hist-ignoredups t
          helm-eshell-hist-ignoredups t
          )
    (define-key isearch-mode-map (kbd "M-y") 'helm-show-kill-ring)
    (add-hook 'eshell-mode-hook
              #'(lambda ()
                  (define-key eshell-mode-map (kbd "TAB")     #'helm-esh-pcomplete)
                  (define-key eshell-mode-map (kbd "C-c C-l") #'helm-eshell-history)))
    (define-key helm-map (kbd "C-w") 'backward-kill-word)
    (define-key helm-grep-map (kbd "C-w") 'backward-kill-word)
    (define-key helm-generic-files-map (kbd "C-w") 'backward-kill-word))
  :bind
  (
   ("C-c h i" . hh-indent-buffer)
   ("C-c h u ". magit-status)
   ("C-x b" . helm-buffers-list)
   ("C-x C-f" . helm-find-files)
   ("C-x C-r" . helm-recentf)
   ("C-c h y" . helm-yas-complete)
   ("M-y" . helm-show-kill-ring)
   ("C-c h r" . helm-register)
   ("C-c h k" . helm-all-mark-rings)
   ))

(use-package helm-ag
  :ensure t
  :config
  (require 'helm-ag)
  (define-key helm-ag-edit-map (kbd "C-x C-s") 'helm-ag--edit-commit)
  (setq  helm-ag-insert-at-point 'symbol
         )
  )
;; (use-package helm-descbinds
;;   :ensure t)

(use-package helm-gtags
  :ensure t
  :config
  (progn
    (add-hook 'c++-mode-hook 'helm-gtags-mode)
    (define-key helm-gtags-mode-map (kbd "C-c g t") 'helm-gtags-find-tag)
    (define-key helm-gtags-mode-map (kbd "C-c g r") 'helm-gtags-find-rtag)
    (define-key helm-gtags-mode-map (kbd "C-c g s") 'helm-gtags-find-symbol)
    (define-key helm-gtags-mode-map (kbd "C-c g p") 'helm-gtags-parse-file)
    (define-key helm-gtags-mode-map (kbd "C-c g n") 'helm-gtags-next-history)
    (define-key helm-gtags-mode-map (kbd "C-c g p") 'helm-gtags-parse-file)
    (define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)
    (define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-dwim)
    (define-key helm-map (kbd "C-h") 'helm-ff-delete-char-backward)
    (define-key helm-find-files-map (kbd "C-h") 'helm-ff-delete-char-backward)
    (setq
     helm-gtags-ignore-case t
     helm-gtags-auto-update t
     helm-gtags-use-input-at-cursor t
     helm-gtags-pulse-at-cursor t
     helm-gtags-path-style 'relative
     )
    ))

(add-hook 'c++-mode-hook 'key-bind-hook )
(custom-set-faces
 '(ac-candidate-face ((t (:family "DejaVu Sans Mono"))))
 '(helm-selection ((t (:background "cyan" :foreground "black" :underline t))))
 '(helm-selection-line ((t (:background "cyan" :foreground "black" :underline t)))))
(eval-after-load 'helm-grep
  '(setq helm-grep-default-command helm-grep-default-recurse-command))

(use-package helm-swoop
  :ensure t
  :init
  (progn
    (require 'helm)
    (require 'helm-swoop))
  :config
  (progn
    (global-set-key (kbd "M-i") 'helm-swoop)
    (global-set-key (kbd "M-I") 'helm-swoop-back-to-last-point)
    (global-set-key (kbd "C-c M-i") 'helm-multi-swoop)
    (define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)
    (define-key helm-swoop-map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop)
    (define-key helm-swoop-map (kbd "C-r") 'helm-previous-line)
    (define-key helm-swoop-map (kbd "C-s") 'helm-next-line)
    (define-key helm-multi-swoop-map (kbd "C-r") 'helm-previous-line)
    (define-key helm-multi-swoop-map (kbd "C-s") 'helm-next-line)
    (define-key helm-swoop-map (kbd "C-w") 'backward-kill-word)
    (setq helm-multi-swoop-edit-save t)
    (setq helm-swoop-split-with-multiple-windows nil)
    (setq helm-swoop-split-direction 'split-window-vertically)
    (setq helm-swoop-speed-or-color nil)
    )
  )
(defgroup happy-hacking-customize nil
  " custmizing utility"
  :group 'happy-hacking)
;; (defcustom snippet-search-memorize-choice-enable t
;;   "enable memorize snippet search "
;;   :type 'boolean)
(defvar snippet-search-memorize-choice nil)
(require 'savehist)
(add-to-list 'savehist-additional-variables 'snippet-search-memorize-choice)
(add-to-list 'savehist-additional-variables 'snippet-search-memorize-choice-enable)
(defvar key-path-alist nil)
(setq key-path-alist
      '(("effective"  "~/Dropbox/code-snippet/C++/modern-effective-c++/")
        ("cpp"   "~/Dropbox/code-snippet/C++/")
        ("algorithm"   "~/Dropbox/code-snippet/C++/algorithm/" "~/Dropbox/code-snippet/better_base/algorithm/"
         "~/Dropbox/Algorithm")
        ("snippet"  "~/Dropbox/code-snippet/")
        ("python"  "~/Dropbox/code-snippet/python/")
        ("php"  "~/Dropbox/code-snippet/php/")
        ("shell"  "~/Dropbox/code-snippet/shell/")
        ("web"  "~/Dropbox/code-snippet/web/")
        ("dl"  "~/Dropbox/dl/")
        ("emacs"  "~/Dropbox/code-snippet/emacs/")))

(defun search-code-snippet (snippet)
  (interactive
   (let ((snippets
          (mapcar #'car key-path-alist)
          ))
     (list (helm :sources (helm-build-sync-source "snippet"
                            :candidates snippets
                            :fuzzy-match t)
                 :buffer "*helm snippets*"))))
  (if snippet
      (setq snippet-search-memorize-choice snippet))
  (let* ((directory-list (cdr (assoc snippet key-path-alist)))
         (helm-ff-default-directory (cadr (assoc snippet key-path-alist))))
    (helm-do-grep-1  directory-list t nil
                  '("*.org" "*.cpp" "*.cc" "*.h" "makefile" "Makefile" "*.py" "*.hpp" "*.scratch" "*.el" "*.c" "*.php" "*.html" "*.js" "*.sh"))))
(defun search-snippet (arg)
  (interactive "P")
  (if arg (call-interactively 'search-code-snippet)
    (search-code-snippet snippet-search-memorize-choice)))
(global-set-key (kbd "C-c h p") 'search-snippet)
(require 'recentf)
(defun hh-insert-date (prefix)
  "Insert the current date. With prefix-argument, use ISO format. With
   two prefix arguments, write out the day and month name."
  (interactive "P")
  (let ((format (cond
                 ((not prefix) "%d.%m.%Y")
                 ((equal prefix '(4)) "%Y-%m-%d")
                 ((equal prefix '(16)) "%A, %d. %B %Y")))
        (system-time-locale "zh_CN"))
    (insert (format-time-string format))))
(global-set-key (kbd "C-c h d") 'hh-insert-date)

(add-to-list 'helm-sources-using-default-as-input 'helm-source-findutils)
(defun helm-find-2 (dir)
  (let ((default-directory (file-name-as-directory dir)))
    (helm :sources helm-source-findutils
          :buffer "*helm find*"
          :ff-transformer-show-only-basename nil
          :default (thing-at-point 'filename)
          :case-fold-search helm-file-name-case-fold-search)))

(defun hh-golden-search (prefix)
  (interactive "p")
  (cond
   ((equal prefix 1) (progn
                       (let ((helm-findutils-search-full-path t))
                         (helm-find-2 "/home/dxyuan/code/ficus_write"))))
   (t (helm-find ""))))

(setq history-delete-duplicates t)
(global-set-key (kbd "C-x c /") 'hh-golden-search)

(require 'find-dired)
(setq find-ls-option '("-print0 | xargs -0 ls -ldh" . "-ldh"))
(setq browse-url-browser-function 'browse-url-chromium)

(defun my-minibuffer-insert-word-at-point ()
  "Get word at point in original buffer and insert it to minibuffer."
  (interactive)
  (let (word beg)
    (with-current-buffer (window-buffer (minibuffer-selected-window))
      (save-excursion
        (skip-syntax-backward "w_")
        (setq beg (point))
        (skip-syntax-forward "w_")
        (setq word (buffer-substring-no-properties beg (point)))))
    (when word
      (insert word))))

(defun my-minibuffer-setup-hook ()
  (local-set-key (kbd "C-.") 'my-minibuffer-insert-word-at-point))

(add-hook 'minibuffer-setup-hook 'my-minibuffer-setup-hook)
(provide 'init-cpp)
