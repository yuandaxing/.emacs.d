(require 'compile)
(add-hook 'c++-mode-hook
          (lambda ()
            (unless (or (file-exists-p "makefile")
                        (file-exists-p "Makefile"))
              (set (make-local-variable 'compile-command)
                   (let ((file (file-name-nondirectory buffer-file-name)))
                     (format "%s -I%s -L%s -std=c++11 -o %s.exe %s  %s"
                             "g++"
                             (substitute-in-file-name "$HOME/Dropbox/3rdparty/cpp/include")
                             (substitute-in-file-name "$HOME/Dropbox/3rdparty/cpp/lib")
                             (file-name-sans-extension file)
                             file
                             "-lpthread -lgflags"
                             ))))))

(add-hook 'c-mode-hook
          (lambda ()
            (unless (or (file-exists-p "makefile")
                        (file-exists-p "Makefile"))
              (set (make-local-variable 'compile-command)
                   (let ((file (file-name-nondirectory buffer-file-name)))
                     (format "%s  -o %s.exe %s %s"
                             "gcc"
                             (file-name-sans-extension file)
                             file
                             "-fdiagnostics-color=auto -lpthread"
                             ))))))

(defun c++-mode-hook-setting()
  (c-set-offset 'substatement-open 0)
  (c-set-offset 'innamespace 0)
  (c-set-offset 'case-label '+)
  (setq c-basic-offset 4)
  (setq ff-search-directories '("../include/*" "../src" "." "../../src" "../../include/*"))
  (setq helm-zgrep-file-extension-regexp ".*\\(\\.h\\|\\.cpp\\|\\.cc\\|\\.hpp\\)$"))

(defun execute-below-eshell-return ()
  (interactive)
  (save-some-buffers t nil)
  (progn
    (dolist (w (window-list nil nil nil))
      (if (string= (buffer-name (window-buffer w))
                   "*eshell*")
          (delete-window w)))
    (while (window-in-direction 'below)
      (delete-window (window-in-direction 'below)))
    (while (window-in-direction 'above)
      (delete-window (window-in-direction 'above)))
    (let ((buf-name (buffer-name)))
      (split-window nil nil 'above)
      (eshell)
      (goto-char (point-max))
      (helm-eshell-history)
      (eshell-send-input)
      (switch-to-buffer-other-window buf-name))))
(global-set-key (kbd "C-c h e") 'execute-below-eshell-return)


(add-hook 'c++-mode-hook 'c++-mode-hook-setting)
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
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
          helm-move-to-line-cycle-in-source t
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
   ("C-x b" . helm-buffers-list)
   ("C-x C-f" . helm-find-files)
   ("C-x C-r" . helm-recentf)
   ("C-c h y" . helm-yas-complete)
   ("M-y" . helm-show-kill-ring)
   ("C-c h r" . helm-register)
   ("C-c h k" . helm-all-mark-rings)
   ))

(defun helm-kill-ring-transformer (candidates _source)
  "Display only the `helm-kill-ring-max-lines-number' lines of candidate."
  (cl-loop for i in candidates
           do (set-text-properties 0 (length i) '(read-only nil) i)
           for nlines = (with-temp-buffer (insert i) (count-lines (point-min) (point-max)))
           if (and helm-kill-ring-max-lines-number
                   (> nlines helm-kill-ring-max-lines-number))
           collect (cons
                    (with-temp-buffer
                      (insert i)
                      (goto-char (point-min))
                      (concat
                       (buffer-substring
                        (point-min)
                        (save-excursion
                          (forward-line helm-kill-ring-max-lines-number)
                          (point)))
                       "[...]")) i)
           else collect i))
(use-package helm-ag
  :ensure t
  :config
  (require 'helm-ag)
  (define-key helm-ag-edit-map (kbd "C-x C-s") 'helm-ag--edit-commit)
  (setq  helm-ag-insert-at-point 'symbol
         )
  )
(use-package helm-descbinds
  :defer t)
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
    (define-key helm-gtags-mode-map (kbd "C-c g u") 'helm-gtags-update-tags)
    (define-key helm-gtags-mode-map (kbd "C-c g c") 'helm-gtags-create-tags)
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
 '(helm-selection ((t (:background "tan" :underline (:color "dark orange" :style wave)))))
 '(helm-selection-line ((t (:background "cornsilk" :foreground "dark orange" :underline nil)))))
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
    (setq helm-swoop-move-to-line-cycle t)
    (setq helm-swoop-use-line-number-face t))
  :bind
  (("C-c h s" . helm-multi-swoop-all)))

(defun async-make (project)
  (interactive
   (let ((projects
          '("facesaas" "ficus" "common" "reset compile")))
     (list (helm :sources (helm-build-sync-source "test"
                            :candidates projects
                            :fuzzy-match t)
                 :buffer "*helm test*"))))
  (progn
    (save-some-buffers t nil)
    (async-shell-command
     (format "source %s ; build %s" (substitute-in-file-name "$HOME/Dropbox/secret/work_shortcut.sh") project))))

(defvar key-path-alist
  '(("ficus-common" . "~/code/ficus/ficus/common/")
    ("effective" . "~/Dropbox/code-snippet/C++/modern-effective-c++/")
    ("test" . "~/Dropbox/code-snippet/C++/test/")
    ("algorithm" . "~/Dropbox/code-snippet/emacs-search/algorithm")
    ("skillset" . "~/code/skillset/")
    ("snippet" . "~/Dropbox/code-snippet/")
    ))

(defun search-code-snippet (snippet)
  (interactive
   (let ((snippets
          '("trunk" "effective" "test" "algorithm" "skillset"
            "snippet" "ambition")))
     (list (helm :sources (helm-build-sync-source "snippet"
                            :candidates snippets
                            :fuzzy-match t)
                 :buffer "*helm snippets*"))))
  (helm-do-grep-1 (list (cdr (assoc snippet key-path-alist))) t nil
                  '("*.org" "*.cpp" "*.cc" "*.h" "makefile" "Makefile" "*.py" "*.hpp" "*.scratch" "*.el" ".c")))

(defun search-snippet (arg)
  (interactive "P")
  (if arg (call-interactively 'search-code-snippet)
    (search-code-snippet "skillset")))
(global-set-key (kbd "C-c h p") 'search-snippet)
(global-set-key (kbd "C-c h m") 'async-make)

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

;(add-to-list 'helm-sources-using-default-as-input 'helm-source-findutils)
(defun helm-find-1 (dir)
  (let ((default-directory (file-name-as-directory dir)))
    (helm :sources 'helm-source-findutils
          :buffer "*helm find*"
          :ff-transformer-show-only-basename nil
          :default (thing-at-point 'filename)
          :case-fold-search helm-file-name-case-fold-search)))
(defun hh-golden-search (prefix)
  (interactive "p")
  (cond
   ((equal prefix 1) (progn
                       (let ((helm-findutils-search-full-path t))
                         (helm-find-1 "/home/dxyuan/code/ficus_write/ficus"))))
   (t (helm-find ""))))

(setq history-delete-duplicates t)
(global-set-key (kbd "C-x c /") 'hh-golden-search)
(setq x-select-enable-clipboard  nil
      x-select-enable-primary  nil)
(require 'find-dired)
(setq find-ls-option '("-print0 | xargs -0 ls -ldh" . "-ldh"))
(provide 'init-cpp)
