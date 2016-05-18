(require 'compile)
(add-hook 'c++-mode-hook
          (lambda ()
            (unless (or (file-exists-p "makefile")
                        (file-exists-p "Makefile"))
              (set (make-local-variable 'compile-command)
                   (let ((file (file-name-nondirectory buffer-file-name)))
                     (format "%s  -o %s.exe %s %s"
                             "g++"
                             (file-name-sans-extension file)
                             file
                             "-std=c++11 -fdiagnostics-color=auto -lpthread"
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
  (set-default 'c-basic-offset 2)
  (setq ff-search-directories '("../include/*" "../src" "." "../../src" "../../include/*"))
  (setq helm-zgrep-file-extension-regexp ".*\\(\\.h\\|\\.cpp\\|\\.cc\\|\\.hpp\\)$"))

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
    (setq helm-candidate-number-limit 100)
    (setq helm-input-idle-delay 0.01
          helm-yas-display-key-on-candidate t
          helm-quick-update t
          helm-M-x-requires-pattern nil
          helm-recentf-fuzzy-match t
          helm-buffers-fuzzy-matching t
          helm-recentf-fuzzy-match t
          helm-M-x-fuzzy-match t
          helm-semantic-fuzzy-match t
          helm-imenu-fuzzy-match t
          helm-apropos-fuzzy-match t
          helm-lisp-fuzzy-completion t
          helm-completion-in-region-fuzzy-match t
          helm-split-window-default-side 'below
          helm-split-window-in-side-p t
          helm-buffer-max-length nil)
    (setq helm-grep-ignored-files (append helm-grep-ignored-files (list "*.pyc" "*.exe" "GTAGS"  "GPATH" "GSYMS" "GRTAGS")))
    (setq helm-grep-ignored-directories (append helm-grep-ignored-directories (list ".git" "elpa")))
    (global-set-key (kbd "C-x b") 'helm-buffers-list)
    (global-set-key (kbd "C-x C-r") 'helm-recentf)
    (define-key isearch-mode-map (kbd "M-y") 'helm-show-kill-ring)
    ;; (setq helm-grep-default-command "ack-grep -Hn --color --smart-case --no-group %e %p %f"
    ;;       helm-grep-default-recurse-command "ack-grep -H --color --smart-case --no-group %e %p %f")
    (add-hook 'eshell-mode-hook
              #'(lambda ()
                  (define-key eshell-mode-map (kbd "TAB")     #'helm-esh-pcomplete)
                  (define-key eshell-mode-map (kbd "C-c C-l") #'helm-eshell-history))))

  :bind
  (("C-c h y" . helm-yas-complete)
   ("M-y" . helm-show-kill-ring)
   ))

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
          '("all_others" "ileaf" "ileafnew" "adselector" "all_others clean"
            "ileaf clean" "ileafnew clean" "ileafnew test" "adselector clean" "adselector test")))
     (list (helm :sources (helm-build-sync-source "test"
                          :candidates projects
                          :fuzzy-match t)
               :buffer "*helm test*"))))
  (async-shell-command
   (concatenate 'string "source /home/yuandx/rsa_keys/work_shortcut.sh ; commit_syn " project)))

(defvar key-path-alist
        '(
    ("trunk" . "/home/yuandx/code/trunk/common/")
    ("effective" . "~/Dropbox/code-snippet/C++/modern-effective-c++/")
    ("test" . "~/Dropbox/code-snippet/C++/test/")
    ("algorithm" . "~/Dropbox/code-snippet/emacs-search/algorithm")
    ("skillset" . "~/code/skillset/")
    ("snippet" . "~/Dropbox/code-snippet/emacs-search/")
    ))
(defun search-snippet (snippet)
  (interactive
   (let ((snippets
          '("trunk" "effective" "test" "algorithm" "skillset"
            "snippet")))
     (list (helm :sources (helm-build-sync-source "snippet"
                          :candidates snippets
                          :fuzzy-match t)
                 :buffer "*helm snippets*"))))
  (helm-do-grep-1 (list (cdr (assoc snippet key-path-alist))) t nil
                  '("*.org" "*.cpp" "*.cc" "*.h" "makefile" "Makefile"
                    "*.py" "*.hpp" "*.scratch" "*.el" ".c")))

(defun grep-snippet (snippet)
  (interactive
   (let ((snippets
          '("trunk" "effective" "test" "algorithm" "skillset"
            "snippet")))
     (list (helm :sources (helm-build-sync-source "snippet"
                          :candidates snippets
                          :fuzzy-match t)
                 :buffer "*helm snippets*"))))
  (find-grep (list (cdr (assoc snippet key-path-alist)))))


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
(require 'find-dired)
(setq find-ls-option '("-print0 | xargs -0 ls -ldh" . "-ldh"))
(provide 'init-cpp)
