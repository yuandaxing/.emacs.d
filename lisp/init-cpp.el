(require 'compile)
(add-hook 'c++-mode-hook
          (lambda ()
            (unless (file-exists-p "Makefile")
              (set (make-local-variable 'compile-command)
                   (let ((file (file-name-nondirectory buffer-file-name)))
                     (format "%s  -o %s.exe %s %s"
                             "g++"
                             (file-name-sans-extension file)
                             file
                             "-std=c++11 -fdiagnostics-color=auto"
                             ))))))

(defun c++-mode-hook-setting()
  (c-set-offset 'substatement-open 0)
  (c-set-offset 'innamespace 0)
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
  :config t
  :diminish helm-mode
  :config
  (
   (require 'helm-config)
   (setq helm-candidate-number-limit 100)
   (setq helm-input-idle-delay 0.01
         helm-yas-display-key-on-candidate t
         helm-quick-update t
         helm-M-x-requires-pattern nil
         helm-ff-skip-boring-files t)
   (helm-mode))
  :bind
  (("C-c h m" . helm-mini)
   ("C-c h b" . helm-buffers-list)
   ("C-c h s" . helm-swoop)
   ("C-c h y" . helm-yas-complete)
   ("M-y" . helm-show-kill-ring)
   ))
(use-package helm-descbinds
  :defer t
  :bind (("C-c h d" . helm-descbinds)))
(use-package helm-gtags
  :ensure t
  :config
  (progn
    (add-hook 'c++-mode-hook 'helm-gtags-mode)
    (define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-dwim)
    (define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)
    (define-key helm-map (kbd "C-h") 'helm-ff-delete-char-backward)
    (define-key helm-find-files-map (kbd "C-h") 'helm-ff-delete-char-backward)
    (setq
     helm-gtags-ignore-case t
     helm-gtags-auto-update t
     helm-gtags-use-input-at-cursor t
     helm-gtags-pulse-at-cursor t
     helm-gtags-prefix-key "\C-cg"
     helm-gtags-suggested-key-mapping t
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
    (setq helm-swoop-use-line-number-face t)
    (global-set-key (kbd "C-x M-i") 'helm-multi-swoop-all)))
(provide 'init-cpp)
