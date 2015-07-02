(require 'compile)
(add-hook 'c-mode-hook
          (lambda ()
            (unless (file-exists-p "Makefile")
              (set (make-local-variable 'compile-command)
                   ;; emulate make's .c.o implicit pattern rule, but with
                   ;; different defaults for the CC, CPPFLAGS, and CFLAGS
                   ;; variables:
                   ;; $(CC) -c -o $@ $(CPPFLAGS) $(CFLAGS) $<
                   (let ((file (file-name-nondirectory buffer-file-name)))
                     (format "%s -c -o %s.o %s %s %s"
                             (or (getenv "CC") "gcc")
                             (file-name-sans-extension file)
                             (or (getenv "CPPFLAGS") "-DDEBUG=9")
                             (or (getenv "CFLAGS") "-ansi -pedantic -Wall -g")
                             file))))))
(defun c++-mode-hook-setting()
  (c-set-offset 'substatement-open 0)
  (c-set-offset 'innamespace 0)
  (set-default 'c-basic-offset 2)
  (setq ff-search-directories '("../include/*" "../src" "." "../../src" "../../include/*"))
  (setq helm-zgrep-file-extension-regexp ".*\\(\\.h\\|\\.cpp\\|\\.cc\\|\\.hpp\\)$")
  )

(add-hook 'c++-mode-hook 'c++-mode-hook-setting)
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(require-package 'helm)
(require-package 'helm-gtags)
(require-package 'helm-projectile)
(setq
 helm-gtags-ignore-case t
 helm-gtags-auto-update t
 helm-gtags-use-input-at-cursor t
 helm-gtags-pulse-at-cursor t
 helm-gtags-prefix-key "\C-cg"
 helm-gtags-suggested-key-mapping t
 )
(require 'helm-gtags)
;; Enable helm-gtags-mode
(define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-dwim)
(define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)
(add-hook 'c++-mode-hook 'key-bind-hook )
(add-hook 'c++-mode-hook 'helm-gtags-mode)
(custom-set-faces
 '(ac-candidate-face ((t (:family "DejaVu Sans Mono"))))
 '(helm-selection ((t (:background "tan" :underline (:color "dark orange" :style wave)))))
 '(helm-selection-line ((t (:background "cornsilk" :foreground "dark orange" :underline nil)))))
(eval-after-load 'helm-grep
  '(setq helm-grep-default-command helm-grep-default-recurse-command))

(require-package 'helm-swoop)
(require 'helm-swoop)

;; Change the keybinds to whatever you like :)
(global-set-key (kbd "M-i") 'helm-swoop)
(global-set-key (kbd "M-I") 'helm-swoop-back-to-last-point)
(global-set-key (kbd "C-c M-i") 'helm-multi-swoop)
(global-set-key (kbd "C-x M-i") 'helm-multi-swoop-all)

;; When doing isearch, hand the word over to helm-swoop
(define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)
;; From helm-swoop to helm-multi-swoop-all
(define-key helm-swoop-map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop)
;; When doing evil-search, hand the word over to helm-swoop
; (define-key evil-motion-state-map (kbd "M-i") 'helm-swoop-from-evil-search)

;; Move up and down like isearch
(define-key helm-swoop-map (kbd "C-r") 'helm-previous-line)
(define-key helm-swoop-map (kbd "C-s") 'helm-next-line)
(define-key helm-multi-swoop-map (kbd "C-r") 'helm-previous-line)
(define-key helm-multi-swoop-map (kbd "C-s") 'helm-next-line)

;; Save buffer when helm-multi-swoop-edit complete
(setq helm-multi-swoop-edit-save t)

;; If this value is t, split window inside the current window
(setq helm-swoop-split-with-multiple-windows nil)

;; Split direcion. 'split-window-vertically or 'split-window-horizontally
(setq helm-swoop-split-direction 'split-window-vertically)

;; If nil, you can slightly boost invoke speed in exchange for text color
(setq helm-swoop-speed-or-color nil)

;; ;; Go to the opposite side of line from the end or beginning of line
(setq helm-swoop-move-to-line-cycle t)

;; Optional face for line numbers
;; Face name is `helm-swoop-line-number-face`
(setq helm-swoop-use-line-number-face t)
(provide 'init-cpp)
