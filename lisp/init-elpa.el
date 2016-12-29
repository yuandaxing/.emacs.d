;;; Find and load the correct package.el
;; When switching between Emacs 23 and 24, we always use the bundled package.el in Emacs 24
                                        ;(set-default 'package-user-dir "~/Dropbox/emacs/elpa/")
;;; Also use Melpa for most packages
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))

(require 'fullframe)
(require 'cl-lib)

(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-verbose t)
(use-package auto-compile
  :ensure t
  :config (auto-compile-on-load-mode 1))
(setq load-prefer-newer t)
(provide 'init-elpa)
