(require 'eclim)
(setq eclimd-autostart t)
(global-eclim-mode)

(custom-set-variables
  '(eclim-eclipse-dirs '("/home/ydx/install/eclipse/eclipse"))
  '(eclim-executable "/home/ydx/install/eclipse/eclipse/eclimd"))
;; regular auto-complete initialization
(require 'auto-complete-config)
(ac-config-default)

;; add the emacs-eclim source
(ac-emacs-eclim-config)
