(require-package 'mew)

(autoload 'mew "mew" nil t)
(autoload 'mew-send "mew" nil t)
(if (boundp 'read-mail-command)
    (setq read-mail-command 'mew))
(autoload 'mew-user-agent-compose "mew" nil t)
(if (boundp 'mail-user-agent)
    (setq mail-user-agent 'mew-user-agent))
(if (fboundp 'define-mail-user-agent)
    (define-mail-user-agent
       'mew-user-agent
       'mew-user-agent-compose
       'mew-draft-send-message
       'mew-draft-kill
       'mew-send-hook))
(setq mew-pop-size 0)
(setq toolbar-mail-reader 'Mew)
(set-default 'mew-decode-quoted 't)
(setq mew-prog-pgp "gpg")
(setq mew-name "yuandaxing")
(setq mew-user "yuandaxing")
(setq mew-smtp-user "yuandx@smtp.mvad.com")
;(setq mew-mail-domain "smtp.server.com")
;(setq mew-smtp-auth-list nil)
;(setq mew-smtp-auth-list "LOGIN")
(setq mew-smtp-mail-from "yuandx@smtp.mvad.com")
(setq mew-smtp-server "smtp.mvad.com")
(setq mew-pop-server "pop3.mvad.com")
(setq mew-pop-user "yuandx")
(setq mew-pop-auth 'pass) ;;认证方式
(setq mew-use-cached-passwd t)
;(setq mew-nntp-server "")
;(setq mew-icon-directory (expand-file-name "mew/etc" dtsite-dir))
(when (boundp 'utf-translate-cjk)
      (setq utf-translate-cjk t)
      (custom-set-variables
      (utf-translate-cjk t)))
(if (fboundp 'utf-translate-cjk-mode)
    (utf-translate-cjk-mode 1))
(require 'flyspell) ;;

(provide 'init-email)
