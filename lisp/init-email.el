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
(setq mew-pop-ssl 'true)
(setq toolbar-mail-reader 'Mew)
(set-default 'mew-decode-quoted 't)
(setq mew-use-cached-passwd t)
(setq mew-ssl-verify-level 0)
(setq mew-config-alist
      '(
        ("default"
         ("pop-server"           . "pop.mvad.com")
         ("name"                 . "yuandx@mvad.com")
         ("user"                 . "yuandx@mvad.com")
         ("mail-domain"          . "mvad.com")
         ("pop-user"             . "yuandx@mvad.com")
         ("pop-port"     .  "995")
         ("smtp-user"            . "yuandx@mvad.com")
         ("smtp-server"          . "smtp.mvad.com")
         ("smtp-port"    .  "994")
         ("mailbox-type" .  pop)
         ("pop-auth"     .  pass)
         ("smtp-auth-list" . ("PLAIN" "LOGIN" "CRAM-MD5"))
         )
        ))
(when (boundp 'utf-translate-cjk)
      (setq utf-translate-cjk t)
      (custom-set-variables
      (utf-translate-cjk t)))
(if (fboundp 'utf-translate-cjk-mode)
    (utf-translate-cjk-mode 1))
(require 'flyspell) ;;
(provide 'init-email)
