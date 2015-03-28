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

(setq mew-config-alist
      '(
        ;; ("default"
        ;;  ("pop-server"           . "pop.gmail.com")
        ;;  ("name"                 . "yuandaxing")
        ;;  ("user"                 . "mryuan0")
        ;;  ("mail-domain"          . "gmail.com")
        ;;  ("pop-user"             . "mryuan0@gmail.com")
        ;;  ("smtp-user"            . "mryuan0@gmail.com")
        ;;  ("smtp-server"          . "smtp.gmail.com")
        ;; )
        ("MVAD"
         ("pop-server"           . "pop.mvad.com")
         ("name"                 . "yuandaxing")
         ("user"                 . "yuandx")
         ("mail-domain"          . "mvad.com")
         ("pop-user"             . "popuser@mvad.com")
         ("smtp-user"            . "smtpuser@mvad.com")
         ("smtp-server"          . "smtp.mvad.com")
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
