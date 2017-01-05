(require 'compile)
(add-hook 'c++-mode-hook
          (lambda ()
            (unless (or (file-exists-p "makefile")
                        (file-exists-p "Makefile"))
              (set (make-local-variable 'compile-command)
                   (let ((file (file-name-nondirectory buffer-file-name)))
                     (format "%s -I%s -I%s -I%s -I%s -L%s -std=c++11 -o %s.exe %s  %s"
                             "g++"
                             (substitute-in-file-name "$HOME/Dropbox/3rdparty/cpp/include")
                             (substitute-in-file-name "$HOME/Dropbox/3rdparty/cpp/include/mysql")
                             (substitute-in-file-name "$HOME/Dropbox/3rdparty/cpp/include/hiredis")
                             (substitute-in-file-name "$HOME/Dropbox/3rdparty/cpp/include/libevent")
                             (substitute-in-file-name "$HOME/Dropbox/3rdparty/cpp/lib")
                             (file-name-sans-extension file)
                             file
                             "-lgflags -lmysqlclient -lmysqlcppconn-static -ldl -lhiredis -levent -lpthread "
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
                             "-fdiagnostics-color=auto -lpthread -lmysqlclient"
                             ))))))

(defun c++-mode-hook-setting()
  (c-set-offset 'substatement-open 0)
  (c-set-offset 'innamespace 0)
  (c-set-offset 'case-label '+)
  (setq c-basic-offset 4)
  (setq ff-search-directories '("../include/*" "../src" "." "../../src" "../../include/*"))
  (setq helm-zgrep-file-extension-regexp ".*\\(\\.h\\|\\.cpp\\|\\.cc\\|\\.hpp\\)$"))

(add-hook 'c++-mode-hook 'c++-mode-hook-setting)
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

(defun async-make (project)
  (interactive
   (let ((projects
          '("facesaas" "ficus" "common" "reset compile" "sync" "misc")))
     (list (helm :sources (helm-build-sync-source "test"
                            :candidates projects
                            :fuzzy-match t)
                 :buffer "*helm test*"))))
  (progn
    (save-some-buffers t nil)
    (async-shell-command
     (format "source %s ; build %s" (substitute-in-file-name "$HOME/Dropbox/secret/work_shortcut.sh") project))))
(defun project-make (prefix)
  (interactive "p")
  (if (= prefix 1)
      (async-make "misc")
    (call-interactively 'async-make)))
(global-set-key (kbd "C-c h m") 'project-make)

(provide 'init-compile-run)
