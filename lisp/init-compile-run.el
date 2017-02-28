(require 'compile)
(add-hook 'c++-mode-hook
          (lambda ()
            (unless (or (file-exists-p "makefile")
                        (file-exists-p "Makefile"))
              (set (make-local-variable 'compile-command)
                   (let ((file (file-name-nondirectory buffer-file-name)))
                     (if (string-equal (file-name-extension file) "cu")
                         (format "nvcc -o %s.exe %s"
                                 (file-name-sans-extension file)
                                 file)

                       (format "%s -I%s -I%s -I%s -I%s -L%s -std=c++11 -o %s.exe %s  %s"
                               "g++"
                               (substitute-in-file-name "$HOME/Dropbox/3rdparty/cpp/include")
                               (substitute-in-file-name "$HOME/Dropbox/3rdparty/cpp/include/mysql")
                               (substitute-in-file-name "$HOME/Dropbox/3rdparty/cpp/include/hiredis")
                               (substitute-in-file-name "$HOME/Dropbox/3rdparty/cpp/include/libevent")
                               (substitute-in-file-name "$HOME/Dropbox/3rdparty/cpp/lib")
                               (file-name-sans-extension file)
                               file
                               "-lgflags -lmysqlclient -lmysqlcppconn-static -ldl -lhiredis -levent -lpthread  -ljsoncpp"
                               )))))))

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
(add-to-list 'auto-mode-alist '("\\.\\(h\\|cu\\|cuh\\)\\'" . c++-mode))

(defun shell-async-build (project)
  (async-shell-command
   (format "source %s ; build %s"
           (substitute-in-file-name "$HOME/Dropbox/secret/work_shortcut.sh")
           project)))
(defcustom remote-machine nil
  "enable memorize snippet search "
  :type 'string)
(defun shell-async-build-remote (project)
  (async-shell-command
   (format "source %s ; build_remote %s %s"
           (substitute-in-file-name "$HOME/Dropbox/secret/work_shortcut.sh")
           project remote-machine)))
(defun simple_cpp (not_use)
  (compile compile-command))

(defvar project-mapping nil)
(setq project-mapping
      '(("facesaas" shell-async-build)
        ("ficus"  shell-async-build)
        ("common" shell-async-build)
        ("misc"  shell-async-build)
        ("buildtags" shell-async-build)
        ("simple_cpp" simple_cpp)
        ("remote_misc" shell-async-build-remote)
        ("remote_face" shell-async-build-remote)
        ("remote_common" shell-async-build-remote)
        ("remote_reset" shell-async-build-remote)
        ("remote_video_service" shell-async-build-remote)
        ("remote_face_video" shell-async-build-remote)
        ("remote_alignment" shell-async-build-remote)))

(defvar project-name nil)               ;保存编译的project name
(require 'savehist)
(add-to-list 'savehist-additional-variables 'project-name) ;添加到savehist列表中， 重启恢复该变量
(add-to-list 'savehist-additional-variables 'remote-machine) ;添加到savehist列表中， 重启恢复该变量
(defun async-make (project) ; 使用helm 选择make的project
  (interactive
   (let (
         (projects (mapcar #'car project-mapping))
         )
     (list (helm :sources (helm-build-sync-source "test"
                            :candidates projects
                            :fuzzy-match t)
                 :buffer "*helm test*"))))
  (progn
    (save-some-buffers t nil)
    (if project
        (setq project-name project))
    (funcall
     (cadr (assoc project project-mapping))
                                        ; 记录当前的选择，后面就不用输入了
     project)))
(require 'helm-gtags)
(defun project-make (prefix)            ; prefix argument
  (interactive "p")
  (if (= prefix 1)
      (async-make (if project-name
                      project-name
                    "misc"))            ; 默认使用project-name参数
    (call-interactively 'async-make)))
(global-set-key (kbd "C-c h m") 'project-make)
(provide 'init-compile-run)
