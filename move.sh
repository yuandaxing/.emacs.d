#! /bin/bash

mkdir -vp ./lisp/ ~/.emacs.d/lisp/
if [ "$1" == "commit" ]; then
	echo -e "\033[35mstart to move commit files\033[0m"
	cp -vrf ~/.emacs.d/start-init.el ./
	cp -vrf  ~/.emacs.d/lisp/* ./lisp/
elif [ "$1" == "install" ]; then
	echo -e "\033[35mstart to move install files\033[0m"
	cp -vrf ./start-init.el ~/.emacs.d/ 
	cp -vrf lisp/* ~/.emacs.d/lisp/
else 
    echo -e "\033[33merror command\033[0m"
	echo -e "\033[33m./move.sh [(commit)|(install)]\033[0m"
fi
