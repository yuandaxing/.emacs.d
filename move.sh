#! /bin/bash

mkdir -vp ./lisp/ ~/.emacs.d/lisp/
echo $1
if [ "$1"=="commit" ]; then
	echo "start to move commit files"
	cp -vrf ~/.emacs.d/start-init.el ./
	cp -vrf  ~/.emacs.d/lisp/* ./lisp/
elif [ "$1"=="install" ]; then
	echo "start to move install files"
	cp -vrf ./start-init.el ~/.emacs.d/ 
	cp -vrf lisp/* ~/.emacs.d/lisp/
else 
    echo "error command"
	echo "./move.sh [(commit)|(install)]"
fi
