#!/bin/bash

sbcl --no-userinit --no-sysinit --non-interactive \
     --load ~/quicklisp/setup.lisp \
     --eval '(ql:quickload "cl-opengl")' \
     --eval '(ql:quickload "cl-glu")' \
     --eval '(ql:quickload "cl-glut")' \

     --eval '(ql:quickload "cl-utilities")' \
     --eval '(ql:quickload "lispbuilder-sdl")' \
     --eval '(ql:write-asdf-manifest-file "quicklisp-manifest.txt")'

buildapp \
	--manifest-file quicklisp-manifest.txt \
    --load-system cl-opengl \
    --load-system lispbuilder-sdl \
    --load-system cl-utilities \
    --output clocktopus.app/Contents/MacOS/macclocktopus \
	--asdf-path ~/src/lisp/sandbox/ \
	--load-system sandbox \
    --entry sandbox:arise
