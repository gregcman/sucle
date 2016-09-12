sbcl --no-userinit --no-sysinit --non-interactive \
     --load ~/quicklisp/setup.lisp \
     --eval '(ql:quickload "sandbox")' \
     --eval '(sandbox:arise)' \