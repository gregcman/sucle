(in-package :aplayground)

(defun print-bits (n &optional (stream *standard-output*))
  (format stream "~64,'0b" n)
  n)

(progno
 (defun fmakunbounds (symbol-list)
   (dolist (symbol symbol-list)
     (fmakunbound symbol)))
 (defun makunbounds (symbol-list)
   (dolist (symbol symbol-list)
     (makunbound symbol)))
 (defmacro xfmakunbounds (&body symbols)
   `(fmakunbounds (quote ,symbols)))
 (defmacro xmakunbounds (&body symbols)
   `(makunbounds (quote ,symbols))))
