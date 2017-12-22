(in-package :aplayground)

#+nil
(progno
 (defconstant +single-float-pi+ (coerce pi 'single-float))
 (defconstant +single-float-two-pi+ (coerce (* 2 pi) 'single-float))
 (defconstant +single-float-half-pi+ (coerce (/ pi 2) 'single-float)))

#+nil
(defun clamp (x min max)
  (max (min x max) min))

#+nil
(progno
 (defparameter *temp-matrix* (cg-matrix:identity-matrix))
 (defparameter *temp-matrix2* (cg-matrix:identity-matrix))
 (defparameter *temp-matrix3* (cg-matrix:identity-matrix))
 (defparameter *x-unit* (cg-matrix:vec 1.0 0.0 0.0)))

#+nil
(defun conspack-save (path thing &optional (overwritep nil))
  (with-open-file (stream path
			  :direction :output
			  :if-does-not-exist :create
			  :if-exists (if overwritep :supersede :error)
			  :element-type '(unsigned-byte 8))
    (conspack:tracking-refs ()
			    (conspack:encode thing :stream stream))))
#+nil
(defun conspack-load (path)
  (conspack:tracking-refs ()
			  (conspack:decode (alexandria:read-file-into-byte-vector path))))

#+nil

(progn
  (defun skey-p (enum)
    (e:key-p enum;(cffi:convert-to-foreign enum (quote %cl-glfw3::key))
	     ))
  (defun skey-j-r (enum)
    (e:key-j-r enum;(cffi:convert-to-foreign enum (quote %cl-glfw3::key))
	       ))
  (defun skey-j-p (enum)
    (e:key-j-p enum;(cffi:convert-to-foreign enum (quote %cl-glfw3::key))
	       ))
  (defun smice-p (enum)
    (e:mice-p enum;(cffi:convert-to-foreign enum (quote %cl-glfw3::mouse))
	      ))
  (defun smice-j-p (enum)
    (e:mice-j-p enum;(cffi:convert-to-foreign enum (quote %cl-glfw3::mouse))
		)))

#+nil
(defun make-eq-hash ()
  (make-hash-table :test (quote eq)))

#+nil
(progn
  (declaim (ftype (function (symbol t)) set-symbol-value))
  (with-unsafe-speed
    (defun set-symbol-value (symbol value)
      #+sbcl (sb-impl::%set-symbol-value symbol value)
      #-sbcl (set symbol value))))
