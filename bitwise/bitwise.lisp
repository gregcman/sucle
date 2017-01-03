(in-package :bitwise)
;;print the twos complement binary representation of a number, padded to size
;;size is by default 62 because that is fixnums on 64 bit machines
(defun print-bits (n &optional (size 62))
  (let ((string (concatenate 'string "~" (write-to-string size) ",'0B")))
    (let ((num (ldb (byte size 0) n)))      
      (format t string num)
      num)))

;;;;
(defun getshit (a b c)
  (declare (optimize (speed 3) (safety 0)))
  (declare (type (simple-array (unsigned-byte 16) (*)) a)
	   (type (simple-array (unsigned-byte 32) (*)) b)
	   (type (simple-array (signed-byte 32) (*)) c))
  (values (aref a 0)
	  (aref b 1)
	  (aref c 0)))

(defun getshit2 (a)
  (declare (optimize (speed 3) (safety 0)))
  (let ((x a)
	(y a)
	(z a))
    (declare (type (simple-array (unsigned-byte 2) (*)) x)
	     (type (simple-array (unsigned-byte 4) (*)) y)
	     (type (simple-array (signed-byte 8) (*)) z))
    (values (aref x 0)
	    (aref y 0)
	    (aref z 0))))

(defparameter atest
  (make-array 64 :element-type 'bit :initial-element 1))

(defun fuk (x)
  (declare (optimize (speed 3) (safety 0))
	   (type (signed-byte 62) x))
  (1+ x))

(defun what? (x)
  (sb-sys:sap-ref-64 (sb-sys:int-sap (sb-kernel:get-lisp-obj-address x)) 1))

(defun peek-mem (val &optional (num 64))
  (handler-bind ((sb-sys:memory-fault-error
		  (lambda (c)
		    (declare (ignorable c))
		    (invoke-restart 'lol))))
    (dotimes (x num)
      (restart-case
	  (print (sb-sys:sap-ref-lispobj (sb-sys:int-sap (sb-kernel:get-lisp-obj-address val))
					 (+ 5 (* x 8))))
	(lol () nil)))))
