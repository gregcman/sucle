
(defparameter fnv-1a-hash-nums
  (list 16777619 
	2166136261
	1099511628211
	14695981039346656037))

(defconstant +fnv-offset-basis-64+ 14695981039346656037)
(defconstant +fnv-offset+ 860922984064492325)
(defconstant +fnv-prime-64+ 1099511628211)
(defconstant +fnv-first-64+ (logand most-positive-fixnum (*  +fnv-offset+ +fnv-prime-64+)))

(defun fnv-1a-64bit (num)
  (declare (values fixnum)
	   (type fixnum num)
	   (optimize (speed 3) (safety 0)))
  (macrolet ((k (body)
	       `(let ((result ,body))
		  (setf num (ash num -8))
		  (logxor (mod num 256)
			  (sb-vm::*-mod64 +fnv-prime-64+ result))))
	     (the-start ()
	       `(logxor +fnv-first-64+ (mod num 256))))
    (the fixnum
	 (logand most-positive-fixnum
		 (k
		  (k 
		   (k 
		    (k 
		     (k 
		      (k 
		       (k 
			(the-start))))))))))))

