(in-package :cl-user)
(defgeneric wow (x))

(defmethod wow :before ((x t))
  (terpri)
  (format t "before wow ~a" x))

(defmethod wow :after ((x t))
  (terpri)
  (format t "after wow ~a" x))

(defmethod wow ((x t))
  (terpri)
  (format t "t: ~a" x))

(defmethod wow ((x fixnum))
  (if (= x 1)
      (call-next-method)
      (progn
	(terpri)
	(format t "fixnum: ~a" x))))

(defun wot ()
  (print (funland:etouq (gensym))))

(defun woah ()
  (let ((times 0))
    (tagbody wow
       (restart-case 
	   (handler-bind ((unbound-variable
			   (function
			    (lambda (c)
			     (print c)
			     (print (compute-restarts))
			     (invoke-restart (find-restart 'huh) 1 2 3 4)))))
	     (incf times)
	     (when (> 16 times)
	       (wot)))
	 (huh (&rest v)
	   (declare (ignorable v))
	   (go wow))))))
