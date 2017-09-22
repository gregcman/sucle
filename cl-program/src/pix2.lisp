(in-package :aplayground)

(progn
  (declaim (ftype (function () (simple-vector)) make-chunk))
  (defun make-chunk ()
    (make-array (1+ (* 16 16))
		:element-type t
		:initial-element nil)))

(progn
  (declaim (ftype (function (fixnum fixnum simple-vector)
			    (values simple-vector fixnum))
		  area2))
  (with-unsafe-speed
    (defun area2 (x y world)
      (let ((xbig (pix:page x 4))
	    (ybig (pix:page y 4)))
	(let ((index (pix:index xbig ybig 6 6)))
	  (let ((subarray (aref world index)))
	    (declare (type (or null simple-vector) subarray))
	    (values (or subarray
			(setf (aref world index)
			      (make-chunk)))
		    (pix:index x y 4 4))))))))

(progn
  (eval-when (:compile-toplevel)
    (defparameter *world* (make-array (* 256 256) :initial-element nil)))
  (defun (setf get-char) (value x y)
    (set-char value x y))
  (defun get-char (x y)
    (multiple-value-bind (chunk offset) (area2 x y (etouq *world*))
      (aref chunk offset)))
  (defun set-char (value x y)
    (multiple-value-bind (chunk offset) (area2 x y (etouq *world*))
      (setf (aref chunk offset) value)))
  (defun scwu (value x y)
    (multiple-value-bind (chunk offset) (area2 x y (etouq *world*))
      (setf (aref chunk offset) value)
      (setf (aref chunk (* 16 16)) *ticks*))))


#+nil
(progno
  (deftype pix-world ()
    (quote hash-table))
  (defun make-world ()
    (make-hash-table :test (quote eq)))
  (defun area (x y world)
    (let ((xbig (page x 4))
	  (ybig (page y 4)))
      (let ((index (xy-index xbig ybig)))
	(let ((subarray (gethash index world)))
	  (declare (type (or null simple-vector) subarray))
	  (values (or subarray
		      (setf (gethash index world)
			    (make-chunk)))
		  (index x y 4 4)))))))

#+nil
(progno
 (declaim (ftype (function (fixnum) (values fixnum fixnum)) index-xy)
	  (ftype (function (fixnum fixnum) fixnum) xy-index)	  
	  (inline index-xy xy-index))

 (with-unsafe-speed 
   
   (defun xy-index (x y)
     (index x y 31 31))
   (defun index-xy (index)
     (values (offset index 31)
	     (page index 31)))))
#+nil
(progno
  (progn
    (declaim (ftype (function (fixnum fixnum simple-vector) t)
		    get2)
	     (inline get2))
    (with-unsafe-speed
      (defun get2 (x y world)
	(let ((xbig (page x 4))
	      (ybig (page y 4)))
	  (let ((index (index xbig ybig 8 8)))
	    (let ((subarray (aref world index)))
	      (declare (type (or null simple-vector) subarray))
	      (if subarray
		  (let ((sub-index (index x y 4 4)))
		    (aref subarray sub-index)))))))))

  (progn
    (declaim (ftype (function (t fixnum fixnum simple-vector) t) (setf get2)))
    (declaim (inline (setf get2)))
    (with-unsafe-speed
      (defun (setf get2) (value x y world)
	(set2 x y world value))))

  (progn
    (declaim (ftype (function (fixnum fixnum simple-vector t) t)
		    set2))
    (declaim (inline set2))
    (with-unsafe-speed
      (defun set2 (x y world value)
	(let ((xbig (page x 4))
	      (ybig (page y 4)))
	  (let ((index (index xbig ybig 8 8)))
	    (let ((subarray (aref world index)))
	      (declare (type (or null simple-vector) subarray))
	      (let ((sub-index (index x y 4 4)))
		(if subarray
		    (setf (aref subarray sub-index) value)
		    (let ((newarray (make-chunk)))
		      (setf (aref world index) newarray)
		      (setf (aref newarray sub-index) value)))))))))))
