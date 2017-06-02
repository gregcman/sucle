(defpackage :pix
  (:use #:cl #:fuktard))

(in-package :pix)

(defconstant +available-bits+ (logcount most-positive-fixnum))

(defun fixnum-not (n)
  (logand most-positive-fixnum (lognot n)))

(progn
  (declaim (inline n-bits)
	   (ftype (function (fixnum) fixnum)
		  n-bits))
  (defun n-bits (n)
    (1- (ash 1 n))))
(defconstant
    +n-bits+ 
  (let ((array (make-array +available-bits+)))
    (dotimes (x (length array))
      (setf (aref array x) (n-bits x)))
    array))
(progn
  (deftype pix-world ()
    (quote hash-table))
  (defun make-world ()
    (make-hash-table :test (quote eq))))

(progn
  (declaim (inline index)
	   (ftype (function (fixnum fixnum (unsigned-byte 6) (unsigned-byte 6))
			    fixnum)
		  index))
  (with-unsafe-speed
    (defun index (x y xsize ysize)
      (let ((xmask (aref +n-bits+ xsize))
	    (ymask (aref +n-bits+ ysize)))
	(declare (type fixnum xmask ymask))
	(let ((xbits (logand xmask x))
	      (ybits (logand ymask y)))
	  (declare (type fixnum xbits ybits))
	  (let* ((yshift (ash ybits xsize))
		 (ans (logior xbits yshift)))
	    (declare (type fixnum yshift ans))
	    ans))))))

(progn
  (declaim (inline page offset)
	   (ftype (function (fixnum (unsigned-byte 6))
			    fixnum)
		  page offset))
  (with-unsafe-speed
    (defun page (n size)
      (ash n (- size)))
    (defun offset (n size)
      (let ((mask (aref +n-bits+ size)))
	(declare (type fixnum mask))
	(logand n mask)))))

(progn
  (declaim (ftype (function () (simple-vector)) make-chunk))
  (defun make-chunk ()
    (make-array (1+ (* 16 16))
		:element-type t
		:initial-element nil))

  (declaim (ftype (function (fixnum) (values fixnum fixnum)) index-xy)
	   (ftype (function (fixnum fixnum) fixnum) xy-index)	  
	   (inline index-xy xy-index))

  (with-unsafe-speed 
    
    (defun xy-index (x y)
      (index x y 31 31))
    (defun index-xy (index)
      (values (offset index 31)
	      (page index 31)))))

(defun area (x y world)
  (let ((xbig (page x 4))
	(ybig (page y 4)))
    (let ((index (xy-index xbig ybig)))
      (let ((subarray (gethash index world)))
	(declare (type (or null simple-vector) subarray))
	(values (or subarray
		    (setf (gethash index world)
			  (make-chunk)))
		(index x y 4 4))))))

(export (quote (index-xy xy-index make-world make-chunk pix-world)))

(progn
  (defparameter *world* (make-array (* 256 256) :initial-element nil))

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
