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

(export (quote (page offset index)))
