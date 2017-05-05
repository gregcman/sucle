(defpackage :pix
  (:use #:cl #:fuktard))

(in-package :pix)

(progn
  (deftype pix-world ()
    (quote hash-table))
  (defconstant +available-bits+ (logcount most-positive-fixnum))
  (defconstant +x-bits-start+ (floor +available-bits+ 2))
  (defconstant +x-chunk-bits+ 4)
  (defconstant +x-chunk-size+ (ash 1 +x-chunk-bits+))
  (defconstant +x-bitmask+ (1- +x-chunk-size+))
  (defconstant +y-chunk-bits+ 4)
  (defconstant +y-chunk-size+ (ash 1 +y-chunk-bits+))
  (defconstant +y-bitmask+ (1- +y-chunk-size+))
  (defconstant +xy-bitmask+ (1- (* +y-chunk-size+ +x-chunk-size+)))
  (defconstant +index-mask+ (logior (ash +x-bitmask+ +x-bits-start+)
				    +y-bitmask+))
  (defconstant +hash-mask+ (logxor +index-mask+ most-positive-fixnum))
  (defconstant +right-shift+ (- +y-chunk-bits+ +x-bits-start+))
  (defconstant +y-mask+ (1- (ash 1 +x-bits-start+)))
  (defconstant +chunk-capacity+ (* +x-chunk-size+ +y-chunk-size+))

  (declaim (ftype (function () (simple-vector)) make-chunk))
  (defun make-chunk ()
    (make-array +chunk-capacity+
		:element-type t
		:initial-element nil))

  (defun make-world ()
    (make-hash-table :test (quote eq)))


  (defmacro with-chunk-or-null ((chunk &optional (hash-id (gensym))) (place hash) &body body)
    `(let* ((,hash-id (logand ,place +hash-mask+))
	    (,chunk (gethash ,hash-id ,hash)))
       (declare (type (or null simple-vector) ,chunk))
       ,@body))

  (declaim (ftype (function (fixnum) (values fixnum fixnum)) index-xy)
	   (ftype (function (fixnum fixnum) fixnum) xy-index)
	   (ftype (function (fixnum hash-table) t) get-obj)
	   (ftype (function (fixnum t hash-table) t) set-obj)
	   (ftype (function (fixnum) fixnum) chunk-ref)
	   (inline index-xy xy-index get-obj set-obj chunk-ref))

  (with-unsafe-speed 
    (defun chunk-ref (place)
      (let* ((num (logand place +index-mask+))
	     (num2 (ash num +right-shift+))
	     (num3 (logand +xy-bitmask+ (logior num num2))))
	num3))
    (defun set-obj (place value world)
      (with-chunk-or-null (chunk hash-id) (place world)
	(unless chunk
	  (let ((new-chunk (make-chunk)))
	    (setf (gethash hash-id world) new-chunk)
	    (setf chunk new-chunk)))
	(setf (aref chunk (chunk-ref place)) value)
	hash-id))
    (defun get-obj (place world)
      (with-chunk-or-null (chunk) (place world)
	(if chunk
	    (aref chunk (chunk-ref place)))))
    (defun xy-index (x y)
      (let ((fnum (ash x +x-bits-start+)))
	(logior (logand most-positive-fixnum fnum)
		(logand +y-mask+ y))))
    (defun index-xy (index)
      (values (ash index (- +x-bits-start+))
	      (logand index +y-mask+))))
  (progn
    (declaim (inline (setf get-obj)))
    (defun (setf get-obj) (value place hash-table)
      (set-obj place value hash-table))))

(export (quote (index-xy xy-index get-obj set-obj chunk-ref make-world make-chunk pix-world)))

(defun fixnum-not (n)
  (logand most-positive-fixnum (lognot n)))

(progn
  (declaim (inline n-bits)
	   (ftype (function (fixnum) fixnum)
		  n-bits))
  (defun n-bits (n)
    (1- (ash 1 n))))
(defconstant +n-bits+ (let ((array (make-array +available-bits+)))
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
  (declaim (inline page)
	   (ftype (function (fixnum fixnum (unsigned-byte 6) (unsigned-byte 6))
			    (values fixnum fixnum))
		  page))
  (with-unsafe-speed
    (defun page (x y xsize ysize)
      (let ((xshift (- xsize))
	    (yshift (- ysize)))
	(declare (type fixnum xshift yshift))
	(let* ((yans (ash x xshift))
	       (xans (ash y yshift)))
	  (declare (type fixnum yans xans))
	  (values xans yans))))))

(defparameter *world* (make-array (* 256 256) :initial-element nil))

(progn
  (declaim (ftype (function (fixnum fixnum simple-vector) t)
		  get2)
	   (inline get2))
  (with-unsafe-speed
    (defun get2 (x y world)
      (multiple-value-bind (xbig ybig) (page x y 4 4)
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
      (multiple-value-bind (xbig ybig) (page x y 4 4)
	(let ((index (index xbig ybig 8 8)))
	  (let ((subarray (aref world index)))
	    (declare (type (or null simple-vector) subarray))
	    (let ((sub-index (index x y 4 4)))
	      (if subarray
		  (setf (aref subarray sub-index) value)
		  (let ((newarray (make-chunk)))
		    (setf (aref world index) newarray)
		    (setf (aref newarray sub-index) value))))))))))
