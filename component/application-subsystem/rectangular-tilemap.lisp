(defpackage :rectangular-tilemap
  (:use :cl))

(in-package :rectangular-tilemap)

(progn
  (declaim (ftype (function (fixnum fixnum fixnum)
			    (values single-float single-float
				    single-float single-float))
		  sequential-index-generator))
  (defun sequential-index-generator (width height num)
    (let ((usize (/ 1f0 (float width)))
	  (vsize (/ 1f0 (float height))))
      (multiple-value-bind (vfoo ufoo) (floor num width)
	(let ((u (* (float ufoo) usize))
	      (v (* (float vfoo) vsize)))
	  (values u v
		  (+ u usize) 
		  (+ v vsize)))))))

;;;;3 4
;;;;1 2
;;flip-y switches the top and bottom y coord when the texture is unflipped,
;;making it like:
;;1 2
;;3 4
(progn
  (declaim (ftype (function (fixnum fixnum &key (:flip-y t)) simple-array)
		  regular-enumeration))
  (defun regular-enumeration (width height &key (flip-y t))
    (let ((tot (* width height)))
      (let ((ret (make-array (* tot 4))))
	(dotimes (x tot)
	  (multiple-value-bind (x0 y0 x1 y1) (sequential-index-generator width height x)
	    (when flip-y
	      (rotatef y0 y1))
	    (let ((p (* x 4)))
	      (setf (aref ret (+ 0 p)) x0
		    (aref ret (+ 1 p)) y0
		    (aref ret (+ 2 p)) x1
		    (aref ret (+ 3 p)) y1))))
	ret))))
(progn
  (declaim (ftype (function (simple-vector fixnum)
			    (values t t t t))
		  index-quad-lookup))
  (defun index-quad-lookup (array code)
    (declare (optimize (speed 3) (safety 0)))
    (let ((p (* code 4)))
      (declare (type fixnum p))
      (values (aref array p)
	      (aref array (1+ p))
	      (aref array (+ 2 p))
	      (aref array (+ 3 p))))))

(export (quote (sequential-index-generator regular-enumeration index-quad-lookup)))
