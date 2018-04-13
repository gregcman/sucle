(defpackage :color-test
  (:use :cl))
(in-package :color-test)

(declaim (inline cie-xyz-cie-xyl))
(defun cie-xyz-cie-xyl (x y z)
  (let ((a (+ x y z)))
    (values (/ x a)
	    (/ y a)
	    y)))

(declaim (inline cie-xyl-cie-xyz))
(defun cie-xyl-cie-xyz (x y luminance)
  (values (/ (* luminance x)
	     y)
	  luminance
	  (/ (* luminance (- 1 x y))
	     y)))

(defun test-same (x0 y0 z0)
  (multiple-value-bind (x y z) (cie-xyl-cie-xyz x0 y0 z0)
    (multiple-value-bind (x y z) (cie-xyz-cie-xyl x y z)
      (assert (= x x0))
      (assert (= y y0))
      (assert (= z z0)))))

(defun test (x y luminance)
  (multiple-value-bind (x y z) (cie-xyl-cie-xyz x y luminance)
    (rs-colors::adobe-rgb-from-cie-xyz x y z)))

(defparameter *path*
  "/home/imac/quicklisp/local-projects/symmetrical-umbrella/unused/test/color/color-test.png")
(defun test69 (&optional (path *path*))
  (let ((array (make-array '(256 256 4) :element-type '(unsigned-byte 8))))
    (dotimes (x 256)
      (dotimes (y 256)
	(unless (= 0 y)
	  (multiple-value-bind (r g b) (test (/ x 255.0)
					     (/ y 255.0)
					     1.0)
	    (flet ((fun (elt value)
		     (setf (aref array (- 255 y) x elt)
			   (floor (* value 255.0)))))
	      (fun 0 r)
	      (fun 1 g)
	      (fun 2 b)
	      (fun 3 1.0))))))
    (opticl:write-png-file
     path
     array)))
;A B C D E

;;luminance
(( 0.1787 -1.4630)
 (-0.3554  0.4275)
 (-0.0227  5.3251)
 ( 0.1206 -2.5771)
 (-0.0670  0.3703))

;x
((-0.0193 -0.2592)
 (-0.0665 -0.0008)
 (-0.0004  0.2125)
 (-0.0641 -0.8989)
 (-0.0033  0.0452))

;y
((-0.0167 -0.2608)
 (-0.0950  0.0092)
 (-0.0079  0.2102)
 (-0.0441 -1.6537)
 (-0.0109  0.0529))

