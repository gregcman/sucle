(defpackage :color-test
  (:use :cl :utility))
(in-package :color-test)

(defmacro wtf (x)
  (once-only (x)
    `(= ,x ,x)))

(defmacro wtf2 (x)
  (once-only (x)
    `(wtf (* 0.0 ,x))))

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
    (if (and (wtf2 x)
	     (wtf2 y)
	     (wtf2 z))
       
	(rs-colors::cie-rgb-from-cie-xyz x y z)
	(values 0.0 0.0 0.0))))

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
;;;;A B C D E

;;luminance
(defparameter *luminance-distribution-parameters*
  '(( 0.1787 -1.4630)
    (-0.3554  0.4275)
    (-0.0227  5.3251)
    ( 0.1206 -2.5771)
    (-0.0670  0.3703)))

;;;x
(defparameter *x-distribution-parameters*
  '((-0.0193 -0.2592)
    (-0.0665 -0.0008)
    (-0.0004  0.2125)
    (-0.0641 -0.8989)
    (-0.0033  0.0452)))

;;;y
(defparameter *y-distribution-parameters*
  '((-0.0167 -0.2608)
    (-0.0950  0.0092)
    (-0.0079  0.2102)
    (-0.0441 -1.6537)
    (-0.0109  0.0529)))

(defun foobar (turbity-var parms)
  (destructuring-bind (tvar onevar) parms
    `(+ (* ,tvar ,turbity-var) ,onevar)))

(defmacro distribtion-parameters (turbidity params)
  (once-only (turbidity)
    `(values
      ,@(mapcar (lambda (x)
		  (foobar turbidity x))
		(eval params)))))

(defun x-distribtion-parameters (turbidity)
  (distribtion-parameters turbidity *x-distribution-parameters*))
(defun y-distribtion-parameters (turbidity)
  (distribtion-parameters turbidity *y-distribution-parameters*))
(defun luminance-distribtion-parameters (turbidity)
  (distribtion-parameters turbidity *luminance-distribution-parameters*))


(defun luminance-zenith (turbidity solar-angle)
  (let ((x (* (- (/ 4.0 9.0)
		 (/ turbidity 120.0))
	      (- (floatify pi)
		 (* 2.0 solar-angle)))))
    (+ (*  (- (* 4.0453 turbidity)
	      4.9710)
	   (floatify (tan x)))
       (* -0.2155 turbidity)
       2.4192)))

;;zenith x
(defparameter *zenith-x*
  (make-array
   '(3 4) :element-type 'single-float :initial-contents
   '(( 0.00166 -0.00375  0.00209  0.0)
     (-0.02903  0.06377 -0.03202  0.00394)
     ( 0.11693 -0.21196  0.06052  0.25886))))

;;zenith y
(defparameter *zenith-y*
  (make-array
   '(3 4) :element-type 'single-float :initial-contents
   '(( 0.00275 -0.00610  0.00317  0.0)
     (-0.04214  0.08970 -0.04153  0.00516)
     ( 0.15346 -0.26756  0.06670  0.26688))))

(defmacro row-vector (&rest args)
  `(make-array
    '(1 ,(length args)) :element-type 'single-float :initial-contents
    (list (list ,@args))))

(defmacro column-vector (&rest args)
  `(make-array
    '(,(length args) 1) :element-type 'single-float :initial-contents
    (list ,@ (mapcar (lambda (x) (list (quote list) x)) args))))

(defun xy-luminance (array turbidity solar-angle)
  (row-major-aref
   (opticl::matrix-multiply
    (opticl::matrix-multiply
     (row-vector (expt turbidity 2)
		 turbidity
		 1.0)
     array)
    (column-vector
     (expt solar-angle 3)
     (expt solar-angle 2)
     solar-angle
     1.0))
   0))

(defun luminances (turbidity solar-angle)
  (values (xy-luminance *zenith-x* turbidity solar-angle)
	  (xy-luminance *zenith-y* turbidity solar-angle)
	  (luminance-zenith turbidity solar-angle)))


(defmacro with-float-traps-masked (&body body)
  `(progn
     (sb-int:with-float-traps-masked (:invalid :overflow :inexact :underflow :divide-by-zero)
      ,@body)))
(defconstant +e+ (floatify (exp 1.0)))
(defun funfun (a b c d e)
  (lambda (theta gamma)
    (with-float-traps-masked
      (let ((value
	     (* (+ 1
		   (* a (exp (/ b (cos theta)))))
		(+ 1
		   (* c (exp (* d gamma)))
		   (* e (expt (cos gamma) 2))))))
	(if (wtf value)
	    value
	    most-positive-single-float)))))

(defun luminance-fun (turbidity solar-angle)
  (multiple-value-bind (xlum ylum lumlum) (luminances turbidity solar-angle)
    (let* ((lumfun (multiple-value-call #'funfun (luminance-distribtion-parameters turbidity)))
	   (lumscale (/ lumlum (funcall lumfun 0.0 solar-angle)))
	   
	   (xfun (multiple-value-call #'funfun (x-distribtion-parameters turbidity)))
	   (xscale (/ xlum (funcall xfun 0.0 solar-angle)))
	   
	   (yfun (multiple-value-call #'funfun (y-distribtion-parameters turbidity)))
	   (yscale (/ ylum (funcall yfun 0.0 solar-angle))))
      (lambda (theta gamma)
	(values
	 (* xscale (funcall xfun theta gamma))
	 (* yscale (funcall yfun theta gamma))
	 (* lumscale (funcall lumfun theta gamma)))))))

(defun test42 (turbidity a b)
  (let* ((width 128)
	 (fwidth (floatify width))
	 (foo2 (* 0.5 (floatify pi)))
	 (foo (* 1.0 (floatify pi)))
	 (sunpos (complex (* foo a)
			  (* foo b)))
	 (zenith (complex (* foo 0.5)
			  (* foo 0.5)))
	 (solar-angle ;(abs (- sunpos zenith))
	  ))
    (let ((array (make-array (list width width 4) :element-type '(unsigned-byte 8)))
	  (fun (luminance-fun ;turbidity solar-angle
	;	1.0
		(+ 1.0 (* 6.0 b))
		(* foo a)
		)))
      (with-float-traps-masked
	(dotimes (x width)
	  (dotimes (y width)
	    ;(unless (= 0 y))
	    (let ((spot
		   ;;distance to zenith can never be greater than pi/2
		   (complex (* foo2 (/ x fwidth))
			    
			    (* foo (/ y fwidth)))))
	      (multiple-value-bind (xval yval lumval)
		  (funcall fun
			   (realpart spot) ;;distance to zenith is x
					;(abs (- spot zenith))
			   (imagpart spot) ;;distance to sun is y
					;(abs (- spot sunpos))
			   )
		(when (and (wtf2 xval)
			   (wtf2 yval)
			   (wtf2 lumval))
		  (multiple-value-bind (r g b)
		      (test xval yval (- 1.0 (exp (* (- 0.1) lumval))))
		    (flet ((fun (elt value)
			     (setf (aref array (- (1- width) y) x elt)
				   (floor (* value 255.0)))))
		      (fun 0 r)
		      (fun 1 g)
		      (fun 2 b)
		      (fun 3 1.0)))))))))
      array)))
