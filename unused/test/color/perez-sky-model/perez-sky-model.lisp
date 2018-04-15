(defpackage :perez-sky-model
  (:use :cl :utility :bad-floats :matrix :cie-color-space))
(in-package :perez-sky-model)

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

(defun xy-luminance (array turbidity solar-angle)
  (row-major-aref
   (matrix-multiply
    (matrix-multiply
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
	(if (float-not-nan-p value)
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

(defun test-array (turbidity a b)
  (let* ((width 128)
	 (fwidth (floatify width))
	 (foo2 (* 0.5 (floatify pi)))
	 (foo (* 1.0 (floatify pi)))
	 (sunpos (complex (* foo a)
			  (* foo b)))
	 (zenith (complex (* foo 0.5)
			  (* foo 0.5)))
	 (solar-angle (* foo2 a)
	  ))
    (let ((array (make-array (list width width 4) :element-type '(unsigned-byte 8)))
	  (fun (luminance-fun ;turbidity solar-angle
		1.0
	;	(+ 1.0 (* 6.0 b))
		solar-angle
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
		(when (and (float-good-p xval)
			   (float-good-p yval)
			   (float-good-p lumval))
		  (multiple-value-bind (r g b)
		      (cie-xyl-cie-rgb
		       xval yval
					; lumval
		       (- 1.0 (exp (* (- 0.5) lumval)))
		       )
		    (flet ((fun (elt value)
			     (setf (aref array (- (1- width) y) x elt)
				   (floor (* value 255.0)))))
		      (fun 0 r)
		      (fun 1 g)
		      (fun 2 b)
		      (fun 3 1.0)))))))))
      array)))
