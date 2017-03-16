(in-package :sandbox)

(defun complex-modulus (c)
  (sqrt (realpart (* c (conjugate c)))))

(defun averager (amount)
  (let ((the-array (make-array amount :element-type 'fixnum))
	(index 0)
	(tot 0))
    (lambda (x)
      (let ((old (aref the-array index)))
	(setf tot (+ tot x (- old)))
	(setf (aref the-array index) x))
      (setf index (mod (1+ index) amount))
      (values (/ (coerce tot 'single-float) amount) the-array))))

(defun clamp (x min max)
  (max (min x max) min))

(defparameter float-pi (coerce pi 'single-float))
(defun rad-deg (rad)
  (* rad (/ 180.0 float-pi)))
(defun deg-rad (deg)
  (* deg (/ float-pi 180.0)))

(defun ease (x target fraction)
  (+ x (* fraction (- target x))))

(define-modify-macro *= (&rest args)
  *)

;;return the pitch and yaw of a unit direction vector
(defun extract-polar-coords (vec)
  (let ((zero (aref vec 0))
	(two (aref vec 2)))
    (values (asin (aref vec 1))
	    (atan two zero))))

(defun unit-pitch-yaw (result pitch yaw)
  (let ((cos-pitch (cos pitch)))
    (setf (aref result 0) (* cos-pitch (cos yaw))
	  (aref result 1) (sin pitch)
	  (aref result 2) (* cos-pitch (sin yaw))))
  result)

(defparameter *fixnum-compare*
  #+sbcl 'eq
  #-sbcl 'eql)

