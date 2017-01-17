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
(defun rad-deg (rad)
  (* rad (/ 180.0d0 pi)))
(defun deg-rad (deg)
  (* deg (/ pi 180.0d0)))

(defun ease (x target fraction)
  (+ x (* fraction (- target x))))

(define-modify-macro *= (&rest args)
  *)

(defun new-direction (dx dy)
  (let ((dir (sb-cga:vec 1.0 0.0 0.0))
	(rotation-axis (sb-cga:normalize (sb-cga:vec 0.0 (- dx) dy)))
	(rot-factor (sqrt (+ (* dx dx) (* dy dy)))))
    (let ((rot (sb-cga:rotate-around rotation-axis rot-factor)))
      (let ((new-dir (sb-cga:transform-direction dir rot)))
	(multiple-value-bind (p y) (extract-polar-coords new-dir)
	  (values y p))))))

;;return the pitch and yaw of a unit direction vector
(defun extract-polar-coords (vec)
  (let ((zero (aref vec 0))
	(two (aref vec 2)))
    (values (asin (aref vec 1))
	    (atan two zero))))

(defun unit-pitch-yaw (pitch yaw)
  (let ((cos-pitch (cos pitch)))
    (sb-cga:vec
     (* cos-pitch (cos yaw))
     (sin pitch)
     (* cos-pitch (sin yaw)))))


