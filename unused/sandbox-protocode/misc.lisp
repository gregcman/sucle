(in-package :sandbox)

#+nil
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

#+nil
(progno
 ;;return the pitch and yaw of a unit direction vector
 (defun extract-polar-coords (vec)
   (let ((zero (aref vec 0))
	 (two (aref vec 2)))
     (values (asin (aref vec 1))
	     (atan two zero))))

 (%sphere-mouse-help x y)

 (defun %sphere-mouse-help (x y)
   (if (zerop x)
       (if (zerop y)
	   (values 0.0 0.0)
	   (values 0.0 y))
       (if (zerop y)
	   (values x 0.0)
	   (new-direction (coerce x 'single-float)
			  (coerce y 'single-float)))))

 (defparameter *temp-matrix3* (cg-matrix:identity-matrix))

 (defparameter *x-unit* (cg-matrix:vec 1.0 0.0 0.0))
 (defparameter *new-dir* (cg-matrix:vec 0.0 0.0 0.0))
 (defun new-direction (dx dy)
   (let ((size (sqrt (+ (* dx dx) (* dy dy)))))
     (let ((dir *x-unit*))
       (let ((rot (cg-matrix:%rotate-around*
		   *temp-matrix3*
		   0.0 (/ (- dx) size) (/ dy size) size)))
	 (let ((new-dir (cg-matrix:%transform-direction *new-dir* dir rot)))
	   (multiple-value-bind (p y) (extract-polar-coords new-dir)
	     (values y p))))))))

