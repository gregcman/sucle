


(defun test (a b)
  (multiple-value-bind (c d) (extract-polar-coords (unit-pitch-yaw (coerce (deg-rad a) 'single-float)
								   (coerce (deg-rad b) 'single-float)))
    (values (rad-deg c)
	    (rad-deg d))))

(defun test2 (dx dy)
  (multiple-value-bind (y p) (new-direction
			      (coerce dx 'single-float)
			      (coerce dy 'single-float)
			      (coerce (/  pi 180) 'single-float))
    (values (rad-deg y) (rad-deg p))))
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
	    (values y p)))))))
