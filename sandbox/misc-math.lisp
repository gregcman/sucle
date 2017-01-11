(in-package :sandbox)

(defun clamp (x min max)
  (max (min x max) min))
(defun rad-deg (rad)
  "converts radians to degrees"
  (* rad 180 (/ 1 pi)))
(defun deg-rad (deg)
  "converts degrees to radians"
  (* deg pi 1/180))

(defun ease (x target fraction)
  (+ x (* fraction (- target x))))

(define-modify-macro *= (&rest args)
  *)

(defun int-scale (int scale)
  (truncate (* int scale)))

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


