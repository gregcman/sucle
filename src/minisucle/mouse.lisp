(in-package :sucle)

;;;;************************************************************************;;;;
;;;;This code basically has not changed in forever.

(defparameter *raw-mouse-x* 0.0d0)
(defparameter *raw-mouse-y* 0.0d0)
(defun cursor-motion-difference
    (&optional (x window:*mouse-x*) (y window:*mouse-y*))
  ;;Return the difference in position of the last time the
  ;;cursor was observed.
  ;;*raw-mouse-x* and *raw-mouse-y* hold the last value
  ;;of the cursor.
  (multiple-value-prog1
      (values (- x *raw-mouse-x*)
	      (- y *raw-mouse-y*))
    (setf *raw-mouse-x* x
	  *raw-mouse-y* y)))

(defparameter *mouse-x* 0.0d0)
(defparameter *mouse-y* 0.0d0)
(defparameter *lerp-mouse-x* 0.0d0)
(defparameter *lerp-mouse-y* 0.0d0)
(defparameter *mdx* 0d0)
(defparameter *mdy* 0d0)
(defun update-moused (&optional (smoothing-factor 1.0))
  (multiple-value-bind (dx dy) (cursor-motion-difference)
    (let ((x (+ *mouse-x* dx))
	  (y (+ *mouse-y* dy)))
      (setf *mouse-x* x)
      (setf *mouse-y* y)))
  ;;*lerp-mouse-x* and *lerp-mouse-y* are used
  ;;for camera smoothing with the framerate.
  (let ((oldx *lerp-mouse-x*)
	(oldy *lerp-mouse-y*))
    (setf *lerp-mouse-x* (alexandria:lerp smoothing-factor *lerp-mouse-x* *mouse-x*))
    (setf *lerp-mouse-y* (alexandria:lerp smoothing-factor *lerp-mouse-y* *mouse-y*))
    (setf *mdx* (- *lerp-mouse-x* oldx))
    (setf *mdy* (- *lerp-mouse-y* oldy))))
(defparameter *mouse-multiplier* 0.002617)
(defparameter *mouse-multiplier-aux* (/ (* 0.5 pi 0.9999) *mouse-multiplier*))
(defun neck-values ()
  (values
   (floatify (* *mdx* *mouse-multiplier*))
   (floatify (* *mdy* *mouse-multiplier*))))

(defun unit-pitch-yaw (pitch yaw &optional (result (sb-cga:vec 0.0 0.0 0.0)))
  (setf yaw (- yaw))
  (let ((cos-pitch (cos pitch)))
    (with-vec (x y z) (result symbol-macrolet)
      (setf x (* cos-pitch (sin yaw))
	    y (- (sin pitch))
	    z (* cos-pitch (cos yaw)))))
  result)
