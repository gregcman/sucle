(in-package :sandbox)

(defun unit-pitch-yaw (result pitch yaw)
  (let ((cos-pitch (cos pitch)))
    (setf (aref result 0) (* cos-pitch (sin yaw))
	  (aref result 1) (sin pitch)
	  (aref result 2) (* cos-pitch (cos yaw))))
  result)

(defstruct necking
  (yaw 0.0)
  (pitch 0.0))

(defun look-around (neck dyaw dpitch)
  (let ((twopi (coerce (* 2 pi) 'single-float))
	(halfpi (coerce (/ pi 2) 'single-float)))
    (let ((yaw0? (zerop dyaw))
	  (pitch0? (zerop dpitch)))
      (symbol-macrolet ((yaw (necking-yaw neck))
			(pitch (necking-pitch neck)))
	(unless yaw0?
	  (setf yaw (mod (+ yaw dyaw) twopi)))
	(unless pitch0?
	  (setf pitch
		(alexandria:clamp
		 (+ pitch dpitch)
		 (* -0.99 halfpi)
		 (* 0.99 halfpi))))))))

(defun necktovec (neck result-vec)
  (unit-pitch-yaw result-vec
		  (necking-pitch neck)
		  (necking-yaw neck)))
