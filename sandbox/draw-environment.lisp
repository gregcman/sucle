(in-package :sandbox)

(defun fractionalize (x)
  (clamp x 0.0 1.0))

(defun set-overworld-fog (time)
  (let ((x (fractionalize (* time 0.68)))
	(y (fractionalize (* time 0.8)))
	(z (fractionalize (* time 1.0)))
	(w 1.0))
    (gl:clear-color x y z w)
    (glshader:set-vec3 "fogcolor"
	      (vector x y z w))))
