(in-package :sucle)

(defun b@ (x y z)
  (voxel-chunks:getobj x y z))
(defun (setf b@) (value x y z)
  (setf (voxel-chunks:getobj x y z) value))

(dotimes (x 16)
  (dotimes (y 16)
    (setf (b@ x 0 y)
	  (wrap (+ (wrap y 1 3)
		   (* (wrap x 7 15) 16))
		0 256))))

(defun wrap (val min max)
  (+ min (mod (- val min) (- max min))))

(setf (b@ 0 0 0) (+ 0 (- 193 (* 16 3))))

;;red = 129
;;green = 145
;;light green = 146
;;blue = 177
;;light blue = 178

;;113 114 225 black gray light grey


(ql:quickload :black-tie)

(black-tie:perlin-noise-sf)

(dobox ((x 0 128)
	(z 0 128))
       (let ((value (black-tie:perlin-noise-sf
		     (* 0.1 (floatify z))
		     0.0
		     (* 0.1 (floatify x)))))
	 (setf (b@ x (floor (* 10 value)) z) (random 177))))

(dobox ((x -128 128)
	(z -128 128)
	(y -128 128))
       (setf (b@ x y z) 0))

(defun box (x0 x1 y0 y1 z0 z1 value)
  (dobox ((x x0 x1)
	  (z y0 y1)
	  (y z0 z1))
	 (setf (b@ x y z) value)))


