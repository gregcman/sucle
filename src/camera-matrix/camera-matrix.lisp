(defpackage #:camera-matrix
  (:use #:cl)
  (:export
   #:make-camera
   #:camera-vec-forward
   #:camera-vec-position
   #:camera-vec-noitisop
   #:camera-aspect-ratio
   #:camera-fov
   #:camera-frustum-far
   #:camera-frustum-near
   #:camera-matrix-projection-view-player)
  (:export
   #:update-matrices))
(in-package #:camera-matrix)

(struct-to-clos:struct->class 
 (defstruct camera
   (vec-position (sb-cga:vec 0.0 0.0 0.0) :type sb-cga:vec)

   (vec-up (sb-cga:vec 0.0 1.0 0.0) :type sb-cga:vec)
   (vec-forward (sb-cga:vec 1.0 0.0 0.0) :type sb-cga:vec)

   (vec-noitisop (sb-cga:vec 0.0 0.0 0.0) :type sb-cga:vec) ;;;the negative of position
   (matrix-player (sb-cga:identity-matrix)) ;;positional information of camera
   (matrix-view (sb-cga:identity-matrix))		    ;;view matrix
   (matrix-projection (sb-cga:identity-matrix))	    ;;projection matrix
   (matrix-projection-view (sb-cga:identity-matrix)) ;;projection * view matrix
   (matrix-projection-view-player (sb-cga:identity-matrix))
   
   (fov (coerce (/ pi 2.0) 'single-float) :type single-float)

   (aspect-ratio 1.0 :type single-float)
   (frustum-near 0.0078125 :type single-float)
   (frustum-far 128.0 :type single-float)

   (cam-up (sb-cga:vec 0.0 1.0 0.0) :type sb-cga:vec)
   (cam-right (sb-cga:vec 0.0 0.0 1.0) :type sb-cga:vec)
   
   ;;The normals of each plane.
   ;;one is vec-forward, another -vec-forward
   planes
   edges))

(defun projection-matrix (result camera)
  (let ((half-fovy (* 0.5 (camera-fov camera)))
	(aspect (camera-aspect-ratio camera))
	(near (camera-frustum-near camera))
	(far (camera-frustum-far camera)))
    (let ((cot (/ (cos half-fovy)
		  (sin half-fovy))))
      (let ((sum (+ far near))
	    (difference (- near far)))
	;;[FIXME]necessary?
	(nsb-cga:%matrix result 
			 (* cot aspect) 0.0 0.0 0.0
			 0.0 cot 0.0 0.0
			 0.0 0.0 (/ sum difference) (/ (* 2.0 far near) difference)
			 0.0 0.0 -1.0 0.0)))))

(defun calculate-frustum-edge-vectors (camera)
  (let* ((half-fovx (* 0.5 (camera-fov camera)))
	 (half-fovy (* half-fovx (camera-aspect-ratio camera)))
	 (forward (camera-vec-forward camera))
	 (up (camera-cam-up camera))
	 (right (camera-cam-right camera)))
    (let ((x (nsb-cga:rotate-around up half-fovx))
	  (-x (nsb-cga:rotate-around up (- half-fovx)))
	  (y (nsb-cga:rotate-around right half-fovy))
	  (-y (nsb-cga:rotate-around right (- half-fovy))))
      (setf
       (camera-edges camera)
       (list
	;;forward
	;;(nsb-cga:vec* forward -1.0)
	(nsb-cga:transform-point forward (sb-cga:matrix* x y))
	(nsb-cga:transform-point forward (sb-cga:matrix* -x y))
	(nsb-cga:transform-point forward (sb-cga:matrix* -x -y))
	(nsb-cga:transform-point forward (sb-cga:matrix* x -y)))))))
#+nil
(nsb-cga:cross-product
 (nsb-cga:vec 1.0 0.0 0.0)
 (nsb-cga:vec 0.0 1.0 0.0))

(defun relative-lookat (result relative-target up)
  (let ((camright (sb-cga:cross-product up relative-target)))
    (sb-cga:%normalize camright camright)
    (let ((camup (sb-cga:cross-product relative-target camright)))
      (sb-cga:%normalize camup camup)
      (values
       (get-lookat result camright camup relative-target)
       camright
       camup))))

(defun get-lookat (result right up direction)
  (let ((rx (aref right 0))
	(ry (aref right 1))
	(rz (aref right 2))
	(ux (aref up 0))
	(uy (aref up 1))
	(uz (aref up 2))
	(dx (aref direction 0))
	(dy (aref direction 1))
	(dz (aref direction 2)))    
    (nsb-cga:%matrix
     result
     rx ry rz 0.0
     ux uy uz 0.0
     dx dy dz 0.0
     0.0 0.0 0.0 1.0)))

(defun update-matrices (camera)
  (let ((projection-matrix (camera-matrix-projection camera))
	(view-matrix (camera-matrix-view camera))
	(projection-view-matrix (camera-matrix-projection-view camera))
	(projection-view-player-matrix (camera-matrix-projection-view-player camera))
	(player-matrix (camera-matrix-player camera))
	(forward (camera-vec-forward camera))
	(up (camera-vec-up camera)))
    (projection-matrix projection-matrix camera)
    (multiple-value-bind (a right up)
	(relative-lookat view-matrix
			 (nsb-cga:vec* forward -1.0)
			 up)
      (declare (ignorable a))
      (setf (camera-cam-up camera) up
	    (camera-cam-right camera) right))
    (nsb-cga:%matrix* projection-view-matrix projection-matrix view-matrix)
    (let ((cev (camera-vec-noitisop camera))
	  (position (camera-vec-position camera)))
      (nsb-cga:%vec* cev position -1.0)
      (nsb-cga:%translate player-matrix cev))
    (nsb-cga:%matrix* projection-view-player-matrix projection-view-matrix player-matrix))
  (calculate-frustum-edge-vectors camera))


;;;
;;;
;;;
#+nil
(defun spec-projection-matrix (near far left right top bottom)
  (let ((near-2 (* 2 near))
	(top-bottom (- top bottom))
	(far-near (- far near)))
      (sb-cga:matrix
       (/ near-2 (- right left)) 0.0 (/ (+ right left) (- right left)) 0.0
       0.0 (/ near-2 top-bottom) (/ (+ top bottom) top-bottom) 0.0
       0.0 0.0 (- (/ (+ far near) far-near)) (/ (* -2 far near) far-near)
       0.0 0.0 -1.0 0.0)))
