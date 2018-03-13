(defpackage #:camat
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
(in-package #:camat)

(defstruct camera
  (vec-position (%sb-cga:vec 0.0 0.0 0.0) :type %sb-cga:vec)

  (vec-up (%sb-cga:vec 0.0 1.0 0.0) :type %sb-cga:vec)
  (vec-forward (%sb-cga:vec 1.0 0.0 0.0) :type %sb-cga:vec)

  (vec-noitisop (%sb-cga:vec 0.0 0.0 0.0) :type %sb-cga:vec) ;;;the negative of position
  (matrix-player (%sb-cga:identity-matrix)) ;;positional information of camera
  (matrix-view (%sb-cga:identity-matrix))		    ;;view matrix
  (matrix-projection (%sb-cga:identity-matrix))	    ;;projection matrix
  (matrix-projection-view (%sb-cga:identity-matrix)) ;;projection * view matrix
  (matrix-projection-view-player (%sb-cga:identity-matrix))
  
  (fov (coerce (/ pi 2.0) 'single-float) :type single-float)

  (aspect-ratio 1.0 :type single-float)
  (frustum-near 0.0078125 :type single-float)
  (frustum-far 128.0 :type single-float))

(defun projection-matrix (result camera)
  (let ((fovy (camera-fov camera))
	(aspect (camera-aspect-ratio camera))
	(near (camera-frustum-near camera))
	(far (camera-frustum-far camera)))
    (let ((cot (/ (cos (/ fovy 2.0))
		  (sin (/ fovy 2.0)))))
      (let ((sum (+ far near))
	    (difference (- near far)))
	(%sb-cga:%matrix result 
			   (/ cot aspect) 0.0 0.0 0.0
			   0.0 cot 0.0 0.0
			   0.0 0.0 (/ sum difference) (/ (* 2.0 far near) difference)
			   0.0 0.0 -1.0 0.0)))))

(defun relative-lookat (result relative-target up)
  (let ((camright (%sb-cga:cross-product up relative-target)))
    (declare (dynamic-extent camright))
    (%sb-cga:%normalize camright camright)
    (let ((camup (%sb-cga:cross-product relative-target camright)))
      (declare (dynamic-extent camup))
      (get-lookat result
		  camright
		  camup
		  relative-target))))

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
    (%sb-cga:%matrix result
     rx ry rz 0.0
     ux uy uz 0.0
     dx dy dz 0.0
     0.0 0.0 0.0 1.0)))


;;#b100 - projection
;;#b010 - rotation
;;#b001 - translation

(defun update-matrices (camera)
  (let ((flags #b111))
    (let ((translation? #b001)
	  (rotation? #b010)
	  (projection? #b100))
      (let ((projection-matrix (camera-matrix-projection camera))
	    (view-matrix (camera-matrix-view camera))
	    (projection-view-matrix (camera-matrix-projection-view camera))
	    (projection-view-player-matrix (camera-matrix-projection-view-player camera))
	    (player-matrix (camera-matrix-player camera))
	    (forward (camera-vec-forward camera))
	    (up (camera-vec-up camera)))
	(when (logtest projection? flags)
	  (projection-matrix projection-matrix camera))
	(when (logtest rotation? flags)
	  (relative-lookat view-matrix forward up))
	(let ((num (logior rotation? projection?)))
	  (when (logtest num flags)
	    (%sb-cga:%matrix* projection-view-matrix projection-matrix view-matrix)))
	(when (logtest translation? flags)
	  (%sb-cga:%translate player-matrix (camera-vec-noitisop camera)))
	(let ((num (logior rotation? projection? translation?)))
	  (when (logtest num flags)
	    (%sb-cga:%matrix* projection-view-player-matrix projection-view-matrix player-matrix)))))))


;;;
;;;
;;;
#+nil
(defun spec-projection-matrix (near far left right top bottom)
  (let ((near-2 (* 2 near))
	(top-bottom (- top bottom))
	(far-near (- far near)))
      (%sb-cga:matrix
       (/ near-2 (- right left)) 0.0 (/ (+ right left) (- right left)) 0.0
       0.0 (/ near-2 top-bottom) (/ (+ top bottom) top-bottom) 0.0
       0.0 0.0 (- (/ (+ far near) far-near)) (/ (* -2 far near) far-near)
       0.0 0.0 -1.0 0.0)))
