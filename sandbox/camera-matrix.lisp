(in-package :sandbox)

(defun spec-projection-matrix (near far left right top bottom)
  (sb-cga:matrix
   (/ (+ near near) (- right left)) 0.0 (/ (+ right left) (- right left)) 0.0
   0.0 (/ (+ near near) (- top bottom)) (/ (+ top bottom) (- top bottom)) 0.0
   0.0 0.0 (- (/ (+ far near) (- far near))) (/ (* -2 far near) (- far near))
   0.0 0.0 -1.0 0.0))

(defun projection-matrix (fovy aspect near far)
  (let ((cot (/ (cos (/ fovy 2))
		(sin (/ fovy 2)))))
    (sb-cga:matrix 
     (/ cot aspect) 0.0 0.0 0.0
     0.0 cot 0.0 0.0
     0.0 0.0 (/ (+ far near) (- near far)) (/ (* -2 far near) (- far near))
     0.0 0.0 -1.0 0.0)))

(defun relative-lookat (eye relative-target up)
  (let ((camright (sb-cga:cross-product up relative-target)))
    (let ((camup (sb-cga:cross-product relative-target camright)))
      (get-lookat (sb-cga:normalize camright)
		  (sb-cga:normalize camup)
		  relative-target eye))))

(defun get-lookat (right up direction position)
  (let ((rx (aref right 0))
	(ry (aref right 1))
	(rz (aref right 2))
	(ux (aref up 0))
	(uy (aref up 1))
	(uz (aref up 2))
	(dx (aref direction 0))
	(dy (aref direction 1))
	(dz (aref direction 2))
	(px (aref position 0))
	(py (aref position 1))
	(pz (aref position 2)))    
    (sb-cga:matrix*
     (sb-cga:matrix
      rx ry rz 0.0
      ux uy uz 0.0
      dx dy dz 0.0
      0.0 0.0 0.0 1.0)
     (sb-cga:matrix
      1.0 0.0 0.0 (- px)
      0.0 1.0 0.0 (- py)
      0.0 0.0 1.0 (- pz)
      0.0 0.0 0.0 1.0))))

(defun unit-pitch-yaw (pitch yaw)
  (let ((cos-pitch (cos pitch)))
    (sb-cga:vec
     (* cos-pitch (cos yaw))
     (sin pitch)
     (* cos-pitch (sin yaw)))))
