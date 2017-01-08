(in-package #:mat)

(defun fucking-projection-matrix (near far left right top bottom)
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

(defun get-lookat (right up direction position)
  (let* ((rx (row-major-aref right 0))
	 (ry (row-major-aref right 1))
	 (rz (row-major-aref right 2))
	 (ux (row-major-aref up 0))
	 (uy (row-major-aref up 1))
	 (uz (row-major-aref up 2))
	 (dx (row-major-aref direction 0))
	 (dy (row-major-aref direction 1))
	 (dz (row-major-aref direction 2))
	 (px (row-major-aref position 0))
	 (py (row-major-aref position 1))
	 (pz (row-major-aref position 2)))    
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

(defun absolute-lookat (eye target up)
  (let* ((camdirection (mat:subtract eye target))
	 (camright
	  (mat:normalize!
	   (mat:cross up camdirection)))
	 (camup
	  (mat:cross camdirection camright)))
    (mat:get-lookat
     camright camup camdirection eye)))

(defun easy-lookat (eye pitch yaw)
  (relative-lookat
   eye 
   (sb-cga:normalize
    (sb-cga:vec
     (* (cos pitch) (cos yaw))
     (sin pitch)
     (* (cos pitch) (sin yaw))))
   (sb-cga:vec 0.0 1.0 0.0)))

(defun relative-lookat (eye relative-target up)
  (let* ((camright
	  (sb-cga:normalize
	   (sb-cga:cross-product up relative-target)))
	 (camup
	  (sb-cga:normalize
	   (sb-cga:cross-product relative-target camright))))
    (mat:get-lookat
     camright camup relative-target eye)))
