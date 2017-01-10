(in-package :sandbox)

(defun spec-projection-matrix (near far left right top bottom)
  (let ((near-2 (* 2 near))
	(top-bottom (- top bottom))
	(far-near (- far near)))
      (sb-cga:matrix
       (/ near-2 (- right left)) 0.0 (/ (+ right left) (- right left)) 0.0
       0.0 (/ near-2 top-bottom) (/ (+ top bottom) top-bottom) 0.0
       0.0 0.0 (- (/ (+ far near) far-near)) (/ (* -2 far near) far-near)
       0.0 0.0 -1.0 0.0)))

(defun projection-matrix (fovy aspect near far)
  (let ((cot (/ (cos (/ fovy 2))
		(sin (/ fovy 2)))))
    (let ((sum (+ far near))
	  (difference (- near far)))
      (sb-cga:matrix 
       (/ cot aspect) 0.0 0.0 0.0
       0.0 cot 0.0 0.0
       0.0 0.0 (/ sum difference) (/ (* 2 far near) difference)
       0.0 0.0 -1.0 0.0))))

(defun relative-lookat (relative-target up)
  (let ((camright (sb-cga:cross-product up relative-target)))
    (let ((camup (sb-cga:cross-product relative-target camright)))
      (get-lookat (sb-cga:normalize camright)
		  (sb-cga:normalize camup)
		  relative-target))))

(defun get-lookat (right up direction)
  (let ((rx (aref right 0))
	(ry (aref right 1))
	(rz (aref right 2))
	(ux (aref up 0))
	(uy (aref up 1))
	(uz (aref up 2))
	(dx (aref direction 0))
	(dy (aref direction 1))
	(dz (aref direction 2)))    
    ;(sb-cga:matrix*)
    (sb-cga:matrix
     rx ry rz 0.0
     ux uy uz 0.0
     dx dy dz 0.0
     0.0 0.0 0.0 1.0)))
