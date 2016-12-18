(in-package :sandbox)

(defparameter lastw nil)
(defparameter wprev most-negative-fixnum)
(defparameter wpressprev nil)
(defparameter isprinting nil)

(defun controls ()
  (let ((camera ourcam))
    (mat:add!
     cameraVelocity
     (keymovement camera))
    (progn
      (if (in:key-p :w)
	 ;;if it was pressed last round
	 (progn
	   (if (not wpressprev)
	       (progn
		 (if (> 150 (- (get-internal-run-time) wprev))
		     (setf isprinting t))))
	   (setf wpressprev t))
	 (progn
	   (setf isprinting nil)
	   (if wpressprev
	       (progn
		 (setf wprev (get-internal-run-time))
		 (setf wpressprev nil))))))
    
    (if (in:key-p :left-shift)
	(progn
	  (setf isprinting nil)
	  (setf isneaking t))
	(setf isneaking nil))

    (in:key-pressed-hook #\h (lambda () (someseq
			    (floor (row-major-aref (simplecam-pos camera) 0) 16)
			    (floor (row-major-aref (simplecam-pos camera) 2) 16))))

    (if (in:key-pressed-p :z)
	(aplatform
	 (mat-world-pos (simplecam-pos camera))
	 2))
    (if (in:key-pressed-p :x)
	(progn
	  (notaplatform (mat-world-pos (simplecam-pos camera)))
	  (notaplatform (vecadd (mat-world-pos (simplecam-pos camera)) (vector 0 1 0)))))
    (if (in:key-pressed-p :c) 
	(oneplatform
	 (mat-world-pos (simplecam-pos camera))
	 91))))

(defun mouse-looking (camera)
  (let* ((change (in:delta))
	 (x (* 1.25 1/360 (aref change 0)))
	 (y (* 1.25 1/360 (aref change 1))))
    (setf
     (simplecam-yaw camera)
     (mod (+ (simplecam-yaw camera) x) (* 2 pi)))
    (setf (simplecam-pitch camera)
	  (anothershit
	   (+ (simplecam-pitch camera) y) (/ pi 2)))))

(defun anothershit (x whatthefuck)
  "used to clamp the pitch"
  (if (> x whatthefuck)
      whatthefuck
      (if (< x (- whatthefuck))
	  (- whatthefuck)
	  x)))

(defun good-func (some-number)
  "maps keys to vectors"
  (lambda (x)
    (if (in::akeydown (first x))
	(mat:add! some-number
		  (mat:onebyfour (second x))))))

(defun key-legs ()
  "keys for walking"
  (let* ((delta (empty-vec4))
	 (lemod (good-func delta)))
    (mapcar
     lemod
     '((:s ( 1  0  0  0))
       (:w (-1  0  0  0))
       (:a ( 0  0  1  0))
       (:d ( 0  0 -1  0))))
    (mat:scale! (mat:normalize! delta) (* 0.4 (expt tickscale 2)))
    (if isneaking
	(mat:scale! delta 0.2))
    (if isprinting
	(mat:scale! delta 1.3))
    (if (not onground)
	(mat:scale! delta 0.2))
    delta))

(defun key-jumps ()
  "keys for jumping"
  (let* ((delta (empty-vec4))
	 (lemod (good-func delta)))
    (if onground
	(mapcar
	 lemod
	 `((:space (0 ,(* 0.42 (expt tickscale 1)) 0 0)))))
    delta))

(defun keymovement (camera)
  "total keymovement"
  (mat:mmul! (mat:add (key-legs) (key-jumps))
	     (mat:rotation-matrix 0 1 0
				  (simplecam-yaw camera))))

(defun aplatform (pos blockid)
  (let ((i (elt pos 0))
	(j (elt pos 1))
	(k (elt pos 2)))
    (dotimes (a 3)
      (dotimes (b 3)
	(setblock-with-update (+ a i -1) (- j 1) (+ b k -1) blockid)))))

(defun oneplatform (pos blockid)
  (let ((i (elt pos 0))
	(j (elt pos 1))
	(k (elt pos 2)))
    (setblock-with-update (+ i) (- j 1) (+ k) blockid)))

(defun notaplatform (pos)
  (let ((i (elt pos 0))
	(j (elt pos 1))
	(k (elt pos 2)))
    (dotimes (a 3)
      (dotimes (b 3)
	(setblock-with-update (+ a i -1) (+ j) (+ b k -1) 0)))))
