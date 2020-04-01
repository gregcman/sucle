(in-package :sucle)
;;;;************************************************************************;;;;
(defun pos-to-block-aabb (x y z)
  (let ((the-block (world:getblock x y z)))
    (block-to-block-aabb the-block)))
(defun block-to-block-aabb (blockid)
  (declare (ignore blockid))
  ;;FIXME :use defmethod on objects?
  *block-aabb*)

;;;;

(struct-to-clos:struct->class
 (defstruct fist
   (selected-block (vector 0 0 0))
   (normal-block (vector 0 0 0))
   (exists nil)
   (position (vector 0 0 0))))

(defun standard-fist (px py pz vx vy vz &optional (fist (make-fist)))
  (multiple-value-bind (xyzclamp frac x y z)
      (fist-trace px py pz vx vy vz)
    (cond ((= #b000 xyzclamp)
	   ;;The raycasted fist did not run into anything solid.
	   (setf (fist-exists fist) nil))
	  (t
	   (macrolet ((setvec3d (vec x y z)
			(let ((a (gensym)))
			  `(let ((,a ,vec))
			     (setf (aref ,a 0) ,x
				   (aref ,a 1) ,y
				   (aref ,a 2) ,z)))))
	     (let ((a (+ px (* frac vx)))
		   (b (+ py (* frac vy)))
		   (c (+ pz (* frac vz))))
	       ;;The block that it is collided with
	       (setvec3d (fist-selected-block fist) x y z)
	       ;;The resulting location of the fist
	       (setvec3d (fist-position fist) a b c)
	       (let ((dx 0)
		     (dy 0)
		     (dz 0))
		 ;;Only choose one direction, don't have a fist
		 ;;end up on the corner!!
		 (cond ((logtest xyzclamp #b100)
			(setf dx (if (plusp vx) 1 -1)) 0)
		       ((logtest xyzclamp #b010)
			(setf dy (if (plusp vy) 1 -1)) 0)
		       ((logtest xyzclamp #b001)
			(setf dz (if (plusp vz) 1 -1)) 0))
		 (setvec3d (fist-normal-block fist)
			   (- x dx)
			   (- y dy)
			   (- z dz)))))
	   (setf (fist-exists fist) t))))
  fist)

(defun fist-trace (px py pz vx vy vz &optional (aabb *fist-aabb*))
  (block first-block
    (aabbcc:aabb-collect-blocks (px py pz vx vy vz aabb)
	(x y z contact)
      (declare (ignorable contact))
      (when (block-data:data (world:getblock x y z) :hard)
	(multiple-value-bind (minimum type)
	    (aabbcc:aabb-collide
	     aabb
	     px py pz
	     (pos-to-block-aabb x y z)
	     x y z
	     vx vy vz)
	  (declare (ignorable minimum))
	  (unless (zerop type)
	    (return-from first-block (values type minimum x y z))))))
    #b000))

;; ;;;;</PHYSICS>
;; ;;;;************************************************************************;;;;
