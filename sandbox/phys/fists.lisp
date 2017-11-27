(in-package :sandbox)


(defstruct fister
  (selected-block (vector 0 0 0))
  (normal-block (vector 0 0 0))
  (exists nil)
  (position (vector 0 0 0))
  fun)

(defmacro setvec3d (vec x y z)
  (let ((a (gensym)))
    `(let ((,a ,vec))
       (setf (aref ,a 0) ,x
	     (aref ,a 1) ,y
	     (aref ,a 2) ,z))))

(defun standard-fist (fist px py pz vx vy vz)
  (multiple-value-bind (frac xclamp yclamp zclamp)
      (funcall (fister-fun fist) px py pz vx vy vz)
    (if (or xclamp yclamp zclamp)
	(progn
	  (let ((a (+ px (* frac vx)))
		(b (+ py (* frac vy)))
		(c (+ pz (* frac vz))))
	    (let ((dx (if xclamp (if (plusp vx) 1 -1) 0))
		  (dy (if yclamp (if (plusp vy) 1 -1) 0))
		  (dz (if zclamp (if (plusp vz) 1 -1) 0)))
	      (setvec3d (fister-selected-block fist)
			(floor (+ dx a))
			(floor (+ dy b))
			(floor (+ dz c))))
	    (setvec3d (fister-position fist)
		      a 
		      b
		      c)
	    (setvec3d (fister-normal-block fist)
		      (floor a) 
		      (floor b)
		      (floor c)))
	  (setf (fister-exists fist) t))
	(setf (fister-exists fist) nil))))

(defun use-fist (fist left-p right-p left-fun right-fun)
  (let ((fist? (fister-exists fist))
	(selected-block (fister-selected-block fist))
	(normal-block (fister-normal-block fist)))
    (when fist?
      (when left-p
	(funcall left-fun
		 (aref selected-block 0)
		 (aref selected-block 1)
		 (aref selected-block 2)))
      (when right-p
	(funcall right-fun
		 (aref normal-block 0)
		 (aref normal-block 1)
		 (aref normal-block 2))))))
