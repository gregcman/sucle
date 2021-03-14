(in-package :sucle)

(defun b@ (x y z)
  (voxel-chunks:getobj x y z))
#+nil
(defun (setf b@) (value x y z)
  (setf (voxel-chunks:getobj x y z) value))
;;constrained
(defun (setf b@) (value x y z)
  (setf (voxel-chunks:getobj (mod x 512) (mod y 512) (mod z 512)) value))

(defun wools (xn yn zn)
  (dotimes (x 16)
    (dotimes (y 16)
      (setf (b@ (+ xn x) (+ 0 yn) (+ zn y))
	    (wrap (+ (wrap y 1 3)
		     (* (wrap x 7 15) 16))
		  0 256)))))

(setf *fun* 'wools)

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
	 (setf (b@ x (floor (* 10 value)) z) 177)))

(defun perlin (&key (pairs '((0.1 0.1 10.0)))
		 (x 512)
		 (z 512)
		 (h 64)
		 (b 177)
		 (overwrite t)
		 (offset 0))
  (dobox ((x 0 x)
	  (z 0 z))
	 (let ((value 0.0)
	       (xf (floatify x))
	       (zf (floatify z)))
	   (dolist (item pairs)
	     (destructuring-bind (xs zs scale) item
	       (incf value
		     (* scale
			(black-tie:perlin-noise-sf (* xs xf) 0.0 (* zs zf))))))
	   (dotimes (y h)
	     (if (< (+ offset y) value)
		 (setf (b@ x y z) b)
		 (when overwrite
		   (setf (b@ x y z) 0)))))))

(perlin :pairs '((0.1 0.1 10.0)
		 (0.1 0.1 10.0)
		 (0.1 0.1 10.0)
		 (0.1 0.1 10.0)))

(perlin :pairs '((0.05 0.05 10.0)
		 (0.005 0.005 100.0)
		 ;;(0.1 0.1 10.0)
		 ;;(0.1 0.1 10.0)
		 ))

(perlin :pairs '(;;(0.5 0.5 2)
		 ;;(0.25 0.25 4)
		 ;;(0.125 0.125 8)
		 (0.e0625 0.0625 16)
		 (0.03125 0.03125 32)
		 ;;(0.5 0.5 64)
		 )
	:b 145)
(perlin :pairs '((0.0 0.0 1))
	:overwrite nil
	:offset -1)


(voxel-chunks:clearworld)

(dobox ((x -128 128)
	(z -128 128)
	(y -128 128))
       (setf (b@ x y z) 0))

(defun box (x0 x1 y0 y1 z0 z1 value)
  (dobox ((x x0 x1)
	  (z y0 y1)
	  (y z0 z1))
	 (setf (b@ x y z) value)))

(defun hollowbox (x0 x1 y0 y1 z0 z1 value)
  (dobox ((x x0 x1)
	  (y y0 y1)
	  (z z0 z1))
	 (let ((sides 0))
	   ;;(print (list x y z))
	   (when (or (= x x0) (= x (1- x1)))
	     (incf sides))
	   (when (or (= y y0) (= y (1- y1)))
	     (incf sides))
	   (when (or (= z z0) (= z (1- z1)))
	     (incf sides))
	   ;;(print sides)
	   (when (< 1 sides)
	     ;;(print 34)
	     (setf (b@ x y z) value)))))

(defun hollowcube (x y z)
  ;;(print 11)
  (let ((v (random 20)))
    (hollowbox x (+ x v)
	       y (+ y v)
	       z (+ z v)
	       1)))

(setf *fun* 'hollowcube)

;;r for radius
(defun circle (x y z &optional (v 113) (r 10))
  (dobox ((x0 (- x r) (+ x r))
	  (z0 (- z r) (+ z r)))
	 (when (> (* r r)
		  (+ (expt (- x0 x) 2)
		     (expt (- z0 z) 2)))
	   (setf (b@ x0 y z0) v))))

(setf *fun* 'circle)

(defun sphere (x y z &optional (v 114) (r 4))
  (dobox ((x0 (- x r) (+ x r))
	  (z0 (- z r) (+ z r))
	  (y0 (- y r) (+ y r)))
	 (when (> (* r r)
		  (+ (expt (- x0 x) 2)
		     (expt (- z0 z) 2)
		     (expt (- y0 y) 2)))
	   (setf (b@ x0 y0 z0) v))))

(setf *fun* 'sphere)

(defun detect-way ()
  (destructuring-bind (dx dy dz) *normals*
    ()))

(struct-to-clos:struct->class
 (defstruct Steps
   (past (make-array 5))
   (index 0)
   (size 5)))

(set-pprint-dispatch 'steps
		     (lambda (stream obj)
		       (format stream "~s" (steps-past obj))))

(defun add-steps (x y steps)
  (with-slots (index size past) steps
    (setf index
	  (mod (1+ index) size))
    (setf (aref past index)
	  (cons x y))))

(defun stairway (x y z &optional (v (b@ x y z)))
  (let ((max 40))
    (destructuring-bind (dx dy dz) *normals*
      (loop :named out :do
	 (progn
	   (decf max)
	   (when (> 0 max)
	     (return-from out))
	   (let ((rotation-attempts 0))
	     (tagbody
	      :top
		(let ((downx (+ x dx))
		      (downz (+ z dz)))
		  (flet ((rot ()
			   (setf (values dx dz) (rotate dx dz))))
		    (dotimes (i (random 4))
		      (rot))
		    (cond ((and (emptyp downx (- y 1) downz)
				(emptyp downx (+ y 0) downz)
				(emptyp downx (+ y 1) downz)
				(emptyp downx (+ y 2) downz))
			   (progn
			     (setf x downx
				   y (1- y)
				   z downz)
			     
			     (setf (b@ x y z) v)
			     (setf (b@ x (1- y) z) v)))
			  ;;try to rotate
			  (t
			   (rot)
			   (incf rotation-attempts)
			   (when (> 4 rotation-attempts)
			     (go :top)))))))))))))

(defun emptyp (x y z)
  (zerop (b@ x y z)))

(setf *fun* (lambda (x y z) (stairway x y z 129)))

(defun rotate (x y)
  (values (- y) x))

(in-package :voxel-chunks)
(conspack:defencoding voxel-chunks::chunk
  modified
  ;;last-saved
  type
  x y z
  key
  data
  hash
  
  ;;Invalidate a chunk. If used by the main cache to invalidate
  ;;chunks in chunk-array cursors.
  alive?

  ;;(last-read 0)
  last-modified
  ;;(last-access 0)
  empty-space
  empty-chunk-data)
(conspack:defencoding voxel-chunks::voxels empty-space empty-chunk-data main-cache cache)
(in-package :sucle)

(defun saveworld ()
  (sucle-serialize:save (merge-pathnames "/home/master/Documents/saves/"
					 (format nil "~s.cpack" (local-time:timestamp-to-unix
								 (local-time:now))))
			voxel-chunks::*voxels*)
  (values))

(defun loadworld ()
  (setf voxel-chunks::*voxels* (sucle-serialize:load "/home/master/Documents/saves/1615730702.cpack"))
  (values))

(setf *fun*
      (lambda (x y z &optional (val 103) (o 3) (g 6) (q 2))
	(dobox ((xi 0 g)
		(yi 0 g)
		(zi 0 g))
	       (when (and (> q (mod xi o))
			  (> q (mod yi o))
			  (> q (mod zi o)))
		 (let ((xb (+ x (* xi o)))
		       (yb (+ y (* yi o)))
		       (zb (+ z (* zi o))))
		   (dobox ((x xb (+ xb o))
			   (y yb (+ yb o))
			   (z zb (+ zb o)))
			  (setf (b@ x y z) val)))))))

(setf *fun*
      (lambda (x y z)
	(print (b@ x y z))))

(defun random-block ()
  (random 256))

(setf *fun*
      (lambda (x y z &optional (v (random-block)) (r 4) (% 0.7))
	(dobox ((x0 (- x r) (+ 1 x r))
		(z0 (- z r) (+ 1 z r)))
	       (let* ((what
		       (max 0 (+ (- (* % r))
				 (floor
				  (sqrt
				   (+ (expt (- x0 x) 2)
				      (expt (- z0 z) 2)))))))
		      (what (floor (expt what 2))))
		 (dotimes (i what)
		   (setf (b@ x0 (+ i y) z0) v))))))

(setf *fun*
      (lambda (x y z)
	(setf *block*
	      (b@ x y z))))

(setf *fun*
      (lambda (xn yn zn)
	(dotimes (x 16)
	  (dotimes (y 16)
	    (setf (b@ (+ xn x) (+ 0 yn) (+ zn y))
		  (wrap (+ (wrap y 0 16)
			   (* (wrap x 0 16) 16))
			0 256))))))

(setf *fun*
      (lambda (x y z)
	(sphere x y z 0 5)))

(setf *fun*
      (lambda (x y z &aux (v (b@ x y z)))
	(unless (= v 0)
	  (labels ((next (x y z)
		     (when (= (b@ x y z) v)
		       (setf (b@ x y z) 0)
		       (next (+ 1 x) (+ 0 y) (+ 0 z))
		       (next (+ -1 x) (+ 0 y) (+ 0 z))
		       (next (+ 0 x) (+ 1 y) (+ 0 z))
		       (next (+ 0 x) (+ -1 y) (+ 0 z))
		       (next (+ 0 x) (+ 0 y) (+ -1 z))
		       (next (+ 0 x) (+ 0 y) (+ 1 z)))))
	    (next x y z)))))

(load (merge-pathnames "extra/queue.lisp"
		       (asdf:system-source-directory :minisucle)))

;;get all blocks of a type and delete them
(setf *fun*
      (lambda (x y z &aux (v (b@ x y z)) (q (queue:make-uniq-q)))
	(unless (= v 0)
	  (labels ((add (x y z)
		     (when (= v (b@ x y z))
		       (queue:uniq-push (list x y z) q)))
		   (next (x y z)		     
		     (add (+ 1 x) (+ 0 y) (+ 0 z))
		     (add (+ -1 x) (+ 0 y) (+ 0 z))
		     (add (+ 0 x) (+ 1 y) (+ 0 z))
		     (add (+ 0 x) (+ -1 y) (+ 0 z))
		     (add (+ 0 x) (+ 0 y) (+ -1 z))
		     (add (+ 0 x) (+ 0 y) (+ 1 z)))
		   (del (x y z)
		     (setf (b@ x y z) 0)))
	    (next x y z)
	    (loop :named out :do
	       (multiple-value-bind (value p) (queue:uniq-pop q)
		 (when (not p)
		   (return-from out))
		 (apply #'del value)
		 (apply #'next value)))))))
