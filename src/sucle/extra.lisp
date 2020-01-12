(in-package :sucle)

;;;;************************************************************************;;;;
;;;;<BRUSH>

;;

(defmacro with-xyz (&body body)
  `(let ((*x* *x*)
	 (*y* *y*)
	 (*z* *z*))
     ,@body))

(progn
  (defun b@ (&optional (x *x*) (y *y*) (z *z*))
    (world:getblock x y z))
  (defun (setf b@) (value &optional (x *x*) (y *y*) (z *z*))
    (world::plain-setblock x y z value)))

(defun b= (b0 b1)
  (eql b0 b1))

(defmacro nick (nickname)
  `(block-data::blockid ,nickname))

;;convert dirt, stone, and grass into their 'correct' forms given air:
;;grass, dirt, dirt, stone
(defun correct-earth (&rest rest)
  (declare (ignore rest))
  (let ((b0 (b@)))
    (when (or (b= (nick :stone) b0)
	      (b= (nick :grass) b0)
	      (b= (nick :dirt) b0))
      (let ((b1 (with-xyz
		  (incf *y* 1)
		  (b@))))
	(if (b= (nick :air) b1)
	    (setf (b@) (nick :grass))
	    (let ((b2 (with-xyz
			(incf *y* 2)
			(b@))))
	      (if (b= (nick :air) b2)
		  (setf (b@) (nick :dirt))
		  (let ((b3 (with-xyz
			      (incf *y* 3)
			      (b@))))
		    (if (b= (nick :air) b3)
			(setf (b@) (nick :dirt))
			(let (#+nil
			      (b3 (with-xyz
				    (incf *y* 4)
				    (b@))))
			  (setf (b@) (nick :stone))))))))))))

(defun correct-earth (&rest rest)
  (declare (ignore rest))
  (let ((b0 (b@)))
    (when (b= (nick :air) b0)
      (setf (b@) (nick :lamp)))))
(defun correct-earth (&rest rest)
  (declare (ignore rest))
  (let ((b0 (b@)))
    (when (b= (nick :air) b0)
      (when 
	;;#+nil
	(<= 3 (neighbors))
	(setf (b@) (nick :sandstone))))))

(defun neighbors (&aux (count 0))
  (dobox ((x (+ -1 *x*) (+ 2 *x*))
	  (y (+ -1 *y*) (+ 2 *y*))
	  (z (+ -1 *z*) (+ 2 *z*)))
	 (unless (or (= x 0))
	   (b= (b@ x y z) (nick :air))
	   (incf count)))
  count)

(defun player-feet ()
  (let ((miny
	 (aabbcc::aabb-miny
	  (entity-aabb *ent*))))
    (with-vec (x y z) ((player-position))
      (values (floor x)
	      (1- (floor (+ miny y)))
	      (floor z)))))

(defun neighbors (&aux (count 0))
  (flet ((countw ()
	   (unless (b= (b@) (nick :air))
	     (incf count))))
    (progn
      (with-xyz
	(incf *x*)
	(countw))
      (with-xyz
	(decf *x*)
	(countw)))
    (progn
      (with-xyz
	(incf *y*)
	(countw))
      (with-xyz
	(decf *y*)
	(countw)))
    (progn
      (with-xyz
	(incf *z*)
	(countw))
      (with-xyz
	(decf *z*)
	(countw))))
  count)

;;

(defun player-feet-at (&rest rest)
  (declare (ignorable rest))
  (multiple-value-bind (x y z) (player-feet)
    ;;(print (list x y z))
    (dobox ((x (+ x -1) (+ x 2))
	    (z (+ z -1) (+ z 2)))
	   (setf (b@ x y z) (nick :grass)))))

(defun line (px py pz &optional
			(vx *x*)
			(vy *y*)
			(vz *z*)
	       (blockid *blockid*)
	       (aabb *fist-aabb*))
  (aabbcc::aabb-collect-blocks ((+ 0.5 px)
				(+ 0.5 py)
				(+ 0.5 pz)
				(- vx px)
				(- vy py)
				(- vz pz)
				aabb)
      (x y z dummy)
    (declare (ignore dummy))
    (when (b= (nick :air) (b@ x y z))
      (world::plain-setblock x y z blockid))))

(defun degree-to-rad (&optional (n (random 360)))
  (* n (load-time-value (floatify (/ pi 180)))))
(defun rotate-normal (&optional
			(x (degree-to-rad))
			(y (degree-to-rad))
			(z (degree-to-rad)))
  (sb-cga:transform-point
   (sb-cga::vec 1.0 0.0 0.0)
   (sb-cga::rotate* x y z)))
(defun vec-values (&optional (vec (sb-cga::vec 1.0 2.0 3.0)))
  (with-vec (x y z) (vec)
    (values x y z)))
;;
(defun tree (&optional (x *x*) (y *y*) (z *z*) (minfactor 6.0))
  (labels ((rec (place minfactor)
	     (when (>= minfactor 0)
	       (dotimes (x (random 5))
		 (let ((random-direction (sb-cga::vec* (rotate-normal) (expt 1.5 minfactor))))
		   (let ((new (sb-cga::vec+ place random-direction)))
		     (multiple-value-call
			 'line
		       (vec-values place)
		       (vec-values new)
		       (if (>= 4 minfactor)
			   (nick :leaves)
			   (nick :log))
		       (create-aabb (* 0.1 minfactor)))
		     (rec new (1- minfactor))))))))
    (rec (multiple-value-call 'sb-cga::vec (floatify2 x y z))
	 minfactor)))
(defun floatify2 (&rest values)
  (apply 'values (mapcar 'floatify values)))
;;

(defun line-to-player-feet (&rest rest)
  (declare (ignorable rest))
  (multiple-value-bind (x y z) (player-feet)
    ;;(print (list x y z))
    (line x
	  y
	  z
	  *x*
	  *y*
	  *z*
	  (nick :glass;:planks
		))))
(defun get-chunk (x y z)
  (multiple-value-bind (x y z) (voxel-chunks::chunk-coordinates-from-block-coordinates x y z)
    ;;FIXME::use actual chunk dimensions, not magic number 16
    (values (* x 16)
	    (* y 16)
	    (* z 16))))
(defun background-generation (key)
  (let ((job-key (cons :world-gen key)))
    (sucle-mp::submit-unique-task
     job-key
     ((lambda ()
	(generate-for-new-chunk key))
      :callback (lambda (job-task)
		  (declare (ignore job-task))
		  (world::dirty-push-around key)
		  (sucle-mp::remove-unique-task-key job-key))))))

(utility:with-unsafe-speed
  (defun generate-for-new-chunk (key)
    (multiple-value-bind (x y z) (voxel-chunks::unhashfunc key)
      (declare (type fixnum x y z))
      ;;(print (list x y z))
      (when (>= y -1)
	(dobox ((x0 x (the fixnum (+ x 16)))
		(y0 y (the fixnum (+ y 16)))
		(z0 z (the fixnum (+ z 16))))
	       (let ((block (let ((threshold (/ y 512.0)))
			      (if (> threshold (black-tie::perlin-noise-single-float
						(* x0 0.05)
						(+ (* 1.0 (sin y0)) (* y0 0.05))
						(* z0 0.05)))
				  0
				  1))))
		 (setf (voxel-chunks:getobj x0 y0 z0)
		       (world:blockify block
				       (case block
					 (0 15)
					 (1 0))
				       0))))))))

#+nil
(defun 5fun (x y z)
  (multiple-value-bind (x y z) (get-chunk x y z)
    (dobox ((0x (- x 16) (+ x 32) :inc 16)
	    (0y (- y 16) (+ y 32) :inc 16)
	    (0z (- z 16) (+ z 32) :inc 16))
	   (background-generation (multiple-value-call
				      'voxel-chunks::create-chunk-key
				    (voxel-chunks::chunk-coordinates-from-block-coordinates 
				     0x
				     0y
				     0z))))))
#+nil
(defun 5fun (x y z)
  (loop :repeat 10 :do
     (let ((newx x)
	   (newy y)
	   (newz z))
       (progn
	 (let ((random (random 3))
	       (random2 (- (* 2 (random 2)) 1)))
	   (case random
	     (0 (incf newx (* random2 3)))
	     (1 (incf newy (* random2 3)))
	     (2 (incf newz (* random2 3)))))
	 (line
	  x y z
	  newx newy newz
	  (nick :gravel))
	 (setf x newx
	       y newy
	       z newz)))))

(defun 5fun (x y z)
  ;;put a layer of grass on things
  (around (lambda (x y z)
	    (when (and (b= (nick :air) (b@ x y z))
		       (not (b= (nick :air) (b@ x (1- y) z)))
		       (not (b= (nick :grass) (b@ x (1- y) z))))
	      (setf (b@ x y z) (nick :grass))))
	  x y z))

(defun around (fun x y z)
  (let ((radius 5))
    (dobox ((0x (- x radius) (+ x radius 1))
	    (0y (- y radius) (+ y radius 1))
	    (0z (- z radius) (+ z radius 1)))
	   (funcall fun 0x 0y 0z))))

;;;;</BRUSH>
;;;;************************************************************************;;;;

;;;;************************************************************************;;;;
;;;;<AUDIO>

#+nil
(defun particle-al-listener (particle)
  (let ((pos (pointmass-position particle))
	(vel (pointmass-velocity particle)))
    (al:listener :position pos)
    (al:listener :velocity vel)
    ))
#+nil
(defun camera-al-listener (camera)
  (let ((look (camera-matrix::camera-vec-forward camera))
	(up (camera-matrix::camera-vec-up camera)))   
    (cffi:with-foreign-object (array :float 6)
      (let ((count 0))
	(flet ((add (x)
		 (setf (cffi:mem-aref array :float count)
		       x)
		 (incf count)))
	  (with-vec (x y z) (look)
	    (add (- x))
	    (add (- y))
	    (add (- z)))
	  (with-vec (x y z) (up)
	    (add x)
	    (add y)
	    (add z))))
      (%al:listener-fv :orientation array)
      )))

  #+nil
  (progn
    (particle-al-listener (entity-particle *ent*))
    (camera-al-listener *camera*))
;;;;</AUDIO>
;;;;************************************************************************;;;;
