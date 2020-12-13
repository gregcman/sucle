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
    (vocs::pen-get x y z))
  (defun (setf b@) (value &optional (x *x*) (y *y*) (z *z*))
    (setf (vocs::pen-get x y z) value)))

(defun b= (b0 b1)
  (eql b0 b1))

(defmacro nick (nickname)
  `(block-data:lookup ,nickname))

(progn
  (defun b2@ (&optional (x *x*) (y *y*) (z *z*))
    (ldb (byte 8 0) (b@ x y z))))

;;convert dirt, stone, and grass into their 'correct' forms given air:
;;grass, dirt, dirt, stone
(defun %correct-earth (&rest rest)
  (declare (ignore rest))
  (let ((b0 (b2@)))
    (when (or (b= (nick :stone) b0)
	      (b= (nick :grass) b0)
	      (b= (nick :dirt) b0))
      (let ((b1 (with-xyz
		  (incf *y* 1)
		  (b2@))))
	(if (b= (nick :air) b1)
	    (setf (b@) (nick :grass))
	    (let ((b2 (with-xyz
			(incf *y* 2)
			(b2@))))
	      (if (b= (nick :air) b2)
		  (setf (b@) (nick :dirt))
		  (let ((b3 (with-xyz
			      (incf *y* 3)
			      (b2@))))
		    (if (b= (nick :air) b3)
			(setf (b@) (nick :dirt))
			(let (#+nil
			      (b3 (with-xyz
				    (incf *y* 4)
				    (b@))))
			  (setf (b@) (nick :stone))))))))))))

(defun correct-earth (x y z)
  (let ((*x* x)
	(*y* y)
	(*z* z))
    (%correct-earth)))
#+nil
(defun correct-earth (&rest rest)
  (declare (ignore rest))
  (let ((b0 (b@)))
    (when (b= (nick :air) b0)
      (setf (b@) (nick :lamp)))))
#+nil
(defun correct-earth (&rest rest)
  (declare (ignore rest))
  (let ((b0 (b@)))
    (when (b= (nick :air) b0)
      (when 
	;;#+nil
	(<= 3 (neighbors))
	(setf (b@) (nick :sandstone))))))
;;#+nil
(defun neighbors (&aux (count 0))
  (dobox ((x (+ -1 *x*) (+ 2 *x*))
	  (y (+ -1 *y*) (+ 2 *y*))
	  (z (+ -1 *z*) (+ 2 *z*)))
	 (unless (or (= x 0))
	   (b= (b@ x y z) (nick :air))
	   (incf count)))
  count)
(defun adjacent-neighbors (x y z)
  (let ((tot 0))
    (macrolet ((aux (i j k)
		 `(unless (b= (nick :air)
			      (world::getblock (+ x ,i) (+ y ,j) (+ z ,k)))
		    (incf tot))))
      (aux 1 0 0)
      (aux -1 0 0)
      (aux 0 1 0)
      (aux 0 -1 0)
      (aux 0 0 1)
      (aux 0 0 -1))
    tot))

(defun player-feet ()
  (let ((miny
	 (aabbcc:aabb-miny
	  (entity-aabb *ent*))))
    (with-vec (x y z) ((entity-position *ent*))
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

(setf sucle::*5-fist* 'box-at)

(defun player-feet-at (&rest rest)
  (declare (ignorable rest))
  (multiple-value-bind (x y z) (player-feet)
    ;;(print (list x y z))
    (dobox ((x (+ x -1) (+ x 2))
	    (z (+ z -1) (+ z 2)))
	   (setf (b@ x y z) (nick :grass)))))

(setf sucle::*5-fist* 'player-feet-at)

(defparameter *size* 4)
(defun box-at (&optional
		 (x *x*)
		 (y *y*)
		 (z *z*))
  (dobox ((x (+ x -1) (+ x 2))
	  (z (+ z -1) (+ z 2)))
	 (setf (b@ x y z) (nick :grass))))

(setf sucle::*5-fist* 'box-tower2345)
(defun box-tower2345 (&optional
			(px *x*)
			(y *y*)
			(pz *z*)
			&aux (shift 2))
  (dotimes (dy 50)
    (box-at px (+ dy y) pz)
    (incf px (rir shift))
    (incf pz (rir shift))))

(defun random-in-range (n)
  (- (random (+ n n 1)) n))
(defun rir (n)
  (random-in-range n))

(setf sucle::*5-fist* 'test456)
(defun test456 ()
  (mvc 'line (spread (entity-position *ent*))))
(defun line (px py pz &optional
			(vx *x*)
			(vy *y*)
			(vz *z*)
	       (blockid *blockid*)
	       (aabb *fist-aabb*))
  (aabbcc:aabb-collect-blocks ((+ 0.5 px)
				(+ 0.5 py)
				(+ 0.5 pz)
				(- vx px)
				(- vy py)
				(- vz pz)
				aabb)
      (x y z dummy)
    (declare (ignore dummy))
    (when (b= (nick :air) (b@ x y z))
      (world:plain-setblock x y z blockid))))

(defun degree-to-rad (&optional (n (random 360)))
  (* n (load-time-value (floatify (/ pi 180)))))
(defun rotate-normal (&optional
			(x (degree-to-rad))
			(y (degree-to-rad))
			(z (degree-to-rad)))
  (sb-cga:transform-point
   (sb-cga:vec 1.0 0.0 0.0)
   (sb-cga:rotate* x y z)))
(defun vec-values (&optional (vec (sb-cga:vec 1.0 2.0 3.0)))
  (with-vec (x y z) (vec)
    (values x y z)))
(setf *5-fist* 'tree)
(defun tree (&optional (x *x*) (y *y*) (z *z*) (minfactor 6.0))
  (labels ((rec (place minfactor)
	     (when (>= minfactor 0)
	       (dotimes (x (random 5))
		 (let ((random-direction (sb-cga:vec* (rotate-normal) (expt 1.5 minfactor))))
		   (let ((new (sb-cga:vec+ place random-direction)))
		     (multiple-value-call
			 'line
		       (vec-values place)
		       (vec-values new)
		       (if (>= 4 minfactor)
			   (nick :leaves)
			   (nick :log))
		       (create-aabb (* 0.1 minfactor)))
		     (rec new (1- minfactor))))))))
    (rec (multiple-value-call 'sb-cga:vec (floatify2 x y z))
	 minfactor)))
(defun floatify2 (&rest values)
  (apply 'values (mapcar 'floatify values)))

(defun rotate-random (vec &optional (n 4.0))
  (let* ((v (rotate-normal))
	 (c (nsb-cga:cross-product v vec)))
    (sb-cga:transform-point
     vec
     (nsb-cga:rotate-around c (floatify (/ pi n))))))

(setf *5-fist* 'tree3)
(defun tree3 (&optional (x *x*) (y *y*) (z *z*) (minfactor 6.0))
  (labels ((rec (place minfactor direction)
	     (when (>= minfactor 0)
	       (dotimes (x (+ 1 (random 2)))
		 (let* ((random-direction (rotate-random direction 8.0))
			;;Random direction scaled
			(rds (nsb-cga:vec* random-direction
					   (expt 1.4 minfactor))))
		   (let ((new (sb-cga:vec+ place rds)))
		     (multiple-value-call
			 ;;(lambda (&rest rest) (print rest))
			 'line
		       (vec-values place)
		       (vec-values new)
		       (if (>= 4 minfactor)
			   (nick :leaves)
			   (nick :log))
		       (create-aabb 0.1 ;;(* 0.1 minfactor)
				    ))
		     (rec new (1- minfactor) random-direction)))))))
    (rec (multiple-value-call 'sb-cga:vec (floatify2 x y z))
	 minfactor
	 (nsb-cga:vec 0.0 1.0 0.0))))

(setf *5-fist* 'tree35)
(defun tree35 (&optional (x *x*) (y *y*) (z *z*) (minfactor 10.0))
  (labels ((rec (place minfactor direction width)
	     (when (>= minfactor 0)
	       (let* ((random-direction direction ;;(rotate-random direction 800.0)
			)
		      ;;Random direction scaled
		      (rds (nsb-cga:vec* random-direction 3.0
					 #+nil
					 (expt 1.4 minfactor))))
		 (let ((new (sb-cga:vec+ place rds)))
		   (multiple-value-call
		       ;;(lambda (&rest rest) (print rest))
		       'line
		     (vec-values place)
		     (vec-values new)
		     (if (>= 4 minfactor)
			 (nick :leaves)
			 (nick :log))
		     (create-aabb (/ width 5.0) 1.0 (/ width 5.0)))
		   (rec new (1- minfactor) random-direction (max 0.1 (+ width (random-in-range 3)))))))))
    (rec (multiple-value-call 'sb-cga:vec (floatify2 x y z))
	 minfactor
	 (nsb-cga:vec 0.0 1.0 0.0)
	 10.0)))
;;
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
	  (nick :grass
		))))
(defun get-chunk (x y z)
  (multiple-value-bind (x y z) (vocs::bcoord->ccoord x y z)
    ;;[FIXME]use actual chunk dimensions, not magic number 16
    (values (* x 16)
	    (* y 16)
	    (* z 16))))
(defun background-generation (key)
  (let ((job-key (cons :world-gen key)))
    (sucle-mp:submit-unique-task
     job-key
     ((lambda ()
	(generate-for-new-chunk key))
      :callback (lambda (job-task)
		  (declare (ignore job-task))
		  (world:dirty-push-around key)
		  (sucle-mp:remove-unique-task-key job-key))))))

(defun octave (x y z xscale yscale zscale total-scale)
  (floor (* total-scale
	    (black-tie:perlin-noise-single-float
	     (floatify (* x xscale))
	     (floatify (* y yscale))
	     (floatify (* z zscale))))))
;;FIXME::have a separate test file for drawing things?
(defun make-terrain ()
  ;;(declare (optimize speed (safety 0)))
  (let ((count 0))
    (dobox ((x -100 100)
	    (z -100 100))
      (let ((difference
	       (+ (octave x 0.0 z 0.05 1.0 0.05 10)
		  ;;(octave x 0.0 z 0.25 1.0 0.25 2)
		  ;;(octave x 0.0 z 0.02 1.0 0.02 20)
		  )))
	(dobox ((y -100 100))
	  (incf count)
	  (when (zerop (mod count 10000))
	    (print count))
	  (if (< y difference)
	      (setf (vocs::pen-get x y z)
		    (nick :grass))
	      (setf (vocs::pen-get x y z)
		    (world:blockify (nick :air) 0 15))))))))

(defun deg-rad (n)
  (* n (floatify (/ pi 180))))
(setf *5-fist* 'spiral)
(defun spiral (&optional (x *x*) (y *y*) (z *z*) (iterations 1000))
  (let* ((angle (deg-rad 1))
	 (granularity 1.0)
	 (radius 100.0)
	 (direction (ng:vec 0.0 1.0 0.0))
	 (rotation-per-step (* 2 (asin (/ granularity (* 2 radius)))))
	 (steps-per-turn (floatify (/ (* 2 pi)
				      rotation-per-step)))
	 (shrinkage 0.8)
	 (s (expt shrinkage (/ 1.0 steps-per-turn)))
	 (mat (ng:matrix*
	       (ng:scale* s s s)
	       (ng:rotate-around direction rotation-per-step))))
    (labels ((rec (place iterations direction)
	       (when (>= iterations 0)
		 (let* ((rds (ng:transform-point direction mat)))
		   (let ((new (ng:vec+ place rds)))
		     (multiple-value-call
			 'line
		       (vec-values place)
		       (vec-values new)
		       (nick :stone)
		       (create-aabb 0.1))
		     (rec new (1- iterations) rds))))))
      (rec (multiple-value-call 'sb-cga:vec (floatify2 x y z))
	   iterations
	   (ng:vec*
	    (ng:vec (cos angle) (sin angle) 0.0)
	    granularity)))))

(setf *5-fist* 'le-box)
(defun a-box (x y z xsize ysize zsize block)
  (dobox ((x x (+ x xsize))
	  (y y (+ y ysize))
	  (z z (+ z zsize)))
    (setf (vocs::pen-get x y z) block)))
(defun le-box (&optional (x *x*) (y *y*) (z *z*) (iterations 100))
  (dotimes (i iterations)
    (a-box x y z 3 3 3 (nick :log))
    (incf x 4)))

(defun fun-box (x y z xsize ysize zsize fun &optional (center #b000))
  (when (logtest #b100 center)
    (decf x (floor xsize 2)))
  (when (logtest #b010 center)
    (decf y (floor ysize 2)))
  (when (logtest #b001 center)
    (decf z (floor zsize 2)))
  (dobox ((x x (+ x xsize))
	  (y y (+ y ysize))
	  (z z (+ z zsize)))
    (funcall fun x y z)))

(defun distance (x y z x0 y0 z0)
  (flet ((foo (n) (* n n)))
    (sqrt (+ (foo (- x x0))
	     (foo (- y y0))
	     (foo (- z z0))))))

(setf *5-fist* 'gauss)
(defun gauss (&optional (x *x*) (y *y*) (z *z*) ;;(iterations 100)
		)
  (let ((radius 20)
	(scale 6))
    (fun-box x y z radius radius radius
	     (lambda (x0 y0 z0)
	       (let ((likelyhood
		      (cl-mathstats:gaussian-significance (* (/ (distance x y z x0 y0 z0) radius)
							     scale)
							  :both)))
		 (let ((foo (/ 1.0 likelyhood)))
		   (print foo)
		   (when (< (random foo) 1.0)
		     (setf (vocs::pen-get x0 y0 z0) (nick :sand))))))
	     #b111)))

(defparameter *air* (world::blockify (nick :air) 0 15))
(defun set-air (x y z)
  (setf (vocs::pen-get x y z) *air*))
(setf *5-fist* 'erase)
(defun erase (&optional (x *x*) (y *y*) (z *z*))
  (fun-box x y z 20 20 20 'set-air
	   #b111))

(setf *5-fist* 'erase)
(defun erase (&optional (x *x*) (y *y*) (z *z*))
  (fun-box x y z 20 20 20 'set-air
	   #b111))

(defun set-5 (fun &optional (radius 20) &aux (r radius))
  (setf *5-fist* (lambda (&optional (x *x*) (y *y*) (z *z*))
		   (fun-box x y z r r r fun #b111)
		   (fun-box x y z r r r
			    (lambda (x y z)
			      (world::block-dirtify x y z))
			    #b111))))

(set-5 'ensmoothen)
(defun ensmoothen (x y z)
  (let ((blockid (world:getblock x y z)))
    (unless (= (nick :air) blockid)
      (let ((naybs (adjacent-neighbors x y z)))
	(when (> 3 naybs)
	  (set-air x y z))))))

(set-5 'fillgap)
(defun fillgap (x y z)
  (let ((blockid (world:getblock x y z)))
    (when (= (nick :air) blockid)
      (let ((naybs (adjacent-neighbors x y z)))
	(when (< 2 naybs)
	  (setf (vocs::pen-get x y z)
		(majority x y z)))))))

(defun majority (x y z)
  (let (acc)
    (fun-box x y z 3 3 3
	     (lambda (x y z)
	       (let ((bid (b@ x y z)))
		 (incf (getf acc bid 0))))
	     #b111)
    (let ((alist (alexandria:plist-alist acc)))
      (car (rassoc (reduce 'max
			   alist
			   :key 'cdr)
		   alist)))))

(set-5 'correct-earth 60)

#+nil
(utility:with-unsafe-speed
  (defun generate-for-new-chunk (key)
    (multiple-value-bind (x y z) (voxel-chunks:unhashfunc key)
      (declare (type fixnum x y z))
      ;;(print (list x y z))
      (when (>= y -1)
	(dobox ((x0 x (the fixnum (+ x 16)))
		(y0 y (the fixnum (+ y 16)))
		(z0 z (the fixnum (+ z 16))))
	       (let ((block (let ((threshold (/ y 512.0)))
			      (if (> threshold (black-tie:perlin-noise-single-float
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
(defun generate-for-new-chunk (key)
    (multiple-value-bind (x y z) (voxel-chunks:unhashfunc key)
      (declare (type fixnum x y z))
      ;;(print (list x y z))
      (when (>= y -1)
	(dobox ((x0 x (the fixnum (+ x 16)))
		(y0 y (the fixnum (+ y 16)))
		(z0 z (the fixnum (+ z 16))))
	       (let ((block (let ((threshold (/ y 512.0)))
			      (if (> threshold (black-tie:perlin-noise-single-float
						(* x0 0.05)
						(+ (* 1.0 (sin y0)) (* y0 0.05))
						(* z0 0.05)))
				  0
				  1))))
		 (setf (voxel-chunks::pen-get x0 y0 z0)
		       (world:blockify block
				       (case block
					 (0 15)
					 (1 0))
				       0)))))))

(defun generate-for-new-chunk (key)
    (multiple-value-bind (x y z) (voxel-chunks:unhashfunc key)
      (declare (type fixnum x y z))
      ;;(print (list x y z))
      (dobox ((x0 x (the fixnum (+ x 16)))
	      (y0 y (the fixnum (+ y 16)))
	      (z0 z (the fixnum (+ z 16))))
	     (setf (voxel-chunks::pen-get x0 y0 z0)
		   (world:blockify 4 15 0 )))))



(defun test0 ()
  (dotimes (x 100)
    (dotimes (z 6)
      (generate-for-new-chunk (vocs::create-chunk-key x z z)))))
#+nil
(defun 5fun (x y z)
  (multiple-value-bind (x y z) (get-chunk x y z)
    (dobox ((0x (- x 16) (+ x 32) :inc 16)
	    (0y (- y 16) (+ y 32) :inc 16)
	    (0z (- z 16) (+ z 32) :inc 16))
	   (background-generation (multiple-value-call
				      'voxel-chunks:create-chunk-key
				    (voxel-chunks:chunk-coordinates-from-block-coordinates 
				     0x
				     0y
				     0z))))))
#+nil
(defun 5fun (x y z)
  (incf y)
  (let ((index 0))
    (loop :repeat 1000 :do
       (let ((newx x)
	     (newy y)
	     (newz z))
	 (progn
	   (let* ((random (random 3))
		  (random2 (- (* 2 (random 2)) 1))
		  (distance (* random2 (random 10))))
	     (case random
	       (0 (incf newx distance))
	       (1 (incf newy distance))
	       (2 (incf newz distance))))
	   (cond
	     ((zerop (world:getblock newx newy newz))
	      (incf index)
	      (line
	       x y z
	       newx newy newz
	       (case (mod (floor index 30) 2)
		 (0 (nick :stone))
		 (otherwise
		  (nick :sandstone))))
	      (setf x newx
		    y newy
		    z newz))
	     (t
	      (setf newx x newy y newz z))))))))
;;#+nil
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
  (let ((look (camera-matrix:camera-vec-forward camera))
	(up (camera-matrix:camera-vec-up camera)))   
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
;;(defparameter *swinging* nil)
#+nil
(progn
  (defparameter *big-fist-fun* (constantly nil))
  (defparameter *middle-fist-fnc* 'place-block-at)
  (defparameter *4-fist-fnc* 'tree)
  (defparameter *5-fist-fnc*
    '5fun))
#+nil
(progn
  (setf *big-fist-fun* 'correct-earth)
  (setf *middle-fist-fnc* 'player-feet-at)
  (setf *middle-fist-fnc* 'line-to-player-feet))
#+nil
(progn
  (when (window:button :key :pressed #\2) 
    (toggle *dirtying2*))
  (when (window:button :key :pressed #\1) 
    (toggle *dirtying*))
  (when (window:button :key :pressed #\3) 
    (toggle *swinging*)))


#+nil
(defparameter *big-fist-reach* 32)
#+nil
(when (window:mouse-locked?)
  (with-vec (px py pz) (pos)
    (with-vec (vx vy vz) (look-vec)
      (when *swinging*
	(let ((u *big-fist-reach*))
	  (aabbcc:aabb-collect-blocks
	      (px py pz (* u vx) (* u vy) (* u vz)
		  *big-fist-aabb*)
	      (x y z contact)
	    (declare (ignorable contact))
	    (let ((*x* x)
		  (*y* y)
		  (*z* z))
	      (funcall *big-fist-fun* x y z)))))))

  )
  #+nil
  (progn
    (when (window:button :mouse :pressed :middle)
      (with-vec (a b c) ((fist-selected-block *fist*))
	(let ((*x* a)
	      (*y* b)
	      (*z* c))
	  (funcall *middle-fist-fnc* a b c))))
    (when (window:button :mouse :pressed :4)
      (with-vec (a b c) ((fist-selected-block *fist*))
	(let ((*x* a)
	      (*y* b)
	      (*z* c))
	  (funcall *4-fist-fnc* a b c))))
    (when (window:button :mouse :pressed :5)
      (with-vec (a b c) ((fist-selected-block *fist*))
	(let ((*x* a)
	      (*y* b)
	      (*z* c))
	  (funcall *5-fist-fnc* a b c)))))

;;;; Changing the color of the sky based on which way we're looking.
#+nil
(defun deg-rad (deg)
  (* deg (load-time-value (utility:floatify (/ pi 180)))))
#+nil
(defparameter *sun-direction* (unit-pitch-yaw (deg-rad 90) (deg-rad 0)))
#+nil
(defparameter *sky-color-foo* '(0.0 0.0 0.0))
#+nil
(defun neck-angle ()
  (/ (+ 1.0
	(-
	 (sb-cga:dot-product
	  (sb-cga:normalize (camera-matrix:camera-vec-forward *camera*))
	  (sb-cga:normalize *sun-direction*))))
     2.0))
   #+nil
   (mapcar 
    (lambda (a0 a1)
      (expt (alexandria:lerp (neck-angle) a0 a1) 0.5))
    *sky-color2*
    *sky-color*)
;;;;
#+nil
(defun select-block-with-scroll-wheel ()
  (setf *blockid*
	(let ((seq
	       #(3 13 12 24 1 2 18 17 20 5 89)))
	  (elt seq (mod (round window:*scroll-y*)
			(length seq))))))

;;FIXME -> select-block-with-scroll-wheel should use events instead?
#+nil
(select-block-with-scroll-wheel)

#+nil
(defparameter *slab-aabb*
  ;;;;slab
  (create-aabb 1.0  #+nil 0.5 1.0 1.0 0.0 0.0 0.0))

#+nil
(defparameter *big-fist-aabb*
  (create-aabb
   ;;0.5
   ;;1.5
   8.0))
