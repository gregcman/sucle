(in-package :sucle)
;;;;************************************************************************;;;;
;;;;<PHYSICS>

;;;math modify macros
(define-modify-macro *= (&rest args) *)
(define-modify-macro += (&rest args) +)
(define-modify-macro -= (&rest args) -)
(define-modify-macro /f (&rest args) /)
(define-modify-macro &= (&rest args) logand)
(define-modify-macro ^= (&rest args) logxor)
(define-modify-macro |\|=| (&rest args) logior)
(define-modify-macro <<= (&rest args) ash)
(define-modify-macro >>= (&rest args) hsa)
(defmacro hsa (a b)
  `(ash ,b ,a))

(define-modify-macro logiorf (&rest args) logior)
;;;end math modify macros


(defun collide-world2 (aabb-gen-fnc x y z dx dy dz aabb)
  (multiple-value-bind (new-x new-y new-z xyzclamp)
      (step-motion aabb-gen-fnc
		   x y z dx dy dz aabb)
    (values new-x new-y new-z
	    (if (logtest #b100 xyzclamp) 0 dx)
	    (if (logtest #b010 xyzclamp) 0 dy)
	    (if (logtest #b001 xyzclamp) 0 dz))))
(defun step-motion (get-collision-data px py pz vx vy vz aabb)
  (let ((dead-axis #b000) ;;axis which are zeroed
	(clamp #b000)) ;;the final clamping value for each axis
    (flet ((deadify (x bit)
	     (when (zerop x)
	       (logiorf dead-axis bit))))
      (deadify vx #b100)
      (deadify vy #b010)
      (deadify vz #b001))
    (loop
       (flet ((exit ()
		(return (values px py pz clamp))))
	 (if (= #b111 dead-axis) ;;all velocities are zero
	     (exit)
	     (multiple-value-bind (newclamp ratio)
		 (funcall get-collision-data px py pz vx vy vz aabb)
	       (logiorf clamp newclamp)
	       (let ((whats-left (- 1 ratio)))
		 (macrolet ((axis (pos vel bit)
			      `(unless (logtest ,bit dead-axis)
				 (incf ,pos (* ratio ,vel))
				 (if (logtest ,bit newclamp)
				     (setf ,vel 0)
				     (*= ,vel whats-left)))))
		   (axis px vx #b100)
		   (axis py vy #b010)
		   (axis pz vz #b001))
		 (when (>= 0 whats-left) (exit)))
	       (logiorf dead-axis newclamp)))))))


(defun pos-to-block-aabb (x y z)
  (let ((the-block (world:getblock x y z)))
    (block-to-block-aabb the-block)))
(defun block-to-block-aabb (blockid)
  (case blockid
    (3 *slab-aabb*)
    (t *block-aabb*)))

(defparameter *dirtying2* nil)
(defun collide-fucks (px py pz vx vy vz aabb)
  (aabbcc:with-touch-collector (collect-touch collapse-touch min-ratio)
    ;;[FIXME] aabb-collect-blocks does not check slabs, only blocks upon entering.
    ;;also check "intersecting shell blocks?"
    (aabbcc:aabb-collect-blocks (px py pz vx vy vz aabb)
	(x y z contact)
      (declare (ignorable contact))
      (let ((blockid (world:getblock x y z)))
	(when (block-data:data blockid :hard)
	  (when *dirtying2*
	    (world:plain-setblock x y z (1+ (random 5)) 0))
	  (let ((blockaabb (block-to-block-aabb blockid)))
	    (#+nil
	     let
	     #+nil((args
		   (list
		    aabb
		    px py pz
		    blockaabb
		    x y z
		    vx vy vz)))
	     progn
	      (multiple-value-bind (minimum type)
		  ;;(apply 'aabbcc:aabb-collide args)
		;;#+nil
		(aabbcc:aabb-collide
		 aabb
		 px py pz
		 blockaabb
		 x y z
		 vx vy vz)
		;;(print (list minimum type (cons 'aabbcc:aabb-collide args)))
		(collect-touch minimum type)))))))
    (values
     (collapse-touch vx vy vz)
     min-ratio)))

(defparameter *dirtying* nil)
(defun a-contact-fun (px py pz aabb)
  (let ((acc #b000000))
    (aabbcc:get-blocks-around (px py pz aabb) (mx my mz contact-var)
      (declare (ignorable contact-var))
      (let ((blockid (world:getblock mx my mz)))
	(when (block-data:data blockid :hard)
	  (when *dirtying*
	    (world:plain-setblock mx my mz (1+ (random 5)) 0))
	  (logiorf acc (aabbcc:aabb-contact px py pz aabb mx my mz
					    (block-to-block-aabb blockid))))))
    acc))

  ;;;;150 ms delay for sprinting
;;;;player eye height is 1.5, subtract 1/8 for sneaking

;;gravity is (* -0.08 (expt tickscale 2)) 0 0
;;falling friction is 0.98
;;0.6 * 0.91 is walking friction

(defmacro modify (fun a &rest rest)
  (once-only (a)
    `(,fun ,a ,a ,@rest)))
(defparameter *ticks-per-second* 60.0)
(defparameter *temp-vec* (nsb-cga:vec 0.0 0.0 0.0))
(defun physics (entity yaw dir pointmass
		noclip gravity fly
		is-jumping
		is-sneaking
		contact-handler
		world-collision-fun
		aabb &optional
		       (temp-vec *temp-vec*))
  (declare (optimize (debug 3))
	   (ignorable is-sneaking))
  (step-pointmass pointmass)
  (flet ((vec (x y z)
	   (with-vec (a b c) (temp-vec symbol-macrolet)
	     (setf a x
		   b y
		   c z))
	   temp-vec))
    (let ((vel (pointmass-velocity pointmass))
	  (pos (pointmass-position pointmass))
	  (mass (pointmass-mass pointmass))
	  (force (pointmass-force pointmass)))
      (fill force 0.0)
      (let* ((contact-state (if (and noclip (not *dirtying*))
				#b000000
				(with-vec (px py pz) (pos)
				  (funcall contact-handler px py pz aabb))))
	     (vel-length (nsb-cga:vec-length vel))
	     (total-speed (* *ticks-per-second* vel-length))
	     (old-onground (logtest (entity-contact entity) #b000100)))

	;;wind resistance
	;;#+nil
	(let ((drag (* total-speed
		       total-speed))
	      (drag-scale (if fly
			      0.005
			      0.0003)))
	  (nsb-cga:%vec* temp-vec			     
			 vel
			 (* *ticks-per-second* drag drag-scale))
	  (modify nsb-cga:%vec-
		  force
		  temp-vec))
	(let ((onground (logtest contact-state #b000100)))
	  (let* ((walkspeed 4.317)
		 (speed walkspeed)
		 (step-power 4.0))
	    (cond
	      (fly
	       (*= speed 4.0))		
	      (t
	       (cond
		 (onground
		  (when (and (not dir)
			     old-onground)
		    (nsb-cga:%vec* temp-vec vel *ticks-per-second*)
		    (modify nsb-cga:%vec* temp-vec
			    4.0
			    )
		    (modify nsb-cga:%vec-
			    force
			    temp-vec))
		  (when is-jumping
		    
		    (let ((base 4.0))
		      (modify nsb-cga:%vec+ force
			      (vec
			       0.0
			       (* base *ticks-per-second*)
			       0.0)))))
		 (t (*= step-power 0.6
			)))))
	    (let* ((yvalue (if fly
			       (if is-jumping
				   speed
				   (if is-sneaking
				       (- speed )
				       0.0))
			       0.0))
		   (target-vec
		    (if dir
			(let ((diraux (+ dir yaw)))
			  (nsb-cga:vec
			   (* speed (sin diraux))
			   yvalue
			   (* speed (cos diraux))))
			(prog1
			    (nsb-cga:vec 0.0 yvalue 0.0)
			  (setf step-power 1.0)))))
	      (when (or dir fly)
		(let ((velocity (nsb-cga:vec (* (aref vel 0) *ticks-per-second*)
					     (if fly
						 (* (aref vel 1) *ticks-per-second*)
						 0.0)
					     (* (aref vel 2) *ticks-per-second*))))
		  (let* ((difference (nsb-cga:vec-
				      target-vec
				      velocity))
			 (difference-length (nsb-cga:vec-length difference)))
		    (unless (zerop difference-length)
		      (let* ((dot (nsb-cga:dot-product difference target-vec)))
			(let ((bump-direction			     
			       (if (and (not onground)
					(> 0.0 dot))
				   ;;in the air?
				   (let ((vec
					  (nsb-cga:cross-product
					   (nsb-cga:cross-product target-vec difference)
					   target-vec)))
				     (let ((value (nsb-cga:vec-length vec)))
				       (if (zerop value)
					   (progn
					;	   (error "wtf")
					     vec
					     )
					   (nsb-cga:vec/ vec value))))
				   (nsb-cga:vec/ 
				    difference
				    difference-length))))
			  (let ((step-force (* 2.0 difference-length)))
			    (modify nsb-cga:%vec+ force
				    (nsb-cga:vec* bump-direction
						  (* step-power step-force)))))))))))))
	;;to allow walking around block corners
	;;we introduce a frame of gravity lag
	(progn
	  (when (and (not old-onground)
		     gravity)
	    (modify nsb-cga:%vec- force
		    (load-time-value
		     (nsb-cga:vec
		      0.0
		      (or 13.0
					;		9.8
			  )
		      0.0))))
	  (setf (entity-contact entity) contact-state))
	(modify nsb-cga:%vec/ force (* (* *ticks-per-second*
					  *ticks-per-second* 0.5) mass))
	(modify nsb-cga:%vec+ vel force)
	(contact-handle contact-state vel))
      (let ((fun (if noclip
		     (lambda (&rest args)
		       (declare (ignore args))
		       (values #b000 1.0))
		     (progn
		       world-collision-fun))))
	(with-vec (vx vy vz) (vel symbol-macrolet)
	  (with-vec (px py pz) (pos symbol-macrolet)
	    (multiple-value-bind (a b c d e f)
		(collide-world2
		 fun
		 px py pz vx vy vz aabb)
	      (let ((a (floatify a))
		    (b (floatify b))
		    (c (floatify c))
		    (d (floatify d))
		    (e (floatify e))
		    (f (floatify f)))		
		(setf (values px py pz vx vy vz)
		      (values a b c d e f))))))))))

(defun contact-handle (acc vel)
  (multiple-value-bind (i+ i- j+ j- k+ k-)
      (values (logtest acc #b100000)
	      (logtest acc #b010000)
	      (logtest acc #b001000)
	      (logtest acc #b000100)
	      (logtest acc #b000010)
	      (logtest acc #b000001))
    (with-vec (xvel yvel zvel) (vel symbol-macrolet)
      (etouq 
       (cons
	'progn
	(mapcar
	 (lambda (args)
	   (apply
	    (lambda (axis plus minus)
	      (alexandria:with-gensyms (var)
		`(let ((,var ,axis))
		   (when (or (and (plusp ,var) ,plus)
			     (and (minusp ,var) ,minus))
		     (setf ,axis 0.0)))))
	    args))
	 '((xvel i+ i-)
	   (yvel j+ j-)
	   (zvel k+ k-))))))))

(struct-to-clos:struct->class
 (defstruct entity
   particle ;;center of mass
   neck ;;
   hips ;; walking direction
   aabb 
   contact ;;touching each side
   fly?
   gravity?
   clip?
   jump?
   sneak?
   collision-fun
   contact-fun))



(defun gentity ()
  (make-entity :collision-fun 'collide-fucks
	       :contact-fun 'a-contact-fun
	       :particle (make-pointmass)
	       :neck (make-necking)
	       :aabb *player-aabb*
	       :hips nil
	       :contact #b000000
	       :fly? t
	       :gravity? nil
	       :clip? t
	       :jump? nil
	       :sneak? nil))

(defun physentity (entity)
  (physics
   entity
   (necking-yaw (entity-neck entity))
   (entity-hips entity)
   (entity-particle entity)
   (not (entity-clip? entity))
   (entity-gravity? entity)
   (entity-fly? entity)
   (entity-jump? entity)
   (entity-sneak? entity)
   (entity-contact-fun entity)
   (entity-collision-fun entity)
   (entity-aabb entity)))



(struct-to-clos:struct->class
 (defstruct fister
   (selected-block (vector 0 0 0))
   (normal-block (vector 0 0 0))
   (exists nil)
   (position (vector 0 0 0))
   fun))

(defun standard-fist (fist px py pz vx vy vz)
  (multiple-value-bind (xyzclamp frac x y z) (funcall (fister-fun fist) px py pz vx vy vz *fist-aabb*)
    (cond ((= #b000 xyzclamp)
	   (setf (fister-exists fist) nil))
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
	       
	       (setvec3d (fister-selected-block fist)
			 x
			 y
			 z)
	       (setvec3d (fister-position fist)
			 a 
			 b
			 c)
	       (let ((dx 0)
		     (dy 0)
		     (dz 0))
		 (cond ((logtest xyzclamp #b100)
			(setf dx (if (plusp vx) 1 -1)) 0)
		       ((logtest xyzclamp #b010)
			(setf dy (if (plusp vy) 1 -1)) 0)
		       ((logtest xyzclamp #b001)
			(setf dz (if (plusp vz) 1 -1)) 0))
		 (setvec3d (fister-normal-block fist)
			   (- x dx)
			   (- y dy)
			   (- z dz)))))
	   (setf (fister-exists fist) t)))))

(defun collide-fucks2 (px py pz vx vy vz aabb)
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
(defun gen-fister ()
  (let ((fist (make-fister)))
    (setf (fister-fun fist) #'collide-fucks2)
    fist))

(struct-to-clos:struct->class
 (defstruct necking
   (yaw 0.0)
   (pitch 0.0)))

(struct-to-clos:struct->class
 (defstruct pointmass
   (position (nsb-cga:vec 0.0 0.0 0.0))
   (position-old (nsb-cga:vec 0.0 0.0 0.0))
   (velocity (nsb-cga:vec 0.0 0.0 0.0))
   (force (nsb-cga:vec 0.0 0.0 0.0))
   (mass 1.0)))
(defun step-pointmass (p)
  (let ((old (pointmass-position-old p))
	(curr (pointmass-position p)))
    (nsb-cga:%copy-vec old curr)))
;;;;</PHYSICS>
;;;;************************************************************************;;;;
