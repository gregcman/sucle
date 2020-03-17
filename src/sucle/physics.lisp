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
  ;;FIXME :use defmethod on objects?
  (case blockid
    (3 *slab-aabb*)
    (t *block-aabb*)))
#+nil
(defparameter *dirtying2* nil)
(defun entity-collision (px py pz vx vy vz aabb)
  (aabbcc:with-touch-collector (collect-touch collapse-touch min-ratio)
    ;;[FIXME] aabb-collect-blocks does not check slabs, only blocks upon entering.
    ;;also check "intersecting shell blocks?"
    (aabbcc:aabb-collect-blocks (px py pz vx vy vz aabb)
	(x y z contact)
      (declare (ignorable contact))
      (let ((blockid (world:getblock x y z)))
	(when (block-data:data blockid :hard)
	  #+nil
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
#+nil
(defparameter *dirtying* nil)
(defun find-blocks-in-contact-with (px py pz aabb)
  (let ((acc #b000000))
    (aabbcc:get-blocks-around (px py pz aabb) (mx my mz contact-var)
      (declare (ignorable contact-var))
      (let ((blockid (world:getblock mx my mz)))
	(when (block-data:data blockid :hard)
	  #+nil
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
  ;;[FIXME] This function is a total mess, a nightmare?
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
      (let* ((contact-state (if noclip ;;(and noclip (not *dirtying*))
				#b000000
				(mvc contact-handler
				     (spread pos)
				     aabb)))
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
					  *ticks-per-second* 0.5)
				       mass))
	(modify nsb-cga:%vec+ vel force)
	(contact-handle
	 vel
	 (logtest contact-state #b100000)
	 (logtest contact-state #b010000)
	 (logtest contact-state #b001000)
	 (logtest contact-state #b000100)
	 (logtest contact-state #b000010)
	 (logtest contact-state #b000001)))
      (let ((aabb-gen-fnc
	     (if noclip
		 (lambda (&rest args)
		       (declare (ignore args))
		       (values #b000 1.0))
		     (progn
		       world-collision-fun))))
	(with-vec (vx vy vz) (vel symbol-macrolet)
	  (with-vec (px py pz) (pos symbol-macrolet)
	    (multiple-value-bind (new-x new-y new-z xyzclamp)
		(step-motion aabb-gen-fnc px py pz vx vy vz aabb)
	      ;;Update the position and velocity, after taking into
	      ;;account the collision data.
	      (psetf px (floatify new-x)
		     py (floatify new-y)
		     pz (floatify new-z)
		     vx (floatify (if (logtest #b100 xyzclamp) 0 vx))
		     vy (floatify (if (logtest #b010 xyzclamp) 0 vy))
		     vz (floatify (if (logtest #b001 xyzclamp) 0 vz))))))))))

(defun contact-handle (velocity i+ i- j+ j- k+ k-)
  ;;velocity is a 3 float array.
  ;;If we are moving along an axis, but the contact
  ;;state says that direction is obstructed, then
  ;;set the physical motion along that direction to 0
  (with-vec (xvel yvel zvel) (velocity symbol-macrolet)
    (when (or (and (plusp xvel) i+)
	      (and (minusp xvel) i-))
      (setf xvel 0.0))
    (when (or (and (plusp yvel) j+)
	      (and (minusp yvel) j-))
      (setf yvel 0.0))
    (when (or (and (plusp zvel) k+)
	      (and (minusp zvel) k-))
      (setf zvel 0.0))))

;;[FIXME] how to do this better? CLOS?
(struct-to-clos:struct->class
 (defstruct entity
   ;;The center of mass
   particle
   ;;The values which ultimately determine the pitch/yaw of the camera
   neck
   ;;The walking direction. Used for WASD controls
   hips
   ;;The bounding box used in physical calculation.
   aabb
   ;;Bitfield representing whether or not each side is being
   ;;touched.
   contact
   
   collision-fun
   contact-fun

   gravity?
   fly?
   clip?

   jump?
   sneak?
   ))

(defun create-entity ()
  (make-entity :collision-fun 'entity-collision
	       :contact-fun 'find-blocks-in-contact-with
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

(defun run-physics-for-entity (entity)
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

(struct-to-clos:struct->class
 (defstruct necking
   (yaw 0.0)
   (pitch 0.0)))
(defun set-neck-values (neck yaw pitch)
  (setf (necking-yaw neck) yaw
	(necking-pitch neck) pitch))

(struct-to-clos:struct->class
 (defstruct pointmass
   (position (nsb-cga:vec 0.0 0.0 0.0))
   (position-old (nsb-cga:vec 0.0 0.0 0.0))
   (velocity (nsb-cga:vec 0.0 0.0 0.0))
   (force (nsb-cga:vec 0.0 0.0 0.0))
   (mass 1.0)))
(defun copy-pointmass (p)
  (make-pointmass
   :position (nsb-cga:copy-vec (pointmass-position p))
   :position-old (nsb-cga:copy-vec (pointmass-position-old p))
   :velocity (pointmass-velocity p)
   :force (pointmass-force p)
   :mass (pointmass-mass p)))
(defun step-pointmass (p)
  (let ((old (pointmass-position-old p))
	(curr (pointmass-position p)))
    (nsb-cga:%copy-vec old curr)))
(defun translate-pointmass (p dx dy dz)
  (let* ((new-p (copy-pointmass p))
	 (prev (pointmass-position-old new-p))
	 (curr (pointmass-position new-p))
	 ;;FIXME::uses a throwaway vec?
	 (translation (nsb-cga:vec dx dy dz)))
    ;;FIXME:does this do anything? garbage collection?
    (declare (dynamic-extent translation))
    (nsb-cga:%vec+ prev prev translation)
    (nsb-cga:%vec+ curr curr translation)
    new-p))
;;;;</PHYSICS>
;;;;************************************************************************;;;;
