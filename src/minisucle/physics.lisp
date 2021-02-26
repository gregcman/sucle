(in-package :sucle)
;;;;************************************************************************;;;;
;;;;<PHYSICS>

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
				     (setf ,vel (* ,vel whats-left))))))
		   (axis px vx #b100)
		   (axis py vy #b010)
		   (axis pz vz #b001))
		 (when (>= 0 whats-left) (exit)))
	       (logiorf dead-axis newclamp)))))))
(defun noclip-motion-fun (&rest args)
  ;;Always return no contact state.
  ;;and the clamp value [second value]
  ;;is 1.0 representing moving along the entire
  ;;path unobstructed.
  (declare (ignore args))
  (values #b000 1.0))

(defun set-doublejump (ent)
  (when (and (not (entity-on-ground-p ent))
	     (not (eq (entity-doublejump ent) :finished))
	     (eq (entity-doublejump ent) :fresh))
    (setf (entity-doublejump ent) :jump)))

(defun entity-on-ground-p (entity)
  (on-ground-p (entity-contact entity)))
(defun on-ground-p (contact-state)
  (logtest contact-state #b000100))

(defmacro modify (fun a &rest rest)
  (once-only (a)
    `(,fun ,a ,a ,@rest)))
(defparameter *ticks-per-second* 60.0)
(defparameter *temp-vec* (nsb-cga:vec 0.0 0.0 0.0))
(defun run-physics-for-entity (entity &aux (temp-vec *temp-vec*))
  (multiple-value-bind (entity yaw dir pointmass
			       noclip gravity fly
			       is-jumping
			       is-sneaking
			       contact-handler
			       world-collision-fun
			       aabb)
      ;;[FIXME] This function is a total mess, a nightmare?
      (values entity
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
	      (entity-aabb entity))
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
	       (old-onground (entity-on-ground-p entity)))

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
	  (let ((onground (on-ground-p contact-state)))
	    (let* ((walkspeed 4.317)
		   (speed walkspeed)
		   (step-power 4.0))
	      ;;FIXME:add great friction when on ground and sneaking
	      (when is-sneaking
		(setf speed (* speed 1.75)))
	      (block out
		(when fly
		  (setf speed (* speed 4.0))
		  (return-from out))
		(when onground
		  (setf (entity-doublejump entity) :fresh)
		  (when (and (not dir)
			     old-onground)
		    (modify nsb-cga:%vec- force (nsb-cga:vec* vel (* 4.0 *ticks-per-second*)))))
		(when (and is-jumping
			   (or onground
			       (when (eq (entity-doublejump entity) :jump)
				 (setf (entity-doublejump entity) :finished)
				 (when (minusp (aref vel 1))
				   (setf (aref vel 1) 0.0))
				 t)))
		  (let ((base 4.0))
		    (when is-sneaking
		      (setf base (* base 1.5)))
		    (modify nsb-cga:%vec+ force
			    (vec 0.0 (* base *ticks-per-second*) 0.0))))
		(return-from out)
		;;(*= step-power 0.6)

		)
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
			     (* speed (- (sin diraux)))
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
	  (nullify-velocity-where-obstructed
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
		       vz (floatify (if (logtest #b001 xyzclamp) 0 vz)))))))))))


(defun vec-x (vec)
  (aref vec 0))
(defun vec-y (vec)
  (aref vec 1))
(defun vec-z (vec)
  (aref vec 2))
(defun (setf vec-x) (new vec)
  (setf (aref vec 0) new))
(defun (setf vec-y) (new vec)
  (setf (aref vec 1) new))
(defun (setf vec-z) (new vec)
  (setf (aref vec 2) new))

(defun nullify-velocity-where-obstructed (velocity i+ i- j+ j- k+ k-)
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

   doublejump))
(defun entity-position (entity)
  (let* ((player-pointmass (entity-particle entity))
	 (curr (pointmass-position player-pointmass)))
    curr))

(defun create-entity ()
  (make-entity :collision-fun (lambda (&rest rest) (values 0 1.0)) ;;'entity-collision
	       :contact-fun (lambda (&rest rest) 0) ;;find-blocks-in-contact-with
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