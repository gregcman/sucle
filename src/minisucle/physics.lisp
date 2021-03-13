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
(defun run-physics-for-entity (entity)
  (declare (optimize (debug 3)))
  
  (let* (;;Bind to entity
	 (yaw (necking-yaw (entity-neck entity)))
	 (dir (entity-hips entity))
	 (pointmass (entity-particle entity))
	 (noclip (not (entity-clip? entity)))
	 (gravity (entity-gravity? entity))
	 (fly (entity-fly? entity))
	 (is-sneaking (entity-sneak? entity))
	 (is-jumping (entity-jump? entity))
	 (contact-handler (entity-contact-fun entity))
	 (world-collision-fun (entity-collision-fun entity))
	 (aabb (entity-aabb entity))
	 ;;Bind to entity's point mass
	 (vel (pointmass-velocity pointmass))
	 (pos (pointmass-position pointmass))
	 (mass (pointmass-mass pointmass))
	 (force (pointmass-force pointmass))
	 (contact-state (if noclip
			    #b000000
			    (mvc contact-handler
				 (spread pos)
				 aabb)))
	 (old-onground (entity-on-ground-p entity))
	 (onground (on-ground-p contact-state)))
    ;;Update pointmass
    (step-pointmass pointmass)
    ;;Reset the force
    (fill force 0.0)
    
    ;;Set entity touching ground
    (when onground
      (setf (entity-doublejump entity) :fresh)
      (when (and (not dir)
		 old-onground)
	(modify nsb-cga:%vec- force (nsb-cga:vec* vel (* 4.0 *ticks-per-second*)))))
    
    ;;Apply jump force
    (when (and is-jumping
	       (or onground
		   (when (eq (entity-doublejump entity) :jump)
		     (setf (entity-doublejump entity) :finished)
		     (when (minusp (aref vel 1))
		       (setf (aref vel 1) 0.0))
		     t)))
      (modify nsb-cga:%vec+ force
	      (nsb-cga:vec 0.0 (* 4.0 *ticks-per-second*) 0.0)))
    		
    (let* ((speed (if fly (* 4.317 4) 4.317))
	   (step-power 4.0)
	   (yvalue (if fly
		       (cond (is-jumping speed)
			     (is-sneaking (- speed))
			     (t 0.0))
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
	(let ((velocity (nsb-cga:vec* vel *ticks-per-second*)))
	  (unless fly
	    (setf (vec-y velocity) 0.0))
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
					  (* step-power step-force)))))))))))
    
    ;;Apply gravity, but introduce a frame of gravity lag to allow walking around block corners
    (progn
      (when (and (not old-onground)
		 gravity)
	(modify nsb-cga:%vec- force
		(load-time-value
		 (nsb-cga:vec 0.0 13.0 0.0))))
      (setf (entity-contact entity) contact-state))
    (modify nsb-cga:%vec/ force (* (* *ticks-per-second*
				      *ticks-per-second* 0.5)
				   mass))
    ;;Apply the force to the velocity
    (modify nsb-cga:%vec+ vel force)
    (nullify-velocity-where-obstructed
     vel
     (logtest contact-state #b100000)
     (logtest contact-state #b010000)
     (logtest contact-state #b001000)
     (logtest contact-state #b000100)
     (logtest contact-state #b000010)
     (logtest contact-state #b000001))
    (let ((aabb-gen-fnc
	   (cond (noclip
		  (lambda (&rest args)
		    (declare (ignore args))
		    (values #b000 1.0)))
		 (t world-collision-fun))))
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
		   vz (floatify (if (logtest #b001 xyzclamp) 0 vz)))))))))


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


;;;;
(defun create-aabb (&optional (maxx 1.0) (maxy maxx) (maxz maxx)
		      (minx (- maxx)) (miny (- maxy)) (minz (- maxz)))
  (floatf maxx maxy maxz minx miny minz)
  (aabbcc:make-aabb
   :minx minx
   :maxx maxx
   :miny miny
   :maxy maxy
   :minz minz
   :maxz maxz))

(defparameter *block-aabb*
  ;;;;1x1x1 cube
  (create-aabb 1.0 1.0 1.0 0.0 0.0 0.0))

;;;;[FIXME]The point of this is to reduce the amount of bits to store the hitbox.
;;;;Why? because when there is an inexact number, like 0.3, there are bits at the end which
;;;;get chopped off or something, thus leading to strange clipping.
;;;;This effectively reduces the precision, giving leeway for math operations.
;;;;My prediction could be wrong though.
(defun round-to-nearest (x &optional (n (load-time-value (/ 1.0 128.0))))
  (* n (round (/ x n))))
(defparameter *player-aabb*
  (apply #'create-aabb
	 (mapcar 'round-to-nearest	 
		 '(0.3 0.12 0.3 -0.3 -1.5 -0.3))))

;;;;

(defun block-to-block-aabb (blockid)
  (declare (ignore blockid))
  ;;FIXME :use defmethod on objects?
  *block-aabb*)

(defun entity-collision (px py pz vx vy vz aabb)
  (aabbcc:with-touch-collector (collect-touch collapse-touch min-ratio)
    ;;[FIXME] aabb-collect-blocks does not check slabs, only blocks upon entering.
    ;;also check "intersecting shell blocks?"
    (aabbcc:aabb-collect-blocks (px py pz vx vy vz aabb)
	(x y z contact)
      (declare (ignorable contact))
      (let ((blockid (voxel-chunks:getobj x y z)))
	(when (not (empty-air-p blockid))
	  (let ((blockaabb (block-to-block-aabb blockid)))
	    (multiple-value-bind (minimum type)
		(aabbcc:aabb-collide
		 aabb
		 px py pz
		 blockaabb
		 x y z
		 vx vy vz)
	      (collect-touch minimum type))))))
    (values
     (collapse-touch vx vy vz)
     min-ratio)))

(defun find-blocks-in-contact-with (px py pz aabb)
  (let ((acc #b000000))
    (aabbcc:get-blocks-around (px py pz aabb) (mx my mz contact-var)
      (declare (ignorable contact-var))
      (let ((blockid (voxel-chunks:getobj mx my mz)))
	(when (not (empty-air-p blockid))
	  (logiorf acc (aabbcc:aabb-contact px py pz aabb mx my mz
					    (block-to-block-aabb blockid))))))
    acc))

(defun create-entity (&optional (aabb
				 (create-aabb 0.3 0.12 0.3 -0.3 -1.5 -0.3)))
  ;;Reapply comments to make it noclip
  (make-entity :collision-fun ;;(lambda (&rest rest) (values 0 1.0))
	       #'entity-collision
	       :contact-fun ;;(lambda (&rest rest) 0)

	       #'find-blocks-in-contact-with
	       :particle (make-pointmass)
	       :neck (make-necking)
	       :aabb aabb 
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
;;fist
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

;;;a very small cubic fist
(defparameter *fist-aabb* (create-aabb 0.00005))

(defparameter *block-aabb*
  ;;;;1x1x1 cube
  (create-aabb 1.0 1.0 1.0 0.0 0.0 0.0))
(defun fist-trace (px py pz vx vy vz &optional (aabb *fist-aabb*))
  (block first-block
    (aabbcc:aabb-collect-blocks (px py pz vx vy vz aabb)
	(x y z contact)
      (declare (ignorable contact))
      (when (not (zerop (voxel-chunks:getobj x y z)))
	(multiple-value-bind (minimum type)
	    (aabbcc:aabb-collide
	     aabb
	     px py pz
	     *block-aabb*
	     x y z
	     vx vy vz)
	  (declare (ignorable minimum))
	  (unless (zerop type)
	    (return-from first-block (values type minimum x y z))))))
    #b000))
