(defpackage #:sucle
  (:use #:cl #:utility #:application #:control #:struct-to-clos)
  (:export #:start))
(in-package :sucle)

(defparameter *app* nil)
(defparameter *sandbox* t)

(defparameter *with-functions*
  #+nil
  (list
   (lambda (x)
     (print 34)
     (unwind-protect 
	  (funcall x)
       (print 2))))
  (list
   'sandbox::call-with-world-meshing-lparallel))
(defun run-with (fun)
  (flet ((nest (with-fun cont)
	   (lambda ()
	     (funcall with-fun cont))))
    (dolist (with-fun *with-functions*)
      (setf fun (nest with-fun fun))))
  fun)

(defun start ()
  (application:main
   *sucle-app-function*
   :width (floor (* 80 text-sub::*block-width*))
   :height (floor (* 25 text-sub::*block-height*))
   :title ""))

(defparameter *sucle-app-function*
  (run-with
   (lambda ()
     #+nil
     (setf (entity-fly? *ent*) nil
	   (entity-gravity? *ent*) t)
     ;;(our-load)
     (let ((text-sub::*text-data-what-type* :framebuffer))
       (window::set-vsync t)
       (fps:set-fps 60)
       (unwind-protect
	    (loop
	       (application:poll-app)
	       (when *sandbox*
		 (per-frame))
	       ;;#+nil
	       (when *app*
		 (progn
		   #+nil
		   (when (window:skey-j-p (window::keyval #\e))
		     (window::toggle-mouse-capture))))
	       ;;#+nil
	       (when (window:skey-j-p (window::keyval #\h))
		 (toggle *app*))
	       (when (window:skey-j-p (window::keyval #\j))
		 (toggle *sandbox*)))
	 (save))))))

(defun save ()
  ;;(atest::remove-zeroes)
  ;;FIXME::don't remove all the chunks?
  (sandbox::msave))
#+nil
(defun our-load ()
  (sandbox::mload))

(eval-when (:load-toplevel :execute)
  (setf sandbox::*world-directory*
	;;"first/"
	;;#+nil
	"test/"
	)
  #+nil
  (progn
    (setf sandbox::*some-saves*
	  (cdr (assoc (machine-instance) 
		      '(("gm3-iMac" . #P"/media/imac/share/space/lispysaves/saves/sandbox-saves/")
			("nootboke" . #P"/home/terminal256/Documents/saves/"))
		      :test 'equal))))
  ;;#+nil
  (progn
    (setf sandbox::*some-saves*
	  (sucle-temp:path "save/"))))

(defun load-world-again (name)
  (setf sandbox::*persist* nil)
  (setf sandbox::*world-directory* name)
  (load-world t))
;;;;************************************************************************;;;;

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

(defparameter *block-aabb*
  (aabbcc:make-aabb
   :minx 0.0
   :miny 0.0
   :minz 0.0
   :maxx 1.0 ;1.0
   :maxy 1.0 ;1.0
   :maxz 1.0)
  "an aabb representing a 1x1x1 cube")

(defparameter *slab-aabb*
  (aabbcc:make-aabb
   :minx 0.0
   :miny 0.0
   :minz 0.0
   :maxx 1.0 ;1.0
   :maxy 1.0 ;0.5
   :maxz 1.0)
  "an aabb representing a 1x0.5x1 slab")

(defun pos-to-block-aabb (x y z)
  (let ((the-block (world:getblock x y z)))
    (block-to-block-aabb the-block)))
(defun block-to-block-aabb (blockid)
  (case blockid
    (3 *slab-aabb*)
    (t *block-aabb*)))

(defparameter *dirtying2* nil)
(defun collide-fucks (px py pz vx vy vz aabb)
  (aabbcc::with-touch-collector (collect-touch collapse-touch min-ratio)
    ;;FIXME:: aabb-collect-blocks does not check slabs, only blocks upon entering.
    ;;also check "intersecting shell blocks?"
    (aabbcc::aabb-collect-blocks (px py pz vx vy vz aabb)
	(x y z contact)
      (declare (ignorable contact))
      (let ((blockid (world:getblock x y z)))
	(when (aref block-data:*iscollidable*
		    blockid)
	  (when *dirtying2*
	    (sandbox::plain-setblock x y z (1+ (random 5)) 0))
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
    (aabbcc::get-blocks-around (px py pz aabb) (mx my mz contact-var)
      (declare (ignorable contact-var))
      (let ((blockid (world:getblock mx my mz)))
	(when (aref block-data:*iscollidable* blockid)
	  (when *dirtying*
	    (sandbox::plain-setblock mx my mz (1+ (random 5)) 0))
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

(struct->class
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

;;;;FIXME::The point of this is to reduce the amount of bits to store the hitbox.
;;;;Why? because when there is an inexact number, like 0.3, there are bits at the end which
;;;;get chopped off or something, thus leading to strange clipping.
;;;;This effectively reduces the precision, giving leeway for math operations.
;;;;My prediction could be wrong though.
(defun round-to-nearest (x &optional (n (load-time-value (/ 1.0 128.0))))
  (* n (round (/ x n))))
(defparameter *player-aabb*
  (apply #'aabbcc:make-aabb
	 (mapcan (lambda (n param)
		   `(,param ,(round-to-nearest n)))	 
		 '(-0.3 -1.5 -0.3 0.3 0.12 0.3)
		 '(:minx 
		   :miny 
		   :minz 
		   :maxx
		   :maxy 
		   :maxz))))

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

;;;a very small cubic fist
(defparameter *fist-aabb*
  (aabbcc:make-aabb
   :minx -0.00005
   :miny -0.00005
   :minz -0.00005
   :maxx 0.00005
   :maxy 0.00005
   :maxz 0.00005))

(struct->class
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
    (aabbcc::aabb-collect-blocks (px py pz vx vy vz aabb)
	(x y z contact)
      (declare (ignorable contact))
      (when (aref block-data:*iscollidable*
		  (world:getblock x y z))
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

(struct->class
 (defstruct necking
   (yaw 0.0)
   (pitch 0.0)))

(struct->class
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *fov* (* (floatify pi) (/ 85 180)))

(defparameter *camera* (camera-matrix:make-camera
			:frustum-far (* 256.0)
			:frustum-near (/ 1.0 8.0)))

(defparameter *sky-color*
  #+nil
  (vector 0.0 0.0 0.0)
  ;#+nil
  (vector 0.68 0.8 1.0))
  
(defparameter *fog-ratio* 0.75
  )
(defparameter *sky-color-foo* (vector 0.0 0.0 0.0))
(defun render-stuff ()
  ;;camera setup
  (setf (camera-matrix:camera-aspect-ratio *camera*)
	(/ (floatify window::*width*)
	   (floatify window::*height*)))
  (setf (camera-matrix:camera-fov *camera*) *fov*)
  (setf (camera-matrix:camera-frustum-far *camera*) (* 1024.0 256.0))
  (camera-matrix:update-matrices *camera*)

  ;;draw to default framebuffer
  (glhelp::bind-default-framebuffer)

  ;;setup clipping area
  (glhelp::set-render-area 0 0 window::*width* window::*height*)

  (flet ((fractionalize (x)
	      (alexandria:clamp x 0.0 1.0)))
       (map-into *sky-color-foo*
		 (lambda (x)
		   (fractionalize (* x sandbox::*daytime*)))
		 *sky-color*))
  ;;change the sky color according to time
  (with-vec (r g b) (*sky-color-foo*)
    (gl:clear-color r g b 1.0))

  (gl:depth-func :less)
  (gl:clear-depth 1.0)
  (cond
    ;;optimization to see if drawing a fullscreen quad is faster than a gl:clear
    #+nil
    (nil 
     (let ((shader (getfnc 'sandbox::gl-clear-color-buffer)))
       (glhelp::use-gl-program shader)
       (glhelp::with-uniforms uniform shader 
	 (with-vec (x y z) (*sky-color-foo*)
	   (%gl:uniform-4f (uniform :color) x y z 1.0))))
     (gl:disable :depth-test)
     ;;(gl:disable :cull-face)

     (gl:depth-mask nil)
     (gl:polygon-mode :front-and-back :fill)
     (sandbox::draw-fullscreen-quad)
     (gl:depth-mask t)
     (gl:clear :depth-buffer-bit))
    (t (gl:clear
       :color-buffer-bit
       :depth-buffer-bit
       )))
  (gl:enable :depth-test)
  (gl:enable :cull-face)
  (gl:disable :blend)

  ;;set up shader
  (let ((shader (getfnc 'blockshader)))
    (glhelp::use-gl-program shader)

    ;;uniform crucial for first person 3d
    (glhelp:with-uniforms uniform shader
      (gl:uniform-matrix-4fv 
       (uniform :pmv)
       (camera-matrix:camera-matrix-projection-view-player *camera*)
       nil))

    ;;other cosmetic uniforms
    (glhelp:with-uniforms
     uniform shader
     (with-vec (x y z) (*sky-color-foo*)
       (%gl:uniform-3f (uniform :fogcolor)
		       x y z))
     (gl:uniformfv (uniform :camera-pos)
		   (camera-matrix:camera-vec-position *camera*))
     (%gl:uniform-1f (uniform :foglet)
		     (/ -1.0
			;;FIXME::16 assumes chunk is a 16x16x16 cube
			(* 16 sandbox::*chunk-radius*)
			#+nil
			(or 128 (camera-matrix:camera-frustum-far *camera*))
			*fog-ratio*))
     (%gl:uniform-1f (uniform :aratio)
		     (/ 1.0 *fog-ratio*))
     (%gl:uniform-1f (uniform :time)
		     sandbox::*daytime*)

      (glhelp::set-uniforms-to-textures
       ((uniform :sampler)
	(glhelp::handle (getfnc 'terrain))))))
  (gl:polygon-mode :front-and-back :fill)
  ;;render chunks
  (gl:front-face :ccw)
  (sandbox::get-chunks-to-draw)
  (multiple-value-bind (shown hidden) (sandbox::draw-world)
    ;;Wow, so occlusion queries reduce the amount of chunks shown by 10 to 25 times? who knew?
    #+nil
    (let ((total
	   (hash-table-count sandbox::*g/chunk-call-list*)
	    #+nil
	    (+ hidden shown)))
      (unless (zerop total)
	(format t "~%~s" (* 100.0 (/ shown total 1.0))))))
  (progn
    (let ((shader (getfnc 'sandbox::occlusion-shader)))
      (glhelp::use-gl-program shader)
      ;;uniform crucial for first person 3d
      (glhelp:with-uniforms uniform shader
			    (gl:uniform-matrix-4fv 
			     (uniform :pmv)
			     (camera-matrix:camera-matrix-projection-view-player *camera*)
			     nil)))
    (sandbox::render-occlusion-queries)))

#+nil
(defun draw-fullscreen-quad ()
  (gl:call-list
   (glhelp::handle (application::getfnc 'fullscreen-quad))))
#+nil
(glhelp::deflazy-gl fullscreen-quad ()
  (make-instance
   'glhelp::gl-list
   :handle
   (glhelp::with-gl-list
     (macrolet ((vvv (darkness u v x y z)
		  `(progn #+nil(%gl:vertex-attrib-1f 8 ,darkness)
			  #+nil
			  (%gl:vertex-attrib-2f 2 ,u ,v)
			  ;;FIXME::when using %gl:vertex-attrib, the 0 attrib marks the
			  ;;end.
			  (%gl:vertex-attrib-4f 0 ,x ,y ,z 1.0)
			  )))
       (gl:with-primitives :quads
	 (vvv 0.0 w2 h3 1.0 1.0 0.99999994)
	 (vvv 0.0 w2 h2 -1.0 1.0 0.99999994)
	 (vvv 0.0 w1 h2 -1.0 -1.0 0.99999994)
	 (vvv 0.0 w1 h3 1.0 -1.0 0.99999994))))))
#+nil
(glhelp:deflazy-gl gl-clear-color-buffer ()
  (glhelp::create-opengl-shader
   "in vec4 position;

void main () {
gl_Position = position;

}"
   "
uniform vec4 color = vec4(0.6,0.7,0.2,1.0); 
void main () {
gl_FragColor = color;
}"
   '(("position" 0)) 
   '((:color "color"))))


(defun quadratic-formula (a b c)
  (let ((two-a (+ a a)))
    (let ((term2 (/ (sqrt (- (* b b)
			     (* 4 a c)))
		    two-a))
	  (term1 (/ (- b)
		    two-a)))
      (values (+ term1 term2)
	      (- term1 term2)))))

(defun sum-of-first-n-integers (n)
  (/ (* (+ n 1) n)
     2))

(defun reverse-sum-of-first-n-integers (n)
  (quadratic-formula 0.5 0.5 (- n)))

(defun oct-24-2018 ()
  (let ((pick
	 (random
	  (sum-of-first-n-integers 256))
	  ))
    (let ((a (floor
	      (reverse-sum-of-first-n-integers pick))))
      (values a
	      (- pick (sum-of-first-n-integers a))))))

(progn
  (defun color-grasses (terrain)
    (flet ((color ()
	     (multiple-value-call #'foliage-color
	       (values 255 0)
	       #+nil
	       (oct-24-2018)
	       )
	     
	     ;;;does not distribute evenly. it picks a slice, then a height on the slice.
	     ;;;points on small slices have a greater chance of being picked than
	     ;;;points on large slices.
	     #+nil
	     (let ((value (random 256)))
	       (foliage-color value (random (1+ value))))))
      (modify-greens 80 192 :color
		     (color)
		    
		     ;(foliage-color 255 0)
		     :terrain terrain)
      (modify-greens 0 240 :color
		     (color)
		     
		     ;(foliage-color 255 0)
		     :terrain terrain))
    terrain)
  (defun getapixel (h w image)
    (destructuring-bind (height width c) (array-dimensions image)
      (declare (ignore height))
      (make-array 4 :element-type (array-element-type image)
		  :displaced-to image
		  :displaced-index-offset (* c (+ w (* h width))))))

  (defun modify-greens (xpos ypos
			&key
			  (color #(0 0 0 0))
			  (terrain (error "no image"))
			  (height 256)
			  (texheight 16))
    ;;#+nil
    (setf xpos (* 2 xpos)
	  ypos (* 2 ypos)
	  height (* 2 height)
	  texheight (* 2 texheight))
    (dobox ((x xpos (+ texheight xpos))
	    (y ypos (+ texheight ypos)))
	   ((lambda (vecinto other)
	      (map-into vecinto (lambda (a b) (truncate (* a b) height)) vecinto other))
	    (getapixel (- (- height 1) y) x terrain) color))))

(defun barycentric-interpolation (px py vx1 vy1 vx2 vy2 vx3 vy3)
  (let ((denominator (+ (*
			 (- vy2 vy3)
			 (- vx1 vx3))
			(*
			 (- vx3 vx2)
			 (- vy1 vy3))))
	(py-yv3 (- py vy3))
e	(px-xv3 (- px vx3)))
    (let* ((w1 (/
		(+
		 (*
		  (- vy2 vy3)
		  px-xv3)
		 (*
		  (- vx3 vx2)
		  py-yv3))
		  denominator))
	   (w2 (/
		(+
		 (*
		  (- vy3 vy1)
		  px-xv3)
		 (*
		  (- vx1 vx3)
		  py-yv3))
		denominator))
	   (w3 (- 1 w1 w2)))
      (values w1 w2 w3))))


(defun  foliage-color (a b)
  (multiple-value-bind (w1 w2 w3)
      (barycentric-interpolation a b 0.0 0.0 255.0 0.0 255.0 255.0)
    (mapcar (lambda (x y z)
	      (+ (* x w1)
		 (* y w2)
		 (* z w3)))
	    '(71.0 205.0 51.0)
	    '(191.0 183.0 85.0)
	    '(128.0 180.0 151.0))))

(deflazy terrain-png ()
  (image-utility::load-image-from-file
   (sucle-temp:path #P"res/terrain.png")))

(deflazy modified-terrain-png (terrain-png)
  (color-grasses
   (alexandria::copy-array terrain-png)))

(glhelp:deflazy-gl terrain (modified-terrain-png)
  (glhelp::wrap-opengl-texture
   (glhelp::create-opengl-texture-from-data modified-terrain-png)))
(glhelp:deflazy-gl blockshader ()
  (glhelp::create-opengl-shader
   "
out float color_out;
out vec2 texcoord_out;
out float fogratio_out;

in vec4 position;
in vec2 texcoord;
in vec4 blocklight;
in vec4 skylight;
uniform mat4 projection_model_view;
uniform float time = 0.0;

uniform float foglet;
uniform float aratio;
uniform vec3 camera_pos;

void main () {
gl_Position = projection_model_view * position;
vec4 light = max(skylight*time, blocklight);
color_out = dot(light,vec4(0.25));
texcoord_out = texcoord;

float distance = 
//distance(position.xyz,vec3(0.0));
//distance(camera_pos.xyz, position.xyz);
max(distance(camera_pos.x, position.x), max(distance(camera_pos.z, position.z),distance(camera_pos.y, position.y)));
fogratio_out = clamp(aratio+foglet*distance, 0.0, 1.0);
}"
   "
in vec2 texcoord_out;
in float color_out;
uniform sampler2D sampler;
in float fogratio_out;
uniform vec3 fogcolor;

void main () {
vec4 pixdata = texture2D(sampler,texcoord_out.xy);
vec3 temp = mix(fogcolor, color_out * pixdata.rgb, fogratio_out);
if (pixdata.a == 0.0){discard;}
gl_FragColor.rgb = temp; 
}"
   '(("position" 2) 
     ("texcoord" 8)
     ("blocklight" 1)
     ("skylight" 0))
   '((:pmv "projection_model_view")
     (:fogcolor "fogcolor")
     (:foglet "foglet")
     (:aratio "aratio")
     (:camera-pos "camera_pos")
     (:sampler "sampler")
     (:time "time"))))


;;;;************************************************************************;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun moused (&optional (data (load-time-value (cons 0.0d0 0.0d0))))
  (multiple-value-bind (x y) (values window::*mouse-x* window::*mouse-y*)
    (multiple-value-prog1
	(values (- x (car data))
		(- y (cdr data)))
	(setf (car data) x
	      (cdr data) y))))
(progn
  (defparameter *tmouse-x* 0.0d0)
  (defparameter *tmouse-y* 0.0d0)
  (defparameter *lerp-mouse-x* 0.0d0)
  (defparameter *lerp-mouse-y* 0.0d0)
  (defun update-moused (clamp &optional (smoothing-factor 1.0))
    (multiple-value-bind (dx dy) (moused)
      (let ((x (+ *tmouse-x* dx))
	    (y (+ *tmouse-y* dy)))
	(when (> y clamp)
	  (setf y clamp))
	(let ((negative (- clamp)))
	  (when (< y negative)
	    (setf y negative)))
	(setf *tmouse-x* x)
	(setf *tmouse-y* y)
	(setf *lerp-mouse-x* (alexandria:lerp smoothing-factor *lerp-mouse-x* x))
	(setf *lerp-mouse-y* (alexandria:lerp smoothing-factor *lerp-mouse-y* y))))))

(defun unit-pitch-yaw (result pitch yaw)
  (let ((cos-pitch (cos pitch)))
    (with-vec (x y z) (result symbol-macrolet)
      (setf x (* cos-pitch (sin yaw))
	    y (sin pitch)
	    z (* cos-pitch (cos yaw)))))
  result)

;;;;

#+nil
(defun start ()
  (application::main
   (lambda ()
     (sandbox::call-with-world-meshing-lparallel 
      (lambda ()
	(loop
	   (application:poll-app)
	   (per-frame)))))
   :width 720
   :height 480
   :title "conceptually simple block game"))


(glhelp:deflazy-gl solidshader ()
  (glhelp::create-opengl-shader
   "
out vec3 color_out;
in vec4 position;
in vec3 color;
uniform mat4 projection_model_view;

void main () {
gl_Position = projection_model_view * position;
color_out = color;
}"
   "
in vec3 color_out;
void main () {
gl_FragColor.a = 1.0;
gl_FragColor.rgb = color_out;
}"
   '(("position" 0) 
     ("color" 3))
   '((:pmv "projection_model_view"))))
(defparameter *block-aabb2*
  (let* ((offset 0.1)
	 (small (- 0.0 offset))
	 (large (+ 1.0 offset)))
    (aabbcc:make-aabb
     :minx small
     :miny small
     :minz small
     :maxx large
     :maxy large
     :maxz large)))
(defparameter *chunk-aabb*
  (aabbcc:make-aabb
   :minx 0.0
   :miny 0.0
   :minz 0.0
   :maxx (floatify world::*chunk-size-x*)
   :maxy (floatify world::*chunk-size-y*)
   :maxz (floatify world::*chunk-size-z*)))
(defun render? ()
  (let ((shader (application:getfnc 'solidshader)))
    (glhelp::use-gl-program shader)
    ;;uniform crucial for first person 3d
    (glhelp:with-uniforms
     uniform shader
     (gl:uniform-matrix-4fv 
      (uniform :pmv)
      ;;(nsb-cga::identity-matrix)
      
      (camera-matrix:camera-matrix-projection-view-player *camera*)
      nil)))
  (gl:disable :blend)
  (gl:disable :cull-face)
  (gl:polygon-mode :front-and-back :line)
  (gl:line-width 2)
  ;;FIXME::render the fist again
  (when (fister-exists *fist*)
    (let ((selected-block (fister-selected-block *fist*)))
      (with-vec (a b c) (selected-block)
	(let ((sandbox::*iterator* (scratch-buffer:my-iterator)))
	  (let ((times (sandbox::draw-aabb a b c *block-aabb2*)))
	    (declare (type fixnum times)
		     (optimize (speed 3) (safety 0)))
	    ;;mesh-fist-box
	    (let ((box
		   (let ((n 0.06))			 
		     (scratch-buffer:flush-bind-in* ((sandbox::*iterator* xyz))			 
		       (glhelp:create-vao-or-display-list-from-specs
			(:quads times)
			((3 n n n)
			 (0 (xyz) (xyz) (xyz))))
		       ))))
	      (glhelp::slow-draw box)
	      (glhelp::slow-delete box)
	      )))
	)))
  #+nil
  (draw-aabb
   (* 16.0 sandbox::*chunk-coordinate-center-x*)
   (* 16.0 sandbox::*chunk-coordinate-center-y*)
   (* 16.0 sandbox::*chunk-coordinate-center-z*)
   *chunk-aabb*)
  ;;render crosshairs
  (progn
    (glhelp:set-render-area
     (- (/ window::*width* 2.0) 1.0)
     (- (/ window::*height* 2.0) 1.0)
     2
     2)
    (gl:clear-color 1.0 1.0 1.0 1.0)
    (gl:clear
     :color-buffer-bit
     )))

(defparameter *last-session* nil)
(defparameter *paused* nil)
(defparameter *session* nil)
(defun per-frame ()
  ;;FIXME::where is the best place to flush the job-tasks?
  (sucle-mp::flush-job-tasks)
  
  (application::on-session-change *session*
    (load-world t))
  (when (window::skey-j-p (window::keyval #\))
    (application::quit))
  (when (window::skey-j-p (window::keyval #\E))
      (window::toggle-mouse-capture)
      (moused))
  (setf *paused* (window::mice-free-p))
  ;;FIXME::?
  (setf sucle-mp::*paused* *paused*)
  (cond (*paused*
	 (fps:tick))
	(t
	 ;;Polling
	 ;;Physics
	 ;;Rendering Chunks
	 ;;Rendering Other stuff
	 ;;Meshing
	 ;;Waiting on vsync
	 ;;Back to polling
	 
	 ;;Physics and Polling should be close together to prevent lag
	 
	 ;;physics
	 (stuff)
	 ;;render chunks and such
	 ;;handle chunk meshing
	 (application::on-session-change *last-session*
	   (sandbox::reset-chunk-display-list)
	   ;;FIXME::update vao according to position, not 0 0 0
	   (sandbox::update-world-vao))
	 (render-stuff)
	 ;;selected block and crosshairs
	 (render?)
	 ;;FIXME::what is glFlush good for?
	 ;;(gl:flush)
	 (sandbox::designatemeshing)))
  #+nil
  (progn
    (particle-al-listener (entity-particle *ent*))
    (camera-al-listener *camera*)))

;;;
(defparameter *mouse-multiplier* 0.002617)
(defparameter *mouse-multiplier-aux* (/ (* 0.5 pi 0.9999) *mouse-multiplier*))
;;;

(defparameter *ent* (gentity))
(defparameter *fist* (gen-fister))
(defparameter *swinging* nil)
(defparameter *ticks* 0)

#+nil
(progn
  (defparameter *start-fov* (* 95 (floatify (/ pi 180.0))))
  (defparameter *target-fov* (* 110 (floatify (/ pi 180.0))))
  (defun ease (a b &optional (modifier 0.5))
    (alexandria:lerp modifier a b))
  (define-modify-macro easef (b &optional (modifier 0.5)) ease))

(defparameter *reach* 64.0)
;;FIXME::easier api for getting player position and such
(defun stuff ()
  (setf *blockid* (let ((seq
			 #(3 13 12 24 1 2 18 17 20 5 89)))
		    (elt seq (mod (round window::*scroll-y*)
				  (length seq)))))
  
  (let* ((player-pointmass (entity-particle *ent*))
	 (pos (pointmass-position player-pointmass))
	 (entity *ent*))
    (symbol-macrolet ((is-jumping (entity-jump? entity))
		      (is-sneaking (entity-sneak? entity))
		      (fly (entity-fly? entity))
		      (gravity (entity-gravity? entity))
		      (noclip (entity-clip? entity)))
      (setf is-jumping (window::skey-p (window::keyval #\ )))
      (setf is-sneaking
	    (or (when (window::skey-p (window::keyval :left-shift))
		  0)
		(when (window::skey-p (window::keyval :left-control))
		  1)))
      (when (window:mice-locked-p)
	(when (window::skey-j-p (window::keyval #\V))
	  (toggle noclip))
	(when (window::skey-j-p (window::keyval #\P))
	  (sandbox::update-world-vao))
	(when (window::skey-j-p (window::keyval #\F))
	  (toggle fly)
	  (toggle gravity)))
      (setf (entity-hips *ent*)
	    (wasd-mover
	     (window::skey-p (window::keyval #\W))
	     (window::skey-p (window::keyval #\A))
	     (window::skey-p (window::keyval #\S))
	     (window::skey-p (window::keyval #\D))))
      #+nil
      (case is-sneaking
	(1 (easef *fov* *target-fov* 0.1))
	(otherwise (easef *fov* *start-fov* 0.1)))
      (fist-stuff pos)
      (multiple-value-bind (fraction times)
	  (fps:tick
	    (incf *ticks*)
	    (setf sandbox::*daytime*
		  (floatify		     
		   1.0;;0.8
		   #+nil
		   (let ((seconds (or 60 840)))
		     (/ (+ 1 (sin
			     (/ *ticks*
				;;60
				seconds)))
			2))))
	    (physentity *ent*))
	(declare (ignorable times))
	(let ((neck (entity-neck *ent*)))
	  (when (window:mice-locked-p)
	    (update-moused *mouse-multiplier-aux* 0.5
			   )
	    (setf (necking-yaw neck)
		  (floatify (- (* *lerp-mouse-x* *mouse-multiplier*)))
		  (necking-pitch neck)
		  (floatify (* *lerp-mouse-y* *mouse-multiplier*))))
	  (unit-pitch-yaw (camera-matrix:camera-vec-forward *camera*)
			  (necking-pitch neck)
			  (necking-yaw neck))
	  (let ((curr (pointmass-position player-pointmass))
		(prev (pointmass-position-old player-pointmass))
		(camera *camera*))
	    (load-world)
	    (let ((vec (camera-matrix:camera-vec-position camera))
		  (cev (camera-matrix:camera-vec-noitisop camera)))
	      (nsb-cga:%vec-lerp vec prev curr fraction)
	      #+nil
	      (when (and (not fly)
			 (eql 0 is-sneaking))
		(nsb-cga:%vec- vec vec (load-time-value (nsb-cga:vec 0.0 0.125 0.0))))
	      (nsb-cga:%vec* cev vec -1.0))))))))

(defun player-position ()
  (let* ((player-pointmass (entity-particle *ent*))
	 (curr (pointmass-position player-pointmass)))
    curr))
(defun load-world (&optional (force nil))
  (with-vec (x y z) ((player-position))
    (sandbox::set-chunk-coordinate-center x y z))
  (let ((maybe-moved (sandbox::maybe-move-chunk-array)))
    (when (or force
	      maybe-moved)
      (sandbox::load-chunks-around)
      (sandbox::unload-extra-chunks))))

(defparameter *big-fist-reach* 32)
(defparameter *big-fist-aabb*
  (load-time-value
   #+nil
   (aabbcc:make-aabb
    :minx -1.5
    :miny -1.5
    :minz -1.5
    :maxx  1.5
    :maxy  1.5
    :maxz  1.5)
   #+nil
   (aabbcc:make-aabb
    :minx -0.5
    :miny -0.5
    :minz -0.5
    :maxx  0.5
    :maxy  0.5
    :maxz  0.5)
   (aabbcc:make-aabb
    :minx -8.0
    :miny -8.0
    :minz -8.0
    :maxx  8.0
    :maxy  8.0
    :maxz  8.0)))
(defparameter *x* 0)
(defparameter *y* 0)
(defparameter *z* 0)
(defun fist-stuff (pos)
  (let ((look-vec (load-time-value (nsb-cga:vec 0.0 0.0 0.0))))
    (nsb-cga:%vec* look-vec (camera-matrix:camera-vec-forward *camera*) -1.0)
    (with-vec (px py pz) (pos)
      (with-vec (vx vy vz) (look-vec)	
	(when (window:mice-locked-p)
	  (when (window::skey-j-p (window::keyval 2))
	    (toggle *dirtying2*))
	  (when (window::skey-j-p (window::keyval 1))
	    (toggle *dirtying*))

	  (when (window::skey-j-p (window::keyval 3))
	    (toggle *swinging*))
	  (when *swinging*
	    (let ((u *big-fist-reach*))
	      (aabbcc::aabb-collect-blocks
		  (px py pz (* u vx) (* u vy) (* u vz)
		      *big-fist-aabb*)
		  (x y z contact)
		(declare (ignorable contact))
		(let ((*x* x)
		      (*y* y)
		      (*z* z))
		  (funcall *big-fist-fun* x y z))))))
	(let ((fist *fist*))
	  (let ((left-p (window::skey-j-p (window::mouseval :left)))
		(right-p (window::skey-j-p (window::mouseval :right)))
		(middle-p (window::skey-j-p (window::mouseval :middle)))
		(4-p (window::skey-j-p (window::mouseval :4)))
		(5-p (window::skey-j-p (window::mouseval :5))))
	    #+nil
	    (when (or left-p right-p))
	    (standard-fist
	     fist
	     px py pz
	     (* *reach* vx) (* *reach* vy) (* *reach* vz))
	    (let ((fist? (fister-exists fist))
		  (selected-block (fister-selected-block fist))
		  (normal-block (fister-normal-block fist)))
	      (when fist?
		(when left-p
		  (with-vec (a b c) (selected-block)
		    (let ((*x* a)
			  (*y* b)
			  (*z* c))
		      (funcall *left-fist-fnc* a b c))))
		(when right-p
		  (with-vec (a b c) (normal-block)
		    (let ((*x* a)
			  (*y* b)
			  (*z* c))
		      (funcall *right-fist-fnc* a b c))))
		(when middle-p
		  (with-vec (a b c) (selected-block)
		    (let ((*x* a)
			  (*y* b)
			  (*z* c))
		      (funcall *middle-fist-fnc* a b c))))
		(when 4-p
		  (with-vec (a b c) (selected-block)
		    (let ((*x* a)
			  (*y* b)
			  (*z* c))
		      (funcall *4-fist-fnc* a b c))))
		(when 5-p
		  (with-vec (a b c) (selected-block)
		    (let ((*x* a)
			  (*y* b)
			  (*z* c))
		      (funcall *5-fist-fnc* a b c))))))))))))

;;;detect more entities
;;;detect block types?
(defun not-occupied (x y z &optional (ent *ent*))
  (let ((aabb (entity-aabb ent))
	(pos (pointmass-position
	      (entity-particle ent))))
    (aabbcc::aabb-not-overlap
     (pos-to-block-aabb x y z)
     (floatify x)
     (floatify y)
     (floatify z)
     aabb
     (aref pos 0)
     (aref pos 1)
     (aref pos 2))))

(defparameter *big-fist-fun* (constantly nil))
(defparameter *left-fist-fnc* 'destroy-block-at)
(defparameter *right-fist-fnc* 'place-block-at)
(defparameter *middle-fist-fnc* 'place-block-at)
(defparameter *4-fist-fnc* 'tree)
(defparameter *5-fist-fnc* '5fun)

(defun destroy-block-at (x y z)
  ;;(blocksound x y z)
  (sandbox::plain-setblock x y z (block-data::blockid :air) 15))

(defparameter *blockid* 1)

(defun place-block-at (x y z &optional (blockval *blockid*))
  (when (not-occupied x y z)
    (sandbox::plain-setblock
     x
     y
     z
     blockval
     (aref block-data:*lightvalue* blockval))
    ;;(blocksound x y z)
    ))


(defmacro with-xyz (&body body)
  `(let ((*x* *x*)
	 (*y* *y*)
	 (*z* *z*))
     ,@body))

(progn
  (defun b@ (&optional (x *x*) (y *y*) (z *z*))
    (world::getblock x y z))
  (defun (setf b@) (value &optional (x *x*) (y *y*) (z *z*))
    (sandbox::plain-setblock x y z value)))

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

(setf *big-fist-fun* 'correct-earth)
(defun player-feet-at (&rest rest)
  (declare (ignorable rest))
  (multiple-value-bind (x y z) (player-feet)
    ;;(print (list x y z))
    (dobox ((x (+ x -1) (+ x 2))
	    (z (+ z -1) (+ z 2)))
	   (setf (b@ x y z) (nick :grass)))))
(setf *middle-fist-fnc* 'player-feet-at)

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
      (sandbox::plain-setblock x y z blockid))))

(defun create-aabb (&optional (maxx 1.0) (maxy maxx) (maxz maxx)
		      (minx (- maxx)) (miny (- maxy)) (minz (- maxz)))
  (aabbcc::make-aabb
   :minx minx
   :maxx maxx
   :miny miny
   :maxy maxy
   :minz minz
   :maxz maxz))

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

(setf *middle-fist-fnc* 'line-to-player-feet)


(defun get-chunk (x y z)
  (multiple-value-bind (x y z) (world::chunk-coordinates-from-block-coordinates x y z)
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
		  (sandbox::dirty-push-around key)
		  (sucle-mp::remove-unique-task-key job-key))))))

(utility:with-unsafe-speed
  (defun generate-for-new-chunk (key)
    (multiple-value-bind (x y z) (world::unhashfunc key)
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
		 (setf (world::getobj x0 y0 z0)
		       (world::blockify block
					(case block
					  (0 15)
					  (1 0))
					0))))))))

(defun 5fun (x y z)
  (multiple-value-bind (x y z) (get-chunk x y z)
    (dobox ((0x (- x 16) (+ x 32) :inc 16)
	    (0y (- y 16) (+ y 32) :inc 16)
	    (0z (- z 16) (+ z 32) :inc 16))
	   (background-generation (multiple-value-call
				      'world::create-chunk-key
				    (world::chunk-coordinates-from-block-coordinates 
				     0x
				     0y
				     0z))))))

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
