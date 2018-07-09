(defpackage #:sandbox-sub
  (:use :cl :utility :application :struct-to-clos :math-modify-macros))
(in-package #:sandbox-sub)

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
   :maxx 1.0
   :maxy 1.0
   :maxz 1.0))

(defparameter *dirtying2* nil)
(defun collide-fucks (px py pz vx vy vz aabb)
  (aabbcc::with-touch-collector (collect-touch collapse-touch min-ratio)
    (aabbcc::aabb-collect-blocks (px py pz vx vy vz aabb)
	(x y z contact)
      (declare (ignorable contact))
      (when (aref block-data:*iscollidable*
		  (world:getblock x y z))
	(when *dirtying2*
	  (sandbox::plain-setblock x y z (1+ (random 5)) 0))
	(multiple-value-bind (minimum type)
	    (aabbcc:aabb-collide
	     aabb
	     px py pz
	     *block-aabb*
	     x y z
	     vx vy vz)
	  (collect-touch minimum type))))
    (values
     (collapse-touch vx vy vz)
     min-ratio)))

(defparameter *dirtying* nil)
(defun a-contact-fun (px py pz aabb)
  (let ((acc #b000000))
    (aabbcc::get-blocks-around (px py pz aabb) (mx my mz contact-var)
      (declare (ignorable contact-var))
      (when (aref block-data:*iscollidable* (world:getblock mx my mz))
	(when *dirtying*
	  (sandbox::plain-setblock mx my mz (1+ (random 5)) 0))
	(logiorf acc (aabbcc:aabb-contact px py pz aabb mx my mz *block-aabb*))))
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
(defparameter *jump-frame-count* 0)
(defparameter *jump-rising* nil)
(defparameter *jump-count* 0.0)
(defun physics (entity yaw dir pointmass
		noclip gravity fly
		is-jumping
		is-sneaking
		contact-handler
		world-collision-fun
		aabb &optional
		       (temp-vec *temp-vec*))
  (declare (optimize (debug 3)))
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
      (let* ((contact-state (if (and noclip (not sandbox-sub::*dirtying*))
				#b000000
				(with-vec (px py pz) (pos)
				  (funcall contact-handler px py pz aabb))))
	     (vel-length (nsb-cga:vec-length vel))
	     (total-speed (* *ticks-per-second* vel-length))
	     (ground-speed
	      (let ((x (* *ticks-per-second* (aref vel 0)))
		    (z (* *ticks-per-second* (aref vel 2))))
		(sqrt (+ (* x x)
			 (* z z))))))

	;;wind resistance
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
	  (if onground
	      (setf *jump-frame-count* 0)
	      (incf *jump-frame-count*))
	  (let* ((walkspeed 4.317)
		 (speed walkspeed)
		 (step-power 4.0))
	    (case is-sneaking 
	      (0 (and (not fly)
		      (*= speed 0.25)))
	      (1 (*= speed 1.3)))
	    (cond
	      (fly
	       (*= speed 4.0))		
	      (t
	       (when (> 0.0 (aref vel 1))
		 (setf *jump-rising* nil))
	       (cond
		 (onground
		  (setf *jump-rising* nil)
		  (unless dir
		    (nsb-cga:%vec* temp-vec vel *ticks-per-second*)
		    (modify nsb-cga:%vec* temp-vec
			    4.0
			    )
		    (modify nsb-cga:%vec-
			    force
			    temp-vec))
		  (when is-jumping
		    (setf *jump-rising* t)
		    (let ((foo (/ (- (max walkspeed ground-speed)
				     walkspeed) walkspeed)))
		      (setf *jump-count* (* *ticks-per-second*
					    (cond ((eql 0 is-sneaking) 0.0)
						  (t foo))))
		      (let ((base 4.0))
			#+nil
			(incf base (* 2.0 foo))
			(modify nsb-cga:%vec+ force
				(vec
				 0.0
				 (* base *ticks-per-second*)
				 0.0))))))
		 (t (*= step-power 0.6)))
	       #+nil
	       (when *jump-rising*
		 (when (and is-jumping (< *jump-frame-count*
					  *jump-count*))
		   (modify nsb-cga:%vec+ force
			   (vec
			    0.0
			    (* 6.0 (/ (- *jump-count* *jump-frame-count*)
				      *jump-count*))
			    0.0))))))
	    (let* ((yvalue (if fly
			       (if is-jumping
				   speed
				   (case is-sneaking
				     (0 (- speed))
				     (otherwise 0.0)))
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
						(* step-power step-force))))))))))))
	;;to allow walking around block corners
	;;we introduce a frame of gravity lag
	(progn
	  (let ((old-onground (logtest (entity-contact entity) #b000100)))
	    (when (and (not old-onground)
		       gravity)
	      (modify nsb-cga:%vec- force
		      (load-time-value
		       (nsb-cga:vec
			0.0
			(or 13.0
					;9.8
			    )
			0.0)))))
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
;		(assert (float-good-p a))
;		(assert (float-good-p b))
;		(assert (float-good-p c))
;		(assert (float-good-p d))
;		(assert (float-good-p e))
;		(assert (float-good-p f))		
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

(defparameter *player-aabb*
  (aabbcc:make-aabb
   :minx -0.3
   :miny -1.5
   :minz -0.3
   :maxx 0.3
   :maxy 0.12
   :maxz 0.3))

(defun gentity ()
  (make-entity :collision-fun (function collide-fucks)
	       :contact-fun (function a-contact-fun)
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
	     *block-aabb*
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

(defparameter *last-session* nil)
(defun per-frame ()
  ;;handle chunk meshing
  (on-session-change *last-session*
    (clrhash sandbox::*g/chunk-call-list*)
    (sandbox::update-world-vao 0 0 0))
  (sandbox::designatemeshing)
  (render-stuff))

(defparameter *fov* (* (floatify pi) (/ 110 180)))

(defparameter *camera* (camera-matrix:make-camera
			:frustum-far (* 256.0)
			:frustum-near (/ 1.0 8.0)))

(defparameter *sky-color*
;  #+nil
  (vector 0.0 0.0 0.0)
  #+nil
  (vector 0.68 0.8 1.0))
  
(defparameter *fog-ratio* 0.75)

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

  ;;change the sky color according to time
  (let ((daytime sandbox::*daytime*))
    (let ((r (* daytime (aref *sky-color* 0)))
	  (g (* daytime (aref *sky-color* 1)))
	  (b (* daytime (aref *sky-color* 2))))
      (gl:clear-color r g b 1.0)))
  
  (gl:enable :depth-test)
  (gl:depth-func :less)
  (gl:clear-depth 1.0)
  (gl:clear
   :color-buffer-bit
   :depth-buffer-bit)
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
    (glhelp:with-uniforms uniform shader
      (flet ((fractionalize (x)
	       (alexandria:clamp x 0.0 1.0)))
	(let ((time sandbox::*daytime*))
	  (let ((x (fractionalize (* (aref *sky-color* 0) time)))
		(y (fractionalize (* (aref *sky-color* 1) time)))
		(z (fractionalize (* (aref *sky-color* 2) time))))
	    (%gl:uniform-3f (uniform :fogcolor)
			    x y z)
	    (gl:uniformfv (uniform :camera-pos)
			  (camera-matrix:camera-vec-position *camera*))
	    (%gl:uniform-1f (uniform :foglet)
			    (/ -1.0 (or 128 (camera-matrix:camera-frustum-far *camera*)) *fog-ratio*))
	    (%gl:uniform-1f (uniform :aratio)
			    (/ 1.0 *fog-ratio*)))))
      (%gl:uniform-1f (uniform :time)
		      sandbox::*daytime*)

      (progn
	(gl:uniformi (uniform :sampler) 0)
	(glhelp::set-active-texture 0)
	(gl:bind-texture :texture-2d
			 (glhelp::handle (getfnc 'terrain))
			 ))))
  (gl:polygon-mode :front-and-back :fill)
  ;;render chunks
  (sandbox::draw-world))

(progn
  (defun color-grasses (terrain)
    (flet ((color ()
	     (let ((value (random 256)))
	       (foliage-color value (random (1+ value))))))
      (modify-greens 80 192 :color (foliage-color 255 255) :terrain terrain)
      (modify-greens 0 240 :color (foliage-color 255 255) :terrain terrain))
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
    (dobox ((x xpos (+ texheight xpos))
	    (y ypos (+ texheight ypos)))
	   ((lambda (vecinto other)
	      (map-into vecinto (lambda (a b) (truncate (* a b) height)) vecinto other))
	    (getapixel (- (- height 1) y) x terrain) color))))

(defun load-png (filename)
  (image-utility:read-png-file filename))

(defvar *ourdir* (filesystem-util:this-directory))

(defun  foliage-color (a b)
  (multiple-value-bind (w1 w2 w3)
      (triangles:barycentric-interpolation a b 0.0 0.0 255.0 0.0 255.0 255.0)
    (mapcar (lambda (x y z)
	      (+ (* x w1)
		 (* y w2)
		 (* z w3)))
	    '(71.0 205.0 51.0)
	    '(191.0 183.0 85.0)
	    '(128.0 180.0 151.0))))

(deflazy terrain-png ()
  (load-png 
   (filesystem-util:rebase-path #P"terrain.png" *ourdir*)))

(deflazy modified-terrain-png (terrain-png)
  (color-grasses
   (alexandria::copy-array terrain-png)))

(deflazy terrain (modified-terrain-png gl-context)
  (make-instance
   'glhelp::gl-texture
   :handle
   (prog1
       (glhelp:pic-texture
	modified-terrain-png
	:rgba)
     (glhelp:apply-tex-params
      (quote ((:texture-min-filter . :nearest)
	      (:texture-mag-filter . :nearest)
	      (:texture-wrap-s . :repeat)
	      (:texture-wrap-t . :repeat)))))))
(deflazy blockshader (blockshader-text gl-context)
  (glhelp::create-gl-program blockshader-text))

(deflazy blockshader-text ()
  (glslgen::ashader
   :version 120
   :vs
   (glslgen2::make-shader-stage
    :out '((color-out "float")
	   (texcoord-out "vec2")
	   
	   (fogratio-out "float"))
    :in '((position "vec4")
	  (texcoord "vec2")
	  (blocklight "vec4")
	  (skylight "vec4")
	  (projection-model-view "mat4")

	  (time "float" 0.0)
	  
	  (foglet "float")
	  (aratio "float")
	  (camera-pos "vec3"))
    :program
    '(defun "main" void ()
      (= "gl_Position" (* projection-model-view position))
      (= color-out (dot (max (* skylight time)
			     blocklight)
		    (vec4 0.25)))
      (= texcoord-out texcoord)

      (= fogratio-out (min 1.0 (+ (* foglet (distance camera-pos (|.| position "xyz"))) aratio)))))
   :frag
   (glslgen2::make-shader-stage
    :in '((texcoord "vec2")
	  (color "float")
	  (sampler "sampler2D")

	  (fogratio "float")
	  (fogcolor "vec3"))
    :program
    '(defun "main" void ()
      (/**/ vec4 pixdata)
      (= pixdata ("texture2D" sampler texcoord))
      (/**/ vec3 temp)
      (= temp 
       (mix 
	fogcolor
	(* color
	   (|.| pixdata "rgb"))
	fogratio
	))
    ;  #+nil
      (if (== (|.| pixdata "a") 0.0)
	  (/**/ "discard;"))
      (= (|.| :gl-frag-color "rgb") temp)))
   :attributes
   '((position . 2) 
     (texcoord . 8)
     (blocklight . 1)
     (skylight . 0))
   :varyings
   '((color-out . color)
     (texcoord-out . texcoord)
     (fogratio-out . fogratio))
   :uniforms
   '((:pmv (:vertex-shader projection-model-view))
     (:fogcolor (:fragment-shader fogcolor))
     (:foglet (:vertex-shader foglet))
     (:aratio (:vertex-shader aratio))
     (:camera-pos (:vertex-shader camera-pos))
     (:sampler (:fragment-shader sampler))
     (:time (:vertex-shader time)))))

(defun draw-aabb (x y z &optional (aabb *fist-aabb*))
  ;;;;draw the fist hitbox
  (progn
    (with-slots ((minx aabbcc::minx) (miny aabbcc::miny) (minz aabbcc::minz)
		 (maxx aabbcc::maxx) (maxy aabbcc::maxy) (maxz aabbcc::maxz))
	aabb
      (draw-box (+ minx x -0) (+  miny y -0) (+  minz z -0)
		(+ maxx x -0) (+  maxy y -0) (+  maxz z -0)))))

(defmacro vvv (darkness u v x y z)
  `(progn #+nil(%gl:vertex-attrib-1f 8 ,darkness)
	  #+nil
	  (%gl:vertex-attrib-2f 2 ,u ,v)
	  (%gl:vertex-attrib-3f 3 0.06 0.06 0.06)
	  (%gl:vertex-attrib-3f 0 ,x ,y ,z)))

(defun draw-box (minx miny minz maxx maxy maxz)
  (let ((h0 0.0)
	(h1 (/ 1.0 3.0))
	(h2 (/ 2.0 3.0))
	(h3 (/ 3.0 3.0))
	(w0 0.0)
	(w1 (/ 1.0 4.0))
	(w2 (/ 2.0 4.0))
	(w3 (/ 3.0 4.0))
	(w4 (/ 4.0 4.0)))
    (gl:with-primitives :quads
      (vvv 0.0 w2 h3 minx maxy minz)
      (vvv 0.0 w2 h2 maxx maxy minz)
      (vvv 0.0 w1 h2 maxx maxy maxz)
      (vvv 0.0 w1 h3 minx maxy maxz)

      ;;j-
      (vvv 0.0 w2 h0 minx miny minz)
      (vvv 0.0 w1 h0 minx miny maxz)
      (vvv 0.0 w1 h1 maxx miny maxz)
      (vvv 0.0 w2 h1 maxx miny minz)

      ;;k-
      (vvv 0.0 w3 h2 minx maxy minz)
      (vvv 0.0 w3 h1 minx miny minz)
      (vvv 0.0 w2 h1 maxx miny minz)
      (vvv 0.0 w2 h2 maxx maxy minz)

      ;;k+
      (vvv 0.0 w1 h1 maxx miny maxz)
      (vvv 0.0 w0 h1 minx miny maxz)
      (vvv 0.0 w0 h2 minx maxy maxz)
      (vvv 0.0 w1 h2 maxx maxy maxz)
      
      ;;i-
      (vvv 0.0 w3 h1 minx miny minz)
      (vvv 0.0 w3 h2 minx maxy minz)
      (vvv 0.0 w4 h2 minx maxy maxz)
      (vvv 0.0 w4 h1 minx miny maxz)

      ;;i+
      (vvv 0.0 w2 h1 maxx miny minz)
      (vvv 0.0 w1 h1 maxx miny maxz)
      (vvv 0.0 w1 h2 maxx maxy maxz)
      (vvv 0.0 w2 h2 maxx maxy minz))))
