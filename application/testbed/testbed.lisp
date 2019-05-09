(defpackage #:testbed
  (:use #:cl #:utility #:control))
(in-package :testbed)

#+nil
(defun particle-al-listener (particle)
  (let ((pos (sandbox-sub::pointmass-position particle))
	(vel (sandbox-sub::pointmass-velocity particle)))
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
  (let* ((offset 0.001)
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
      
      (camera-matrix:camera-matrix-projection-view-player sandbox-sub::*camera*)
      nil)))
  (gl:disable :blend)
  (gl:disable :cull-face)
  (gl:polygon-mode :front-and-back :line)
  (gl:line-width 2)
  (when (sandbox-sub::fister-exists *fist*)
    (let ((selected-block (sandbox-sub::fister-selected-block testbed::*fist*)))
      (with-vec (a b c) (selected-block)
	(sandbox-sub::draw-aabb a b c *block-aabb2*))))
  #+nil
  (sandbox-sub::draw-aabb
   (* 16.0 sandbox::*chunk-coordinate-center-x*)
   (* 16.0 sandbox::*chunk-coordinate-center-y*)
   (* 16.0 sandbox::*chunk-coordinate-center-z*)
   testbed::*chunk-aabb*)
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

(defparameter *paused* nil)
(defparameter *session* nil)
(defun per-frame ()
  ;;FIXME::where is the best place to flush the job-tasks?
  (sandbox.multiprocessing::flush-job-tasks)
  
  (application::on-session-change *session*
    (load-world t))
  (when (window::skey-j-p (window::keyval #\))
    (application::quit))
  (when (window::skey-j-p (window::keyval #\E))
      (window::toggle-mouse-capture)
      (moused))
  (setf *paused* (window::mice-free-p))
  ;;FIXME::?
  (setf sandbox.multiprocessing::*paused* *paused*)
  (cond (*paused*
	 (fps-independent-timestep::tick *ticker* ()))
	(t
	 (sandbox-sub::per-frame)
	 (render?)
	 (stuff)))
  #+nil
  (progn
    (particle-al-listener (sandbox-sub::entity-particle *ent*))
    (camera-al-listener sandbox-sub::*camera*)))

;;;
(defparameter *mouse-multiplier* 0.002617)
(defparameter *mouse-multiplier-aux* (/ (* 0.5 pi 0.9999) *mouse-multiplier*))
;;;
(defparameter *ticker*
  (fps-independent-timestep:make-ticker
   (floor 1000000 60)
   most-positive-fixnum))

(defparameter *ent* (sandbox-sub::gentity))
(defparameter *fist* (sandbox-sub::gen-fister))
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
  
  (let* ((player-pointmass (sandbox-sub::entity-particle *ent*))
	 (pos (sandbox-sub::pointmass-position player-pointmass))
	 (entity *ent*))
    (symbol-macrolet ((is-jumping (sandbox-sub::entity-jump? entity))
		      (is-sneaking (sandbox-sub::entity-sneak? entity))
		      (fly (sandbox-sub::entity-fly? entity))
		      (gravity (sandbox-sub::entity-gravity? entity))
		      (noclip (sandbox-sub::entity-clip? entity)))
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
      (setf (sandbox-sub::entity-hips *ent*)
	    (wasd-mover
	     (window::skey-p (window::keyval #\W))
	     (window::skey-p (window::keyval #\A))
	     (window::skey-p (window::keyval #\S))
	     (window::skey-p (window::keyval #\D))))
      #+nil
      (case is-sneaking
	(1 (easef sandbox-sub::*fov* *target-fov* 0.1))
	(otherwise (easef sandbox-sub::*fov* *start-fov* 0.1)))
      (fist-stuff pos)
      (multiple-value-bind (fraction times)
	  (fps-independent-timestep::tick *ticker* ()
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
	    (sandbox-sub::physentity *ent*))
	(declare (ignorable times))
	(let ((neck (sandbox-sub::entity-neck *ent*)))
	  (when (window:mice-locked-p)
	    (update-moused *mouse-multiplier-aux* 0.5
			   )
	    (setf (sandbox-sub::necking-yaw neck)
		  (floatify (- (* *lerp-mouse-x* *mouse-multiplier*)))
		  (sandbox-sub::necking-pitch neck)
		  (floatify (* *lerp-mouse-y* *mouse-multiplier*))))
	  (unit-pitch-yaw (camera-matrix:camera-vec-forward sandbox-sub::*camera*)
			  (sandbox-sub::necking-pitch neck)
			  (sandbox-sub::necking-yaw neck))
	  (let ((curr (sandbox-sub::pointmass-position player-pointmass))
		(prev (sandbox-sub::pointmass-position-old player-pointmass))
		(camera sandbox-sub::*camera*))
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
  (let* ((player-pointmass (sandbox-sub::entity-particle *ent*))
	 (curr (sandbox-sub::pointmass-position player-pointmass)))
    curr))
(defun load-world (&optional (force nil))
  (with-vec (x y z) ((player-position))
    (sandbox::set-chunk-coordinate-center x y z))
  (let ((maybe-moved (sandbox::maybe-move-chunk-array)))
    (when (or force
	      maybe-moved)
      (sandbox::load-chunks-around)
      (sandbox::unload-extra-chunks))))

(defun fist-stuff (pos)
  (let ((look-vec (load-time-value (nsb-cga:vec 0.0 0.0 0.0))))
    (nsb-cga:%vec* look-vec (camera-matrix:camera-vec-forward sandbox-sub::*camera*) -1.0)
    (with-vec (px py pz) (pos)
      (with-vec (vx vy vz) (look-vec)	
	(when (window:mice-locked-p)
	  (when (window::skey-j-p (window::keyval 2))
	    (toggle sandbox-sub::*dirtying2*))
	  (when (window::skey-j-p (window::keyval 1))
	    (toggle sandbox-sub::*dirtying*))

	  (when (window::skey-j-p (window::keyval 3))
	    (toggle *swinging*))
	  (when *swinging*
	    (let ((u 80))
	      (aabbcc::aabb-collect-blocks
		  (px py pz (* u vx) (* u vy) (* u vz)
		      (load-time-value
		       #+nil
		       (aabbcc:make-aabb
			:minx -1.5
			:miny -1.5
			:minz -1.5
			:maxx  1.5
			:maxy  1.5
			:maxz  1.5)
		       (aabbcc:make-aabb
			:minx -0.5
			:miny -0.5
			:minz -0.5
			:maxx  0.5
			:maxy  0.5
			:maxz  0.5)))
		  (x y z contact)
		(declare (ignorable contact))		     
		(funcall *big-fist-fun* x y z)))))
	(let ((fist *fist*))
	  (let ((left-p (window::skey-j-p (window::mouseval :left)))
		(right-p (window::skey-j-p (window::mouseval :right))))
	    #+nil
	    (when (or left-p right-p))
	    (sandbox-sub::standard-fist
	     fist
	     px py pz
	     (* *reach* vx) (* *reach* vy) (* *reach* vz))
	    (let ((fist? (sandbox-sub::fister-exists fist))
		  (selected-block (sandbox-sub::fister-selected-block fist))
		  (normal-block (sandbox-sub::fister-normal-block fist)))
	      (when fist?
		(when left-p
		  (with-vec (a b c) (selected-block)
		    (funcall *left-fist-fnc* a b c)))
		(when right-p
		  (with-vec (a b c) (normal-block)
		    (funcall *right-fist-fnc* a b c)))))))))))

;;;detect more entities
;;;detect block types?
(defun not-occupied (x y z &optional (ent *ent*))
  (let ((aabb (sandbox-sub::entity-aabb ent))
	(pos (sandbox-sub::pointmass-position
	      (sandbox-sub::entity-particle ent))))
    (aabbcc::aabb-not-overlap
     (sandbox-sub::pos-to-block-aabb x y z)
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

(defun destroy-block-at (x y z)
  ;;(sandbox-sub::blocksound x y z)
  (sandbox::plain-setblock x y z 0 15))

(defparameter *blockid* 1)

(defun place-block-at (x y z &optional (blockval *blockid*))
  (when (not-occupied x y z)
    (sandbox::plain-setblock
     x
     y
     z
     blockval
     (aref block-data:*lightvalue* blockval))
    ;;(sandbox-sub::blocksound x y z)
      ))
