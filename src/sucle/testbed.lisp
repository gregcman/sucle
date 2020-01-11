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
      
      (camera-matrix:camera-matrix-projection-view-player sandbox-sub::*camera*)
      nil)))
  (gl:disable :blend)
  (gl:disable :cull-face)
  (gl:polygon-mode :front-and-back :line)
  (gl:line-width 2)
  ;;FIXME::render the fist again
  (when (sandbox-sub::fister-exists *fist*)
    (let ((selected-block (sandbox-sub::fister-selected-block testbed::*fist*)))
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
	 (sandbox-sub::render-stuff)
	 ;;selected block and crosshairs
	 (render?)
	 ;;FIXME::what is glFlush good for?
	 ;;(gl:flush)
	 (sandbox::designatemeshing)))
  #+nil
  (progn
    (particle-al-listener (sandbox-sub::entity-particle *ent*))
    (camera-al-listener sandbox-sub::*camera*)))

;;;
(defparameter *mouse-multiplier* 0.002617)
(defparameter *mouse-multiplier-aux* (/ (* 0.5 pi 0.9999) *mouse-multiplier*))
;;;

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
(defparameter *middle-fist-fnc* 'place-block-at)
(defparameter *4-fist-fnc* 'tree)
(defparameter *5-fist-fnc* '5fun)

(defun destroy-block-at (x y z)
  ;;(sandbox-sub::blocksound x y z)
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
    ;;(sandbox-sub::blocksound x y z)
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
	  (sandbox-sub::entity-aabb *ent*))))
    (with-vec (x y z) ((testbed::player-position))
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
	       (aabb sandbox-sub::*fist-aabb*))
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
