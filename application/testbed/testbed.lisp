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
     (sandbox::with-world-meshing-lparallel
       (loop
	  (application:poll-app)
	  (per-frame))))
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
gl_FragColor.rgb = color_out;
}"
   '(("position" 0) 
     ("color" 3))
   '((:pmv "projection-model-view"))))
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
(defun render? ()
  (when (sandbox-sub::fister-exists *fist*)
    (let ((shader (application:getfnc 'solidshader)))
      (glhelp::use-gl-program shader)
      ;;uniform crucial for first person 3d
      (glhelp:with-uniforms uniform shader
	(gl:uniform-matrix-4fv 
	 (uniform :pmv)
	 (camera-matrix:camera-matrix-projection-view-player sandbox-sub::*camera*)
	 nil)))

    (gl:disable :cull-face)
    (gl:polygon-mode :front-and-back :line)
    (gl:line-width 2)
    (let ((selected-block (sandbox-sub::fister-selected-block testbed::*fist*)))
      (with-vec (a b c) (selected-block)
	(sandbox-sub::draw-aabb a b c *block-aabb2*))))
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
  (application::on-session-change *session*
    (load-world t))
  (when (window::skey-j-p (window::keyval #\))
    (application::quit))
  (when (window::skey-j-p (window::keyval #\E))
      (window::toggle-mouse-capture)
      (moused))
  (setf *paused* (window::mice-free-p))
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
	(with-vec (x y z) (pos)
	  (when (window::skey-j-p (window::keyval #\P))
	    (sandbox::update-world-vao x y z)))
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
		   1.0
		   #+nil
		   (let ((seconds (or 60 840)))
		     (sin
		      (/ *ticks*
			 60
			 seconds)))))
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
	    (let ((u 32))
	      (aabbcc::aabb-collect-blocks
		  (px py pz (* u vx) (* u vy) (* u vz)
		      (load-time-value
		       (aabbcc:make-aabb
			:minx -1.5
			:miny -1.5
			:minz -1.5
			:maxx  1.5
			:maxy  1.5
			:maxz  1.5)))
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

(defparameter *blockid* 1)
(defparameter *count* 0)
(defparameter *right-fist-fnc*
  (lambda (x y z)
    (when 
	(not-occupied x y z)
      (let ((blockval *blockid*))
	(sandbox::setblock-with-update
	 x
	 y
	 z
	 blockval
	 (aref block-data:*lightvalue* blockval))
	(sandbox-sub::blocksound x y z)
	))))

(defun dirtngrass (x y z)
  (dirts x y z)
  (grassify x y z))

(defun dirts (x y z)
  (let ((blockid (world:getblock x y z)))
    (when (= blockid 1)
      (when (or (zerop (world:getblock x (+ 2 y) z))
		(zerop (world:getblock x (+ 3 y) z)))
	(sandbox::plain-setblock x y z 3 0)))))

(defun grassify (x y z)
  (let ((blockid (world:getblock x y z)))
    (when (= blockid 3)
      (let ((idabove (world:getblock x (1+ y) z)))
	(when (zerop idabove)
	  (sandbox::plain-setblock x y z 2 0))))))

(let* ((b 4)
       (a (- b)))
  (defparameter *box* (vector
		       a b
		       a b
		       a b)))

(defun map-box (func &optional (box *box*))
  (declare (type (function (fixnum fixnum fixnum)) func)
	   (type simple-vector box))
  (with-vec (x0 x1 y0 y1 z0 z1) (box)
    (dobox ((x x0 x1)
	    (y y0 y1)
	    (z z0 z1))
	   (funcall func x y z))))
(defun translate-box (x y z box)
  (with-vec (a b c d e f) (box)
    (vector (+ a x) (+ b x)
	    (+ c y) (+ d y)
	    (+ e z) (+ f z))))
(defun sphere (fun &optional (box *box*))
    (with-vec (x0 x1 y0 y1 z0 z1) (box)
      (let* ((x2 (ash (+ x0 x1) -1))
	     (y2 (ash (+ y0 y1) -1))
	     (z2 (ash (+ z0 z1) -1))
	     (x4 (ash (- x1 x0) -1))
	     (y4 (ash (- y1 y0) -1))
	     (z4 (ash (- z1 z0) -1))
	     (x5 (* y4 z4 y4 z4))
	     (y5 (* x4 z4 x4 z4))
	     (z5 (* y4 x4 y4 x4))
	     (tot (* x4 y4 z4 x4 y4 z4)))
	(lambda (x y z)
	  (let ((x3 (- x2 x))
		(y3 (- y2 y))
		(z3 (- z2 z)))
	    (when (> tot (+ (* x3 x3 x5)
			    (* y3 y3 y5)
			    (* z3 z3 z5)))
	      (funcall fun x y z)))))))

(defparameter *big-fist-fun*
  (nth 1
       (list
	(lambda (x y z)
	  (let ((id (world::getblock x y z)))
	    (when (zerop id)
	      (sandbox::setblock-with-update x y z *blockid* 0))))
	#'dirtngrass
	(lambda (x y z)
	  (let ((id (world::getblock x y z)))
	    (unless (zerop id)
	      (sandbox::setblock-with-update x y z 0 0)))))))

(defun neighbors (x y z)
  (let ((tot 0))
    (macrolet ((aux (i j k)
		 `(unless (zerop (world:getblock (+ x ,i) (+ y ,j) (+ z ,k)))
		   (incf tot))))
      (aux 1 0 0)
      (aux -1 0 0)
      (aux 0 1 0)
      (aux 0 -1 0)
      (aux 0 0 1)
      (aux 0 0 -1))
    tot))

(defun bonder (x y z)
  (let ((blockid (world:getblock x y z)))
    (unless (zerop blockid)
      (let ((naybs (neighbors x y z)))
	(when (> 3 naybs)	  
	  (sandbox::setblock-with-update x y z 0))))))
(defun bonder2 (x y z)
  (let ((blockid (world:getblock x y z)))
    (when (zerop blockid)
      (let ((naybs (neighbors x y z)))
	(when (< 1 naybs)	  
	  (sandbox::setblock-with-update x y z 1))))))
(defun bonder3 (x y z)
  (let ((blockid (world:getblock x y z)))
    (when (zerop blockid)
      (let ((naybs (neighbors x y z)))
	(when (< 2 naybs)	  
	  (sandbox::setblock-with-update x y z 1))))))

(defun remove-empty-chunks ()
  (let ((times 0))
    (maphash (lambda (k v)
	       (when (all-zeroes-p v)
		 (remhash k world::*lispobj*)
		 (incf times)))
	     world::*lispobj*)
    (format t "removed ~s chunks" times)))
(defun all-zeroes-p (sequence)
  (dotimes (x (length sequence))
    (unless (zerop (mod (aref sequence x) 256))
      (return-from all-zeroes-p nil)))
  t)

(defun tree (x y z)
  (incf y)
  (let ((trunk-height (+ 1 (random 3))))
    (let ((yup (+ y trunk-height)))
      (dobox ((z0 -2 3)
	      (x0 -2 3)
	      (y0 0 2))
	     (unless (and (or (= z0 -2)
			      (= z0 2))
			  (or (= x0 -2)
			      (= x0 2))
			  (= y0 1)
			  (zerop (random 2)))
	       (sandbox::setblock-with-update (+ x x0) (+ yup y0) (+ z z0) 18))))
    (let ((yup (+ y trunk-height 2)))
      (dobox ((x0 -1 2)
	      (z0 -1 2)
	      (y0 0 2))
	     (unless (and
		      (= y0 1)
		      (or (= z0 -1)
			  (= z0 1))
		      (or (= x0 -1)
			  (= x0 1))
		      #+nil
		      (zerop (random 2)))
	       (sandbox::setblock-with-update (+ x x0) (+ yup y0) (+ z z0) 18))))
    (dobox ((y0 y (+ y (+ 3 trunk-height))))
	   (sandbox::setblock-with-update x y0 z 17))))

(defparameter *left-fist-fnc*
  #+nil
  (lambda (x y z)
    (let ((*box* (translate-box x y z *box*)))
      (map-box
       (sphere
	(nth 5 ;(random 6)
	     (list
	      (lambda (x y z)
		(dotimes (i 5)
		  (bonder x y z)))
	      (lambda (x y z)
		(dotimes (i 5)
		  (bonder2 x y z)))
	      (lambda (x y z)
		(dotimes (i 5)
		  (bonder3 x y z)))
	      #'dirtngrass
	      (lambda (x y z)
		(sandbox::setblock-with-update
		 x y z
		 *blockid*))
	      (lambda (x y z)
					;(unless (zerop (world:getblock x y z)))
		(sandbox::setblock-with-update x y z 0))))))))
  ;;#'tree
  ;;#+nil
  (lambda (x y z)
    (sandbox-sub::blocksound x y z)
    (sandbox::setblock-with-update x y z 0 0)))
