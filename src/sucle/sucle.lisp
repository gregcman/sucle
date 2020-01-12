
(in-package :sucle)

;;;;************************************************************************;;;;
;;;;<BOXES?>
(defun create-aabb (&optional (maxx 1.0) (maxy maxx) (maxz maxx)
		      (minx (- maxx)) (miny (- maxy)) (minz (- maxz)))
  (aabbcc::make-aabb
   :minx minx
   :maxx maxx
   :miny miny
   :maxy maxy
   :minz minz
   :maxz maxz))

(defparameter *block-aabb*
  ;;;;1x1x1 cube
  (create-aabb 1.0 1.0 1.0 0.0 0.0 0.0))

(defparameter *slab-aabb*
  ;;;;slab
  (create-aabb 1.0 0.5 1.0 0.0 0.0 0.0))

;;;;FIXME::The point of this is to reduce the amount of bits to store the hitbox.
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

;;;a very small cubic fist
(defparameter *fist-aabb* (create-aabb 0.00005))

(defparameter *chunk-aabb*
  (apply 'create-aabb
	 (mapcar 'floatify
		 (list
		  world::*chunk-size-x*
		  world::*chunk-size-y*
		  world::*chunk-size-z*
		  0.0
		  0.0
		  0.0))))

(defparameter *big-fist-aabb*
  (create-aabb
   ;;0.5
   ;;1.5
   8.0))
;;;;</BOXES?>

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;************************************************************************;;;;


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
(defparameter *fov* (* (floatify pi) (/ 85 180)))
(defparameter *camera*
  (camera-matrix:make-camera
   :frustum-far (* 256.0)
   :frustum-near (/ 1.0 8.0)))
(defparameter *fog-ratio* 0.75)

(defun update-camera (&optional (camera *camera*))
  (setf (camera-matrix:camera-aspect-ratio camera)
	(/ (floatify window::*width*)
	   (floatify window::*height*)))
  (setf (camera-matrix:camera-fov camera) *fov*)
  (setf (camera-matrix:camera-frustum-far camera) (* 1024.0 256.0))
  (camera-matrix:update-matrices camera))

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
  (cond
    (*paused*
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
     ;;update the camera
     (update-camera *camera*)
     (draw-to-default-area)
     ;;this also clears the depth and color buffer.
     (render-sky)
     (use-chunk-shader *camera*)
     (render-chunks)
     (use-occlusion-shader *camera*)
     (render-chunk-occlusion-queries)
     ;;selected block and crosshairs
     (use-solidshader *camera*)
     (render-fist *fist*)
     (render-crosshairs)
     ;;FIXME::what is glFlush good for?
     ;;(gl:flush)
     (sandbox::designatemeshing))))

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
		;;FIXME::reactive? functional?
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


(defparameter *big-fist-fun* (constantly nil))
(defparameter *left-fist-fnc* 'destroy-block-at)
(defparameter *right-fist-fnc* 'place-block-at)
(defparameter *middle-fist-fnc* 'place-block-at)
(defparameter *4-fist-fnc* 'tree)
(defparameter *5-fist-fnc* '5fun)
#+nil
(progn
  (setf *big-fist-fun* 'correct-earth)
  (setf *middle-fist-fnc* 'player-feet-at)
  (setf *middle-fist-fnc* 'line-to-player-feet))
