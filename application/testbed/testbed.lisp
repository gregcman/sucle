(defpackage #:testbed
  (:use #:cl #:utility))
(in-package :testbed)

(defun particle-al-listener (particle)
  (let ((pos (sandbox-sub::farticle-position particle))
	(vel (sandbox-sub::farticle-velocity particle)))
    (al:listener :position pos)
    (al:listener :velocity vel)))
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
      (%al:listener-fv :orientation array))))
(defun wasd-mover (w? a? s? d?)
  (let ((acc 0))
    (flet ((add (var bit)
	     (when var
	       (setf acc (logior acc bit)))))
      (add w? #b0001)
      (add a? #b0010)
      (add s? #b0100)
      (add d? #b1000))   
    (aref (etouq (let ((array (make-array (expt 2 4))))
		   (dotimes (index (length array))
		     (symbol-macrolet ((w? (logbitp 0 index))
				       (a? (logbitp 1 index))
				       (s? (logbitp 2 index))
				       (d? (logbitp 3 index)))
		       (let ((x 0)
			     (y 0))
			 (when w? (decf x))
			 (when a? (decf y))
			 (when s? (incf x))
			 (when d? (incf y))
			 (if (and (zerop x)
				  (zerop y))
			     (setf (aref array index) nil)
			     (setf (aref array index)
				   (floatify (atan y x)))))))
		   array))
	  acc)))

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

;;;;;
(setf sandbox::*some-saves*
      (cdr (assoc (machine-instance) 
		  '(("gm3-iMac" . #P"/media/imac/share/space/lispysaves/saves/sandbox-saves/")
		    ("nootboke" . #P"/home/terminal256/Documents/saves/"))
		  :test 'equal)))

(defun set-trampoline ()
  (setf application::*trampoline* '(sandbox-sub::per-frame
				    per-frame
				    )))
(defun start ()
  (set-trampoline)
  (application::main))

(defparameter *paused* nil)
(defun per-frame (&optional session)
  (declare (ignorable session))

  (when (window::skey-j-p (window::keyval #\))
    (application::quit))
  (when (window::skey-j-p (window::keyval #\E))
      (window::toggle-mouse-capture)
      (moused))
  (setf *paused* (window::mice-free-p))
  (if *paused*
	(application::tick *ticker* (lambda ()))
	(stuff))
  
  (particle-al-listener (sandbox-sub::entity-particle *ent*))
  (camera-al-listener sandbox-sub::*camera*))

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

(defun physss ()
  (incf *ticks*)
  (let ((seconds (or 60 840)))
    (setf sandbox::*daytime*
	  (or
	   1.0
	   (floatify
	    (/ (abs (- (mod (/ *ticks*
			       60.0)
			    (* 2 seconds))
		       seconds))
	       seconds)))))
  (sandbox-sub::physentity *ent*))
#+nil
(progn
  (defparameter *start-fov* (* 95 (floatify (/ pi 180.0))))
  (defparameter *target-fov* (* 110 (floatify (/ pi 180.0))))
  (defun ease (a b &optional (modifier 0.5))
    (alexandria:lerp modifier a b))
  (define-modify-macro easef (b &optional (modifier 0.5)) ease))

(defparameter *reach* 64.0)

(defun stuff ()
  (let* ((player-farticle (sandbox-sub::entity-particle *ent*))
	 (pos (sandbox-sub::farticle-position player-farticle))
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
      (multiple-value-bind (fraction times) (application::tick *ticker* #'physss)
	(declare (ignorable times))
	(let ((neck (sandbox-sub::entity-neck *ent*)))
	  (when (window:mice-locked-p)
	    (update-moused *mouse-multiplier-aux* 0.5)
	    (setf (sandbox-sub::necking-yaw neck)
		  (floatify (- (* *lerp-mouse-x* *mouse-multiplier*)))
		  (sandbox-sub::necking-pitch neck)
		  (floatify (* *lerp-mouse-y* *mouse-multiplier*))))
	  (unit-pitch-yaw (camera-matrix:camera-vec-forward application::*camera*)
			  (sandbox-sub::necking-pitch neck)
			  (sandbox-sub::necking-yaw neck))
	  (let ((curr (sandbox-sub::farticle-position player-farticle))
		(prev (sandbox-sub::farticle-position-old player-farticle))
		(camera application::*camera*))
	    (let ((vec (camera-matrix:camera-vec-position camera))
		  (cev (camera-matrix:camera-vec-noitisop camera)))
	      (nsb-cga:%vec-lerp vec prev curr fraction)
	      (when (and (not fly)
			 (eql 0 is-sneaking))
		(nsb-cga:%vec- vec vec (load-time-value (nsb-cga:vec 0.0 0.125 0.0))))
	      (nsb-cga:%vec* cev vec -1.0))))))))

(defun fist-stuff (pos)
  (let ((look-vec (load-time-value (nsb-cga:vec 0.0 0.0 0.0))))
    (nsb-cga:%vec* look-vec (camera-matrix:camera-vec-forward application::*camera*) -1.0)
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
			:minx -0.5
			:miny -0.5
			:minz -0.5
			:maxx 0.5
			:maxy 0.5
			:maxz 0.5)))
		  (x y z contact)
		(declare (ignorable contact))		     
		(funcall *big-fist-fun* x y z)))))
	(let ((fist *fist*))
	  (let ((left-p (window::skey-j-p (window::mouseval :left)))
		(right-p (window::skey-j-p (window::mouseval :right))))
	    (when (or left-p right-p)
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
		      (funcall *right-fist-fnc* a b c))))))))))))


;;;detect more entities
;;;detect block types?
(defun not-occupied (x y z &optional (ent *ent*))
  (let ((aabb (sandbox-sub::entity-aabb ent))
	(pos (sandbox-sub::farticle-position
	      (sandbox-sub::entity-particle ent))))
    (aabbcc::aabb-not-overlap
     sandbox-sub::*block-aabb*
     (floatify x)
     (floatify y)
     (floatify z)
     aabb
     (aref pos 0)
     (aref pos 1)
     (aref pos 2))))

(defparameter *blockid* 3)
(defparameter *count* 0)
(defparameter *right-fist-fnc*
  (lambda (x y z)
    (when 
	(not-occupied x y z)
      
      (let ((blockval (let ((seq
			     #(3 13 12 24 1 2 18 17 20 5 89)))
			(elt seq (mod (round window::*scroll-y*)
				      (length seq))))))
	(sandbox::setblock-with-update
	 x
	 y
	 z
	 blockval
	 (aref mc-blocks:*lightvalue* blockval))))))
(defparameter *left-fist-fnc*
  (lambda (x y z)
    (sandbox::setblock-with-update x y z 0 0)))

(defparameter *big-fist-fun*
  (lambda (x y z)
    (let ((id (world::getblock x y z)))
      (unless (zerop id)
	(sandbox::setblock-with-update x y z 0 0)))))

