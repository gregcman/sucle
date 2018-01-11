(defpackage #:flunflair
  (:use #:cl #:funland))
(in-package :flunflair)

(defparameter *reloadables*
  '(
    text
    foo
    foo2
    text2))
(defun floatify (x)
  (coerce x 'single-float))

(setf sandbox::*some-saves*
      (cdr (assoc (machine-instance) 
		  '(("gm3-iMac" . #P"/home/imac/Documents/lispysaves/saves/sandbox-saves/")
		    ("nootboke" . #P"/home/terminal256/Documents/saves/"))
		  :test 'equal)))

(progn
  (defparameter *textx* 0.0)
  (defparameter *texty* 0.0))

(progn
  (progn
    (defparameter *mouse-x* 0.0)
    (defparameter *mouse-y* 0.0))
  (progn
    (defparameter *old-mouse-x* 0.0)
    (defparameter *old-mouse-y* 0.0)))

(defparameter *lastsel* nil)
(defparameter *selection* nil)

(defparameter *paused* nil)
(defun per-frame (&optional session)
  (declare (ignorable session))
  
  (when (window::mice-free-p)
    (let ((newmousex (floatify (/ window::*mouse-x* funtext::*block-width*)))
	  (newmousey (floatify (/ (- window::*height* window::*mouse-y*)
				  funtext::*block-height*))))
      (setf *old-mouse-x* *mouse-x*
	    *old-mouse-y* *mouse-y*)
      (setf *mouse-x* newmousex
	    *mouse-y* newmousey)))
  (when (and (window::mice-free-p)
	     (window::skey-p (window::mouseval :left)))
    (let ((dx (- *mouse-x* *old-mouse-x*))
	  (dy (- *mouse-y* *old-mouse-y*)))
      (unless (= dx dy 0)
	(incf *textx* dx)
	(incf *texty* dy)
	(funtext::flag-text-dirty))))

  (let ((num (num-key-jp)))
    (case num
      (1 (sound-stuff:play-at "/media/imac/Mac 2/Users/gregmanabat/Music/iTunes/iTunes Music/Elton John/Unknown Album/Tiny Dancer.mp3"
				    0.0 0.0 0.0))))

  (progn
    (when (window::skey-p (window::keyval :n))
      (copy-array-buf))
    (when (window::skey-j-p (window::keyval :b))
      (funtext::flag-text-dirty))
    (when (window::skey-j-p (window::keyval :escape))
      (funfair::quit))
    (when (window::skey-j-p (window::keyval :y))
      (toggle sndbx::*depth-buffer?*))
    (when (window::skey-j-p (window::keyval :h))
      (setf *selection* nil))
    (when (window::skey-j-p (window::keyval :r))
      (window::toggle-mouse-capture)
      (moused))
    (setf *paused* (window::mice-free-p))
    (if *paused*
      (funfair::tick *ticker* (lambda ()))
      (stuff))
    (when (window::skey-j-p (window::keyval :c))
      (sound-stuff::cleanup-poller)))
  
  (map nil #'funfair::reload-if-dirty *reloadables*)
  (let* ((particle (sndbx::entity-particle *ent*))
	 (pos (sndbx::farticle-position particle))
	 (vel (sndbx::farticle-velocity particle)))

    (progn  
      (al:listener :position pos)
      (al:listener :velocity vel)
      (let ((curr (load-time-value (vector 1.0 0.0 0.0 ;;look
					   0.0 1.0 0.0 ;;up
					   )))
	    (other (camat::camera-vec-forward sndbx::*camera*))
	    (other2 (camat::camera-vec-up sndbx::*camera*)))
	(setf (aref curr 0) (- (aref other 0)))
	(setf (aref curr 1) (- (aref other 1)))
	(setf (aref curr 2) (- (aref other 2)))
	(setf (aref curr 3) (aref other2 0))
	(setf (aref curr 4) (aref other2 1))
	(setf (aref curr 5) (aref other2 2))
	(al:listener :orientation curr)))
    (with-vec (a b c) (pos)
      (setfoo (format nil
		      "x: ~10,1F
y: ~10,1F
z: ~10,1F"
		      a b c))))
  (unless (eq *lastsel*
	      *selection*)
    (setf *lastsel*
	  *selection*)
    (setfoo2
     (with-output-to-string (*standard-output*)
       (dolist (item *selection*)
	 (pprint item)))))
  (let ((draw-commands (funfair::getfnc 'funtext::draw-commands)))
    (flet ((drawxyz (x y z)
	     (lparallel.queue:with-locked-queue draw-commands
	       (lparallel.queue:push-queue/no-lock x draw-commands)
	       (lparallel.queue:push-queue/no-lock y draw-commands)
	       (lparallel.queue:push-queue/no-lock z draw-commands))))
      (progn
	(drawxyz *textx* *texty* (glhelp::handle (funfair::getfnc 'text)))
	(drawxyz 10.0 10.0  (glhelp::handle (funfair::getfnc 'text2)))))))
(progn
  (defun setfoo2 (obj)
    (let ((*print-case* :downcase))
      (setf *foo2*
	    (write-to-string
	     obj :pretty t :escape nil)))
    (funtext::flag-text-dirty)
    (funfair::reload 'foo2))
  (defparameter *foo2* nil)
  (funfair::deflazy foo2 ()
    *foo2*)
  (funfair::deflazy text2 (foo2)
    (make-instance
     'glhelp::gl-list
     :handle
     (funtext::mesh-string-gl-points -128.0 -128.0 foo2))))

(progn
  (defun setfoo (obj)
    (let ((*print-case* :downcase))
      (setf *foo*
	    (write-to-string
	     obj :pretty t :escape nil)))
    (funfair::reload 'foo))
  (defparameter *foo* nil)
  (funfair::deflazy foo ()
    *foo*)

  (funfair::deflazy text (foo)
    (make-instance
     'glhelp::gl-list
     :handle
     (funtext::mesh-string-gl-points -128.0 -128.0 foo))))

(defun copy-array-buf ()
  (let ((width 256)
	(height 256))
    (cffi:with-foreign-object (b :uint8 (etouq (* 256 256 4)))
      (dobox ((ypos 0 height)
	      (xpos 0 width))
	     (let ((base (the fixnum (+ xpos (the fixnum (* ypos width))))))
	       (let ((offset (the fixnum (* 4 base))))
		 (let ((num
			#+nil
			(logior (char-code (aref *foo* (mod base 1024)))
				(ash 0 8)
				(ash 255 16))
					;	#+nil
			
			(ash (random (1- (ash 1 32))) -2)
			
			 #+nil
			 (get-char-num
			  (get-char (the fixnum (+ xpos xstart))
				    (the fixnum (+ ypos ystart))))))
		   (setf (cffi:mem-aref b :uint8 (+ offset 0)) (ldb (byte 8 16) num)
			 (cffi:mem-aref b :uint8 (+ offset 1)) (ldb (byte 8 8) num)
			 (cffi:mem-aref b :uint8 (+ offset 2)) (logand 255 num) 
			 (cffi:mem-aref b :uint8 (+ offset 3)) (ldb (byte 8 24) num))
		   ))))
      (progn
	(gl:bind-texture :texture-2d (glhelp::texture (funfair::getfnc 'funtext::text-data)))
	(gl:tex-sub-image-2d :texture-2d 0 0 0 width height :bgra :unsigned-byte b)))))

(defun preload ()
  (let ((array (make-array 16)))
    (dobox ((name 0 4)
	    (number 0 4))
	   (setf (aref array (+ name (* number 4)))
		 (sound-stuff::load-all
		  (concatenate
		   'string
		   "/home/imac/.minecraft/resources/sound/step/"
		   (string-downcase (symbol-name (aref #(stone wood gravel grass)
						       name
						       )))
		   (aref #("1" "2" "3" "4") number
			 )
		   ".ogg")
		  :mono8)))
    array))

(funfair::deflazy preloaded-sounds (funfair::al-context)
  (declare (ignorable funfair::al-context))
 ; (print "loading-sounds")
  (preload))

#+nil
(map nil
     (lambda (x) (sound-stuff::free-preloaded x))
     (funfair::getfnc 'flunflair::PRELOADED-SOUNDS)) 
(defun wot ()
  (funfair::reload-if-dirty 'preloaded-sounds)
  (alexandria:random-elt (funfair::getfnc 'preloaded-sounds)))

(setf funfair::*trampoline* '(sndbx::per-frame funtext::per-frame per-frame))

(defparameter *reach* 128.0)
(defun stuff ()
  (let* ((player-farticle (sndbx::entity-particle *ent*))
	 (pos (sndbx::farticle-position player-farticle))
	 (entity *ent*)
	 (window::*control-state* sndbx::*control-state*))
    (symbol-macrolet ((pos (sndbx::farticle-position (sndbx::entity-particle entity)))
		      (is-jumping (sndbx::entity-jump? entity))
		      (is-sneaking (sndbx::entity-sneak? entity))
		      (fly (sndbx::entity-fly? entity))
		      (gravity (sndbx::entity-gravity? entity))
		      (noclip (sndbx::entity-clip? entity)))
      (setf is-jumping (window::skey-p (window::keyval :space)))
      (setf is-sneaking (window::skey-p (window::keyval :a)))
      (when (window:mice-locked-p)
	(when (window::skey-j-p (window::keyval :v))
	  (toggle noclip))
	(with-vec (x y z) (pos)
	  (when (window::skey-j-p (window::keyval :p))
	    (sandbox::update-world-vao x y z)))
	(when (window::skey-j-p (window::keyval :g))
	  (toggle fly)
	  (toggle gravity))))
    (setf (sndbx::entity-hips *ent*)
	  (wasd-mover
	   (window::skey-p (window::keyval :e))
	   (window::skey-p (window::keyval :s))
	   (window::skey-p (window::keyval :d))
	   (window::skey-p (window::keyval :f))))
    (let ((backwardsbug (load-time-value (cg-matrix:vec 0.0 0.0 0.0))))
      (cg-matrix:%vec* backwardsbug (camat:camera-vec-forward funfair::*camera*) -1.0)
      ((lambda (look-vec pos)
	 (let ((fist *fist*))
	   (with-vec (px py pz) (pos)
	     (with-vec (vx vy vz) (look-vec)	
	       (when (window:mice-locked-p)
		 (when (window::skey-j-p (window::keyval :w))
		   (toggle *swinging*))
		 (when *swinging*
		   (let ((u 16))
		     (sndbx::aabb-collect-blocks
		      px py pz (* u vx) (* u vy) (* u vz)
		      (load-time-value
		       (aabbcc:make-aabb
			:minx -0.3
			:miny -0.5
			:minz -0.3
			:maxx 0.3
			:maxy 1.12
			:maxz 0.3))   
		      *big-fist-fun*))))
	       (let ((left-p (window::skey-j-p (window::mouseval :left)))
		     (right-p (window::skey-j-p (window::mouseval :right))))
		 (when (or left-p right-p)
		   (sndbx::standard-fist
		    fist
		    px py pz
		    (* *reach* vx) (* *reach* vy) (* *reach* vz))
		   (let ((fist? (sndbx::fister-exists fist))
			 (selected-block (sndbx::fister-selected-block fist))
			 (normal-block (sndbx::fister-normal-block fist)))
		     (when fist?
		       (when left-p
			 (with-vec (a b c) (selected-block)
			   (cond  ((window::skey-p (window::keyval :left-shift))
				   (push (vector a b c) *selection*))
				  ((window::skey-p (window::keyval :left-control))
				   (push (world::getobj a b c) *selection*))
				  (t
				   (funcall *left-fist-fnc* a b c)))))
		       (when right-p
			 (with-vec (a b c) (normal-block)
			   (cond ((window::skey-p (window::keyval :left-shift))
				  (push (vector a b c) *selection*))
				 ((window::skey-p (window::keyval :left-control))
				  (push (world::getobj a b c) *selection*))
				 (t
				  (funcall *right-fist-fnc* a b c)))))))))))))
       backwardsbug
       pos)))
  (multiple-value-bind (fraction times) (funfair::tick *ticker* #'physss)
    (declare (ignorable times))
    (when (window:mice-locked-p)
      (update-moused 0.5)
      (sndbx::change-entity-neck
       *ent*
       (coerce (* *lerp-mouse-x*
		  -1.0d0 *mouse-multiplier*)
	       'single-float)
       (coerce (* *lerp-mouse-y* *mouse-multiplier*)
	       'single-float)))
    (sndbx::entity-to-camera *ent* funfair::*camera* fraction)))

(defparameter *ticker*
  (tickr:make-ticker
   (floor 1000000 60)
   most-positive-fixnum))

(defparameter *right-fist-fnc*
  (lambda (x y z)
    (let ((value (world::getblock x y z)))
      (when (and (zerop value)
		 (not-occupied x y z))
	(sound-stuff::play-at (flunflair::wot) x y z)
	(let ((blockval 1))
	  (sandbox::plain-setblock
	   x
	   y
	   z
	   blockval
	   (aref mc-blocks:*lightvalue* blockval)))))))
(defparameter *left-fist-fnc*
  (lambda (x y z)
    (sound-stuff::play-at (flunflair::wot) x y z)
    (sandbox::setblock-with-update x y z 0 0)))

(defparameter *big-fist-fun*
  (lambda (x y z)
    (let ((a (world::getblock x y z)))
      (when (or (= a 1)
		(= a 2)
		(= a 3)
		)
	(sound-stuff::play-at (flunflair::wot) x y z)
	(sandbox::setblock-with-update x y z 0 0))))
  )


;;;detect more entities
(defun not-occupied (x y z)
  (let* ((ent *ent*)
	 (aabb (sndbx::entity-aabb ent))
	 (pos (sndbx::farticle-position
	       (sndbx::entity-particle ent))))
    (aabbcc::aabb-not-overlap
     sndbx::*block-aabb*
     (floatify x)
     (floatify y)
     (floatify z)
     aabb
     (aref pos 0)
     (aref pos 1)
     (aref pos 2))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun moused (&optional (data (load-time-value (cons 0.0d0 0.0d0))))
  (multiple-value-bind (x y) (values window::*mouse-x* window::*mouse-y*)
    (multiple-value-prog1
	(values (- x (car data))
		(- y (cdr data)))
	(setf (car data) x
	      (cdr data) y))))
(defun update-moused (&optional (smoothing-factor 0.5))
  (multiple-value-bind (dx dy) (moused)
    (let ((x (+ *tmouse-x* dx))
	  (y (+ *tmouse-y* dy)))
      (let ((value *mouse-multiplier-aux*))
	(when (> y value)
	  (setf y value))
	(when (< y (- value))
	  (setf y (- value))))
      (setf *tmouse-x* x)
      (setf *tmouse-y* y)
      (setf *lerp-mouse-x* (alexandria:lerp smoothing-factor *lerp-mouse-x* x))
      (setf *lerp-mouse-y* (alexandria:lerp smoothing-factor *lerp-mouse-y* y)))))
(defparameter *tmouse-x* 0.0d0)
(defparameter *tmouse-y* 0.0d0)
(defparameter *lerp-mouse-x* 0.0d0)
(defparameter *lerp-mouse-y* 0.0d0)
(progn
  (declaim (ftype (function (single-float) single-float)
		  translator))
  (funland::with-unsafe-speed
    (defun translator (x)
      (let* ((a (* x 0.6))
	     (b (+ 0.2 a))
	     (c (* b b b))
	     (d (* 8.0 0.15 (/ (coerce pi 'single-float) 180.0)))
	     (e (* d c)))
	(declare (type single-float a b c d e))
	e))))

(defparameter *mouse-multiplier* (translator 0.5))
(defparameter *mouse-multiplier-aux* (/ (* 0.5 pi 0.9999) *mouse-multiplier*))
(defparameter *swinging* nil)
(defparameter *fist*
  (sndbx::gen-fister sndbx::*fist-aabb* (list #'sndbx::ahook)))
;;;;;

(defun wasd-mover (w? a? s? d?)
  (let ((x 0)
	(y 0))
    (when w? (decf x))
    (when a? (decf y))
    (when s? (incf x))
    (when d? (incf y))
    (if (and (zerop x)
	     (zerop y))
	nil
	(atan y x))))

(defparameter *ent* (sndbx::gentity))

(defun physss ()
  (sndbx::physentity *ent*))

(defun num-key-jp (&optional (control-state window::*control-state*))
  (etouq
   (cons
    'cond
    (mapcar
     (lambda (n)
       `((window::skey-j-p
	  (window::keyval ,(intern (write-to-string n) :keyword))
	  control-state) ,n))
     '(0 1 2 3 4 5 6 7 8 9)))))
