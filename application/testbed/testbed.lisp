(defpackage #:testbed
  (:use #:cl #:utility))
(in-package :testbed)

(setf sandbox::*some-saves*
      (cdr (assoc (machine-instance) 
		  '(("gm3-iMac" . #P"/media/imac/share/space/lispysaves/saves/sandbox-saves/")
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

(progn
  (defparameter *selection* nil)
  (defparameter *lastpos* nil))

(defparameter *paused* nil)
(defparameter *blockid* 1)

(defun vec-values (vec)
  (apply #'values (coerce vec 'list)))
(defun per-frame (&optional session)
  (declare (ignorable session))

  #+nil
  (when (window::mice-free-p)
    (let ((newmousex (floatify (/ window::*mouse-x* text-sub::*block-width*)))
	  (newmousey (floatify (/ (- window::*height* window::*mouse-y*)
				  text-sub::*block-height*))))
      (setf *old-mouse-x* *mouse-x*
	    *old-mouse-y* *mouse-y*)
      (setf *mouse-x* newmousex
	    *mouse-y* newmousey)))
  #+nil
  (when (and (window::mice-free-p)
	     (window::skey-p (window::mouseval :left)))
    (let ((dx (- *mouse-x* *old-mouse-x*))
	  (dy (- *mouse-y* *old-mouse-y*)))
      (unless (= dx dy 0)
	(incf *textx* dx)
	(incf *texty* dy)
	(text-sub::flag-text-dirty))))

  (when (window::skey-j-p (window::keyval #\))
    (application::quit))
  (when (window::skey-j-p (window::keyval #\E))
      (window::toggle-mouse-capture)
      (moused))
  ;(more-controls)
  (setf *paused* (window::mice-free-p))
  (if *paused*
	(application::tick *ticker* (lambda ()))
	(stuff))
  
  (let* ((particle (sandbox-sub::entity-particle *ent*))
	 (pos (sandbox-sub::farticle-position particle))
	 (vel (sandbox-sub::farticle-velocity particle)))
    (progn  
      (al:listener :position pos)
      (al:listener :velocity vel)
      (let ((curr (load-time-value (vector 1.0 0.0 0.0 ;;look
					   0.0 1.0 0.0 ;;up
					   )))
	    (other (camera-matrix::camera-vec-forward sandbox-sub::*camera*))
	    (other2 (camera-matrix::camera-vec-up sandbox-sub::*camera*)))
	(setf (aref curr 0) (- (aref other 0)))
	(setf (aref curr 1) (- (aref other 1)))
	(setf (aref curr 2) (- (aref other 2)))
	(setf (aref curr 3) (aref other2 0))
	(setf (aref curr 4) (aref other2 1))
	(setf (aref curr 5) (aref other2 2))
	(al:listener :orientation curr)))))

#+nil
(defun copy-array-buf ()
  (let ((width 256)
	(height 256))
    (cffi:with-foreign-object (b :uint8 (etouq (* 256 256 4)))
      (with-unsafe-speed
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
		       )))))
      (progn
	(gl:bind-texture :texture-2d (glhelp::texture (application::getfnc 'text-sub::text-data)))
	(gl:tex-sub-image-2d :texture-2d 0 0 0 width height :bgra :unsigned-byte b)))))

#+nil
(defun more-controls ()
  (progn
    (when (window::skey-j-p (window::keyval :n))
      (copy-array-buf))
    (when (window::skey-j-p (window::keyval :6))
      (setf *blockid* (multiple-value-call #'world::getblock
			(vec-values (pop *selection*)))))
    (when (window::skey-j-p (window::keyval :y))
      (toggle sandbox-sub::*depth-buffer?*))
    (when (window::skey-j-p (window::keyval :r))
      (setf *selection* nil))
    (when (window::skey-j-p (window::keyval :q))
      (pop *selection*))
    (when (window::skey-j-p (window::keyval :2))
      (map-box (hollowbox (lambda (x y z)
			    (sandbox::plain-setblock x y z *blockid* 0))
			  2)))
    (when (window::skey-j-p (window::keyval :5))
      (map-box (hollowbox (lambda (x y z)
			    (sandbox::plain-setblock x y z *blockid* 0))
			  0)))
    (when (window::skey-j-p (window::keyval :4))
      (map-box (sphere (lambda (x y z)
			 (sandbox::plain-setblock x y z *blockid* 0)))))
    (with-vec (a b c) ((sandbox-sub::farticle-position
			(sandbox-sub::entity-particle *ent*)))
      (when (window::skey-j-p (window::keyval :b))
	(setf *box* (make-box (pop *selection*)
			      (pop *selection*))))
      (when (window::skey-j-p (window::keyval :x))      
	(push (vector (floor a) (floor b) (floor c)) *selection*))
      (when (window::skey-p (window::keyval :z))      
	(setfoo (setf *lastpos* (vector (floor a) (floor b) (floor c))))))
    
    (when (window::skey-j-p (window::keyval :1))
      (multiple-value-call #'line2
	(vec-values (pop *selection*))
	(vec-values (pop *selection*))
	*blockid*))
    (when (window::skey-j-p (window::keyval :c))
      (music::cleanup-poller))))

(defun preload ()
  (let ((array (make-array (* 4 7))))
    (dobox ((number 0 4)
	    (name 0 7))
	   (setf (aref array (+ number (* name 4)))
		 (music::load-all
		(print
		  (concatenate
		   'string
		   "/home/imac/.minecraft/resources/sound3/dig/"
		   (string-downcase (symbol-name (aref #(stone wood gravel grass sand snow cloth)
						       name
						       )))
		   (aref #("1" "2" "3" "4") number
			 )
		   ".ogg"))
		  :mono8)))
    array))

(application::deflazy preloaded-sounds (application::al-context)
  (declare (ignorable application::al-context))
  (print "loading-sounds")
  (preload))
#+nil
(map nil
     (lambda (x) (music::free-preloaded x))
     (application::getfnc 'PRELOADED-SOUNDS))
(defparameter *wot-counter* 0)
(defun wot (value)
  (incf *wot-counter*)
  (aref 
   (application::getfnc 'preloaded-sounds)
   (+ (mod *wot-counter* 4)
      (* 4 (sound-dispatch value)))))

(defun sound-dispatch (value)
  (case value
    (0 0) ;air
    ((1 4 7 14 15 16 21 22 23 24) 0)				;stone,cobble
    ((2 18) 3)				;grass
    ((3 13) 2)				;dirt ,gravel
    ((5 17) 1)				;wood, log
    (12 4) ;sand
    ((35 81) 6)
    (otherwise (random 7))))

(defun set-trampoline ()
  (setf application::*trampoline* '(sandbox-sub::per-frame
				    per-frame
				    )))
(defun start ()
  (set-trampoline)
  (application::main))

(defparameter *reach* 64.0)
(defun stuff ()
  (let* ((player-farticle (sandbox-sub::entity-particle *ent*))
	 (pos (sandbox-sub::farticle-position player-farticle))
	 (entity *ent*)
	 (window::*control-state* sandbox-sub::*control-state*))
    (symbol-macrolet ((pos (sandbox-sub::farticle-position (sandbox-sub::entity-particle entity)))
		      (is-jumping (sandbox-sub::entity-jump? entity))
		      (is-sneaking (sandbox-sub::entity-sneak? entity))
		      (fly (sandbox-sub::entity-fly? entity))
		      (gravity (sandbox-sub::entity-gravity? entity))
		      (noclip (sandbox-sub::entity-clip? entity)))
      (setf is-jumping (window::skey-p (window::keyval #\ )))
      (setf is-sneaking (window::skey-p (window::keyval :left-shift)))
      (when (window:mice-locked-p)
	(when (window::skey-j-p (window::keyval #\V))
	  (toggle noclip))
	(with-vec (x y z) (pos)
	  (when (window::skey-j-p (window::keyval #\P))
	    (sandbox::update-world-vao x y z)))
	(when (window::skey-j-p (window::keyval #\F))
	  (toggle fly)
	  (toggle gravity))))
    (setf (sandbox-sub::entity-hips *ent*)
	  (wasd-mover
	   (window::skey-p (window::keyval #\W))
	   (window::skey-p (window::keyval #\A))
	   (window::skey-p (window::keyval #\S))
	   (window::skey-p (window::keyval #\D))))
    (let ((backwardsbug (load-time-value (nsb-cga:vec 0.0 0.0 0.0))))
      (nsb-cga:%vec* backwardsbug (camera-matrix:camera-vec-forward application::*camera*) -1.0)
      ((lambda (look-vec pos)
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
			   (cond
			     #+nil
			     ((window::skey-p (window::keyval :left-control))
			      (push (vector a b c) *selection*))
			     #+nil
			     ((window::skey-p (window::keyval :left-alt))
			      (push (world::getobj a b c) *selection*))
			     (t
			      (funcall *left-fist-fnc* a b c)))))
		       (when right-p
			 (with-vec (a b c) (normal-block)
			   (cond
			     #+nil
			     ((window::skey-p (window::keyval :left-control))
			      (push (vector a b c) *selection*))
			     #+nil
			     ((window::skey-p (window::keyval :left-alt))
			      (push (world::getobj a b c) *selection*))
			     (t
			      (funcall *right-fist-fnc* a b c)))))))))))))
       backwardsbug
       pos)))
  (multiple-value-bind (fraction times) (application::tick *ticker* #'physss)
    (declare (ignorable times))
    (when (window:mice-locked-p)
      (update-moused 0.5)
      (sandbox-sub::change-entity-neck
       *ent*
       (coerce (* *lerp-mouse-x*
		  -1.0d0 *mouse-multiplier*)
	       'single-float)
       (coerce (* *lerp-mouse-y* *mouse-multiplier*)
	       'single-float)))
    (sandbox-sub::entity-to-camera *ent* application::*camera* fraction)))

(defparameter *ticker*
  (fps-independent-timestep:make-ticker
   (floor 1000000 60)
   most-positive-fixnum))

(defparameter *right-fist-fnc*
  (lambda (x y z)
    (let ((value (world::getblock x y z)))
      (when (and (zerop value)
		 (not-occupied x y z))
	(music::play-at (wot value) (+ x 0.5) (+ y 0.5) (+ 0.5 z)
			      0.8 1.0)
	(let ((blockval *blockid*))
	  (sandbox::plain-setblock
	   x
	   y
	   z
	   blockval
	   (aref mc-blocks:*lightvalue* blockval)))))))
(defparameter *left-fist-fnc*
  (lambda (x y z)
    (let ((blockid (world::getblock x y z)))
      (unless (= blockid 0)
      (music::play-at (wot blockid) 
			    (+ x 0.5) (+ y 0.5) (+ 0.5 z)
			    0.8
			    1.0)))
    (sandbox::setblock-with-update x y z 0 0)))

(defparameter *big-fist-fun*
  (lambda (x y z)
    (let ((id (world::getblock x y z)))
      (unless (zerop id)
	(music::play-at (wot id) x y z 0.8 1.0)	
	(sandbox::setblock-with-update x y z 0 0)))))


;;;detect more entities
;;;detect block types?
(defun not-occupied (x y z)
  (let* ((ent *ent*)
	 (aabb (sandbox-sub::entity-aabb ent))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun moused (&optional (data (load-time-value (cons 0.0d0 0.0d0))))
  (multiple-value-bind (x y) (values window::*mouse-x* window::*mouse-y*)
    (multiple-value-prog1
	(values (- x (car data))
		(- y (cdr data)))
	(setf (car data) x
	      (cdr data) y))))
(defun update-moused (&optional (smoothing-factor 1.0))
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
  (with-unsafe-speed
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
  (sandbox-sub::gen-fister))
;;;;;

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

(defparameter *ent* (sandbox-sub::gentity))

(defun physss ()
  (sandbox-sub::physentity *ent*))

#+nil
(defun num-key-jp (&optional (control-state window::*control-state*))
  (etouq
   (cons
    'cond
    (mapcar
     (lambda (n)
       `((window::skey-j-p
	  (window::keyval ,n)
	  control-state) ,n))
     '(0 1 2 3 4 5 6 7 8 9)))))
