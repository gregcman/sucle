(in-package :sandbox)

(defparameter float-pi (coerce pi 'single-float))
(defun rad-deg (rad)
  (* rad (/ 180.0 float-pi)))
(defun deg-rad (deg)
  (* deg (/ float-pi 180.0)))


(defparameter *xpos* 0.0)
(defparameter *ypos* 0.0)
(defparameter *zpos* 0.0)

(defun goto (x y z)
  (setf *xpos* (float x)
	*ypos* (float y)
	*zpos* (float z)))

(defparameter *xvel* 0.0)
(defparameter *yvel* 0.0)
(defparameter *zvel* 0.0)

(defparameter *xpos-old* 0.0)
(defparameter *ypos-old* 0.0)
(defparameter *zpos-old* 0.0)

(defparameter *yaw* 0.0)
(defparameter *pitch* 0.0)
(defparameter defaultfov (deg-rad 70))

(defparameter air-friction 0.98)
(defparameter walking-friction (* 0.6 0.9))

(defparameter noclip nil)
(defparameter gravity nil)
(defparameter fly t)

(defparameter onground nil)

(defparameter *old-scroll-y* 0.0)

(defparameter daytime 1.0)
(defparameter ticks/sec nil)
(defparameter tickscale nil)
(defparameter tick-delay nil)

(progn    
  (setf ticks/sec 60.0)
  (setf tickscale (/ 20.0 ticks/sec))
  (setf tick-delay (/ 1000000.0 ticks/sec)))

(world:setup-hashes)

(defparameter net-scroll 0)

(defparameter *paused* nil)

(defparameter *world-collision-fun*
  (configure-collision-handler
   (lambda (collect set-aabb)
     (funcall set-aabb player-aabb)
     (lambda (x y z)
       (when (aref mc-blocks::iscollidable (world:getblock x y z))
	 (funcall collect x y z block-aabb))))))

(defparameter *contact-handler*
  (configure-contact-handler
   (lambda (collect)
     (lambda (x y z)
       (when (aref mc-blocks::iscollidable (world:getblock x y z))
					;	 (plain-setblock x y z (+ 2 (random 4)) 0)
	 (funcall collect x y z block-aabb))))))

(defun collide-with-world (fun)
  (setf (values *xpos* *ypos* *zpos* *xvel* *yvel* *zvel*)
	(collide-world2
	 fun
	 *xpos* *ypos* *zpos* *xvel* *yvel* *zvel*)))

(defparameter *fist* (gen-fister))
(defun physics (control-state)
  (setf *xpos-old* *xpos*
	*ypos-old* *ypos*
	*zpos-old* *zpos*)
  (when (window::skey-j-p (window::keyval :E) control-state) (window:toggle-mouse-capture))
  (when (window:mice-locked-p)
    (when (window::skey-j-p (window::keyval :v) control-state) (toggle noclip))
    (when (window::skey-j-p (window::keyval :t) control-state) (update-world-vao))
    (when (window::skey-j-p (window::keyval :x) control-state) (toggle *paused*))
    (when (window::skey-j-p (window::keyval :f) control-state)
      (toggle fly)
      (toggle gravity)))
  (let ((aabb player-aabb)
	(noclip noclip))
    (unless *paused*
      (let ((new-scroll window::*scroll-y*))
	(setf *old-scroll-y* net-scroll)
	(setf net-scroll new-scroll)
	#+nil
	(let ((value (- new-scroll *old-scroll-y*)))
	  (unless (zerop value)
					;(print value)
	    )))
      (let ((speed (* 0.4 (expt tickscale 2))))
	(cond (fly
	       (setf speed 0.024)
	       (when (window::skey-p (window::keyval :space) control-state)
		 (incf *yvel* speed))
	       (when (window::skey-p (window::keyval :left-shift) control-state)
		 (decf *yvel* speed)))
	      (t
	       (if onground
		   (when (window::skey-p (window::keyval :space) control-state) ;;jumping
		     (incf *yvel* (prog2 0.32
				      (* 
				       (if t 0.49 0.42) (expt tickscale 1)))))
		   (setf speed (* speed 0.2)))))    
	(let ((dir (complex 0 0)))
	  (when (window::skey-p (window::keyval :w) control-state) ;;w
	    (incf dir #C(-1 0)))
	  (when (or (window::skey-p (window::keyval :a) control-state)) ;;a
	    (incf dir #C(0 1)))
	  (when (window::skey-p (window::keyval :s) control-state) ;;s
	    (incf dir #C(1 0)))
	  (when (or (window::skey-p (window::keyval :d) control-state)) ;;d
	    (incf dir #C(0 -1)))
	  (unless (zerop dir)
	    (let ((rot-dir (* dir (cis *yaw*))))
	      (let ((normalized (/ rot-dir ((lambda (x)
					      (declare (optimize (speed 3) (safety 0))
						       (type (complex single-float) x))
					      (abs x))
					    rot-dir))))
		(incf *xvel* (* speed (realpart normalized)))
		(incf *zvel* (* speed (imagpart normalized))))))))
      (collide-with-world (if noclip
			      (lambda (&rest args)
				(declare (ignore args))
				(values 1 nil nil nil))
			      *world-collision-fun*)) 
      (world-forces)
      (contact-handle (if noclip
			  #b000000
			  (funcall *contact-handler* *xpos* *ypos* *zpos* aabb)))
      (let ((look-vec (load-time-value (cg-matrix:vec 0.0 0.0 0.0))))
	(unit-pitch-yaw look-vec
			(coerce *pitch* 'single-float)
			(coerce *yaw* 'single-float))
	(cg-matrix:%vec* look-vec look-vec -4.0)
	(let ((fist *fist*))
	  (standard-fist fist
			 *xpos* *ypos* *zpos*
			 (aref look-vec 0) (aref look-vec 1) (aref look-vec 2))
	  (when (window:mice-locked-p)
	    (when (window::skey-p (window::keyval :q) control-state)
	      (apply-vec3d (lambda (vx vy vz)
			     (big-swing-fist *xpos* *ypos* *zpos* vx vy vz))
			   look-vec)))
	  (use-fist fist
		    (window::skey-j-p (window::mouseval :left) control-state)
		    (window::skey-j-p (window::mouseval :right) control-state)
		    *left-fist-fnc*
		    *right-fist-fnc*)
	  )))))

(defun apply-vec3d (fun vec)
  (funcall fun
	   (aref vec 0)
	   (aref vec 1)
	   (aref vec 2)))

(defparameter *right-fist-fnc*
  (lambda (x y z)
    (let ((blockval 1))
      (setblock-with-update
       x
       y
       z
       blockval
       (aref mc-blocks::lightvalue blockval)))))
(defparameter *left-fist-fnc*
  (lambda (x y z)
    (setblock-with-update x y z 0 0)))

(defun big-swing-fist (px py pz vx vy vz)
  (let ((u 3))
    (aabb-collect-blocks
     px py pz (* u vx) (* u vy) (* u vz)
     fist-aabb   
     (lambda (x y z)
       (when (and (<= 0 x 127)
		  (<= 0 y 127)
		  (<= -128 z -1))
	 (let ((blockid 0))
	   (setblock-with-update x y z blockid  (aref mc-blocks::lightvalue blockid))))))))

(define-modify-macro *= (&rest args)
  *)
(defun world-forces ()
  (if fly
      (progn
	(setf air-friction 0.9)
	(*= *xvel* air-friction)
	(*= *zvel* air-friction))
      (progn
	(setf air-friction 0.98)
	(cond (onground
	       (*= *xvel* walking-friction)
	       (*= *zvel* walking-friction))
	      (t (*= *xvel* 0.9)
		 (*= *zvel* 0.9)
		 ))))
  (when (and (not onground)
	     gravity)
    (decf *yvel* (* 0.08 (expt tickscale 2))))
  (*= *yvel* air-friction)
  )

(defparameter *pos-previous* (cg-matrix:vec 0.0 0.0 0.0))
(defparameter *pos-current* (cg-matrix:vec 0.0 0.0 0.0))
(defun set-render-cam-pos (camera partial)
  (let ((vec (camera-vec-position camera))
	(cev (camera-vec-noitisop camera))
	(prev *pos-previous*)
	(curr *pos-current*))

    (setf (aref prev 0) *xpos-old*)
    (setf (aref prev 1) *ypos-old*)
    (setf (aref prev 2) *zpos-old*)
    
    (setf (aref curr 0) *xpos*)
    (setf (aref curr 1) *ypos*)
    (setf (aref curr 2) *zpos*)

    (cg-matrix:%vec-lerp vec prev curr partial)
    (cg-matrix:%vec* cev vec -1.0)
    
    (unit-pitch-yaw (camera-vec-forward camera)
		    (coerce *pitch* 'single-float)
		    (coerce *yaw* 'single-float))
    
    (setf (camera-fov camera) defaultfov)
    ))

#+nil
(defun cg-matrix-distance (a b)
  (declare (type cg-matrix:vec a b))
  (sqrt (+ (* (aref a 0)
	      (aref b 0))
	   (* (aref a 1)
	      (aref b 1))
	   (* (aref a 2)
	      (aref b 2)))))

(defun distance-to-player (x y z)
  (let ((dx (- *xpos* x))
	(dy (- *ypos* y))
	(dz (- *zpos* z)))
    (sqrt (+ (* dx dx) (* dy dy) (* dz dz)))))

(defun contact-handle (acc)
  (multiple-value-bind (i+ i- j+ j- k+ k-)
      (values (logtest acc #b100000)
	      (logtest acc #b010000)
	      (logtest acc #b001000)
	      (logtest acc #b000100)
	      (logtest acc #b000010)
	      (logtest acc #b000001))
    (cond ((plusp *xvel*) (when i+ (setf *xvel* 0.0))
	   (minusp *xvel*) (when i- (setf *xvel* 0.0))))
    (cond ((plusp *yvel*) (when j+ (setf *yvel* 0.0))
	   (minusp *yvel*) (when j- (setf *yvel* 0.0))))
    (cond ((plusp *zvel*) (when k+ (setf *zvel* 0.0))
	   (minusp *zvel*) (when k- (setf *zvel* 0.0))))
    (setf onground j-)))

#+nil
(defun block-touches (px py pz aabb)
  (let ((acc 0))
    (get-blocks-around
     px py pz aabb
     (lambda (x y z)
       (when (aref mc-blocks::iscollidable (world:getblock x y z))
					;	 (plain-setblock x y z (+ 2 (random 4)) 0)
	 (logiorf
	  acc
	  (aabbcc::aabb-contact px py pz aabb x y z block-aabb)))))
    acc))

#+nil
(defun block-touches (px py pz aabb)
  (let (x- x+ y- y+ z- z+)
    (get-blocks-around
     px py pz aabb
     (lambda (x y z)
       (when (aref mc-blocks::iscollidable (world:getblock x y z))
					;	 (plain-setblock x y z (+ 2 (random 4)) 0)
	 (multiple-value-bind (i+ i- j+ j- k+ k-)
	     (aabbcc::aabb-contact px py pz aabb x y z block-aabb)
	   (if i+ (setq x+ t))
	   (if i- (setq x- t))
	   (if j+ (setq y+ t))
	   (if j- (setq y- t))
	   (if k+ (setq z+ t))
	   (if k- (setq z- t))))))
    (values x+ x- y+ y- z+ z-)))

#+nil
(xyz? xy? xz? yz? x? y? z?)
#+nil

(macrolet ((null! (&rest args)
	     (let (acc)
	       (dolist (arg args)
		 (push `(setf ,arg nil) acc))
	       `(progn ,@acc))))
  (null! xyz? xy? yz? xz? x? y? z?))
#+nil
(multiple-value-bind (minx miny minz maxx maxy maxz) (get-blocks-around px py pz aabb)
      (dobox ((x minx maxx)
	      (y miny maxy)
	      (z minz maxz))))

#+nil
(defun get-blocks-around (px py pz aabb)
  (declare (ignorable aabb))
  (let ((minx (- (truncate px) 2))
	(miny (- (truncate py) 2))
	(minz (- (truncate pz) 2)))
    (let ((maxx (+ minx 5))
	  (maxy (+ miny 4))
	  (maxz (+ minz 5)))
 ;     (print (list (-  maxx minx) (- maxy miny) (- maxz minz)))
      (values minx miny minz maxx maxy maxz))))


#+nil
(defmacro with-collidable-blocks ((xvar yvar zvar) (px py pz aabb vx vy vz) &body body)
  `(multiple-value-bind (minx miny minz maxx maxy maxz)
       (get-blocks-around ,px ,py ,pz ,aabb ,vx ,vy ,vz)
     (dobox ((,xvar minx maxx)
	     (,yvar miny maxy)
	     (,zvar minz maxz))
	    ,@body)))

#+nil
(multiple-value-bind (minx miny minz maxx maxy maxz)
	  (get-blocks-around px py pz aabb)
	(dobox ((x minx maxx)
		(y miny maxy)
		(z minz maxz))))

#+nil
(defun player-pos ()
  (world:chunkhashfunc (truncate *xpos*) (truncate *zpos*) (truncate *ypos*)))

#+nil
(progno
   (hotbar-add e:*scroll-y*)
   (unless (zerop e:*scroll-y*)
     (lcalllist-invalidate :hotbar-selector))
   (progno
    (macrolet ((k (number-key)
		 (let ((symb (intern (write-to-string number-key) :keyword)))
		   `(when (window::skey-j-p ,symb)
		      (setf *hotbar-selection* ,(1- number-key))
		      (lcalllist-invalidate :hotbar-selector)))))
      (k 1)
      (k 2)
      (k 3)
      (k 4)
      (k 5)
      (k 6)
      (k 7)
      (k 8)
      (k 9))))


#+nil
(progno
 #+nil
 (defparameter walkblock nil)
 (defparameter foo nil)

 (progno
  (print (reduce #'+ foo))
  (/ 21450611.0 23824968.0))
 (progno
  (when (window::skey-j-p (window::keyval :g) control-state) )
  (when (window::skey-j-p (window::keyval :c) control-state) (toggle walkblock)))
 (when walkblock
   (what345
    (truncate *xpos*)
    (- (truncate *ypos*) 2)
    (- (truncate *zpos*) 1)
    *hotbar-selection*)))

#+nil
(defun what345 (x y z w)
  (declare (optimize (speed 3) (safety 0))
	   (type fixnum x y z w))
  (dobox ((x0 -0 1)
	  (z0 -0 1))
	 (plain-setblock (+ x x0) y (+ z z0) w 0)))
#+nil
(progno
 (let ((speed (* net-scroll -0.002)))
   (incf *xvel* (* speed look-x))
   (incf *yvel* (* speed look-y))
   (incf *zvel* (* speed look-z))))


#+nil
(setf *xpos* 0.0
      *ypos* 0.0
      *zpos* 0.0
      *yaw* 0.0
      *pitch* 0.0)

    #+nil
    (when (window::skey-j-p (window::keyval :h) control-state)
      (time (let ((ans (gl:read-pixels 0 0 1000 1000 :rgba :unsigned-byte)))
				(Setf foo ans))))

#+nil
	  (case 1
	    (0 (when (window::skey-j-p (window::mouseval :right) control-state)
		 (print (list xop yop zop))
		 (princ (world:skygetlight xop yop zop))))
	    (1 ))

#+nil
(defparameter *hotbar-selection* 3)

#+nil
(eval-always
 (import (quote (window::skey-p window::skey-j-p window::skey-j-r window::keyval window::mouseval))))


;;;;150 ms delay for sprinting
;;;;player eye height is 1.5, subtract 1/8 for sneaking

;;gravity is (* -0.08 (expt tickscale 2)) 0 0
;;falling friction is 0.98
;;0.6 * 0.91 is walking friction

;;fov::
;;70 is normal
;;110 is quake pro

#+nil
(defun hotbar-add (num)
  (setf *hotbar-selection* (truncate (mod (+ num *hotbar-selection*) 256))))

#+nil
(defun collide-with-world (aabb)
  (setf (values *xpos* *ypos* *zpos* *xvel* *yvel* *zvel*)
	(collide-world2
	 (let ((taco (make-touch-collector)))
	   (lambda (px py pz vx vy vz)
	     (reset-touch-collector taco)
	     (flet ((add-it (x y z)
		      (multiple-value-bind (minimum type)
			  (aabbcc::aabb-collide
			   aabb
			   px py pz
			   block-aabb
			   x y z
			   vx vy vz)
			(collect-touch minimum type taco))))
	       (aabb-collect-blocks
		px py pz vx vy vz aabb
		(lambda (x y z)
		  (when (aref mc-blocks::iscollidable (world:getblock x y z))
		    (add-it x y z)))))
	     (multiple-value-bind (xclamp yclamp zclamp)
		 (collapse-touch vx vy vz taco)
	       (values
		(touch-collector-min-ratio taco)
		xclamp yclamp zclamp))))
	 *xpos* *ypos* *zpos* *xvel* *yvel* *zvel*)))

#+nil
(defun physinnit ()
  )
