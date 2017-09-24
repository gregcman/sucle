(in-package :sandbox)

;;;;150 ms delay for sprinting
;;;;player eye height is 1.5, subtract 1/8 for sneaking

;;gravity is (* -0.08 (expt tickscale 2)) 0 0
;;falling friction is 0.98
;;0.6 * 0.91 is walking friction

;;fov::
;;70 is normal
;;110 is quake pro

(defparameter float-pi (coerce pi 'single-float))
(defun rad-deg (rad)
  (* rad (/ 180.0 float-pi)))
(defun deg-rad (deg)
  (* deg (/ float-pi 180.0)))

(defparameter *yaw* 0.0)
(defparameter *pitch* 0.0)
(defparameter *xpos* 0.0)
(defparameter *ypos* 0.0)
(defparameter *zpos* 0.0)
(defparameter *xpos-old* 0.0)
(defparameter *ypos-old* 0.0)
(defparameter *zpos-old* 0.0)
(defparameter defaultfov (deg-rad 70))

(defparameter *xvel* 0.0)
(defparameter *yvel* 0.0)
(defparameter *zvel* 0.0)

(defparameter air-friction 0.98)
(defparameter walking-friction (* 0.6 0.9))

(defparameter noclip nil)
(defparameter gravity nil)
(defparameter fly t)

#+nil
(defparameter *hotbar-selection* 3)

(defparameter onground nil)

(defparameter fist-side-x nil)
(defparameter fist-side-y nil)
(defparameter fist-side-z nil)

(defparameter fist? nil)
(defparameter fist-side nil)
(defparameter fistx 0.0)
(defparameter fisty 0.0)
(defparameter fistz 0.0)

(defparameter reach 4.0)

(eval-always
 (import (quote (window::skey-p window::skey-j-p window::skey-j-r window::keyval window::mouseval))))

(defparameter *old-scroll-y* 0.0)

(defun controls (control-state)
  (let ((new-scroll window::*scroll-y*))
    (setf *old-scroll-y* net-scroll)
    (setf net-scroll new-scroll)
    (let ((value (- new-scroll *old-scroll-y*)))
      (unless (zerop value)
	;(print value)
	)))
  (let ((speed (* 0.4 (expt tickscale 2))))
    (when fly
      (setf speed 0.024)
      (when (skey-p (keyval :space) control-state)
	(incf *yvel* speed))
      (when (skey-p (keyval :left-shift) control-state)
	(decf *yvel* speed)))
    (unless fly
      (when onground
	(when (skey-p (keyval :space) control-state) ;;jumping
	  (incf *yvel* (prog2 0.32 (* 
			    (if t 0.49 0.42) (expt tickscale 1))))))
      (unless onground
	(setf speed (* speed 0.2))))    
    (let ((dir (complex 0 0)))
      (when (skey-p (keyval :w) control-state) ;;w
	(incf dir #C(-1 0)))
      (when (or (skey-p (keyval :a) control-state)) ;;a
	(incf dir #C(0 1)))
      (when (skey-p (keyval :s) control-state) ;;s
	(incf dir #C(1 0)))
      (when (or (skey-p (keyval :d) control-state)) ;;d
	(incf dir #C(0 -1)))
      #+nil
      (when (not (or fly onground))
	(setf speed 0.001))
      (unless (zerop dir)
	(let ((rot-dir (* dir (cis *yaw*))))
;;	  (print (type-of rot-dir))
	  (let ((normalized (/ rot-dir ((lambda (x)
					  (declare (optimize (speed 3) (safety 0))
						   (type (complex single-float) x))
					  (abs x))
					rot-dir))))
	    (incf *xvel* (* speed (realpart normalized)))
	    (incf *zvel* (* speed (imagpart normalized)))))))))

(defparameter mouse-sensitivity (coerce (* 60.0 pi 1/180) 'single-float))
(defconstant two-pi (coerce (* 2 pi) 'single-float))
(defconstant half-pi (coerce (/ pi 2) 'single-float))


(progn
  (declaim (ftype (function (single-float) single-float)
		  translator))
  (fuktard::with-unsafe-speed
    (defun translator (x)
      (let* ((a (* x 0.6))
	     (b (+ 0.2 a))
	     (c (* b b b))
	     (d (* 8.0 0.15 (/ (coerce pi 'single-float) 180.0)))
	     (e (* d c)))
	(declare (type single-float a b c d e))
	e))))

(defun look-around ()
  (multiple-value-bind (dx dy) (delta)
    (let ((dyaw (* dx (translator 0.5)))
	  (dpitch (* dy (translator 0.5))))
      (unless (zerop dyaw)
	(setf *yaw* (mod (+ *yaw* dyaw) two-pi)))
      (unless (zerop dpitch)
	(setf *pitch* (alexandria:clamp
		       (+ *pitch* dpitch)
		       (* -0.99 half-pi)
		       (* 0.99 half-pi)))))))

(defun delta ()
  (let ((mouse-data (load-time-value (cons 0 0))))
    (multiple-value-bind (newx newy) (window:get-mouse-position)
      (multiple-value-prog1 (values
			     (- newx (car mouse-data))
			     (- newy (cdr mouse-data)))
	(setf (car mouse-data) newx
	      (cdr mouse-data) newy)))))

(defparameter mousecapturestate nil)
(defun remove-spurious-mouse-input ()
  (if (window:mice-locked-p)
      (case mousecapturestate
	((nil)
	 (delta) ;;toss spurious mouse movement 
	 (setf mousecapturestate :justcaptured))
	(:justcaptured (setq mousecapturestate t))
	((t)))
      (when mousecapturestate
	(setq mousecapturestate nil))))

(defun hotbar-add (num)
  (setf *hotbar-selection* (truncate (mod (+ num *hotbar-selection*) 256))))

(defparameter daytime 1.0)
(defparameter ticks/sec nil)
(defparameter tickscale nil)
(defparameter tick-delay nil)

(world:setup-hashes)

(defun physinnit ()
  (clean-dirty)
  (setf ticks/sec 60.0)
  (setf tickscale (/ 20.0 ticks/sec))
  (setf tick-delay (/ 1000000.0 ticks/sec)))

(defparameter net-scroll 0)
(defparameter *block-value* 1)

(defparameter *right-fist-fnc*
  (lambda (x y z)
    (let ((blockval *block-value*))
      (setblock-with-update
       x
       y
       z
       blockval
       (aref mc-blocks::lightvalue blockval)))))

(defparameter *left-fist-fnc*
  (lambda (x y z)
    (setblock-with-update x y z 0 0)))

(define-modify-macro *= (&rest args)
  *)

(defparameter *paused* nil)

(defun physics (control-state)
  (setf *xpos-old* *xpos*
	*ypos-old* *ypos*
	*zpos-old* *zpos*)
  (when (skey-j-p (keyval :E) control-state) (window:toggle-mouse-capture))
  (when (window:mice-locked-p)
    (when (skey-j-p (keyval :v) control-state) (toggle noclip))
    (when (skey-j-p (keyval :t) control-state) (update-world-vao))
    (when (skey-j-p (keyval :x) control-state) (toggle *paused*))
    (when (skey-j-p (keyval :f) control-state)
      (toggle fly)
      (toggle gravity))
    (when fist?
      (when (skey-j-p (mouseval :left) control-state)
	(funcall *left-fist-fnc*
		 fist-side-x
		 fist-side-y
		 fist-side-z))
      (when (skey-j-p (mouseval :right) control-state)
	(funcall *right-fist-fnc*
		 (floor fistx)
		 (floor fisty)
		 (floor fistz)))))
  (unless *paused*
    (controls control-state)
    (collide-with-world) 
    (world-forces)
    
    (contact-handle)
    (compute-fist control-state)))

(defun world-forces ()
  (if fly
      (setf air-friction 0.9)
      (setf air-friction 0.98))
  (if fly
      (progn
	(*= *xvel* air-friction)
	(*= *zvel* air-friction))
      (cond (onground
	     (*= *xvel* walking-friction)
	     (*= *zvel* walking-friction))
	    (t (*= *xvel* 0.9)
	       (*= *zvel* 0.9)
	       )))
  (when (and onground gravity)
    (decf *yvel* (* 0.08 (expt tickscale 2))))
 ; (*= *yvel* air-friction)
  )

(defun compute-fist (control-state)
  (let ((look-vec (load-time-value (cg-matrix:vec 0.0 0.0 0.0))))
    (unit-pitch-yaw look-vec
		    (coerce *pitch* 'single-float)
		    (coerce *yaw* 'single-float))
    (let ((avx (aref look-vec 0))
	  (avy (aref look-vec 1))
	  (avz (aref look-vec 2)))
      (let ((vx (- (* reach avx)))
	    (vy (- (* reach avy)))
	    (vz (- (* reach avz))))
	(when (and (window:mice-locked-p)
		   (skey-p (keyval :q) control-state))
	  (big-swing-fist vx vy vz))
	(standard-fist vx vy vz)))))

(defparameter *fist-function* (constantly nil))
(defun big-swing-fist (vx vy vz)
  (let ((u 3))
    (aabb-collect-blocks (+ *xpos* -0.0) (+ *ypos* 0.0) (+ *zpos* -0.0) (* u vx) (* u vy) (* u vz)
			 fist-aabb
			 
			 (lambda (x y z)
			   (when (and (<= 0 x 127)
				      (<= 0 y 127)
				      (<= -128 z -1))
			     (let ((blockid 0))
			       (setblock-with-update x y z blockid  (aref mc-blocks::lightvalue blockid))))))))

(defun contact-handle ()
  (multiple-value-bind (i- i+ j- j+ k- k+) (block-touches *xpos* *ypos* *zpos* player-aabb)
      (cond ((plusp *xvel*) (when i+ (setf *xvel* 0.0))
	     (minusp *xvel*) (when i- (setf *xvel* 0.0))))
      (cond ((plusp *yvel*) (when j+ (setf *yvel* 0.0))
	     (minusp *yvel*) (when j- (setf *yvel* 0.0))))
      (cond ((plusp *zvel*) (when k+ (setf *zvel* 0.0))
	     (minusp *zvel*) (when k- (setf *zvel* 0.0))))
      (setf onground j-)))

(defun block-touches (px py pz aabb)
  (let (x- x+ y- y+ z- z+)
    (multiple-value-bind (minx miny minz maxx maxy maxz) (get-blocks-around px py pz aabb)
      (dobox ((x minx maxx)
	      (y miny maxy)
	      (z minz maxz))
	     (when (aref mc-blocks::iscollidable (world:getblock x y z))
	       (multiple-value-bind (i+ i- j+ j- k+ k-)
		   (aabbcc::aabb-contact px py pz aabb x y z block-aabb)
		 (if i+ (setq x+ t))
		 (if i- (setq x- t))
		 (if j+ (setq y+ t))
		 (if j- (setq y- t))
		 (if k+ (setq z+ t))
		 (if k- (setq z- t))))))
    (values x- x+ y- y+ z- z+)))

(defun get-blocks-around (px py pz aabb)
  (declare (ignorable aabb))
  (let ((minx (- (truncate px) 2))
	(miny (- (truncate py) 2))
	(minz (- (truncate pz) 2)))
    (let ((maxx (+ minx 5))
	  (maxy (+ miny 4))
	  (maxz (+ minz 5)))
      (values minx miny minz maxx maxy maxz))))


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

(defun collide-with-world ()
  (multiple-value-bind (new-x new-y new-z xclamp yclamp zclamp)
      (aabbcc::step-motion #'myafunc
			   *xpos* *ypos* *zpos*
			   *xvel* *yvel* *zvel*)
    (setf *xpos* new-x)
    (setf *ypos* new-y)
    (setf *zpos* new-z)
    (multiple-value-bind (x y z) (aabbcc::clamp-vec
				  *xvel* *yvel* *zvel*
				  xclamp yclamp zclamp)
      (setf *xvel* x)
      (setf *yvel* y)
      (setf *zvel* z))))

(defun standard-fist (vx vy vz)
  (multiple-value-bind (frac type blockx blocky blockz)
       (punch-func (+ *xpos* -0.0) (+ *ypos* 0.0) (+ *zpos* -0.0) vx vy vz)
       (if frac
	   (setf fist? t
		 fist-side type
		 fist-side-x blockx
		 fist-side-y blocky
		 fist-side-z blockz
		 fistx (+ *xpos* (* frac vx))
		 fisty (+ *ypos* (* frac vy))
		 fistz (+ *zpos* (* frac vz)))
	   (setf fist? nil))))

(defun punch-func (px py pz vx vy vz)
  (let ((tot-min 2)
	(type :nothing)
	(ansx nil)
	(ansy nil)
	(ansz nil))
     (aabb-collect-blocks px py pz vx vy vz fist-aabb
			  (lambda (x y z)
			    (unless (zerop (world:getblock x y z))
			      (multiple-value-bind (minimum contact-type)
				  (aabbcc::aabb-collide
				   fist-aabb
				   px py pz
				   block-aabb
				   x y z
				   vx vy vz)
				(when (< minimum tot-min)
				  (setq tot-min minimum
					type contact-type
					ansx x
					ansy y
					ansz z))))))
     (values (if (= 2 tot-min) nil tot-min) type ansx ansy ansz)))

(defun myafunc (px py pz vx vy vz)
  (if noclip
      (values 1 nil nil nil)
      (blocktocontact player-aabb px py pz vx vy vz)))


(locally (declare (optimize (debug 3)))
  (defun blocktocontact (aabb px py pz vx vy vz)
    (let ((tot-min 1)
;	  (total 0)
	  )
      (let (xyz? xy? xz? yz? x? y? z?)
	(flet ((collide-block (x y z)
	;	 (incf total)
		 (when (aref mc-blocks::iscollidable (world:getblock x y z))
		   (plain-setblock x y z (+ 2 (random 4)) 0)
		   (multiple-value-bind (minimum type)
		       (aabbcc::aabb-collide
			aabb
			px py pz
			block-aabb
			x
			y
			z
			vx vy vz)
		     (unless (> minimum tot-min)
		       (when (< minimum tot-min)
			 (setq tot-min minimum)
			 (macrolet ((null! (&rest args)
				      (let (acc)
					(dolist (arg args)
					  (push `(setf ,arg nil) acc))
					`(progn ,@acc))))
			   (null! xyz? xy? yz? xz? x? y? z?)))
		       (case type
			 (:xyz (setf xyz? t))
			 (:xy (setf xy? t))
			 (:xz (setf xz? t))
			 (:yz (setf yz? t))
			 (:x (setf x? t))
			 (:y (setf y? t))
			 (:z (setf z? t))))))))
	  (aabb-collect-blocks px py pz vx vy vz aabb
			       #'collide-block))
;	(princ " ") (princ total)
	(multiple-value-bind (xclamp yclamp zclamp)
	    (aabbcc::type-collapser vx vy vz xyz? xy? xz? yz? x? y? z?)
	  (values
	   tot-min
	   xclamp yclamp zclamp))))))

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
		   `(when (skey-j-p ,symb)
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
  (when (skey-j-p (keyval :g) control-state) )
  (when (skey-j-p (keyval :c) control-state) (toggle walkblock)))
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
    (when (skey-j-p (keyval :h) control-state)
      (time (let ((ans (gl:read-pixels 0 0 1000 1000 :rgba :unsigned-byte)))
				(Setf foo ans))))

#+nil
	  (case 1
	    (0 (when (skey-j-p (mouseval :right) control-state)
		 (print (list xop yop zop))
		 (princ (world:skygetlight xop yop zop))))
	    (1 ))



