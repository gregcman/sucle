(in-package :sandbox)

;;;;150 ms delay for sprinting
;;;;player eye height is 1.5, subtract 1/8 for sneaking

;;gravity is (* -0.08 (expt tickscale 2)) 0 0
;;falling friction is 0.98
;;0.6 * 0.91 is walking friction

;;fov::
;;70 is normal
;;110 is quake pro


(defparameter *yaw* nil)
(defparameter *pitch* nil)
(defparameter *xpos* nil)
(defparameter *ypos* nil)
(defparameter *zpos* nil)
(defparameter defaultfov (deg-rad 70))

(defparameter *xvel* 0)
(defparameter *yvel* 0)
(defparameter *zvel* 0)

(defparameter air-friction 0.98)
(defparameter walking-friction (* 0.6 0.9))

(defparameter noclip nil)
(defparameter gravity nil)
(defparameter fly t)

(defparameter *hotbar-selection* 2)

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

(setf *xpos* 0.0
      *ypos* 0.0
      *zpos* 0.0
      *yaw* 0.0
      *pitch* 0.0)

(defun controls ()
  (setf net-scroll (clamp (+ net-scroll e:*scroll-y*) -1.0 1.0))
  (let ((speed (* 0.4 (expt tickscale 2))))
    (when fly
      (setf speed 0.024)
      (when (e:key-p :space)
	(incf *yvel* speed))
      (when (e:key-p :left-shift)
	(decf *yvel* speed)))
    (unless fly
      (when onground
	(when (or (e:mice-j-p :|3|) (e:key-p :space)) ;;jumping
	  (incf *yvel* (* (if t 0.49 0.42) (expt tickscale 1)))))
      (unless onground
	(setf speed (* speed 0.2))))    
    (let ((dir (complex 0 0)))
      (when (e:key-p :w) ;;w
	(incf dir #C(-1 0)))
      (when (or (e:key-p :a)) ;;a
	(incf dir #C(0 1)))
      (when (e:key-p :s) ;;s
	(incf dir #C(1 0)))
      (when (or (e:key-p :d)) ;;d
	(incf dir #C(0 -1)))
      (unless (zerop dir)
	(let ((rot-dir (* dir (cis *yaw*))))
	  (let ((normalized (/ rot-dir (complex-modulus rot-dir))))
	    (incf *xvel* (* speed (realpart normalized)))
	    (incf *zvel* (* speed (imagpart normalized)))))))
    (progno
     (let ((speed (* net-scroll -0.002)))
       (incf *xvel* (* speed look-x))
       (incf *yvel* (* speed look-y))
       (incf *zvel* (* speed look-z))))))

(defparameter mouse-sensitivity (coerce (* 60.0 pi 1/180) 'single-float))
(defconstant two-pi (coerce (* 2 pi) 'single-float))
(defconstant half-pi (coerce (/ pi 2) 'single-float))

(defun look-around ()
  (mouse-looking))
(defun mouse-looking ()
  (multiple-value-bind (dx dy) (delta)
    (let ((x (* mouse-sensitivity (/ dx 360.0)))
	  (y (* mouse-sensitivity (/ dy 360.0))))
      (multiple-value-bind (dyaw dpitch) (%sphere-mouse-help x y)
	(setf *yaw* (mod (+ *yaw* dyaw) two-pi))
	(setf *pitch* (clamp (+ *pitch* dpitch)
			     (* -0.99 half-pi)
			     (* 0.99 half-pi)))))))

(defparameter old-mouse-x 0)
(defparameter old-mouse-y 0)
(defun delta ()
  (multiple-value-bind (newx newy) (window:get-mouse-position)
    (multiple-value-prog1 (values
			   (- newx old-mouse-x)
			   (- newy old-mouse-y))
      (setf old-mouse-x newx
	    old-mouse-y newy))))


(defun %sphere-mouse-help (x y)
  (if (zerop x)
      (if (zerop y)
	  (values 0.0 0.0)
	  (values 0.0 y))
      (if (zerop y)
	  (values x 0.0)
	  (new-direction (coerce x 'single-float)
			 (coerce y 'single-float)))))

(defparameter *temp-matrix3* (cg-matrix:identity-matrix))

(defparameter *x-unit* (cg-matrix:vec 1.0 0.0 0.0))
(defparameter *new-dir* (cg-matrix:vec 0.0 0.0 0.0))
(defun new-direction (dx dy)
  (let ((size (sqrt (+ (* dx dx) (* dy dy)))))
    (let ((dir *x-unit*))
      (let ((rot (cg-matrix:%rotate-around*
		  *temp-matrix3*
		  0.0 (/ (- dx) size) (/ dy size) size)))
	(let ((new-dir (cg-matrix:%transform-direction *new-dir* dir rot)))
	  (multiple-value-bind (p y) (extract-polar-coords new-dir)
	    (values y p)))))))

(defparameter mousecapturestate nil)
(defun remove-spurious-mouse-input ()
  (if (window:mice-locked-p)
      (case mousecapturestate
	((nil) 
	 (delta) ;;toss spurious mouse movement 
	 (setf mousecapturestate :justcaptured))
	(:justcaptured (setq mousecapturestate t))
	((t)))
      (setq mousecapturestate nil)))

(defun hotbar-add (num)
  (setf *hotbar-selection* (truncate (mod (+ num *hotbar-selection*) 9))))

(defparameter daytime 1.0)
(defparameter ticks/sec nil)
(defparameter tickscale nil)
(defparameter tick-delay nil)

(defun physinnit ()
  (setf ticks/sec 60.0)
  (setf tickscale (/ 20.0 ticks/sec))
  (setf tick-delay (/ 1000000.0 ticks/sec)))

(defparameter net-scroll 0)

(defparameter walkblock nil)
(defparameter foo nil)
(defun physics ()
  ;;e to escape mouse
  (when (e:key-j-p :E) (window:toggle-mouse-capture))
  (hotbar-add e:*scroll-y*)
  (unless (zerop e:*scroll-y*)
    (lcalllist-invalidate :hotbar-selector))
  (macrolet ((k (number-key)
	       (let ((symb (intern (write-to-string number-key) :keyword)))
		 `(when (e:key-j-p ,symb)
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
    (k 9))
  (when (window:mice-locked-p)
    (when (e:key-j-p :v) (toggle noclip))
    (when (e:key-j-p :g) (toggle gravity))
    (when (e:key-j-p :f) (toggle fly))
    (when (e:key-j-p :c) (toggle walkblock))
    (when (e:key-j-p :h) (time (setf foo (gl:read-pixels 0 0 1000 1000 :rgba :unsigned-byte))))
    (if fly
	(setf air-friction 0.9)
	(setf air-friction 0.98))
    (controls))
  ;;;;(collide-with-world)
  
  (if fly
      (progn
	(setf *xvel* (* *xvel* air-friction))
	(setf *zvel* (* *zvel* air-friction)))
      (cond (onground
	     (setf *xvel* (* *xvel* walking-friction))
	     (setf *zvel* (* *zvel* walking-friction)))
	    (t (setf *xvel* (* *xvel* 0.9))
	       (setf *zvel* (* *zvel* 0.9)))))
  (when (and gravity (not onground))
    (decf *yvel* (* 0.08 (expt tickscale 2))))
  (setf *yvel* (* *yvel* air-friction)))


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

(defparameter *fist-function* (constantly nil))

(defun myafunc (px py pz vx vy vz)
  (if noclip
      (values 1 nil nil nil)
      (blocktocontact player-aabb px py pz vx vy vz)))

(defmacro with-collidable-blocks ((xvar yvar zvar) (px py pz aabb vx vy vz) &body body)
  `(multiple-value-bind (minx miny minz maxx maxy maxz)
       (get-blocks-around ,px ,py ,pz ,aabb ,vx ,vy ,vz)
     (dobox ((,xvar minx maxx)
	     (,yvar miny maxy)
	     (,zvar minz maxz))
	    ,@body)))


(defun smallest (i j k)
  (if (< i j)
      (if (< i k) ;;i < j j is out
	  (values i t nil nil)	  ;;; i < k and i < j
	  (if (= i k)
	      (values i t nil t) ;;;tied for smallest
	      (values k nil nil t)	     ;;; k < i <j
	      ))
      (if (< j k) ;;i>=j
	  (if (= i j)
	      (values i t t nil)
	      (values j nil t nil)) ;;j<k and i<=j k is nout
	  (if (= i k)
	      (values i t t t)
	      (if (= k j)
		  (values k nil t t)
		  (values k nil nil t))) ;;i>=j>=k 
	  )))

(defun aux-func (x dx)
  (if (zerop dx)
      nil
      (if (plusp dx)
	  (floor (1+ x))
	  (ceiling (1- x)))))


(defun aabb-collect-blocks (px py pz dx dy dz aabb func)
  (declare (ignorable aabb))
  (with-slots ((minx aabbcc::minx) (miny aabbcc::miny) (minz aabbcc::minz)
	       (maxx aabbcc::maxx) (maxy aabbcc::maxy) (maxz aabbcc::maxz)) aabb
    (let ((total 1))
      (let ((pluspdx (plusp dx))
	    (pluspdy (plusp dy))
	    (pluspdz (plusp dz))
	    (zeropdx (zerop dx))
	    (zeropdy (zerop dy))
	    (zeropdz (zerop dz)))
	(declare (ignorable pluspdx pluspdy pluspdz zeropdx zeropdy zeropdz))
	(let ((xoffset (if zeropdx 0 (if pluspdx maxx minx)))
	      (yoffset (if zeropdy 0 (if pluspdy maxy miny)))
	      (zoffset (if zeropdz 0 (if pluspdz maxz minz))))
	  (let ((x (+ px xoffset))
		(y (+ py yoffset))
		(z (+ pz zoffset)))
	    (tagbody
	     rep
	       (let ((i-next (aux-func x dx))
		     (j-next (aux-func y dy))
		     (k-next (aux-func z dz)))
		 (mvb (ratio i? j? k?) (smallest (if i-next
						     (/ (- i-next x) dx)
						     most-positive-single-float)
						 (if j-next
						     (/ (- j-next y) dy)
						     most-positive-single-float)
						 (if k-next
						     (/ (- k-next z) dz)
						     most-positive-single-float))	 
		      (let ((newx (if i? i-next (+ x (* dx ratio))))
			    (newy (if j? j-next (+ y (* dy ratio))))
			    (newz (if k? k-next (+ z (* dz ratio)))))
			(let ((aabb-posx (- newx xoffset))
			      (aabb-posy (- newy yoffset))
			      (aabb-posz (- newz zoffset)))
			  (when i?
			    (dobox ((j (floor (+ aabb-posy miny))
				       (ceiling (+ aabb-posy maxy)))
				    (k (floor (+ aabb-posz minz))
				       (ceiling (+ aabb-posz maxz))))
				   (funcall func (if pluspdx newx (1- newx)) j k)))
			  (when j?
			    (dobox ((i (floor (+ aabb-posx minx))
				       (ceiling (+ aabb-posx maxx)))
				    (k (floor (+ aabb-posz minz))
				       (ceiling (+ aabb-posz maxz))))
				   (funcall func i (if pluspdy newy (1- newy)) k)))
			  (when k?
			    (dobox ((j (floor (+ aabb-posy miny))
				       (ceiling (+ aabb-posy maxy)))
				    (i (floor (+ aabb-posx minx))
				       (ceiling (+ aabb-posx maxx))))
				   (funcall func i j (if pluspdz newz (1- newz))))))
			(setf x newx y newy z newz))
		      (decf total ratio)
		      (when (minusp total) (go end))
		      (go rep)))
	     end)))))))
