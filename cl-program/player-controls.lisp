(in-package :sandbox)

(defconstant +single-float-pi+ (coerce pi 'single-float))

(defparameter *yaw* nil)
(defparameter *pitch* nil)
(defparameter *xpos* nil)
(defparameter *ypos* nil)
(defparameter *zpos* nil)
(defparameter defaultfov (* 70 +single-float-pi+ 1/180))

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
	    (incf *zvel* (* speed (imagpart normalized)))))))))

(defun complex-modulus (c)
  (sqrt (realpart (* c (conjugate c)))))

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

;;return the pitch and yaw of a unit direction vector
(defun extract-polar-coords (vec)
  (let ((zero (aref vec 0))
	(two (aref vec 2)))
    (values (asin (aref vec 1))
	    (atan two zero))))


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

  (incf *xpos* *xvel*)
  (incf *ypos* *yvel*)
  (incf *zpos* *zvel*)
  
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


