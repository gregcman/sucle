(in-package :sandbox)

(defparameter noclip nil)

(defparameter *xpos* 0f0)
(defparameter *ypos* 0f0)
(defparameter *zpos* 0f0)

(defparameter *xvel* 0)
(defparameter *yvel* 0)
(defparameter *zvel* 0)

(defparameter fly t)

(defparameter *yaw* 0f0)
(defparameter *pitch* 0f0)
 
(defparameter defaultfov (* 70 +single-float-pi+ 1/180))

(defparameter air-friction 0.98)
(defparameter walking-friction (* 0.6 0.9))

(defparameter gravity nil)


(defparameter onground nil)
(defparameter *fist-function* (constantly nil))

(defun controls ()
  (setf net-scroll (clamp (+ net-scroll e:*scroll-y*) -1.0 1.0))
  (let ((speed (if t 0.5 0.04444445)))
    (when fly
      (setf speed (* 0.024 0.1))
      (when (e:key-p :space)
	(incf *yvel* speed))
      (when (e:key-p :left-shift)
	(decf *yvel* speed)))
    (unless fly
      (when onground
	(when (or (e:mice-j-p :|3|) (e:key-p :space)) ;;jumping
	  (incf *yvel* 0.16333334)))
      (unless onground
	(setf speed (* speed 0.2))))    
    (let ((dir 0))
      (when (e:key-p :w)
	(incf dir #C(-1 0)))
      (when (e:key-p :a)
	(incf dir #C(0 1)))
      (when (e:key-p :s)
	(incf dir #C(1 0)))
      (when (e:key-p :d)
	(incf dir #C(0 -1)))
      (unless (zerop dir)
	(let ((rot-dir (* dir (cis *yaw*))))
	  (let ((normalized (/ rot-dir (complex-modulus rot-dir))))
	    (incf *xvel* (* speed (realpart normalized)))
	    (incf *zvel* (* speed (imagpart normalized)))))))))

(defun complex-modulus (c)
  (sqrt (realpart (* c (conjugate c)))))

(defparameter mouse-sensitivity (coerce (* 60.0 pi 1/180) 'single-float))

(defun look-around ()
  (mouse-looking))
(defun mouse-looking ()
  (multiple-value-bind (dx dy) (delta)
    (let ((x (* mouse-sensitivity (/ dx 360.0)))
	  (y (* mouse-sensitivity (/ dy 360.0))))
      (multiple-value-bind (dyaw dpitch) (%sphere-mouse-help x y)
	(setf *yaw* (mod (+ *yaw* dyaw) +single-float-two-pi+))
	(setf *pitch* (clamp (+ *pitch* dpitch)
			     (* -0.99 +single-float-half-pi+)
			     (* 0.99 +single-float-half-pi+)))))))

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

(defparameter net-scroll 0)

(defun physics ()
  ;;e to escape mouse
  (when (e:key-j-p :e) (window:toggle-mouse-capture))
  (remove-spurious-mouse-input)
  (when (window:mice-locked-p)
    (when (e:key-j-p :v) (toggle noclip))
    (when (e:key-j-p :g) (toggle gravity))
    (when (e:key-j-p :f) (toggle fly))
    (if fly
	(setf air-friction 0.9)
	(setf air-friction 0.98))
    (look-around)
    (controls))

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
    (decf *yvel* 0.008888889))
  (setf *yvel* (* *yvel* air-friction)))
