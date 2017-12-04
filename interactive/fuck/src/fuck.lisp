(in-package :fuck)

(defparameter *thread* nil)
(defun main () 
  (setf *thread*
	(sb-thread:make-thread
	 (just-main))))

(defun just-main ()
  (let ((stdo *standard-output*))
    (lambda ()
      (progv '(window::*iresizable*
	       window::*iwidth*
	       window::*iheight*
	       *standard-output*)
	  (list nil 720 480 stdo)
	(window::wrapper #'init)))))

(progn
  (defparameter *trampoline* (lambda () (throw :end (values))))
  (defun call-trampoline ()
    (catch (quote :end)
      (loop
	 (funcall *trampoline*)))))

(progn
  (defparameter *backup* (make-hash-table :test 'eq))
  (defparameter *stuff* (make-hash-table :test 'eq))
  (defun bornfnc (name func)
    (namexpr *backup* name func))
  (defun getfnc (name)
    (get-stuff name *stuff* *backup*)))

(progn
  (defun namexpr (hash name func)
    (setf (gethash name hash) func))
  (defun get-stuff (name stuff otherwise)
    (multiple-value-bind (val exists-p) (gethash name stuff)
      (if exists-p
	  val
	  (let ((genfunc (gethash name otherwise)))
	    (when (functionp genfunc)
	      (setf (gethash name stuff) (funcall genfunc))))))))

(defparameter *sandbox-on* t)
(defparameter *some-saves* 
  #P "/home/imac/Documents/lispysaves/saves/sandbox-saves/")

(defun call-with-path (path fun)
  (let ((newpath (merge-pathnames path *some-saves*)))
    (cond ((or (uiop:pathname-equal newpath *some-saves*) 
	       (not (uiop:subpathp newpath *some-saves*))
	       (not (uiop:directory-pathname-p newpath)))
	   (error "SAVE DIR INVALID: ~a, ~a" path newpath))
	  (t
	   (ensure-directories-exist newpath)
	   (funcall fun newpath)))))

(defun msave (path)
  (call-with-path path #'sandbox::save-world))

(defun mload (path)
  (call-with-path path #'sandbox::load-world))


(defun init ()
  (setf %gl:*gl-get-proc-address* (window:get-proc-address))
  (setf window:*resize-hook* #'resize-screen)
  (funcall window:*resize-hook* window:*width* window:*height*)
  (let ((hash *stuff*))
    (maphash (lambda (k v)
	       (if (integerp v)
		   (remhash k hash)))
	     hash))
  (window:set-vsync t)
  (when *sandbox-on*
    (sandbox::build-deps #'getfnc
			 #'bornfnc)
    (setf sandbox::*world-display-list* nil)
    (clrhash sandbox::*g/chunk-call-list*)
  
    (setf sandbox::mesher-thread nil)
    (sandbox::clean-dirty)
    )
  (gl:enable :scissor-test)
  (setf *ticker* (tickr:make-ticker :dt (floor 1000000 60)
				    :current-time (microseconds)))
  (call-trampoline))

(defparameter *control-state* (window::make-control-state
			       :curr window::*input-state*))

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

(defun num-key-jp (control-state)
  (etouq
   (cons
    'cond
    (mapcar
     (lambda (n)
       `((window::skey-j-p
	  (window::keyval ,(intern (write-to-string n) :keyword))
	  control-state) ,n))
     '(0 1 2 3 4 5 6 7 8 9)))))

(defparameter *ents* (map-into (make-array 10) #'sandbox::gentity))
(defparameter *ent* (aref *ents* 1))

(defparameter *paused* nil)
(defun physss ()
  (let* ((player-farticle (sandbox::entity-particle *ent*))
	 (pos (sandbox::farticle-position player-farticle))
	 (control-state *control-state*))
    (sandbox::meta-controls control-state
			    *ent*)
    (when (window::skey-j-p (window::keyval :x) control-state)
      (toggle *paused*))
    (let ((num (num-key-jp *control-state*)))
      (when num
	(setf *ent* (aref *ents* num))))
    (unless *paused*
      (setf (sandbox::entity-hips *ent*)
	    (wasd-mover
	     (window::skey-p (window::keyval :e) control-state)
	     (window::skey-p (window::keyval :s) control-state)
	     (window::skey-p (window::keyval :d) control-state)
	     (window::skey-p (window::keyval :f) control-state)))
      (sandbox::physentity *ent*))
    (let ((backwardsbug (load-time-value (cg-matrix:vec 0.0 0.0 0.0))))
      (cg-matrix:%vec* backwardsbug (camat:camera-vec-forward *camera*) -4.0)
      (sandbox::use-fists control-state backwardsbug
			  pos))))

(defparameter *ticker* nil)

(defparameter *camera* (camat:make-camera :frustum-far (* 256.0)
					  :frustum-near (/ 1.0 8.0)))

(defclass render-area ()
  ((x :accessor render-area-x
      :initform 0
      :initarg :x)
   (y :accessor render-area-y
      :initform 0
      :initarg :y)
   (width :accessor render-area-width
	  :initform 0
	  :initarg :width)
   (height :accessor render-area-height
	   :initform 0
	   :initarg :height)))
(defun set-render-area (render-area)
  (with-slots (x y width height) render-area
    (%set-render-area x y width height)))
(defun %set-render-area (x y width height)
  (gl:viewport x y width height)
  (gl:scissor x y width height))

(defparameter *render-area* (make-instance 'render-area))

;;;time in microseconds
(defun microseconds ()
  (multiple-value-bind (s m) (sb-ext:get-time-of-day)
    (+ (* (expt 10 6) (- s 1506020000)) m)))
(defun tick (ticker fun time)
  (tickr:tick-update ticker time)
  (tickr:tick-physics ticker fun)
  (float (/ (tickr:ticker-accumulator ticker)
	    (tickr:ticker-dt ticker))))

(defun farticle-to-camera (farticle camera fraction)
  (let ((curr (sandbox::farticle-position farticle))
	(prev (sandbox::farticle-position-old farticle)))
    (let ((vec (camat:camera-vec-position camera))
	  (cev (camat:camera-vec-noitisop camera)))
      (cg-matrix:%vec-lerp vec prev curr fraction)
      (cg-matrix:%vec* cev vec -1.0))))
(defun entity-to-camera (entity camera fraction)
  (sandbox::necktovec (sandbox::entity-neck entity)
		      (camat:camera-vec-forward camera))	  
  (farticle-to-camera (sandbox::entity-particle entity)
		      camera
		      fraction))

(defun change-entity-neck (entity yaw pitch)
  (let ((neck (sandbox::entity-neck entity)))
    (sandbox::look-around neck yaw pitch)))

(defparameter *fov*
  ((lambda (deg)
     (* deg (coerce (/ pi 180.0) 'single-float)))
   70))

(defun resize-screen (width height)
  (let ((camera *camera*))
    (setf (camat:camera-aspect-ratio camera)
	  (/ (coerce width 'single-float)
	     (coerce height 'single-float))))
  (let ((render-area *render-area*))
    (setf (render-area-width render-area) width
	  (render-area-height render-area) height)))

(defun clear-screen ()
  (gl:clear
   :color-buffer-bit
   :depth-buffer-bit))

(progn
  (defun actual-stuuff ()
    (when window:*status*
      (throw :end (values)))
    (window:poll)
    (window::update-control-state *control-state*)
    (remove-spurious-mouse-input)
    (set-render-area *render-area*)
    (clear-screen)
    
    (setf (camat:camera-fov *camera*) *fov*)
    (when *sandbox-on*
      (when (window:mice-locked-p)
	(multiple-value-call #'change-entity-neck *ent* (delta2)))
      (entity-to-camera *ent* *camera*
			(tick *ticker* #'physss (microseconds)))
      (camat:update-matrices *camera*)
      
      (set-sky-color)
      (gl:disable :blend)
      (let ((blockshader (getfnc :blockshader)))
	(gl:use-program blockshader)
	(camera-shader *camera*))
      (gl:bind-texture :texture-2d (funcall #'getfnc :terrain))
      (sandbox::draw-chunk-meshes)
      
      (sandbox::designatemeshing))
    (window:update-display))
  (setf *trampoline* #'actual-stuuff))

(defun set-sky-color ()
  (let ((time sandbox::*daytime*)
	(avector sandbox::*avector*))
    (map-into avector
	      (lambda (x)
		(max 0.0 (min (* time x) 1.0)))
	      sandbox::*fogcolor*))
  (with-vec (a b c) (sandbox::*avector*)
    (gl:clear-color a b c 1.0)))

(defun camera-shader (camera)
  (declare (optimize (safety 3) (debug 3)))
  (glhelp:with-uniforms uniform sandbox::*blockshader-uniforms*
    (gl:uniformfv
       (uniform :fog-color)
       sandbox::*avector*)
    (gl:uniformf
     (uniform :foglet)
     (/ (/ -1.0 sandbox::*fog-ratio*)
	(camat:camera-frustum-far camera)))
    (gl:uniformf
     (uniform :aratio)
     (/ 1.0 sandbox::*fog-ratio*))
    (gl:uniformfv (uniform :cam-pos)
		  (camat:camera-vec-position camera))
    (gl:uniform-matrix-4fv
     (uniform :pmv)
     (camat:camera-matrix-projection-view-player camera)
     nil)))
#+nil
(#P"terrarium2/" #P"first/" #P"second/" #P"third/" #P"fourth/" #P"world/"
 #P"terrarium/" #P"holymoly/" #P"funkycoolclimb/" #P"ahole/" #P"maze-royale/"
 #P"bloodcut/" #P"wasteland/")
