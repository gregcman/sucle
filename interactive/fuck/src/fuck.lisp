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

#+nil
(#P"terrarium2/" #P"first/" #P"second/" #P"third/" #P"fourth/" #P"world/"
 #P"terrarium/" #P"holymoly/" #P"funkycoolclimb/" #P"ahole/" #P"maze-royale/"
 #P"bloodcut/" #P"wasteland/")

(defun init ()
  (setf %gl:*gl-get-proc-address* (window:get-proc-address))
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
  (setf *ticker* (tickr:make-ticker :dt (floor 1000000 60)
				    :current-time (microseconds)))
  (call-trampoline))

(progn
  (defparameter *trampoline* (lambda () (throw :end (values))))
  (defun call-trampoline ()
    (catch (quote :end)
      (loop
	 (funcall *trampoline*)))))

(defparameter *control-state* (window::make-control-state
			       :curr window::*input-state*))

(defun wasd-mover (w? a? s? d?)
  (let ((x 0)
	(y 0))
    (when w?
      (decf x))
    (when a?
      (decf y))
    (when s?
      (incf x))
    (when d?
      (incf y))
    (if (and (zerop x)
	     (zerop y))
	nil
	(atan y x))))

(defun num-key-jp (control-state)
  (let ((ans nil))
    (macrolet ((k (number-key)
		 (let ((symb (intern (write-to-string number-key) :keyword)))
		   `(when (window::skey-j-p (window::keyval ,symb) control-state)
		      (setf ans ,number-key)))))
      (k 1)
      (k 2)
      (k 3)
      (k 4)
      (k 5)
      (k 6)
      (k 7)
      (k 8)
      (k 9)
      (k 0))
    ans))

(defparameter *ents*
  (let ((array (make-array 10)))
    (dotimes (x (length array))
      (setf (aref array x) (sandbox::gentity)))
    array))

(defparameter *ent* (aref *ents* 1))

(defparameter *paused* nil)
(defun physss ()
  (window::update-control-state *control-state*)

  (when *sandbox-on*
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
			    pos)))))

(defparameter *ticker* nil)

(defparameter *camera* (camat:make-camera :frustum-far (* 256.0)
					     :frustum-near (/ 1.0 8.0)))

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

(defparameter *fov*
  ((lambda (deg)
     (* deg (coerce (/ pi 180.0) 'single-float)))
   70))

(progn
  (defun actual-stuuff ()
    (when window:*status*
      (throw :end (values)))
    (window:poll)
    (remove-spurious-mouse-input)
    (gl:viewport 0 0 window:*width* window:*height*)
    (gl:clear
     :color-buffer-bit
     :depth-buffer-bit)
    (let ((camera *camera*))
      (setf (camat:camera-aspect-ratio camera)
	    (coerce (/ window:*width* window:*height*)
		    'single-float))
      (setf (camat:camera-fov camera) *fov*)
      (let ((fraction (tick *ticker* #'physss (microseconds))))
	(when *sandbox-on*
	  (let ((neck (sandbox::entity-neck *ent*)))
	    (when (window:mice-locked-p)
	      (multiple-value-call #'sandbox::look-around neck (delta2))))
	  (entity-to-camera *ent* *camera* fraction)
	  (camat:update-matrices camera)
	  (render camera
		  #'getfnc
		  fraction))))
    (window:update-display))
  (setf *trampoline* #'actual-stuuff))


(progn
  (defun fractionalize (x)
    (alexandria:clamp x 0.0 1.0))
  (defun render (camera deps partial)
    (declare (ignorable partial))
    (declare (optimize (safety 3) (debug 3)))
    (flet ((getfnc (name)
	     (funcall deps name)))
      (let* ((blockshader (getfnc :blockshader))
	     (blockshader-uniforms sandbox::*blockshader-uniforms*)
	     (fogcolor (glhelp:getuniform blockshader-uniforms :fog-color))
	     (aratio (glhelp:getuniform blockshader-uniforms :aratio))
	     (foglet (glhelp:getuniform blockshader-uniforms :foglet))
	     (pmv (glhelp:getuniform blockshader-uniforms :pmv))
	     (cam-pos (glhelp:getuniform blockshader-uniforms :cam-pos)))
	(gl:use-program blockshader)
	(gl:uniformfv cam-pos (camat:camera-vec-position camera))
	(gl:uniform-matrix-4fv
	 pmv
	 (camat:camera-matrix-projection-view-player camera)
	 nil)
	
	(let ((time sandbox::*daytime*)
	      (avector sandbox::*avector*))
	  (map-into avector
		    (lambda (x)
		      (fractionalize (* time x)))
		    sandbox::*fogcolor*)
	  (gl:clear-color (aref avector 0) (aref avector 1) (aref avector 2) 1.0)
	  (gl:uniformfv fogcolor avector))
	(gl:uniformf foglet (/ -1.0 (camat:camera-frustum-far camera) sandbox::*fog-ratio*))
	(gl:uniformf aratio (/ 1.0 sandbox::*fog-ratio*))
	
	(gl:disable :blend)

  ;;;static geometry with no translation whatsoever
	;; (sandbox::bind-default-framebuffer)
	(gl:bind-texture
	 :texture-2d
	 (getfnc :terrain))
	(sandbox::draw-chunk-meshes))
      (sandbox::designatemeshing))))
