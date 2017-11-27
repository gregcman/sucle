(in-package :fuck)


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
  (defmacro ensure (place otherwise)
    (let ((value-var (gensym))
	  (exists-var (gensym)))
      `(or ,place
	   (multiple-value-bind (,value-var ,exists-var) ,otherwise
	     (if ,exists-var
		 (values (setf ,place ,value-var) ,exists-var))))))
  (defun get-stuff (name stuff otherwise)
    (ensure (gethash name stuff)
	    (let ((genfunc (gethash name otherwise)))
	      (when (functionp genfunc)
		(values (funcall genfunc) t))))))

(defparameter *sandbox-on* t)

(defun handoff-five ()
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
    (sandbox::initialization1))

  (injection3)) 

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
	(sandbox::physentity *ent*)

	#+nil
	(map nil (lambda (ent)
		   (unless (eq ent *ent*)
		     (setf (sandbox::entity-jump? ent) t)
		     (if (sandbox::entity-hips ent)
			 (incf (sandbox::entity-hips ent)
			       (- (random 1.0) 0.5))
			 (setf (sandbox::entity-hips ent) 1.0))
		     )
		   (sandbox::physentity ent)) *ents*))
      (let ((backwardsbug (load-time-value (cg-matrix:vec 0.0 0.0 0.0))))
	(cg-matrix:%vec* backwardsbug (sandbox::camera-vec-forward *camera*) -4.0)
	(sandbox::use-fists control-state backwardsbug
			    pos)))))

(defun seeder ()
  (map nil
       (lambda (ent)
	 (let ((pos (sandbox::farticle-position (sandbox::entity-particle ent))))
	   (setf (sandbox::entity-fly? ent) nil
		 (sandbox::entity-gravity? ent) t)
	   (setf (aref pos 0) 64.0
		 (aref pos 1) 128.0
		 (aref pos 2) -64.0))) *ents*))

(defparameter *ticker* nil)
(defparameter *realthu-nk* (lambda () (throw :end (values))))

(defparameter *camera* (sandbox::make-camera :frustum-far (* 256.0)
					     :frustum-near (/ 1.0 8.0)))
(defun set-render-cam-pos (camera partial curr prev)
  (let ((vec (sandbox::camera-vec-position camera))
	(cev (sandbox::camera-vec-noitisop camera)))

    (cg-matrix:%vec-lerp vec prev curr partial)
    (cg-matrix:%vec* cev vec -1.0)))

(progn
  (defun actual-stuuff ()
    (when window:*status*
      (throw :end (values)))
    (let ((ticker *ticker*))
      (tickr:tick-update ticker (fine-time))
      (tickr:tick-physics ticker (function physss))
      (let ((fraction (float (/ (tickr:ticker-accumulator ticker)
				(tickr:ticker-dt ticker)))))
	(gl:viewport 0 0 window:*width* window:*height*)
	(gl:clear
	 :color-buffer-bit
	 :depth-buffer-bit)
	(when *sandbox-on*
	  (progn
	    (window:poll)
	    (remove-spurious-mouse-input)
	    (let ((camera *camera*))
	      (let ((neck (sandbox::entity-neck *ent*)))
		(when (window:mice-locked-p)
		  (multiple-value-call #'sandbox::look-around neck (delta2)))
		(sandbox::necktovec
		 neck
		 (sandbox::camera-vec-forward camera)))
	      (setf (sandbox::camera-aspect-ratio camera)
		    (coerce (/ window:*width* window:*height*)
			    'single-float))
	      (let* ((player-farticle (sandbox::entity-particle *ent*))
		     (pos (sandbox::farticle-position player-farticle))
		     (old (sandbox::farticle-position-old player-farticle)))
		
		(set-render-cam-pos camera fraction pos old))
	      (let ((defaultfov
		     (load-time-value ((lambda (deg)
					 (* deg (coerce (/ pi 180.0) 'single-float)))
				       70))))
		(setf (sandbox::camera-fov camera)
		      defaultfov))
	      (sandbox::update-matrices camera)
	      (sandbox::render camera
			       #'getfnc
			       fraction)))))
      (window:update-display)))
  (setf *realthu-nk* (function actual-stuuff)))

(defun injection3 ()
  (setf *ticker* (tickr:make-ticker :dt (floor 1000000 60)
		  :current-time (fine-time)))
  (catch (quote :end)
    (loop
       (funcall *realthu-nk*))))

(defparameter *thread* nil)
(defun main3 ()
  (setf *thread*
	(sb-thread:make-thread   
	 (lambda (stdo)
	   (let ((window::*iresizable* t)
		 (window::*iwidth* 256)
		 (window::*iheight* 256)
		 (*standard-output* stdo))
	     (window::wrapper #'handoff-five)))
	 :arguments  (list *standard-output*))))



;;;time in microseconds
(defun fine-time ()
  (multiple-value-bind (s m) (sb-ext:get-time-of-day)
    (+ (* (expt 10 6) (- s 1506020000)) m)))

#+nil
(defun define-time ()
  (eval
   `(defun fine-time ()
      (/ (%glfw::get-timer-value)
	 ,(/ (%glfw::get-timer-frequency) (float (expt 10 6)))))))




#+nil
(dotimes (x 128)
  (when (e::skey-j-p x *control-state*)
    (let ((char (code-char x)))
      (when (typep char 'standard-char)
	(princ char)))))


#+nil
((defparameter *old-scroll-y* 0.0)
 (defparameter net-scroll 0)
 (let ((new-scroll window::*scroll-y*))
   (setf *old-scroll-y* net-scroll)
   (setf net-scroll new-scroll)
   #+nil
   (let ((value (- new-scroll *old-scroll-y*)))
     (unless (zerop value)
					;(print value)
       ))))

#+nil
(multiple-value-bind (newyaw newpitch)
		    (multiple-value-call
			#'look-around
		      *yaw* *pitch*
		      (delta))
		  (when newyaw
		    (setf *yaw* newyaw))
		  (when newpitch
		    (setf *pitch* newpitch)))
#+nil
(sandbox::unit-pitch-yaw 
 (coerce *pitch* 'single-float)
 (coerce *yaw* 'single-float))
#+nil
((defparameter *yaw* 0.0)
 (defparameter *pitch* 0.0))

#+nil
(sandbox::physics (necking-yaw *neck*)
		  
		  player-farticle)

#+nil
(defparameter *farticles*
  (let ((array (make-array 10)))
    (map-into array (lambda () (sandbox::make-farticle)))
    array))

#+nil
((defparameter *player-farticle* (aref *farticles* 0))
 (defparameter *neck* (make-necking))
 )
