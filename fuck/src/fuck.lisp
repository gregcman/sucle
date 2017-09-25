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

(defun physss ()
  (window:poll)
  (window::update-control-state *control-state*)


  (when *sandbox-on*
    (sandbox::physics *control-state*)))

(defparameter *ticker* nil)
(defparameter *realthu-nk* (lambda () (throw :end (values))))

(defparameter *camera* (sandbox::make-camera))


(progn
  (defun actual-stuuff ()
    (when window:*status*
      (throw :end (values)))
    (let ((ticker *ticker*))
      (tick-update ticker (fine-time))
      (tick-physics ticker (function physss))
      (let ((fraction (float (/ (ticker-accumulator ticker)
				(ticker-dt ticker)))))
	(gl:viewport 0 0 window:*width* window:*height*)
	(gl:clear
	 :color-buffer-bit
	 :depth-buffer-bit)
	(when *sandbox-on*
	  (progn
	    (dotimes (x 4) (window:poll))
	    (remove-spurious-mouse-input)
	    (window:poll)
	    (when (window:mice-locked-p)
 	      (multiple-value-bind (newyaw newpitch)
		  (look-around sandbox::*yaw* sandbox::*pitch*)
		(when newyaw
		  (setf sandbox::*yaw* newyaw))
		(when newpitch
		  (setf sandbox::*pitch* newpitch))))
	    (setf (sandbox::camera-aspect-ratio *camera*)
		  (/ window:*width* window:*height* 1.0))
	    (sandbox::set-render-cam-pos *camera* fraction)
	    (sandbox::update-matrices *camera*)
	    (sandbox::render *camera*
			     #'getfnc))))
      (window:update-display)))
  (setf *realthu-nk* (function actual-stuuff)))

(defun injection3 ()
  (setf *ticker* (make-ticker :dt (floor 1000000 60)
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

