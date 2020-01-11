(defpackage #:fps-independent-timestep
  (:use #:cl #:utility #:clock)
  (:export
   #:tick
   #:set-fps
   )
  (:nicknames :fps))

;;;;1. set the fps with SET-FPS
;;;;2. in your simulation/game loop, run (TICK ...forms...)
;;;;3. this will run forms are the desired FPS 

(in-package :fps-independent-timestep)

(defstruct (ticker (:constructor %make-ticker))
  (ticks 0 :type fixnum)
  (dt (floor 1000000 60) :type fixnum)
  (current-time (error "give initial time") :type fixnum)
  (accumulator 0 :type fixnum)
  (bailout (floor 1000000 4) :type fixnum)
  (aux 0.0 :type double-float))

;;(declaim (ftype (function (fixnum fixnum &optional fixnum)) make-ticker))
(defun make-ticker (dt time &optional (bailout (floor 1000000 4))) ;;FIXME
  (%make-ticker :dt dt :current-time time :aux (coerce (/ 1 dt) 'double-float)
		:bailout bailout))

(defmacro run-iterations (ticker &body body)
  (with-gensyms (accumulator ticks times)
    (once-only (ticker)
      `(let ((,accumulator (ticker-accumulator ,ticker))
	     (,ticks (ticker-ticks ,ticker))
	     (,times 0))
	 (let ((dt (ticker-dt ,ticker)))
	   (loop
	      (if (>= ,accumulator dt)
		  (progn
		    ,@body
		    (incf ,ticks dt)
		    (decf ,accumulator dt)
		    (incf ,times))
		  (progn
		    (unless (zerop ,times)
		      (setf (ticker-accumulator ,ticker) ,accumulator)
		      (setf (ticker-ticks ,ticker) ,ticks))
		    (return)))))
	 ,times))))

(defun update-time (ticker new-time)
  (let* ((frame-time (- new-time (ticker-current-time ticker))))
    (let ((bailout (ticker-bailout ticker)))
      (let ((toofar (> frame-time bailout))
	    (toobelow (> 0 frame-time)))
	(when (or toofar toobelow)
	  (setf frame-time bailout))))
    (setf (ticker-current-time ticker) new-time)
    (incf (ticker-accumulator ticker) frame-time)))

(defparameter *ticker*
  ;;overwritten
  (make-ticker (floor 1000000 60) most-positive-fixnum))

(defmacro %tick (ticker (&optional (time '(microseconds))) &body body)
  (once-only (ticker time)
    (with-gensyms (times)
      `(progn
	 (update-time ,ticker ,time)
	 (let ((,times
		(run-iterations ,ticker
				,@body)))
	   (values
	    (coerce (* (ticker-accumulator ,ticker)
		       (ticker-aux ,ticker))
		    'single-float)
	    ,times))))))

(defun set-fps (&optional (n 60))
  (setf *ticker*
	(make-ticker (floor 1000000 n) most-positive-fixnum)))

(defmacro tick (&body body)
  `(%tick *ticker* () ,@body))
