(defpackage #:fps-independent-timestep
  (:use #:cl)
  (:import-from
   #:local-time
   #:%get-current-time)
  (:export
   #:tick
   #:set-fps
   #:microseconds
   
   #:dt
   #:ticks-per-second)
  (:nicknames :fps))

;;;;1. set the fps with SET-FPS
;;;;2. in your simulation/game loop, run (TICK ...forms...)
;;;;3. this will run forms are the desired FPS 

(in-package :fps-independent-timestep)

;;Implementation of https://gafferongames.com/post/fix_your_timestep/

;;;;<CLOCK>
(deftype seconds ()
  ;;[FIXME]arbitrary amount
  '(unsigned-byte 40))
(deftype nanosecond ()
  `(integer 0 ,(1- (expt 10 9))))
(defun microseconds ()
  (declare (optimize (speed 3) (safety 0)))
  #+(or allegro cmu sbcl abcl ccl (and lispworks (or linux darwin)))
  (let ((zeroed-seconds (load-time-value (local-time:timestamp-to-unix (local-time:now)))))
    (declare (type seconds zeroed-seconds))
    (multiple-value-bind (sec nsec) (%get-current-time)
      (declare (type nanosecond nsec)
	       (type seconds sec))
      (+ (* 1000000 (- sec zeroed-seconds))
	 (floor nsec 1000))))
  #-(or allegro cmu sbcl abcl ccl (and lispworks (or linux darwin)))   
  (* (get-internal-real-time)
     (load-time-value
      (progn
	;;throw error if internal-time-units-per-second is less than 1000
	(when (> 1000 internal-time-units-per-second)
	  (error "no suitable clock found"))
	
	(round (/ 1000000 internal-time-units-per-second))))))
;;;;</CLOCK>

(defstruct (ticker (:constructor %make-ticker))
  (ticks 0 :type fixnum)
  (dt (floor 1000000 60) :type fixnum)
  (current-time (error "give initial time") :type fixnum)
  (accumulator 0 :type fixnum)
  (bailout (floor 1000000 4) :type fixnum)
  (aux 0.0 :type double-float))

;;(declaim (ftype (function (fixnum fixnum &optional fixnum)) make-ticker))
(defun make-ticker (dt time &optional (bailout (floor 1000000 4))) ;;[FIXME]
  (%make-ticker :dt dt :current-time time :aux (coerce (/ 1 dt) 'double-float)
		:bailout bailout))

(defmacro run-iterations (ticker &body body)
  (alexandria:with-gensyms (accumulator ticks times)
    (alexandria:once-only (ticker)
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
  (alexandria:once-only (ticker time)
    (alexandria:with-gensyms (times)
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

(defun ticks-per-second (&optional (ticker *ticker*))
  (/ 1000000.0 (ticker-dt ticker)))

(defun dt (&optional (ticker *ticker*))
  (/ 1.0 (ticks-per-second ticker)))
