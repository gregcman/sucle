(defpackage #:fps-independent-timestep
  (:use #:cl #:utility #:clock)
  (:export
   #:ticker-dt
   #:ticker-accumulator
   #:make-ticker
   #:tick-physics
   #:tick-update
   #:ticker-aux))

(in-package :fps-independent-timestep)

(defstruct (ticker (:constructor %make-ticker))
  (ticks 0 :type fixnum)
  (dt (floor 1000000 60) :type fixnum)
  (current-time (error "give initial time") :type fixnum)
  (accumulator 0 :type fixnum)
  (bailout (floor 1000000 4) :type fixnum)
  (aux 0.0 :type double-float))

(declaim (ftype (function (fixnum fixnum &optional fixnum)) make-ticker))
(defun make-ticker (dt time &optional (bailout (floor 1000000 4))) ;;FIXME
  (%make-ticker :dt dt :current-time time :aux (coerce (/ 1 dt) 'double-float)
		:bailout bailout))

(defmacro tick-physics (ticker &body body)
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

(defun tick-update (ticker new-time)
  (let* ((frame-time (- new-time (ticker-current-time ticker))))
    (let ((bailout (ticker-bailout ticker)))
      (let ((toofar (> frame-time bailout))
	    (toobelow (> 0 frame-time)))
	(when (or toofar toobelow)
	  (setf frame-time bailout))))
    (setf (ticker-current-time ticker) new-time)
    (incf (ticker-accumulator ticker) frame-time)))

(defmacro tick (ticker (&optional (time '(microseconds))) &body body)
  (once-only (ticker time)
    (with-gensyms (times)
      `(progn
	 (fps-independent-timestep:tick-update ,ticker ,time)
	 (let ((,times
		(fps-independent-timestep:tick-physics ,ticker
						       ,@body)))
	   (values
	    (coerce (* (fps-independent-timestep:ticker-accumulator ,ticker)
		       (fps-independent-timestep:ticker-aux ,ticker))
		    'single-float)
	    ,times))))))
