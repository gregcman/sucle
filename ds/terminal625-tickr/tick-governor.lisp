(defpackage tickr
  (:use #:cl)
  (:export
   #:ticker-dt
   #:ticker-accumulator
   #:make-ticker
   #:tick-physics
   #:tick-update))

(in-package :tickr)

(defstruct ticker
  (ticks 0 :type fixnum)
  (dt (floor 1000000 60) :type fixnum)
  (current-time (error "give initial time") :type fixnum)
  (accumulator 0 :type fixnum)
  (bailout (floor 1000000 4) :type fixnum))

(defun tick-physics (ticker function)
  (let ((accumulator (ticker-accumulator ticker))
	(ticks (ticker-ticks ticker))
	(changed? nil))
    (let ((dt (ticker-dt ticker)))
      (loop
	 (if (>= accumulator dt)
	     (progn
	       (funcall function)
	       (incf ticks dt)
	       (decf accumulator dt)
	       (setf changed? t))
	     (progn
	       (when changed?
		 (setf (ticker-accumulator ticker) accumulator)
		 (setf (ticker-ticks ticker) ticks))
	       (return)))))))

(defun tick-update (ticker new-time)
  (let* ((frame-time (- new-time (ticker-current-time ticker))))
    (let ((bailout (ticker-bailout ticker)))
      (when (> frame-time bailout)
	(setf frame-time bailout)))
    (setf (ticker-current-time ticker) new-time)
    (incf (ticker-accumulator ticker) frame-time)))

