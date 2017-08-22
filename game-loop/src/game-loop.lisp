(in-package :game-loop)

(defun handoff-five ()
  (sandbox::initialization1)
  (injection3)) 

(defconstant +million+ (expt 10 6))

(defun injection3 ()
  (let ((ttt 0)
	(dt (floor +million+ 20))
	(current-time (fine-time))
	(accumulator 0)
	(previous)
	(current))
    (block leave
      (loop
	 (if window:*status*
	     (return-from leave)
	     (progn
	       (let* ((new-time (fine-time))
		      (frame-time (- new-time current-time)))
		 (let ((quarter-million (* 0.25 +million+)))
		   (if (> frame-time quarter-million)
		       (setf frame-time quarter-million)))
		 (setf current-time new-time)
		 (incf accumulator frame-time)
		 (block later
		   (loop
		      (if (>= accumulator dt)
			  (progn
			    (setf previous current)
			    (window:poll)
			    (sandbox::thunkit)
			    
			    (incf ttt dt)
			    (decf accumulator dt))
			  (return-from later))))
		 (let ((fraction (/ (float accumulator)
				    (float dt))))
		   (sandbox::render fraction))
		 (window:update-display))))))))

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

(defun fine-time ()
  (multiple-value-bind (s m) (sb-ext:get-time-of-day)
    (+ (* (expt 10 6) s) m)))
