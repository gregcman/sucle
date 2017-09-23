(in-package :fuck)

(defparameter *sandbox-on* t)

(defun handoff-five ()
  (setf %gl:*gl-get-proc-address* (e:get-proc-address))
  (let ((hash aplayground::*stuff*))
    (maphash (lambda (k v)
	       (if (integerp v)
		   (remhash k hash)))
	     hash))
  (window:set-vsync t)
  (when *sandbox-on*
    (sandbox::build-deps #'aplayground::getfnc
			 #'aplayground::bornfnc)
    (sandbox::initialization1))

  (injection3)) 

(defparameter *control-state* (window::make-control-state
			       :curr window::*input-state*))

(defun physss ()
  (window:poll)
  (window::update-control-state *control-state*)


  (when *sandbox-on*
    (sandbox::thunkit *control-state*)))

(defparameter *ticker* nil)
(defparameter *realthu-nk* (lambda () (throw :end (values))))

(progn
  (defun actual-stuuff ()
    (when window:*status*
      (throw :end (values)))
    (let ((ticker *ticker*))
      (tick-update ticker (fine-time))
      (tick-physics ticker (function physss))
      (let ((fraction (float (/ (ticker-accumulator ticker)
				(ticker-dt ticker)))))
	(gl:viewport 0 0 e:*width* e:*height*)
	(gl:clear
	 :color-buffer-bit
	 :depth-buffer-bit)
	(when *sandbox-on*
	  (progn
	    (dotimes (x 4) (window:poll))
	    (sandbox::remove-spurious-mouse-input)
	    (window:poll)
	    (when (window:mice-locked-p)
 	      (sandbox::look-around))
	    (sandbox::render fraction
			     #'aplayground::getfnc))))
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




