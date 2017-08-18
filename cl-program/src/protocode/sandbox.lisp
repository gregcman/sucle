(in-package #:aplayground)

(defparameter *gl-thread* nil)
(defun threaded-main (func)
  (unless (and *gl-thread*
	       (bordeaux-threads:thread-alive-p *gl-thread*))
    (setf *gl-thread*
	  (bordeaux-threads:make-thread
	   (lambda ()
	     (window:wrapper func))))))


(defun handoff-three ()
  (glinnit)
  (injection)) 

(defun injection ()
  (window:poll)
  (physics)
  (render)
  (window:update-display)
  (progn
   (unless window:*status*
     (injection))))

(defun main ()
  (threaded-main #'handoff-three))


(defun handoff-four ()
  (sandbox::initialization1)
  (sandbox::glinnit)
  (injection2)) 

(defun injection2 ()
  (window:poll) 
  (sandbox::thunkit)
  (sandbox::render)

  (progno
    (sandbox::bind-default-framebuffer)
    (render)
    )
  (window:update-display)
  (progno
    (map-box #(0 0 20 5)
	     (lambda (x y)
	       (scwu nil x y)))
    (progn
      (copy-string-to-world 0 3 (print-to-buf "xpos: " #'princ))
      (copy-string-to-world 0 2 (print-to-buf "ypos: " #'princ))
      (copy-string-to-world 0 1 (print-to-buf "zpos: " #'princ)))

    (progn
      (copy-string-to-world 6 3 (print-to-buf sandbox::*xpos*))
      (copy-string-to-world 6 2 (print-to-buf sandbox::*ypos*))
      (copy-string-to-world 6 1 (print-to-buf sandbox::*zpos*))

      (when sandbox::fist?
	(copy-string-to-world 0 0 (print-to-buf "blockid: " #'princ))
	(copy-string-to-world 9 0 (print-to-buf (aref mc-blocks::names
						      (world::getblock sandbox::fist-side-x
								       sandbox::fist-side-y
								       sandbox::fist-side-z))
						#'princ))))
    (physics))
  (unless window:*status*
    (injection2)))

(defun main2 ()
  (trivial-main-thread:call-in-main-thread
   (lambda ()
     (let ((window::*iresizable* t)
	   (window::*iwidth* 256)
	   (window::*iheight* 256)
	   )
       (window::wrapper #'handoff-four)))))

(defun handoff-five ()
  (sandbox::initialization1)
  (sandbox::glinnit)
  (injection3)) 

(defconstant +million+ (expt 10 6))

(defun injection3 ()
  (let ((ttt 0)
	(dt (floor +million+ 60))
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
					;(integrate current t dt)
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
  (;trivial-main-thread:call-in-main-thread   
   (lambda ()
     (let ((window::*iresizable* t)
	   (window::*iwidth* 256)
	   (window::*iheight* 256)
	   )
       (window::wrapper #'handoff-five)))))

(defun fine-time ()
  (multiple-value-bind (s m) (sb-ext:get-time-of-day)
    (+ (* (expt 10 6) s) m)))
