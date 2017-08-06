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
  (unless window:*status*
    (injection)))

(defun main ()
  (threaded-main #'handoff-three))


(defun handoff-four ()
  (sandbox::initialization1)
  (glinnit)
  (injection2)) 

(defun injection2 ()
  (window:poll)

  
  (sandbox::thunkit)

  (progn
   (sandbox::bind-default-framebuffer)
   (progn
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
						 #'princ)))))
   (physics)
   (render))
  (window:update-display)
  (unless window:*status*
    (injection2)))

(defun main2 ()
  (threaded-main #'handoff-four))

