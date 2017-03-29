(in-package #:sandbox)

(defparameter *gl-thread* nil)
(defun threaded-main (func)
  (unless (and *gl-thread*
	       (bordeaux-threads:thread-alive-p *gl-thread*))
    (setf *gl-thread*
	  (bordeaux-threads:make-thread
	   (lambda ()
	     (window:wrapper func))))))


(defun handoff-three ()
  (clrhash *g/call-list*)
  (clrhash *g/texture*)
  (clrhash *g/shader*)

  (glinnit)
  (injection)) 

(defun injection ()
  (window:poll)
  (physics)
  (render)
  (unless window:*status* 
    (injection)))

(defun main ()
  (threaded-main #'handoff-three))

