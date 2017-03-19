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
  (macrolet ((k (val)
	       (nthcdr 3 val))))
  (clrhash *g/call-list*)
  (clrhash *g/texture*)
  (clrhash *g/shader*)

  (glinnit)
  (physinnit)
  (injection)) 

(defun injection ()
  (window:poll)
  (physics)
  (set-render-cam-pos *camera*)
  (remove-spurious-mouse-input)
  (render)
  (incf *ticks*)
  (unless window:*status* 
    (injection)))

(defparameter *ticks* 0)
(defun main ()
  (threaded-main #'handoff-three))

