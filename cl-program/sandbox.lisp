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
  (clrhash *g/chunk-call-list*)

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

(defparameter *save* #P"third/")

(defparameter *saves-dir* (merge-pathnames #P"saves/" ourdir))

(defun save (filename &rest things)
  (let ((path (merge-pathnames filename *saves-dir*)))
    (with-open-file (stream path :direction :output :if-does-not-exist :create :if-exists :supersede)
      (dolist (thing things)
	(prin1 thing stream)))))

(defun save2 (thingfilename &rest things)
  (apply #'save (merge-pathnames (format nil "~s" thingfilename) *save*) things))

(defun myload2 (thingfilename)
  (myload (merge-pathnames (format nil "~s" thingfilename) *save*)))

(defun myload (filename)
  (let ((path (merge-pathnames filename *saves-dir*)))
    (let ((things nil))
      (with-open-file (stream path :direction :input :if-does-not-exist nil)
	(tagbody rep
	   (let ((thing (read stream nil nil)))
	     (when thing
	       (push thing things)
	       (go rep)))))
      (nreverse things))))



