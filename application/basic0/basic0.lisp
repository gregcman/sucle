(defpackage #:sucle
  (:use #:cl #:utility #:application)
  (:export #:start))
(in-package :sucle)

(defparameter *app* nil)
(defparameter *draw-pic* nil)
(defparameter *sandbox* t)
(defparameter *draw-graph* nil)
(defparameter *draw-sketch* nil)

(defparameter *with-functions*
  #+nil
  (list
   (lambda (x)
     (print 34)
     (unwind-protect 
	  (funcall x)
       (print 2))))
  (list
   'vecto-stuff::call-with-zpng-lparallel
   'sandbox::call-with-world-meshing-lparallel))
(defun run-with (fun)
  (flet ((nest (with-fun cont)
	   (lambda ()
	     (funcall with-fun cont))))
    (dolist (with-fun *with-functions*)
      (setf fun (nest with-fun fun))))
  fun)

(defun start ()
  (application:main
   *sucle-app-function*
   :width (floor (* 80 fast-text-grid-sprites::*glyph-width*))
   :height (floor (* 25 fast-text-grid-sprites::*glyph-height*))
   :title ""))

(defparameter *sucle-app-function*
  (run-with
   (lambda ()
     #+nil
     (setf (sandbox-sub::entity-fly? testbed::*ent*) nil
	   (sandbox-sub::entity-gravity? testbed::*ent*) t)
     ;;(our-load)
     (let ((text-sub::*text-data-what-type* :framebuffer))
       (window::set-vsync t)
       (unwind-protect
	    (loop
	       (application:poll-app)
	       (when *sandbox*
		 (testbed::per-frame))
	       ;;#+nil
	       (when *app*
		 (progn
		   ;;#+nil
		   (fast-text-grid-sprites::per-frame)
		   #+nil
		   
		   (when (window:skey-j-p (window::keyval #\e))
		     (window::toggle-mouse-capture))))
	       (when *draw-pic*
		 (vecto-stuff::draw-pic))
	       (when *draw-graph*
		 (cartesian-graphing::draw-graph))
	       ;;#+nil
	       (when (window:skey-j-p (window::keyval #\h))
		 (toggle *app*))
	       (when (window:skey-j-p (window::keyval #\u))
		 (toggle *draw-pic*))
	       (when (window:skey-j-p (window::keyval #\j))
		 (toggle *sandbox*))
	       (when (window:skey-j-p (window::keyval #\i))
		 (toggle *draw-graph*))
	       (when (window:skey-j-p (window::keyval #\n))
		 (toggle *draw-sketch*))
	       (when *draw-sketch*
		 (update-sketch-instance (getfnc 'sketch-sketch))
		 (update-sketch-instance (getfnc 'sketch-sketch2))
		 
		 ))
	 (save))))))

(defun update-sketch-instance (instance)
  (setf (sketch-sucle::sketch-height instance) (getfnc 'application::h)
	(sketch-sucle::sketch-width instance) (getfnc 'application::w))
  (sketch-sucle::set-ortho-matrix instance)
  (sketch-sucle::set-shader-values instance)
  (glhelp::bind-default-framebuffer)
  (gl:disable :depth-test)
  (gl:disable :cull-face)
  (gl:polygon-mode :front-and-back :fill)	
  (sketch-sucle::initialize-gl instance)
  (sketch-sucle::render-sketch-instance instance)
  )

(glhelp::deflazy-gl sketch-sketch ()
  ;;DO NOT REEVALUTE THIS WHEN RUNNING!!!
  (let ((instance (make-instance 'sketch-sucle-examples:sinewave)))
    (sketch-sucle::initialize-environment instance)
    (sketch-sucle::prepare instance)
    (values instance)))

(glhelp::deflazy-gl sketch-sketch2 ()
  ;;DO NOT REEVALUTE THIS WHEN RUNNING!!!
  (let ((instance (make-instance 'sketch-sucle-examples:lenna)))
    (sketch-sucle::initialize-environment instance)
    (sketch-sucle::prepare instance)
    (values instance)))

(defmethod deflazy::cleanup-node-value ((object sketch-sucle::sketch))
  (sketch-sucle::with-environment
   (slot-value object 'sketch-sucle::%env)
   (let ((vao (sketch-sucle::env-vao sketch-sucle::*env*)))
     (kit.gl.vao:vao-unbind)
     (kit.gl.vao::gl-delete vao))
    (loop for resource being the hash-values of (sketch-sucle::env-resources sketch-sucle::*env*)
       do (sketch-sucle::free-resource resource))))

(defun save ()
  ;;(atest::remove-zeroes)
  ;;FIXME::don't remove all the chunks?
  (sandbox::msave))
#+nil
(defun our-load ()
  (sandbox::mload))

(eval-when (:load-toplevel :execute)
  (setf sandbox::*world-directory* "test/")
  (setf sandbox::*some-saves*
	(merge-pathnames
	 "save/"
	 (asdf:system-source-directory :sucle)
	 )
	#+nil
	(cdr (assoc (machine-instance) 
		    '(("gm3-iMac" . #P"/media/imac/share/space/lispysaves/saves/sandbox-saves/")
		      ("nootboke" . #P"/home/terminal256/Documents/saves/"))
		    :test 'equal))))
