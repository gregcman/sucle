(defpackage #:sucle
  (:use #:cl #:utility #:application)
  (:export #:start))
(in-package :sucle)

(defparameter *app* nil)
(defparameter *draw-pic* nil)
(defparameter *sandbox* t)
(defparameter *draw-graph* nil)

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
   (run-with
    (lambda ()
      (setf (sandbox-sub::entity-fly? testbed::*ent*) nil
	    (sandbox-sub::entity-gravity? testbed::*ent*) t)
      (our-load)
      (let ((text-sub::*text-data-what-type* :framebuffer))
	(unwind-protect
	  (loop
	     (application:poll-app)
	     (when *sandbox*
	       (testbed::per-frame))
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
	       (toggle *draw-graph*)))
	  (save)))))
   :width (floor (* 80 fast-text-grid-sprites::*glyph-width*))
   :height (floor (* 25 fast-text-grid-sprites::*glyph-height*))
   :title ""))

(defun save ()
  (atest::remove-zeroes)
  (sandbox::msave "test/"))

(defun our-load ()
  (sandbox::mload "test/"))


(setf sandbox::*some-saves*
      (merge-pathnames
       "save/"
       (asdf:system-source-directory :sucle)
		       )
      #+nil
      (cdr (assoc (machine-instance) 
		  '(("gm3-iMac" . #P"/media/imac/share/space/lispysaves/saves/sandbox-saves/")
		    ("nootboke" . #P"/home/terminal256/Documents/saves/"))
		  :test 'equal)))
