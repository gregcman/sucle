(defpackage #:sucle
  (:use #:cl #:utility #:application)
  (:export #:start))
(in-package :sucle)

(defparameter *app* nil)
(defparameter *sandbox* t)

(defparameter *with-functions*
  #+nil
  (list
   (lambda (x)
     (print 34)
     (unwind-protect 
	  (funcall x)
       (print 2))))
  (list
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
   :width (floor (* 80 text-sub::*block-width*))
   :height (floor (* 25 text-sub::*block-height*))
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
       (fps:set-fps 60)
       (unwind-protect
	    (loop
	       (application:poll-app)
	       (when *sandbox*
		 (testbed::per-frame))
	       ;;#+nil
	       (when *app*
		 (progn
		   #+nil
		   (when (window:skey-j-p (window::keyval #\e))
		     (window::toggle-mouse-capture))))
	       ;;#+nil
	       (when (window:skey-j-p (window::keyval #\h))
		 (toggle *app*))
	       (when (window:skey-j-p (window::keyval #\j))
		 (toggle *sandbox*)))
	 (save))))))

(defun save ()
  ;;(atest::remove-zeroes)
  ;;FIXME::don't remove all the chunks?
  (sandbox::msave))
#+nil
(defun our-load ()
  (sandbox::mload))

(eval-when (:load-toplevel :execute)
  (setf sandbox::*world-directory*
	;;"first/"
	;;#+nil
	"test/"
	)
  #+nil
  (progn
    (setf sandbox::*some-saves*
	  (cdr (assoc (machine-instance) 
		      '(("gm3-iMac" . #P"/media/imac/share/space/lispysaves/saves/sandbox-saves/")
			("nootboke" . #P"/home/terminal256/Documents/saves/"))
		      :test 'equal))))
  ;;#+nil
  (progn
    (setf sandbox::*some-saves*
	  (sucle-temp:path "save/"))))

(defun load-world-again (name)
  (setf sandbox::*persist* nil)
  (setf sandbox::*world-directory* name)
  (testbed::load-world t))
