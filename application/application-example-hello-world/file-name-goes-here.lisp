(defpackage :application-example-hello-world
  (:use #:cl)
  (:export #:start))
(in-package :application-example-hello-world)

(defun start ()
  (application:main
   (lambda ()
     (loop (application:poll-app)
	(when (window::skey-j-p (window::keyval #\))
	  (application::quit))
	(when (window::skey-j-p (window::keyval #\E))
	  (window::toggle-mouse-capture))
	(unless (window::mice-free-p)
	  (gl:clear :color-buffer-bit
		    :depth-buffer-bit)
	  (gl:with-primitives :triangles
	    (gl:color 1.0 0.0 0.0)
	    (gl:vertex 1.0 -1.0 0.5)
	    (gl:color 0.0 1.0 0.0)
	    (gl:vertex 0.0 1.0 0.5)
	    (gl:color 0.0 0.0 1.0)
	    (gl:vertex -1.0 -1.0 0.5)))))
   :width 512
   :height 512
   :title "Hello World -_-")) 
