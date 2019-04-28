(defpackage :sucle2
  (:use #:cl)
  (:export #:start))
(in-package :sucle2)

;;;;suspending computer [on imac running ubuntu] will work when:
;;;- glfw window is open
;;;- opengl has received data?
;;;- window has been focused on and mouse hidden atleast once, after opening a terminal??
;;;- not currently sending data to opengl
;;;;otherwise default framebuffer will glitch out, only one will work, in fullscreen only?

(defun triangle ()
  (gl:clear :color-buffer-bit
	    :depth-buffer-bit
	    :stencil-buffer-bit)
  (gl:with-primitives :triangles
    (gl:color 1.0 0.0 0.0)
    (gl:vertex 1.0 -1.0 0.5)
    (gl:color 0.0 1.0 0.0)
    (gl:vertex 0.0 1.0 0.5)
    (gl:color 0.0 0.0 1.0)
    (gl:vertex -1.0 -1.0 0.5)))

(defun start ()
  (application:main
   (lambda ()
     (application:poll-app)
     (gl:clear-color 0.5 0.5 0.5 0.0)
     (triangle)
     
     (loop (application:poll-app)
	(when (window::skey-j-p (window::keyval #\))
	  (application::quit))
	(when (window::skey-j-p (window::keyval #\E))
	  (window::toggle-mouse-capture))
	(unless	(window::mice-free-p)
	  (triangle))))
   :width 512
   :height 512
   :title "Hello World -_-")) 
