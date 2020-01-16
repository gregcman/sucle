;;;;Basics
;;;;1. Opening a window
;;;;2. Testing for button presses
;;;;3. Drawing with the fixed function pipeline
;;;;4. Setting up shaders
;;;;5. deflazy

(defpackage #:test1
  (:use :cl))
(in-package #:test1)
;;;;In order to open a window, 
(defun start ()
  (application:main
   (lambda ()
     ;;The game loop
     (loop
	;;This function needs to be called in order
	;;to observe key board events, mouse events.
	;;without it, the window will not close.
	(application:poll-app)))
   ;;The width of the window
   :width 512
   ;;The height of the window
   :height 512
   ;;The title of the window
   :title "1. Opening a window"))

(defpackage #:test2
  (:use :cl))
(in-package #:test2)
;;;;In order to open a window, 
(defun start ()
  (application:main
   (lambda ()
     ;;The game loop
     (loop
	;;This function needs to be called in order
	;;to observe key board events, mouse events.
	;;without it, the window will not close.
	(application:poll-app)))
   ;;The width of the window
   :width 512
   ;;The height of the window
   :height 512
   ;;The title of the window
   :title "1. Opening a window"))

(defpackage #:sucle-test
  (:use :cl))
(in-package :sucle-test)

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
	(frame)))
   :width 512
   :height 512
   :title "Hello World -_-")) 

(defun frame ()
  (when (window:button :key :down #\Escape)
    (application:quit))
  (when (window::skey-j-p (window::keyval #\E))
    (window::toggle-mouse-capture))
  (unless (window::mice-free-p)
    (triangle)))

 
(defpackage :application-example-hello-world
  (:use #:cl)
  (:export #:start))
(in-package :application-example-hello-world)

(defparameter *view* (ncurses-clone-lem-view:make-view 0 0 58 30 nil))

(defun start ()
  (application:main
   (lambda ()
     (ncurses-clone-for-lem:init)
     (loop (application:poll-app)
	(frame)))
   :width 512
   :height 512
   :title "Draw random text to the screen"))

(defun aux ()
  (alexandria:random-elt
   '("green"
     "red"
     "blue"
     "brown"
     "orange"
     "white"
     "black"
     "light blue"
     "light green")))

(defun frame ()
  (ncurses-clone-for-lem:render)
  (ncurses-clone-lem-view:redraw-view-after *view*)
  (ncurses-clone-lem-view:update-display)
  (lem.term:with-attribute (:fg (aux) :bg (aux)
				:underline
				(zerop (random 3))
				:bold
				(zerop (random 4))
				:reverse
				(zerop (random 2)))
    (ncurses-clone-lem-view:print-into-view
     *view*
     (random 50)
     (random 50)
     (prin1-to-string
      (case (random 4)
	(0 "     ")
	(1 #())
	(2 (code-char (random 2000)))
	(3 (list (random 100)))))))
  (when (window::skey-j-p (window::keyval #\))
    (application::quit)))
