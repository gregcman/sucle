(ql:quickload
 '(:ncurses-clone-for-lem
   :alexandria))
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
