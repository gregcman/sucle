(defpackage #:basic
  (:use #:cl #:utility))
(in-package :basic)

(defun per-frame (&optional session)
  (declare (ignorable session))
  (app))
(defun start ()
  (let ((application::*argument-values*
	 (list nil
	       *window-start-width*
	       *window-start-height*
	       *window-start-title*)))
    (setf application::*trampoline*
	  '(per-frame))
    (application::main)))
(defvar *this-directory* (filesystem-util:this-directory))

(defparameter *window-start-height* 512)
(defparameter *window-start-width* 512)
(defparameter *window-start-title* "basic app")
(defparameter *ticks* 0)
(defun app ()
  (incf *ticks*)
  (when (window::skey-j-p (window::keyval :escape))
    (application::quit))
  (when (window::skey-j-p (window::keyval :a))
    (music::reset-listener)
    (music::play-at (merge-pathnames "wilhelm_scream.wav" *this-directory*) 0.0 0.0 0.0 1.0 1.0))
  (application::%set-render-area 0 0 window:*width* window:*height*)
  (gl:clear-color 1.0 1.0 1.0 0.0)
  (gl:clear :color-buffer-bit)
  (gl:line-width 10.0)
  (gl:with-primitive :line-loop
    (gl:color 1.0 0.0 0.0)
    (gl:vertex -1.0 -1.0)
    (gl:color 0.0 1.0 0.0)
    (gl:vertex 0.0 1.0)
    (gl:color 0.0 0.0 1.0)
    (gl:vertex 1.0 -1.0)))

