(defpackage #:application
  (:use #:cl #:utility)
  (:export
   #:main
   #:*thread*
   #:*main-subthread-p*)
  (:export
   #:poll-app
   #:*quit-token*
   #:on-session-change)  
  (:export
   #:w
   #:h)
  #+nil
  (:export
   #:al-context)
  (:export
   #:quit))
(in-package :application)

(defparameter *quit-token* nil)
(defmacro with-quit-token ((&optional (value '(cons "default" "quit token"))) &body body)
  `(let ((*quit-token* ,value)
	 (window:*status* nil)) ;;[FIXME]nil = alive
     (catch *quit-token*
       ,@body)))

(defparameter *main-subthread-p*
  #-darwin
  t
  #+darwin
  nil)
(defparameter *thread* nil)
(defun main (start-fun &rest args)
  ;;Save *standard-output* 
  (let* ((stdo *standard-output*))
    (flet ((fun ()
	     (declare (optimize (debug 3)))
	     ;;restore *standard-output* from the thread
	     (let ((*standard-output* stdo))
	       (window:wrapper args		     
		 (setf window:*resize-hook* 'root-window-change)
		 (dolist (item '(h w))
		   (deflazy:refresh item t))
		 (window:set-vsync t)
		 (with-quit-token ((cons "trampoline" "token"))
		   (glhelp:with-gl-context (nil)
		     (gl:enable :scissor-test)
		     (funcall start-fun)))))))
      (cond
	(*main-subthread-p*
	 (unless (and *thread*
		      (bordeaux-threads:thread-alive-p *thread*))
	   (setf *thread* (bordeaux-threads:make-thread #'fun))))
	(t
	 #+darwin
	 (trivial-main-thread:call-in-main-thread #'fun)
	 #-darwin
	 (fun))))))

(deflazy:deflazy w ()
  window:*width*)
(deflazy:deflazy h ()
  window:*height*)
(defun root-window-change (w h)
  (unless (= (deflazy:getfnc 'h) h)
    (deflazy:refresh 'h t))
  (unless (= (deflazy:getfnc 'w) w)
    (deflazy:refresh 'w t)))


(defmacro on-session-change (session-place &body body &environment env)
  (multiple-value-bind (vars vals stores setter getter)
      (get-setf-expansion session-place env)
    (with-gensyms (token)
      `(let* (,@ (mapcar #'list vars vals))
	 (let ((,token *quit-token*))
	   (unless (eq ,getter ,token)
	     ,@body
	     (multiple-value-bind ,stores ,token
	       ,setter)))))))

(defmacro quit (&optional form)
  `(progn
     (setf window:*status* t) ;;[FIXME]t = exit
     (throw *quit-token* ,form)))

(defun poll-app ()
  (when window:*status*
    (quit))
  (window:update-control-state2)
  (dependency-graph:flush-refreshes)
  (window:update-display)
  (window:poll)
  (window:update-control-state))

(defun kill ()
  (bt::destroy-thread application:*thread*))

#+nil
(deflazy:deflazy al-context ()
  (music:really-start)
  music:*al-context*)

#+nil
(deflazy:getfnc 'al-context)
#+nil
(defun restart-sound-system ()
  (music:restart-al)
  (deflazy:refresh 'al-context t))

;;;;************************************************************************;;;;
(defpackage #:app
  (:use :cl)
  (:export
   #:enter
   #:default-loop

   #:subapp
   #:push-mode
   #:pop-mode
   #:switch-mode
   #:quit
   #:kill)
  (:import-from #:application
		#:quit
		#:kill))
(in-package #:app)
;;;;Main loop
(defun enter (&optional (app 'default-per-frame))
  (reset-per-frame-and-stack)
  (subapp app)
  (start-window))

(defun start-window ()
  (application:main 'default-loop
   :width (* 80 8)
   :height (* 25 16)
   :title ""))

(defun default-loop ()
  (loop
     (application:poll-app)
     (per-frame)))
;;Popping the last node of the stack is equivalent to quitting,
;;because of the default-per-frame that quits app.

;;A subapp is like a copy of the app,
;;glfw3 window and all, within the app?

(defun unit-circular-list (item)
  (let ((cell (list item)))
    (setf (cdr cell) cell)
    cell))
(defun circular-per-frame ()
  (unit-circular-list 'default-per-frame))
(defparameter *null-per-frame* (circular-per-frame))
(defparameter *null-app-stack* (unit-circular-list *null-per-frame*))
(defparameter *per-frame* *null-per-frame*)
(defparameter *app-stack* *null-app-stack*)
(defun save-modes-to-app-stack ()
  (push *per-frame* *app-stack*)
  (reset-per-frame))
(defun restore-modes-to-app-stack ()
  (setf *per-frame* (pop *app-stack*)))

(defun subapp (fun)
  ;;The application is both a mode
  ;;and a means of quitting.
  (labels ((this-function ()
	     ;;(print "running")
	     (unwind-protect
		  (progn (save-modes-to-app-stack)
		    (application::with-quit-token ()
		      (funcall fun)))
	       (restore-modes-to-app-stack)
	       (pop-mode))))
    (push-mode #'this-function)))

(defun default-per-frame ()
  ;;Do nothing, except quit
  (application:quit))
;;This is a circular list with one element.
;;So if you keep popping the mode,
;;nothing happens.
(defun reset-per-frame-and-stack ()
  (reset-per-frame)
  (reset-app-stack))
(defun reset-per-frame ()
  (setf *per-frame* *null-per-frame*))
(defun reset-app-stack ()
  (setf *app-stack* *null-app-stack*))
(defun per-frame ()
  (funcall (car *per-frame*)))
(defun push-mode (mode)
  (etypecase mode
    ;;mode is either a function
    (function t)
    ;;or a symbol with a 
    (symbol
     (assert (fboundp mode) nil "Symbol:~a is function unbound" mode)))
  (push mode *per-frame*))
(defun pop-mode ()
  (pop *per-frame*))
(defun switch-mode (mode)
  (pop-mode)
  (push-mode mode))

;;test
(defun test-for-modes ()
  (labels ((app-entry ()
	     (push-mode #'app)
	     (default-loop))
	   (app ()
	     (when (window:button :key :repeat #\u)
	       (print "going up!")
	       (subapp #'app-entry))
	     (when (window:button :key :repeat #\m)
	       (print "another mode")
	       (app-entry))
	     (when (window:button :key :repeat #\n)
	       (print "removing mode")
	       ;;FIXME::will this pop too many modes?
	       (pop-mode)
	       )
	     (when (window:button :key :repeat #\q)
	       (print "quitting")
	       (application:quit))
	     (when (window:button :key :pressed #\p)
	       (let ((*print-circle* t))
		 (print *per-frame*)
		 (print *app-stack*)))))
    (enter #'app-entry)))
