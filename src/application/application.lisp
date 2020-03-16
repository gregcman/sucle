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
   #:h
   #:gl-context)
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
