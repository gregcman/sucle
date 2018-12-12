(defpackage #:application
  (:use #:cl #:utility #:deflazy)
  (:export
   #:main
   #:*thread*
   #:*main-subthread-p*)
  (:export
   #:poll-app
   #:*quit-token*
   #:on-session-change)
  (:export
   #:getfnc
   #:deflazy)  
  (:export
   #:w
   #:h
   #:gl-context
   #:al-context))
(in-package :application)

(defparameter *main-subthread-p*
  #-darwin
  t
  #+darwin
  nil)
(defparameter *thread* nil)
(defun main (start-fun &rest rest)
  (let ((fun (apply #'just-main start-fun rest)))
    (if *main-subthread-p*
	(when (or (eq nil *thread*)
		  (not (bordeaux-threads:thread-alive-p *thread*)))
	  (setf
	   *thread*
	   (bordeaux-threads:make-thread
	    fun)))
	(#+darwin
	 trivial-main-thread:call-in-main-thread
	 #-darwin
	 funcall
	 fun))))

(eval-always
  (defparameter *parameters*
    '((title "app")
      (width 720)
      (height 480)
      (resizable nil))))

(etouq
 (flet ((supplyify (sym)
	  (symbolicate2 (list sym "-SUPPLIED-P"))))
   (let ((keys *parameters*))
     `(defun just-main (start-fun &rest rest
			&key
			  ,@(mapcar
			     (lambda (x)
			       (destructuring-bind (name default) x
				 (list name default (supplyify name))))
			     keys)
			  &allow-other-keys)
	,@(mapcar (lambda (pair)
		    (let ((sym (first pair)))
		      `(unless ,(supplyify sym)
			 (push ,sym rest)
			 (push ,(keywordify sym)
			       rest))))
		  keys)
	(let ((stdo *standard-output*)
	      (initfun (init start-fun)))
	  (lambda ()
	    (let ((*standard-output* stdo))
	      (window::wrapper initfun
			       rest))))))))

(deflazy w ()
  window::*width*)
(deflazy h ()
  window::*height*)
(defun root-window-change (w h)
  (unless (= (getfnc 'h) h)
    (refresh 'h t))
  (unless (= (getfnc 'w) w)
    (refresh 'w t)))
(deflazy gl-context ()
  (unless glhelp::*gl-context*
    (error "no opengl context you idiot!")))

(defparameter *quit-token* nil)
(defun init (fun)
  (lambda ()
    (glhelp:with-gl-context ((window:get-proc-address))
      (setf window::*resize-hook* 'root-window-change)
      (dolist (item '(h w gl-context))
	(refresh item t))
      (window:set-vsync t)
      (gl:enable :scissor-test)
      (let ((*quit-token* (cons "trampoline" "token")))
	(catch *quit-token*
	  (funcall fun))))))

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
     (setf window::*status* t)
     (throw *quit-token* ,form)))

(defun poll-app ()
  (when window:*status*
    (quit))
  (window::update-control-state2)
  (window:update-display)
  (flush-refreshes)
  (window:poll)
  (window::update-control-state))

(defmethod deflazy::cleanup-node-value ((object glhelp::gl-object))
  (when (glhelp:alive-p object)
    (glhelp::gl-delete* object)))

#+nil
(deflazy al-context ()
  (music::really-start)
  music::*al-context*)

#+nil
(getfnc 'al-context)
#+nil
(defun restart-sound-system ()
  (music::restart-al)
  (refresh 'al-context t))
