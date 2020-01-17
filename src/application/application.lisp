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
	      (window:wrapper initfun
			       rest))))))))

(deflazy:deflazy w ()
  window:*width*)
(deflazy:deflazy h ()
  window:*height*)
(defun root-window-change (w h)
  (unless (= (deflazy:getfnc 'h) h)
    (deflazy:refresh 'h t))
  (unless (= (deflazy:getfnc 'w) w)
    (deflazy:refresh 'w t)))

(defparameter *quit-token* nil)
(defmacro with-quit-token ((&optional (value '(cons "default" "quit token"))) &body body)
  `(let ((*quit-token* ,value)
	 (window:*status* nil)) ;;[FIXME]nil = alive
     (catch *quit-token*
       ,@body)))
(defun init (fun)
  (lambda ()
    (declare (optimize (debug 3)))
    (glhelp:with-gl-context (nil)
      (setf window:*resize-hook* 'root-window-change)
      (dolist (item '(h w))
	(deflazy:refresh item t))
      (window:set-vsync t)
      (gl:enable :scissor-test)
      (with-quit-token ((cons "trampoline" "token"))
	(funcall fun)))))

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
  (deflazy:flush-refreshes)
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
