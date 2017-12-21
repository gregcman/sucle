(defpackage #:funfair
  (:use #:cl #:funland)

  (:export main)
  (:export *trampoline*)
  (:export bornfnc getfnc deflazy)
  (:export microseconds tick *control-state* *camera* *render-area* *pre-trampoline-hooks*
	  set-render-area render-area-x render-area-y render-area-width render-area-height))

(in-package :funfair)

(defparameter *thread* nil)
(defun main ()
  (when (or (eq nil *thread*)
	    (not (bordeaux-threads:thread-alive-p *thread*)))
    (setf *thread*
	  (bordeaux-threads:make-thread
	   (just-main)))))

(defun just-main ()
  (let ((stdo *standard-output*))
    (lambda ()
      (progv (cons '*standard-output* *arguments*)
	  (cons stdo *argument-values*)
	(window::wrapper #'init)))))

(defparameter *arguments* '(window::*iresizable*
			    window::*iwidth*
			    window::*iheight*))
(defparameter *argument-values* (list nil 720 480
				      ))

(progn
  (defparameter *trampoline* (lambda (exit-token) (throw exit-token (values))))
  (defun call-trampoline ()
    (let ((value (gensym)))
      (catch value
	(loop
	   (trampoline-bounce value *trampoline*))))))

(defun trampoline-bounce (exit-sym fun)
  (when window:*status*
    (throw exit-sym (values)))
  (window:poll)
  (funcall fun exit-sym)
  (window::update-control-state *control-state*)
  (window:update-display))

(defparameter *circle* nil)
(progn
  (defun namexpr (hash name func)
    (setf (gethash name hash) func))
  (defun get-stuff (name stuff otherwise)
    (multiple-value-bind (val exists-p) (gethash name stuff)
      (if exists-p
	  val
	  (let* ((depdata (gethash name otherwise))
		 (genfunc (cdr depdata))
		 (deps (car depdata)))
	    (when (member name *circle*)
	      (error "circular dependency ~a" *circle*))
	    (if genfunc
		(let ((a nil))
		  (dolist (item deps)
		    (let ((*circle* (cons name *circle*)))
		      (push (get-stuff item stuff otherwise) a)))
		  (setf (gethash name stuff) 
			(apply genfunc
			       (nreverse a))))
		(error "no backup fun: ~a" genfunc)))))))
(progn
  (defparameter *backup* (make-hash-table :test 'eq))
  (defparameter *stuff* (make-hash-table :test 'eq)))
(progn
  (defun bornfnc (name func)
    (namexpr *backup* name func))
  (defun getfnc (name)
    (get-stuff name *stuff* *backup*))
  (defun getfnc-no-update (name)
    (gethash name *stuff*))
  (defun map-stuffs (fun)
    (maphash (lambda (k v)
	       (funcall fun k v))
	     *stuff*))
  (defun remove-stuff (k)
    (remhash k *stuff*)))

(defun mangle (sym &optional (start "_source_"))
  (symbolicate2 (list start sym)
		(symbol-package sym)))
(defmacro deflazy (name (&rest deps) &rest gen-forms)
  (let ((fun-name (mangle name)))
    `(progn
       (defun ,fun-name ,(mapcar (lambda (x)
				   (typecase x
				     (symbol x)
				     (otherwise (second x))))
				 deps)
	 ,@gen-forms)
       (bornfnc (quote ,name)
		(cons
		 (quote
		  ,(mapcar (lambda (x)
			     (typecase x
			       (symbol x)
			       (otherwise (first x))))
			   deps))
		 (function ,fun-name))))))
(progn
  (defclass render-area ()
    ((x :accessor render-area-x
	:initform 0
	:initarg :x)
     (y :accessor render-area-y
	:initform 0
	:initarg :y)
     (width :accessor render-area-width
	    :initform 0
	    :initarg :width)
     (height :accessor render-area-height
	     :initform 0
	     :initarg :height)))
  (defun set-render-area (render-area)
    (with-slots (x y width height) render-area
      (%set-render-area x y width height)))
  (defun %set-render-area (x y width height)
    (gl:viewport x y width height)
    (gl:scissor x y width height)))
;;;time in microseconds
(defun microseconds ()
  (multiple-value-bind (s m) (sb-ext:get-time-of-day)
    (+ (* (expt 10 6) (- s 1506020000)) m)))
(defun tick (ticker fun &optional (time (microseconds)))
  (tickr:tick-update ticker time)
  (let ((times
	 (tickr:tick-physics ticker fun)))
    (values
     (coerce (* (tickr:ticker-accumulator ticker)
		(tickr:ticker-aux ticker))
	     'single-float)
     times)))


(defparameter *control-state*
  window::*control-state*
  #+nil
  (window::make-control-state
   :curr window::*input-state*))
(defparameter *camera* (camat:make-camera
			:frustum-far (* 256.0)
			:frustum-near (/ 1.0 8.0)))
(defparameter *render-area* (make-instance 'render-area))

(defun init ()
  (glhelp:with-gl-context
    (setf %gl:*gl-get-proc-address* (window:get-proc-address))
    (window:set-vsync t)
    (gl:enable :scissor-test)
    (dolist (x *pre-trampoline-hooks*) (funcall x))
    (call-trampoline)))
(defparameter *pre-trampoline-hooks* nil)
