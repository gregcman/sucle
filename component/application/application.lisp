(defpackage #:application
  (:use #:cl #:utility)

  (:export main)
  (:export *trampoline*)
  (:export bornfnc getfnc deflazy get-fresh)
  (:export microseconds tick *control-state* *camera* *render-area* *pre-trampoline-hooks*
	   set-render-area render-area-x render-area-y render-area-width render-area-height
	   %set-render-area))

(in-package :application)

(defparameter *thread* nil)
(defun main ()
  (when (or (eq nil *thread*)
	    (not (bordeaux-threads:thread-alive-p *thread*)))
    (setf *thread*
	  (bordeaux-threads:make-thread
	   (just-main)))))

(defun just-main ()
  (let ((stdo *standard-output*)
	(args *argument-values*))
    (lambda ()
      (progv (cons '*standard-output* *arguments*)
	  (cons stdo args)
	(window::wrapper #'init)))))

(defparameter *arguments* '(window::*iresizable*
			    window::*iwidth*
			    window::*iheight*
			    window::*ititle*))
(defparameter *argument-values* (list nil 720 480
				      "app"))

(defparameter *trampoline* nil)
(defun call-trampoline ()
  (let ((value (cons "trampoline" "token")))
    (catch value
      (loop
	 (trampoline-bounce value *trampoline*)))))

(defun trampoline-bounce (exit-token funs)
  (when window:*status*
    (throw exit-token (values)))
  (window:poll)
  (window::update-control-state *control-state*)
  (dolist (fun funs)
    (funcall fun exit-token))
  (window::update-control-state2 *control-state*)
  (window:update-display))

(defun quit ()
  (setf window:*status* t))

;;;;;
(defmacro deflazy (name (&rest deps) &rest gen-forms)
  `(eval-when (:load-toplevel :execute)
    (lazy-place::deflazy (gethash (quote ,name) *stuff*)
	,(mapcar (lambda (x)
		   (typecase x
		     (symbol `(,x (gethash (quote ,x) *stuff*)))
		     (otherwise
		      (destructuring-bind (name nick) x
			`(,nick (gethash (quote ,name) *stuff*))))))
		 deps)
      ,@gen-forms)))

(defvar *stuff* (make-hash-table :test 'eq))
(progn
  (defun getfnc (name)
    (lazy-place::fulfill
     (gethash name *stuff*)))
  (defun getfnc-no-update (name)
    (lazy-place::lazy-place-value
     (gethash name *stuff*)))
  (defun remove-stuff (k)
    (multiple-value-bind (value exists?) (gethash k *stuff*)
      (when exists?
	(lazy-place::destroy value)))))

(defun get-fresh (name)
  (reload-if-dirty name)
  (getfnc name))

(defun reload (name)
  (let ((place (gethash name *stuff*)))
    (if place
	(when (lazy-place::lazy-place-exists-p place)
	  (let ((a (getfnc-no-update name)))
	    (when (and (typep a 'glhelp::gl-object)
		       (glhelp:alive-p a))
	      (glhelp::gl-delete* a))
	    (remove-stuff name)))
	(format t "no place ~s" name))))

(defun dirty? (name)
  (lazy-place::dirty-p (gethash name *stuff*)))
(defun reload-if-dirty (name)
  (when (dirty? name)
    (reload name)))

(defun scrubgl2 ()
  (dohash (k v) *stuff*
    (when (typep (lazy-place::lazy-place-value v)
		 'glhelp::gl-object)
      (remove-stuff k))))

;;;;;;;;;;;;;;;;;;;;;
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
  #+sbcl
  (multiple-value-bind (s m) (sb-ext:get-time-of-day)
    (+ (* (expt 10 6) (- s 1506020000)) m))
  #+nil
  (* (%cl-glfw3::get-timer-value)
     (etouq (round (/ (expt 10 6) (%cl-glfw3::get-timer-frequency)))))
  #-sbcl
  (* (get-internal-real-time)
     (etouq (round (/ (expt 10 6) internal-time-units-per-second)))))
(defun tick (ticker fun &optional (time (microseconds)))
  (fps-independent-timestep:tick-update ticker time)
  (let ((times
	 (fps-independent-timestep:tick-physics ticker fun)))
    (values
     (coerce (* (fps-independent-timestep:ticker-accumulator ticker)
		(fps-independent-timestep:ticker-aux ticker))
	     'single-float)
     times)))


(defparameter *control-state*
  window::*control-state*
  #+nil
  (window::make-control-state
   :curr window::*input-state*))
(defparameter *camera* (camera-matrix:make-camera
			:frustum-far (* 256.0)
			:frustum-near (/ 1.0 8.0)))
(defparameter *render-area* (make-instance 'render-area))

(defun root-window-change (w h)
  (let ((value (getfnc 'h)))
    (unless (= value h)
      (reload 'h)))
  (let ((value (getfnc 'w)))
    (unless (= value w)
      (reload 'w))))

(defun init ()
  (glhelp:with-gl-context
    (setf %gl:*gl-get-proc-address* (window:get-proc-address))
    (setf window::*resize-hook* 'root-window-change)
    (progn
      (remove-stuff 'gl-context)
      (getfnc 'gl-context))
    (getfnc 'al-context)
    (remove-stuff 'h)
    (remove-stuff 'w)
    (scrubgl2)
    (window:set-vsync t)
    (gl:enable :scissor-test)
    (call-trampoline)))

(deflazy gl-context ()
  glhelp::*gl-context*)

(deflazy w ()
  window::*width*)
(deflazy h ()
  window::*height*)

(deflazy al-context ()
  (music::really-start)
  music::*al-context*)

(getfnc 'al-context)

(defun restart-sound-system ()
  (music::restart-al)
  (remove-stuff 'al-context))
