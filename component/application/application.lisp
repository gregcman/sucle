(defpackage #:application
  (:use #:cl #:utility)

  (:export main)
  (:export *trampoline*)
  (:export getfnc deflazy)
  (:export w h gl-context al-context)
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
  (flush-refreshes)
  (dolist (fun funs)
    (funcall fun exit-token))
  (window::update-control-state2 *control-state*)
  (window:update-display))

(defun quit ()
  (setf window:*status* t))

;;;;;TODO: clean this area up with dependency graph 
(defvar *stuff* (make-hash-table :test 'eq))
(defmacro deflazy (name (&rest deps) &rest gen-forms)
  `(eval-when (:load-toplevel :execute)
     (let ((dependency-graph::*namespace* *stuff*))
       (refresh-new-node ',name)
       ,(multiple-value-bind
	 (fun node-deps) (dependency-graph::%defnode deps gen-forms)
	 `(dependency-graph::redefine-node ,fun ',node-deps ',name)))))

;;;;queue node to be unloaded if it already has stuff in it 
(defun refresh-new-node (name)
  (let ((node (dependency-graph::ensure-node name *stuff*)))
    (unless (= 0 (dependency-graph::timestamp node))
      (refresh name))))

(defparameter *refresh* (make-hash-table :test 'eq))
(defparameter *refresh-lock* (bordeaux-threads:make-recursive-lock "refresh"))
(defun refresh (name &optional (main-thread nil))
  (if main-thread
      (%refresh name)
      (bordeaux-threads:with-recursive-lock-held (*refresh-lock*)
	(setf (gethash name *refresh*) t))))
(defun flush-refreshes ()
  (bordeaux-threads:with-recursive-lock-held (*refresh-lock*)
    (let ((length (hash-table-count *refresh*)))
      (unless (zerop length)
	(dohash (name value) *refresh*
		(declare (ignore value))
		(%refresh name))
	(clrhash *refresh*)))))

(defun %refresh (name)
  (let ((node (dependency-graph::get-node name *stuff*)))
    (when node
      (dependency-graph::touch-node node)
      (clean-and-invalidate-node node)
      (dependency-graph::map-dependents2
       name
       #'clean-and-invalidate-node
       #'dependency-graph::dirty-p
       *stuff*))))

(defun getfnc (name)
  (dependency-graph::get-value name *stuff*))

(defgeneric cleanup-node-value (object))
(defmethod cleanup-node-value ((object t))
  (declare (ignorable object)))
(defmethod cleanup-node-value ((object glhelp::gl-object))
  (when (glhelp:alive-p object)
    (glhelp::gl-delete* object)))
(defun cleanup-node (node)
  (let ((value (dependency-graph::value node)))
    (cleanup-node-value value)))

(defun clean-and-invalidate-node (node)
  (when (dependency-graph::state node)
    (cleanup-node node))
  (dependency-graph::%invalidate-node node))
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
  (unless (= (getfnc 'h) h)
    (refresh 'h t))
  (unless (= (getfnc 'w) w)
    (refresh 'w t)))

(defun init ()
  (glhelp:with-gl-context
    (setf %gl:*gl-get-proc-address* (window:get-proc-address))
    (setf window::*resize-hook* 'root-window-change)
    (dolist (item '(h w gl-context))
      (refresh item t))
    (window:set-vsync t)
    (gl:enable :scissor-test)
    (call-trampoline)))

(deflazy gl-context ()
  (unless glhelp::*gl-context*
    (error "no opengl context you idiot!")))

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
  (refresh 'al-context t))
