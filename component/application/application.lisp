(in-package :application)

(defparameter *thread* nil)
(defun main ()
  (when (or (eq nil *thread*)
	    (not (bordeaux-threads:thread-alive-p *thread*)))
    (setf *thread*
	  (bordeaux-threads:make-thread
	   (just-main)))))


(defparameter *arguments* '(window::*iresizable*
			    window::*iwidth*
			    window::*iheight*
			    window::*ititle*))
(defparameter *argument-values* (list nil 720 480
				      "app"))
(defun just-main ()
  (let ((stdo *standard-output*)
	(args *argument-values*))
    (lambda ()
      (progv (cons '*standard-output* *arguments*)
	  (cons stdo args)
	(window::wrapper #'init)))))

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
(defun init ()
  (glhelp:with-gl-context
    (setf %gl:*gl-get-proc-address* (window:get-proc-address))
    (setf window::*resize-hook* 'root-window-change)
    (dolist (item '(h w gl-context))
      (refresh item t))
    (window:set-vsync t)
    (gl:enable :scissor-test)
    (call-trampoline)))

(defparameter *trampoline* nil)
(defun call-trampoline ()
  (let ((value (cons "trampoline" "token")))
    (catch value
      (loop
	 (trampoline-bounce value *trampoline*)))))

(defparameter *control-state*
  window::*control-state*)
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

(deflazy al-context ()
  (music::really-start)
  music::*al-context*)
(getfnc 'al-context)
(defun restart-sound-system ()
  (music::restart-al)
  (refresh 'al-context t))
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
(defparameter *render-area* (make-instance 'render-area))

(defparameter *camera* (camera-matrix:make-camera
			:frustum-far (* 256.0)
			:frustum-near (/ 1.0 8.0)))
