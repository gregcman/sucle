(in-package :application)

(defparameter *thread* nil)
(defun main (fun &rest rest)
  (when (or (eq nil *thread*)
	    (not (bordeaux-threads:thread-alive-p *thread*)))
    (setf
     *thread*
     (bordeaux-threads:make-thread
      (apply #'just-main fun rest)))))

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
     `(defun just-main (fun &rest rest
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
	      (initfun (init fun)))
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
    (glhelp:with-gl-context
      (setf %gl:*gl-get-proc-address* (window:get-proc-address))
      (setf window::*resize-hook* 'root-window-change)
      (dolist (item '(h w gl-context))
	(refresh item t))
      (window:set-vsync t)
      (gl:enable :scissor-test)
      (let ((*quit-token* (cons "trampoline" "token")))
	(catch *quit-token*
	  (funcall fun))))))

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
