(in-package #:window)

(defparameter *scroll-x* nil)
(defparameter *scroll-y* nil)
(defparameter *mouse-x* 0.0d0)
(defparameter *mouse-y* 0.0d0)
(defparameter *width* nil)
(defparameter *height* nil)
(defparameter *mod-keys* 0)

;;;
(defmacro def-key-callback (name (window key scancode action mod-keys) &body body)
  `(%glfw:define-glfw-callback ,name
       ((,window :pointer) (,key :int ;%glfw::key
				 ) (,scancode :int)
	(,action :int) (,mod-keys :unsigned-int))
     ,@body))
(def-key-callback key-callback (window key scancode action mod-keys)
  (declare (ignorable window scancode))
  (setf *mod-keys* mod-keys)
  (unless (= key -1)
    (let ((location (aref (etouq *key-array*) key)))
      (when (= action 2)
	(setf (sbit *repeat-state* location) 1))
      (setf (sbit *input-state*
		  location)
	    (if (zerop action)
		0
		1)))))
;;;
(defmacro def-mouse-button-callback (name (window button action mod-keys) &body body)
  `(%glfw:define-glfw-callback ,name
       ((,window :pointer) (,button :int
					;%glfw::mouse
				    )
	(,action :int) (,mod-keys :unsigned-int))
     ,@body))
(def-mouse-button-callback mouse-callback (window button action mod-keys)
  (declare (ignorable window))
  (setf *mod-keys* mod-keys)
  (setf (sbit *input-state*
	      (aref (etouq *mouse-array*) button))
	(if (zerop action)
	    0
	    1)))
;;;
(defmacro def-char-callback (name (window char) &body body)
  `(%glfw:define-glfw-callback ,name
       ((,window :pointer) (,char :unsigned-int))
     ,@body))
(def-char-callback char-callback (window char)
  (declare (ignorable window)))
;;;
(defmacro def-cursor-callback (name (window x y) &body body)
    `(%glfw:define-glfw-callback ,name
	 ((,window :pointer) (,x :double) (,y :double))
       ,@body))
(def-cursor-callback cursor-callback (window x y)
  (declare (ignorable window))
  (setf *mouse-x* x)
  (setf *mouse-y* y))
;;;
(glfw:def-scroll-callback scroll-callback (window x y)
  (declare (ignore window))
  (incf *scroll-x* x)
  (incf *scroll-y* y))
;;;
(defparameter *resize-hook* (constantly nil))
(glfw:def-window-size-callback update-viewport (window w h)
  (declare (ignorable window))
  (setf *width* w *height* h)
  (funcall *resize-hook* w h))

(defparameter *status* nil)
(defun init ()
  (reset-control-state *control-state*)
  (setf *scroll-x* 0.0
	*scroll-y* 0.0)
  (setq *status* nil)
  #+sbcl (sb-int:set-floating-point-modes :traps nil))

(defun poll-events ()
  (glfw:poll-events))
(defparameter *shift* nil)
(defparameter *control* nil)
(defparameter *alt* nil)
(defparameter *super* nil)
(defun poll ()
  (setq *status* (glfw:window-should-close-p))
  (poll-events)

  (let ((mods *mod-keys*))
    (setf *shift* (logtest +shift+ mods)
	  *control* (logtest +control+ mods)
	  *alt* (logtest +alt+ mods)
	  *super* (logtest +super+ mods))))
(defun get-proc-address ()
  (function glfw:get-proc-address))


(defmacro with-window (window-keys &body body)
  `(unwind-protect
	(progn
	  (apply #'glfw:create-window ,window-keys)
	  ,@body)
     (glfw:destroy-window)))

;;Graphics calls on OS X must occur in the main thread
(defun wrapper (func &optional (params
				'(:title "window"
				  :width 1
				  :height 1
				  :resizable nil)))
  (glfw:with-init
    (window:init)
    (with-window params
      (glfw:set-mouse-button-callback 'mouse-callback)
      (glfw:set-key-callback 'key-callback)
      (glfw:set-scroll-callback 'scroll-callback)
      (glfw:set-window-size-callback 'update-viewport)
      (glfw:set-char-callback 'char-callback)
      
      (glfw:set-cursor-position-callback 'cursor-callback)
      
      (setf (values *width*
		    *height*)
	    (get-window-size))
      (funcall func))))

(defun get-mouse-out ()
  (glfw:set-input-mode :cursor :normal))

(defun toggle-mouse-capture ()
  (if (mice-locked-p)
      (glfw:set-input-mode :cursor :normal)
      (glfw:set-input-mode :cursor :disabled)))

(defun mice-locked-p ()
  (eq :disabled (glfw:get-input-mode :cursor)))

(defun mice-free-p ()
  (eq :normal (glfw:get-input-mode :cursor)))

(defun push-dimensions (width height)
  (setf *width* width
	*height* height)
  (glfw:set-window-size width height))

(defun set-caption (caption)
  (glfw:set-window-title caption))

(defun update-display ()
  (glfw:swap-buffers))

(defun set-vsync (bool)
  (if bool
      (glfw:swap-interval 1) ;;1 is on
      (glfw:swap-interval 0))) ;;0 is off

(defun get-window-size (&optional (window glfw:*window*))
  (cffi:with-foreign-objects ((w :int)
			      (h :int))
    (cffi:foreign-funcall "glfwGetWindowSize"
			  %glfw::window window
			  :pointer w
			  :pointer h :void)
    (values (cffi:mem-ref w :int)
	    (cffi:mem-ref h :int))))

(defun get-mouse-position (&optional (window glfw:*window*)) 
  (cffi:with-foreign-objects ((x :double) (y :double))
    (get-cursor-position window x y)
    (values 
     (cffi:mem-ref x :double)
     (cffi:mem-ref y :double))))
(cffi:defcfun ("glfwGetCursorPos" get-cursor-position)
    :void
  (window %glfw::window)
  (x :pointer)
  (y :pointer))

#+nil
(cffi:defcstruct |GLFWStruct|
  (width :int)
  (height :int)
  (pixels :pointer))
#+nil
(cffi:defcfun ("glfwSetCursor" %glfw::set-cursor) :void
  (window :pointer)
  (cursor :pointer))

#+nil
(cffi:defcfun ("glfwGetTimerFrequency" %glfw::get-timer-frequency) :uint64)
#+nil
(cffi:defcfun ("glfwGetTimerValue" %glfw::get-timer-value) :uint64)

