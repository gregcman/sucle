(in-package #:window)

(defparameter *scroll-x* 0)
(defparameter *scroll-y* 0)
(defparameter *mouse-x* 0.0d0)
(defparameter *mouse-y* 0.0d0)
(defparameter *width* nil)
(defparameter *height* nil)
(defparameter *mod-keys* 0)

;;;; ## Float trap masking for OS X

;; Floating points traps need to be masked around certain
;; foreign calls on sbcl/darwin. Some private part of Cocoa
;; (Apple's GUI Framework) generates a SIGFPE that
;; invokes SBCLs signal handler if they're not masked.
;;
;; Traps also need to be restored during lisp callback execution
;; because SBCL relies on them to check division by zero, etc.
;; This logic is encapsulated in DEFINE-GLFW-CALLBACK.
;;
;; It might become necessary to do this for other implementations, too.

(defparameter *saved-lisp-fpu-modes* :unset)

(defmacro with-float-traps-saved-and-masked (&body body)
  "Turn off floating point traps and stash them
during execution of the given BODY. Expands into a PROGN if
this is not required for the current implementation."
  #+(and sbcl darwin)
    `(let ((*saved-lisp-fpu-modes* (sb-int:get-floating-point-modes)))
       (sb-int:with-float-traps-masked (:inexact :invalid
                                        :divide-by-zero :overflow
                                        :underflow)
         ,@body))
  #-(and sbcl darwin)
    `(progn ,@body))

(defmacro with-float-traps-restored (&body body)
  "Temporarily restore the saved float traps during execution
of the given BODY. Expands into a PROGN if this is not required
for the current implementation."
  #+(and sbcl darwin)
      (with-gensyms (modes)
        `(let ((,modes (sb-int:get-floating-point-modes)))
           (unwind-protect
                (progn
                  (when (not (eq *saved-lisp-fpu-modes* :unset))
                    (apply #'sb-int:set-floating-point-modes
                           *saved-lisp-fpu-modes*))
                  ,@body)
             (apply #'sb-int:set-floating-point-modes ,modes))))
  #-(and sbcl darwin)
  `(progn ,@body))

;;;
(glfw:define-key-callback key-callback (window key scancode action mod-keys)
  (declare (ignorable window scancode))
  (with-float-traps-restored
    (setf *mod-keys* mod-keys)
    (unless (= key -1)
      (let ((location (aref (etouq *key-array*) key)))
	(when (= action 2)
	  (setf (sbit *repeat-state* location) 1))
	(setf (sbit *input-state*
		    location)
	      (if (zerop action)
		  0
		  1))))))

(glfw:define-mouse-button-callback mouse-callback (window button action mod-keys)
  (declare (ignorable window))
  (with-float-traps-restored
    (setf *mod-keys* mod-keys)
    (setf (sbit *input-state*
		(aref (etouq *mouse-array*) button))
	  (if (zerop action)
	      0
	      1))))
;;;

(glfw:define-char-callback char-callback (window char)
  (declare (ignorable window char))
  (with-float-traps-restored)
  )
;;;
(glfw:define-cursor-pos-callback cursor-callback (window x y)
  (declare (ignorable window))
  (with-float-traps-restored
    (setf *mouse-x* x)
    (setf *mouse-y* y)))
;;;
(glfw:define-scroll-callback scroll-callback (window x y)
  (declare (ignore window))
  (with-float-traps-restored
    (incf *scroll-x* x)
    (incf *scroll-y* y)))
;;;
(defparameter *resize-hook* (constantly nil))
(glfw:define-framebuffer-size-callback update-viewport (window w h)
  (declare (ignorable window))
  (with-float-traps-restored
    (setf *width* w *height* h)
    (funcall *resize-hook* w h)))

(defparameter *status* nil)
(defun init ()
  (reset-control-state *control-state*)
  (setf *scroll-x* 0.0
	*scroll-y* 0.0)
  (setq *status* nil)
  #+sbcl (sb-int:set-floating-point-modes :traps nil))

(defun poll-events ()
  (%glfw:poll-events))
(defparameter *shift* nil)
(defparameter *control* nil)
(defparameter *alt* nil)
(defparameter *super* nil)
(defun poll ()
  (setq *status* (let ((value (%glfw:window-should-close *window*)))
		   (cond ((eql value %glfw:+true+) t)
			 ((eql value %glfw:+false+) nil)
			 (t (error "what is this value? ~a" value)))))
  (poll-events)

  (let ((mods *mod-keys*))
    (setf *shift* (logtest +shift+ mods)
	  *control* (logtest +control+ mods)
	  *alt* (logtest +alt+ mods)
	  *super* (logtest +super+ mods))))

(defmacro with-window (window-keys &body body)
  (alexandria:with-gensyms (window)
   `(let ((,window (funcall #'%glfw:create-window
			   (getf 
			    ,window-keys
			    :width )
			   (getf 
			    ,window-keys
			    :height )
			   (getf 
			    ,window-keys
			    :title )
			   (getf 
			    ,window-keys
			    :monitor )
			   (getf 
			    ,window-keys
			    :shared ))))
      (unwind-protect
	   (progn
	     (let ((*window* ,window))
	       (%glfw:make-context-current ,window)
	       ,@body))
	(%glfw:destroy-window ,window)))))

(defparameter *window* nil)
;;Graphics calls on OS X must occur in the main thread
(defun set-callbacks (&optional (window *window*))
  (init)
  (%glfw:set-mouse-button-callback window
				   (cffi:get-callback 'mouse-callback))
  (%glfw:set-key-callback window (cffi:get-callback 'key-callback))
  (%glfw:set-scroll-callback window (cffi:get-callback 'scroll-callback))
  (%glfw:set-window-size-callback window (cffi:get-callback 'update-viewport))
  (%glfw:set-char-callback window (cffi:get-callback 'char-callback)) 
  (%glfw:set-cursor-pos-callback window (cffi:get-callback 'cursor-callback))
  )
(defun wrapper (func &optional (params
				'(:title "window"
				  :width 1
				  :height 1
				  :resizable nil)))
  (with-float-traps-saved-and-masked
    (glfw:with-init ()
      (init)
      (glfw:with-window-hints
	    ;;FIXME::better interface?
	    ((%glfw:+resizable+ (if (getf params :resizable)
				    %glfw:+true+
				    %glfw:+false+)))
	(with-window params
	  (set-callbacks)
	  (setf (values *width*
			*height*)
		(get-window-size))
	  (funcall func))))))

(defun get-mouse-out ()
  (%glfw:set-input-mode *window*  %glfw:+cursor+ %glfw:+cursor-normal+))

(defun toggle-mouse-capture ()
  (if (mice-locked-p)
      (%glfw:set-input-mode *window* %glfw:+cursor+ %glfw:+cursor-normal+)
      (%glfw:set-input-mode *window* %glfw:+cursor+ %glfw:+cursor-disabled+) ))

(defun mice-locked-p ()
  (eq %glfw:+cursor-disabled+
      (%glfw:get-input-mode *window* %glfw:+cursor+)))

(defun mice-free-p ()
  (eq  %glfw:+cursor-normal+
      (%glfw:get-input-mode *window* %glfw:+cursor+)))

(defun push-dimensions (width height)
  (setf *width* width
	*height* height)
  (%glfw:set-window-size *window* width height))

(defun set-caption (caption)
  (%glfw:set-window-title *window* caption))

(defun update-display ()
  (%glfw:swap-buffers *window*))

(defun set-vsync (bool)
  (if bool
      (%glfw:swap-interval 1) ;;1 is on
      (%glfw:swap-interval 0))) ;;0 is off

(defun get-window-size (&optional (window *window*))
  (cffi:with-foreign-objects ((w :int)
			      (h :int))
    (%glfw:get-window-size window w h)
    (values (cffi:mem-ref w :int)
	    (cffi:mem-ref h :int))))

(defun get-mouse-position (&optional (window *window*)) 
  (cffi:with-foreign-objects ((x :double) (y :double))
    (%glfw:get-cursor-pos window x y)
    (values 
     (cffi:mem-ref x :double)
     (cffi:mem-ref y :double))))

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

