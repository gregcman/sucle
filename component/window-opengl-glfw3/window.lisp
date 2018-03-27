(in-package #:window)

(deftype mouse-keyboard-input-array ()
  `(simple-bit-vector 128))
(progn
  (declaim (ftype (function () mouse-keyboard-input-array)
		  make-mouse-keyboard-input-array))
  (defun make-mouse-keyboard-input-array ()
    (make-array 128 :element-type 'bit)))
(progn
  (declaim (type mouse-keyboard-input-array *input-state*))
  (defparameter *input-state* (make-mouse-keyboard-input-array)))

(defstruct control-state
  (prev (make-mouse-keyboard-input-array) :type mouse-keyboard-input-array)
  (curr (make-mouse-keyboard-input-array) :type mouse-keyboard-input-array)
  (diff (make-mouse-keyboard-input-array) :type mouse-keyboard-input-array)
  (jp (make-mouse-keyboard-input-array) :type mouse-keyboard-input-array)
  (jr (make-mouse-keyboard-input-array) :type mouse-keyboard-input-array))
(defparameter *control-state* (make-control-state :curr *input-state*))
(defun reset-control-state (state)
  (fill (control-state-curr state) 0)
  (fill (control-state-prev state) 0)
  (fill (control-state-diff state) 0)
  (fill (control-state-jp state) 0)
  (fill (control-state-jr state) 0))
(defun update-control-state (state)
  (bit-xor (control-state-curr state)
	   (control-state-prev state)
	   (control-state-diff state))
  (bit-and
   (control-state-diff state)
   (control-state-curr state)
   (control-state-jp state))
  (bit-andc2 
   (control-state-diff state)
   (control-state-curr state)
   (control-state-jr state)))
(defun update-control-state2 (state)
  (bit-ior (control-state-curr state)
	   (control-state-curr state)
	   (control-state-prev state)))

;;;glfw enums to positions
(eval-always
 (defparameter *mouse-array*
   (make-array 8 :element-type '(unsigned-byte 8)
	       :initial-contents '(11 12 14 15 16 33 34 35)))
 (defparameter *key-array*
   (make-array 349 :element-type '(unsigned-byte 8)
	       :initial-contents
	       '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 32 0 0 0 0 0
		 0 39 0 0 0 0 44 45 46 47 48 49 50 51 52 53 54 55 56 57 0 59 0 61 0 0 0 65 66
		 67 68 69 70 71 72 73 74 75 76 77 78 79 80 81 82 83 84 85 86 87 88 89 90 91 92
		 93 0 0 96 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
		 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 94 95 0 0 0 0 0 0
		 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
		 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
		 0 0 0 0 0 0 0 0 0 27 10 9 8 58 127 30 31 29 28 60 62 126 122 0 0 0 0 0 0 0 0
		 0 0 123 124 125 64 63 0 0 0 0 0 97 98 99 100 101 102 103 104 105 106 107 108
		 109 110 111 112 113 114 115 116 117 118 119 120 121 0 0 0 0 0 17 18 19 20 21
		 22 23 24 25 26 37 40 42 41 43 13 38 0 0 0 0 1 2 3 4 5 6 7 36)))
 (defparameter *character-keys*
   (let ((array (make-array 128 :element-type 'bit :initial-element 0)))
     (dotimes (i 97)
       (unless (zerop (aref *key-array* i))
	 (setf (sbit array i) 1)))
     array))
   ;;escape, delete, backspace, tab, return/enter? are ascii?
 (defparameter *back-map* 
   (let ((back-map (make-array 128)))
     (flet ((thing (array enum)
	      (dotimes (i (length array))
		(let ((value (cffi:foreign-enum-keyword enum i :errorp nil)))
		  (when value
		    (setf (aref back-map (aref array i))
			  (cons enum value)))))))
       (thing *mouse-array* (quote %glfw::mouse))
       (thing *key-array* (quote %glfw::key)))
     back-map)))


(defmacro do-character-keys ((array-form true-p index) &body body)
  (with-gensyms (array)   
    `(let ((,array ,array-form))
       (declare (type mouse-keyboard-input-array ,array))
       (dotimes (,index 128)
	 (when (character-key-p ,index)
	   (let ((,true-p (= 1 (sbit ,array ,index))))
	     ,@body))))))
(defmacro character-key-p (x)
  `(= 1
      (sbit (etouq *character-keys*) ,x)))

(defmacro mouseval (identifier)
  (etypecase identifier
    (keyword
     (aref *mouse-array*
	(cffi:foreign-enum-value (quote %glfw::mouse) identifier)))
    (integer
     (aref *mouse-array* (1- identifier)))))
(defmacro keyval (identifier)
  (etypecase identifier
    (keyword
     (aref *key-array*
	(cffi:foreign-enum-value (quote %glfw::key)
				 identifier)))
    (character
     (char-code (char-upcase identifier)))
    (integer
     (char-code (digit-char identifier)))))

(progn
  (defun skey-p (value &optional (state *control-state*))
    (= 1 (sbit (control-state-curr state) value)))
  (defun skey-j-p (value &optional (state *control-state*))
    (= 1 (sbit (control-state-jp state) value)))
  (defun skey-j-r (value &optional (state *control-state*))
    (= 1 (sbit (control-state-jr state) value))))

(defparameter *scroll-x* nil)
(defparameter *scroll-y* nil)
(defparameter *mouse-x* 0.0d0)
(defparameter *mouse-y* 0.0d0)
(defparameter *width* nil)
(defparameter *height* nil)

;;;
(defmacro def-key-callback (name (window key scancode action mod-keys) &body body)
  `(%glfw:define-glfw-callback ,name
       ((,window :pointer) (,key :int ;%glfw::key
				 ) (,scancode :int)
	(,action :int) (,mod-keys :unsigned-int))
     ,@body))
(def-key-callback key-callback (window key scancode action mod-keys)
  (unless (= key -1)
    (setf (sbit *input-state*
		(aref (etouq *key-array*) key))
	  (if (zerop action)
	      0
	      1))))
;;;
(defmacro def-mouse-button-callback (name (window button action mod-keys) &body body)
  `(%glfw:define-glfw-callback ,name
       ((,window :pointer) (,button :int
					;%glfw::mouse
				    )
	(,action :int) (,mod-keys :unsigned-int))
     ,@body))
(def-mouse-button-callback mouse-callback (window button action mod-keys)
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
(defun poll ()
  (setq *status* (glfw:window-should-close-p))
  (poll-events))
(defun get-proc-address ()
  (function glfw:get-proc-address))

(defparameter *iresizable* nil)
(defparameter *iwidth* 1)
(defparameter *iheight* 1)
(defparameter *ititle* "window")

;;Graphics calls on OS X must occur in the main thread
(defun wrapper (func)
  (glfw:with-init
    (window:init)
    (glfw:with-window (:title *ititle*
			      :width *iwidth*
			      :height *iheight*
			      :resizable *iresizable*)
      (glfw:set-mouse-button-callback 'mouse-callback)
      (glfw:set-key-callback 'key-callback)
      (glfw:set-scroll-callback 'scroll-callback)
      (glfw:set-window-size-callback 'update-viewport)
      (glfw:set-char-callback 'char-callback)
      
      (glfw:set-cursor-position-callback 'cursor-callback)
      (setf *width* *iwidth*
	    *height* *iheight*)
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

(cffi:defcstruct |GLFWStruct|
  (width :int)
  (height :int)
  (pixels :pointer))

(cffi:defcfun ("glfwSetCursor" %glfw::set-cursor) :void
  (window :pointer)
  (cursor :pointer))

(cffi:defcfun ("glfwGetTimerFrequency" %glfw::get-timer-frequency) :uint64)
(cffi:defcfun ("glfwGetTimerValue" %glfw::get-timer-value) :uint64)

