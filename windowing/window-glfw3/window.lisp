(in-package #:window)

(deftype mouse-keyboard-input-array ()
  `(simple-bit-vector 128))

(progn
  (declaim (ftype (function () mouse-keyboard-input-array)
		  make-mouse-keyboard-input-array))
  (defun make-mouse-keyboard-input-array ()
    (make-array 128 :element-type 'bit)))

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

(declaim (type mouse-keyboard-input-array *input-state* *input-prev* *difference-state*))
(defparameter *input-state* (make-mouse-keyboard-input-array))
(defparameter *input-prev* (make-mouse-keyboard-input-array))

(defparameter *difference-state* (make-mouse-keyboard-input-array))



(defparameter *scroll-x* nil)
(defparameter *scroll-y* nil)

(defparameter *width* nil)
(defparameter *height* nil)

(progn
  (defmacro def-key-callback (name (window key scancode action mod-keys) &body body)
    `(%glfw:define-glfw-callback ,name
	 ((,window :pointer) (,key %glfw::key) (,scancode :int)
	  (,action :int) (,mod-keys :unsigned-int))
       ,@body))

  (defmacro def-mouse-button-callback (name (window button action mod-keys) &body body)
    `(%glfw:define-glfw-callback ,name
	 ((,window :pointer) (,button %glfw::mouse)
	  (,action :int) (,mod-keys :unsigned-int))
       ,@body))
  (defmacro def-char-callback (name (window char) &body body)
    `(%glfw:define-glfw-callback ,name
	 ((,window :pointer) (,char :unsigned-int))
       ,@body)))

;;;;various functions to test the state of the keyboard

;;;"-p" stands for "press" or predicate
;;;"-r" stands for "repeat"
;;;"-j" stands for "just" as in "it was not pressed just a moment ago,
;;;now it is"

;;;for the keyboard

(defun get-press-value (value)
  (logand value +key-state-mask+))
(defun get-mod-value (value)
  (logand value +mod-state-mask+))

(defun key-p (the-key)
  (let ((value (key-enum-index the-key)))     
    (not (zerop (aref *input-state* value)))))
(defun key-j-p (the-key)
  (let ((value (key-enum-index the-key)))     
    (and (not (zerop (aref *input-state* value)))
	 (not (zerop (aref *difference-state* value))))))
(defun key-j-r (the-key)
  (let ((value (key-enum-index the-key)))     
    (and (zerop (aref *input-state* value))
	 (not (zerop (aref *difference-state* value))))))

;;;for mice

(defun mice-p (the-key)
  (let ((value (mouse-enum-index the-key)))     
    (not (zerop (aref *input-state* value)))))
(defun mice-j-p (the-key)
  (let ((value (mouse-enum-index the-key)))     
    (and (not (zerop (aref *input-state* value)))
	 (not (zerop (aref *difference-state* value))))))
(defun mice-j-r (the-key)
  (let ((value (mouse-enum-index the-key)))     
    (and (zerop (aref *input-state* value))
	 (not (zerop (aref *difference-state* value))))))


(defun mouse-enum-index (enum)
  (aref *mouse-array*
	(cffi:foreign-enum-value (quote %glfw::mouse) enum)))

(defun key-enum-index (enum)
  (aref *key-array*
	(cffi:foreign-enum-value (quote %glfw::key)
				 enum)))

;;;glfw callbacks which will update the hashes to contain nil t
;;;+press+ or +release+ per key [each key is a symbol]

(def-key-callback key-callback (window key scancode action mod-keys)
  (setf (sbit *input-state*
	      (key-enum-index key))
	(if (zerop action)
	    0
	    1)))
(def-mouse-button-callback mouse-callback (window button action mod-keys)
  (setf (sbit *input-state*
	      (mouse-enum-index button))
	(if (zerop action)
	    0
	    1)))
(def-char-callback char-callback (window char))

(defun make-fill-vector ()
  (make-array 0
	      :adjustable t
	      :fill-pointer 0))

(defparameter *buttons* (make-fill-vector))
(defparameter *keys* (make-fill-vector))


(glfw:def-scroll-callback scroll-callback (window x y)
  (declare (ignore window))
  (setf *scroll-x* (coerce x 'single-float)
	*scroll-y* (coerce y 'single-float)))
(defparameter *status* nil)
(glfw:def-window-size-callback update-viewport (window w h)
  (declare (ignorable window))
  (setf *width* w *height* h)
  (funcall *resize-hook* w h))

(defparameter *resize-hook* (constantly nil))

(defun init ()
  (fill *input-state* 0)
  (fill *input-prev* 0)
  (fill *difference-state* 0)
  (setf *scroll-x* 0.0
	*scroll-y* 0.0)

  (setq *status* nil)
  #+sbcl (sb-int:set-floating-point-modes :traps nil))

(defun poll ()
  (setf *scroll-x* 0.0
	*scroll-y* 0.0)
  (setq *status* (glfw:window-should-close-p))

  (glfw:poll-events)
  (bit-xor *input-state* *input-prev* *difference-state*)
  (bit-ior *input-state* *input-state* *input-prev*))


(defun get-proc-address ()
  (function glfw:get-proc-address))

;;Graphics calls on OS X must occur in the main thread

(defparameter *iresizable* nil)
(defparameter *iwidth* 1)
(defparameter *iheight* 1)
(defparameter *ititle* "Common Lisp")

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
  (cffi:with-foreign-objects ((x :int) (y :int))
    (cffi:foreign-funcall "glfwGetCursorPos"
			  %glfw::window window :pointer x :pointer y :void)
    (values (coerce (cffi:mem-ref x :double) 'single-float)
	    (coerce (cffi:mem-ref y :double) 'single-float))))

(cffi:defcstruct |GLFWStruct|
  (width :int)
  (height :int)
  (pixels :pointer))

(cffi:defcfun "glfwSetCursor" :void
  (window :pointer)
  (cursor :pointer))

