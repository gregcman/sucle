(in-package :cl-glfw)

(defcfun ("glfwSetWindowCloseCallback" %set-window-close-callback) :void
  (callback :pointer))
(defun set-window-close-callback (callback-name)
  (%set-window-close-callback (cffi:get-callback callback-name)))
(defmacro def-window-close-callback (name () &body body)
  `(defcallback ,name :int
       ()
       ,@body))

(defcfun ("glfwSetWindowSizeCallback" %set-window-size-callback) :void
  (callback :pointer))
(defun set-window-size-callback (callback-name)
  (%set-window-size-callback (cffi:get-callback callback-name)))
(defmacro def-window-size-callback (name (width height) &body body)
  `(defcallback ,name :void
       ((,width :int) (,height :int))
     ,@body))

(defcfun ("glfwSetWindowRefreshCallback" %set-window-refresh-callback) :void
  (callback :pointer))
(defun set-window-refresh-callback (callback-name)
  (%set-window-refresh-callback (cffi:get-callback callback-name)))
(defmacro def-window-refresh-callback (name () &body body)
  `(defcallback ,name :void
       ()
       ,@body))

(defcfun ("glfwSetKeyCallback" %set-key-callback) :void
  (callback :pointer))
(defun set-key-callback (callback-name)
  (%set-key-callback (cffi:get-callback callback-name)))
(defmacro def-set-key-callback (name (key action) &body body)
  `(defcallback ,name :void
       ((,key key) (,action key-action))
       ,@body))

(defcfun ("glfwSetCharCallback" %set-char-callback) :void
  (callback :pointer))
(defun set-char-callback (callback-name)
  (%set-char-callback (cffi:get-callback callback-name)))
(defmacro def-char-callback (name (character action) &body body)
  `(defcallback ,name :void
       ((,character :int) (,action action))
       ,@body))

(defcfun ("glfwSetMouseButtonCallback" %set-mouse-button-callback) :void
  (callback :pointer))
(defun set-mouse-button-callback (callback-name)
  (%set-mouse-button-callback (cffi:get-callback callback-name)))
(defmacro def-mouse-button-callback (name (button action) &body body)
  `(defcallback ,name :void
       ((,button mouse) (,action action))
       ,@body))

(defcfun ("glfwSetMousePosCallback" %set-mouse-pos-callback) :void
  (callback :pointer))
(defun set-mouse-pos-callback (callback-name)
  (%set-mouse-pos-callback (cffi:get-callback callback-name)))
(defmacro def-mouse-pos-callback (name (x y) &body body)
  `(defcallback ,name :void
       ((,x :int) (,y :int))
       ,@body))

(defcfun ("glfwSetMouseWheelCallback" %set-mouse-wheel-callback) :void
  (callback :pointer))
(defun set-mouse-wheel-callback (callback-name)
  (%set-mouse-wheel-callback (cffi:get-callback callback-name)))
(defmacro def-mouse-wheel-callback (name (pos) &body body)
  `(defcallback ,name :void
       ((,pos :int))
       ,@body))
