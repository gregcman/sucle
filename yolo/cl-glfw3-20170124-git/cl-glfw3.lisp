;;;; cl-glfw3.lisp

;;;; # GLFW functions
;;;; Access almost all of the GLFW bindings without the need to use CFFI or %GLFW. Also included are helper functions. Functions are named directly after their GLFW counterparts. GLFW enums are replaced with keywords.

;;;; Much of the convenience in this library comes from the presence of a default window value, *WINDOW*. *WINDOW* can be set by MAKE-CONTEXT-CURRENT, or by CREATE-WINDOW.

;;;; WITH- macros (WITH-INIT, WITH-WINDOW WITH-INIT-WINDOW) are provided for convenience (and inspired by cl-glfw). WITH-CONTEXT, is another convenience function (although not present in cl-glfw).

;;;; Callback creation macros are also provided. These macros ask for the name of the callback to be created, a list of symbols which correspond to the arguments of the callback and the body. Callback setter functions in this package require the (quoted) name of the callback.

;;;; Full documentation for GLFW3 can be found at http://www.glfw.org/docs/3.0/index.html

(in-package #:glfw)

(export
 '(def-error-callback
   set-error-callback
   initialize
   with-init
   def-monitor-callback
   *window*
   create-window
   destroy-window
   with-window
   with-init-window
   window-should-close-p
   set-window-should-close
   set-window-title
   get-window-position
   set-window-position
   get-window-size
   set-window-size
   set-window-monitor
   get-framebuffer-size
   iconify-window
   restore-window
   show-window
   hide-window
   get-window-monitor
   get-window-attribute
   get-context-version
   def-window-position-callback
   def-window-size-callback
   def-window-close-callback
   def-window-refresh-callback
   def-window-focus-callback
   def-window-iconify-callback
   def-framebuffer-size-callback
   set-window-position-callback
   set-window-size-callback
   set-window-close-callback
   set-window-refresh-callback
   set-window-focus-callback
   set-window-iconify-callback
   set-framebuffer-size-callback
   get-input-mode
   set-input-mode
   get-key
   get-mouse-button
   get-cursor-position
   set-cursor-position
   def-key-callback
   def-char-callback
   def-mouse-button-callback
   def-cursor-pos-callback
   def-cursor-enter-callback
   def-scroll-callback
   set-key-callback
   set-char-callback
   set-mouse-button-callback
   set-cursor-position-callback
   set-cursor-enter-callback
   set-scroll-callback
   set-clipboard-string
   get-clipboard-string
   make-context-current
   get-current-context
   with-context
   swap-buffers))

(multiple-value-bind (major minor rev) (%glfw:get-version)
  (when (/= major 3)
    (error "Local GLFW is ~a.~a.~a, should be above 3.x" major minor rev)))

;;;; ## Window and monitor functions
(defmacro import-export (&rest symbols)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (import ',symbols)
     (export ',symbols)))

(defmacro def-error-callback (name (message) &body body)
  (let ((error-code (gensym "error-code")))
    `(%glfw:define-glfw-callback ,name
	 ((,error-code :int) (,message :string))
       (declare (ignore ,error-code))
       ,@body)))

(def-error-callback default-error-fun (message)
  (error message))

(defun set-error-callback (callback-name)
  (%glfw:set-error-callback (cffi:get-callback callback-name)))

(defun initialize ()
  "Start GLFW"
  (let ((result (%glfw:init)))
    (unless result
      (error "Error initializing glfw."))
    result))

(defmacro with-init (&body body)
  "Wrap BODY with an initialized GLFW instance, ensuring proper termination. If no error callback is set when this is called, a default error callback is set."
  `(progn
     (let ((prev-error-fun (set-error-callback 'default-error-fun)))
       (unless (cffi:null-pointer-p prev-error-fun)
	 (%glfw:set-error-callback prev-error-fun)))
     (initialize)
     (unwind-protect (progn ,@body)
       (%glfw:terminate))))

(import-export %glfw:get-monitors %glfw:get-primary-monitor %glfw:get-monitor-physical-size %glfw:get-monitor-name %glfw:set-monitor-callback %glfw:get-video-modes %glfw:get-video-mode %glfw:set-gamma %glfw:get-gamma-ramp %glfw:set-gamma-ramp %glfw:terminate)

(defmacro def-monitor-callback (name (monitor event) &body body)
  `(%glfw:define-glfw-callback ,name
       ((,monitor :pointer) (,event %glfw::monitor-event))
     ,@body))

(defvar *window* nil
  "The window that is currently the default for this library. Can be set through MAKE-CONTEXT-CURRENT.")

(defun create-window (&key
			(width 0) (height 0)
			title
			(monitor (cffi:null-pointer))
			(shared (cffi:null-pointer))
			;; Hints
			(resizable t)
			(visible t)
			(decorated t)
			(red-bits 8) (green-bits 8) (blue-bits 8) (alpha-bits 8)
			(depth-bits 24) (stencil-bits 8)
			(accum-red-bits 0) (accum-green-bits 0) (accum-blue-bits 0)
			(accum-alpha-bits 0)
			(aux-buffers 0)
			(samples 0)
			(refresh-rate 0)
			(stereo nil)
			(srgb-capable nil)
			(client-api :opengl-api)
			(context-version-major 1)
			(context-version-minor 0)
			(context-robustness :no-robustness)
			(opengl-forward-compat nil)
			(opengl-debug-context nil)
			(opengl-profile :opengl-any-profile))
  "This function handles all window hints.

MONITOR: The monitor on which the window should be full-screen.
SHARED: The window whose context to share resources with."
  (macrolet ((output-hints (&rest hints)
	       `(progn
		  ,@(loop for (name type) in hints collect
			 `(%glfw:window-hint
			   ,(intern (string-upcase
				    (symbol-name name)) :keyword)
			   (cffi:convert-to-foreign ,name ,type))))))
    (output-hints
     (resizable :boolean)
     (visible :boolean)
     (decorated :boolean)
     (red-bits :int) (green-bits :int) (blue-bits :int) (alpha-bits :int)
     (depth-bits :int) (stencil-bits :int)
     (accum-red-bits :int) (accum-green-bits :int) (accum-blue-bits :int)
     (accum-alpha-bits :int)
     (aux-buffers :int)
     (samples :int)
     (refresh-rate :int)
     (stereo :boolean)
     (srgb-capable :boolean)
     (client-api '%glfw::opengl-api)
     (context-version-major :int)
     (context-version-minor :int)
     (context-robustness '%glfw::robustness)
     (opengl-forward-compat :boolean)
     (opengl-debug-context :boolean)
     (opengl-profile '%glfw::opengl-profile)))
  (let ((window (%glfw:create-window width height title monitor shared)))
    (if (cffi:null-pointer-p window)
	(error "Error creating window.")
	(make-context-current window))))

(defun destroy-window (&optional (window *window*))
  (when window (%glfw:destroy-window window))
  (when (eq window *window*)
    (setf *window* nil)))

(defmacro with-window ((&rest window-keys) &body body)
  "Convenience macro for using windows."
  `(unwind-protect
	(progn
	  (create-window ,@window-keys)
	  ,@body)
     (destroy-window)))

(defmacro with-init-window ((&rest window-keys) &body body)
  "Convenience macro for setting up GLFW and opening a window."
  `(with-init
     (with-window ,window-keys ,@body)))

(defun window-should-close-p (&optional (window *window*))
  (%glfw:window-should-close-p window))

(defun set-window-should-close (&optional (window *window*) (closep t))
  (%glfw:set-window-should-close window closep))

(defun set-window-title (title &optional (window *window*))
  (%glfw:set-window-title window title))

(defun get-window-position (&optional (window *window*))
  (%glfw:get-window-position window))

(defun set-window-position (x y &optional (window *window*))
  (%glfw:set-window-position window x y))

(defun get-window-size (&optional (window *window*))
  (%glfw:get-window-size window))

(defun set-window-size (w h &optional (window *window*))
  (%glfw:set-window-size window w h))

(defun get-framebuffer-size (&optional (window *window*))
  (%glfw:get-framebuffer-size window))

(defun set-window-monitor (monitor width height &key (window *window*)
                                                  (x-position 0) (y-position 0)
                                                  (refresh-rate %glfw:+dont-care+))
  (let ((monitor (if (null monitor) (cffi:null-pointer) monitor)))
    (%glfw:set-window-monitor window monitor x-position y-position width height refresh-rate)))

(defun iconify-window (&optional (window *window*))
  (%glfw:iconify-window window))

(defun restore-window (&optional (window *window*))
  (%glfw:restore-window window))

(defun show-window (&optional (window *window*))
  (%glfw:show-window window))

(defun hide-window (&optional (window *window*))
  (%glfw:hide-window window))

(defun get-window-monitor (&optional (window *window*))
  (let ((monitor (%glfw:get-window-monitor window)))
    (unless (cffi:null-pointer-p monitor)
      monitor)))

(defun get-window-attribute (attribute &optional (window *window*))
  (let ((value (%glfw:get-window-attribute window attribute)))
    (ccase attribute
      ((:focused :iconified :resizable :visible :decorated :opengl-forward-compat :opengl-debug-context)
       (cffi:convert-from-foreign value :boolean))
      (:client-api (cffi:foreign-enum-keyword '%glfw::opengl-api value))
      ((:context-version-major :context-version-minor :context-revision) value)
      (:opengl-profile (cffi:foreign-enum-keyword '%glfw::opengl-profile value))
      (:context-robustness (cffi:foreign-enum-keyword '%glfw::robustness value)))))

(defun get-context-version (&optional (window *window*))
  "Convenience function returning (opengl-context-major-version opengl-context-minor-version opengl-context-revision)."
  (list (get-window-attribute :context-version-major window) (get-window-attribute :context-version-minor window) (get-window-attribute :context-version-revision window)))

(defmacro def-window-position-callback (name (window x y) &body body)
  `(%glfw:define-glfw-callback ,name
       ((,window :pointer) (,x :int) (,y :int))
     ,@body))

(defmacro def-window-size-callback (name (window w h) &body body)
  `(%glfw:define-glfw-callback ,name
       ((,window :pointer) (,w :int) (,h :int))
     ,@body))

(defmacro def-window-close-callback (name (window) &body body)
  `(%glfw:define-glfw-callback ,name
       ((,window :pointer))
     ,@body))

(defmacro def-window-refresh-callback (name (window) &body body)
  `(%glfw:define-glfw-callback ,name
       ((,window :pointer))
     ,@body))

(defmacro def-window-focus-callback (name (window focusedp) &body body)
  `(%glfw:define-glfw-callback ,name
       ((,window :pointer) (,focusedp :boolean))
     ,@body))

(defmacro def-window-iconify-callback (name (window iconifiedp) &body body)
  `(%glfw:define-glfw-callback ,name
       ((,window :pointer) (,iconifiedp :boolean))
     ,@body))

(defmacro def-framebuffer-size-callback (name (window w h) &body body)
  `(%glfw:define-glfw-callback ,name
       ((,window :pointer) (,w :int) (,h :int))
     ,@body))

(defun set-window-position-callback (callback-name &optional (window *window*))
  (%glfw:set-window-position-callback window (cffi:get-callback callback-name)))

(defun set-window-size-callback (callback-name &optional (window *window*))
  (%glfw:set-window-size-callback window (cffi:get-callback callback-name)))

(defun set-window-close-callback (callback-name &optional (window *window*))
  (%glfw:set-window-close-callback window (cffi:get-callback callback-name)))

(defun set-window-focus-callback (callback-name &optional (window *window*))
  (%glfw:set-window-focus-callback window (cffi:get-callback callback-name)))

(defun set-window-iconify-callback (callback-name &optional (window *window*))
  (%glfw:set-window-iconify-callback window (cffi:get-callback callback-name)))

(defun set-framebuffer-size-callback (callback-name &optional (window *window*))
  (%glfw:set-framebuffer-size-callback window (cffi:get-callback callback-name)))

;;;; ## Events and input
(import-export %glfw:poll-events %glfw:wait-events %glfw:post-empty-event)

(defun get-input-mode (mode &optional (window *window*))
  "Mode is one of :CURSOR :STICKY-KEYS or :STICKY-MOUSE-BUTTONS."
  (let ((value (%glfw:get-input-mode window mode)))
    (ccase mode
      (:cursor
       (cffi:convert-from-foreign value '%glfw::cursor-mode))
      ((:sticky-keys :sticky-mouse-buttons)
       (cffi:convert-from-foreign value :boolean)))))

(defun set-input-mode (mode value &optional (window *window*))
  (let ((value (ccase mode
		 (:cursor
		  (cffi:convert-to-foreign value '%glfw::cursor-mode))
		 ((:sticky-keys :sticky-mouse-buttons)
		  (cffi:convert-to-foreign value :boolean)))))
    (%glfw:set-input-mode window mode value)))

(defun get-key (key &optional (window *window*))
  (%glfw:get-key window key))

(defun get-mouse-button (button &optional (window *window*))
  (%glfw:get-mouse-button window button))

(defun get-cursor-position (&optional (window *window*))
  (%glfw:get-cursor-position window))

(defun set-cursor-position (x y &optional (window *window*))
  (%glfw:set-cursor-position window x y))

(defmacro def-key-callback (name (window key scancode action mod-keys) &body body)
  `(%glfw:define-glfw-callback ,name
       ((,window :pointer) (,key %glfw::key) (,scancode :int)
	(,action %glfw::key-action) (,mod-keys %glfw::mod-keys))
     ,@body))

(defmacro def-char-callback (name (window char) &body body)
  (let ((char-code (gensym "char")))
    `(%glfw:define-glfw-callback ,name
	((,window :pointer) (,char-code :unsigned-int))
      (let ((,char (code-char ,char-code)))
	,@body))))

(defmacro def-mouse-button-callback (name (window button action mod-keys) &body body)
  `(%glfw:define-glfw-callback ,name
       ((,window :pointer) (,button %glfw::mouse)
	(,action %glfw::key-action) (,mod-keys %glfw::mod-keys))
     ,@body))

(defmacro def-cursor-pos-callback (name (window x y) &body body)
  `(%glfw:define-glfw-callback ,name
       ((,window :pointer) (,x :double) (,y :double))
     ,@body))

(defmacro def-cursor-enter-callback (name (window enterp) &body body)
  `(%glfw:define-glfw-callback ,name
       ((,window :pointer) (,enterp :boolean))
     ,@body))

(defmacro def-scroll-callback (name (window x y) &body body)
  `(%glfw:define-glfw-callback ,name
       ((,window :pointer) (,x :double) (,y :double))
     ,@body))

(defun set-key-callback (callback-name &optional (window *window*))
  (%glfw:set-key-callback window (cffi:get-callback callback-name)))

(defun set-char-callback (callback-name &optional (window *window*))
  (%glfw:set-char-callback window (cffi:get-callback callback-name)))

(defun set-mouse-button-callback (callback-name &optional (window *window*))
  (%glfw:set-mouse-button-callback window (cffi:get-callback callback-name)))

(defun set-cursor-position-callback (callback-name &optional (window *window*))
  (%glfw:set-cursor-position-callback window (cffi:get-callback callback-name)))

(defun set-cursor-enter-callback (callback-name &optional (window *window*))
  (%glfw:set-cursor-enter-callback window (cffi:get-callback callback-name)))

(defun set-scroll-callback (callback-name &optional (window *window*))
  (%glfw:set-scroll-callback window (cffi:get-callback callback-name)))

(import-export %glfw:joystick-present-p %glfw:get-joystick-axes %glfw:get-joystick-buttons %glfw:get-joystick-name)

;;;; ## Clipboard

(defun set-clipboard-string (string &optional (window *window*))
  (%glfw:set-clipboard-string window string))

(defun get-clipboard-string (&optional (window *window*))
  (%glfw:get-clipboard-string window))

;;;; ## Time
(import-export %glfw:get-time  %glfw:set-time)

;;;; ## Context
(defun make-context-current (window)
  (setf *window* window)
  (%glfw:make-context-current window))

(defun get-current-context ()
  (%glfw:get-current-context))

(defmacro with-context (window &body body)
  `(let* ((*window* ,window))
     ,@body))

(defun swap-buffers (&optional (window *window*))
  (%glfw:swap-buffers window))

(import-export %glfw:swap-interval %glfw:extension-supported-p %glfw:get-proc-address)
