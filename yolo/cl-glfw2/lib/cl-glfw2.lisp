(in-package #:cl-glfw)

#+ecl
(ffi:load-foreign-library "glfw" :system-library t)

#-ecl
(progn
  (cffi:define-foreign-library libglfw
    (:darwin (:or "libglfw.dylib" (:framework "GLFW")))
    (:unix (:or "glfw" "libglfw.so" "libglfw.so.2"  #P"/usr/lib/x86_64-linux-gnu/libglfw.so.2"))
    (:windows (:or "glfw.dll" "libglfw.dll")) 
    (t (:default "libglfw")))
  (cffi:use-foreign-library libglfw))

(defconstant +false+ 0)
(defconstant +true+ 1)

(defcenum (key-action)
  :release
  :press)

  ;; Keyboard key definitions: 8-bit ISO-8859-1 (Latin 1) encoding is used
  ;; for printable keys (such as A-Z, 0-9 etc), and values above 256
;; represent special (non-printable) keys (e.g. F1, Page Up etc).

(defcenum (key)
  (:unknown -1)
  (:space 32)
  (:apostrophe 39)
  (:comma 44)
  (:minus 45)
  (:period 46)
  (:slash 47)
  (:0 48)
  (:1 49)
  (:2 50)
  (:3 51)
  (:4 52)
  (:5 53)
  (:6 54)
  (:7 55)
  (:8 56)
  (:9 57)
  (:semicolon 59)
  (:equal 61)
  (:a 65)
  (:b 66)
  (:c 67)
  (:d 68)
  (:e 69)
  (:f 70)
  (:g 71)
  (:h 72)
  (:i 73)
  (:j 74)
  (:k 75)
  (:l 76)
  (:m 77)
  (:n 78)
  (:o 79)
  (:p 80)
  (:q 81)
  (:r 82)
  (:s 83)
  (:t 84)
  (:u 85)
  (:v 86)
  (:w 87)
  (:x 88)
  (:y 89)
  (:z 90)
  (:left-bracket 91)
  (:backslash 92)
  (:right-bracket 93)
  (:grave-accent 96)
  (:world-1 161) ;;huh?
  (:world-2 162) ;;huh?
  (:special 256)
  
  (:escape 257)
  (:f1 258)
  (:f2 259)
  (:f3 260)
  (:f4 261)
  (:f5 262)
  (:f6 263)
  (:f7 264)
  (:f8 265)
  (:f9 266)
  (:f10 267)
  (:f11 268)
  (:f12 269)
  (:f13 270)
  (:f14 271)
  (:f15 272)
  (:f16 273)
  (:f17 274)
  (:f18 275)
  (:f19 276)
  (:f20 277)
  (:f21 278)
  (:f22 279)
  (:f23 280)
  (:f24 281)
  (:f25 282)
  (:up 283)
  (:down 284)
  (:left 285)
  (:right 286)
  (:left-shift 287)
  (:right-shift 288)
  (:left-control 289)
  (:right-control 290)
  (:left-alt 291)
  (:right-alt 292)
  (:tab 293)
  (:enter 294)
  (:backspace 295)
  (:insert 296)
  (:delete 297)
  (:page-up 298)
  (:page-down 299)
  (:home 300)
  (:end 301)
  (:kp-0 302)
  (:kp-1 303)
  (:kp-2 304)
  (:kp-3 305)
  (:kp-4 306)
  (:kp-5 307)
  (:kp-6 308)
  (:kp-7 309)
  (:kp-8 310)
  (:kp-9 311)
  (:kp-divide 312)
  (:kp-multiply 313)
  (:kp-subtract 314)
  (:kp-add 315)
  (:kp-decimal 316)
  (:kp-equal 317)
  (:kp-enter 318)
  (:num-lock 319)
  (:caps-lock 320)
  (:scroll-lock 321)
  (:pause 322)
  (:left-super 323)
  (:right-super 324)
  (:menu 325))

  ;; Mouse button definitions

(defcenum (mouse)
  (:1 0)
  (:2 1)
  (:3 2)
  (:4 3)
  (:5 4)
  (:6 5)
  (:7 6)
  (:8 7)
  (:last 7)
  (:left 0)
  (:right 1)
  (:center 2))

  ;; Joystick identifiers

(defcenum (joystick)
  :1
  :2
  :3
  :4
  :5
  :6
  :7
  :8
  :9
  :10
  :11
  :12
  :13
  :14
  :15
  :16
  (:last 15)) 

  ;;========================================================================
  ;; Other definitions
  ;;========================================================================

  ;; glfwOpenWindow modes

(defconstant +window+ #x00010001)
(defconstant +fullscreen+ #x00010002)


(defcenum (window-hint)
   ;; glfwGetWindowParam tokens
  (:opened 131073)
  (:active 131074)
  (:iconified 131075)
  (:accelerated 131076)
  (:red-bits 131077)
  (:green-bits 131078)
  (:blue-bits 131079)
  (:alpha-bits 131080)
  (:depth-bits 131081)
  (:stencil-bits 131082)

  
  ;; The following constants are used for both glfwGetWindowParam
  ;; an d glfwOpenWindowHint
  
  (:refresh-rate 131083)
  (:accum-red-bits 131084)
  (:accum-green-bits 131085)
  (:accum-blue-bits 131086)
  (:accum-alpha-bits 131087)
  (:aux-buffers 131088)
  (:stereo 131089)
  (:window-no-resize 131090)
  (:fsaa-samples 131091)
  (:opengl-version-major 131092)
  (:opengl-version-minor 131093)
  (:opengl-forward-compat 131094)
  (:opengl-debug-context 131095)
  (:opengl-profile 131096)
  (:opengl-core-profile 327681)
  (:opengl-compat-profile 327682))

  ;; glfwEnable/glfwDisable tokens

(defcenum (enable-disable)
  (:mouse-cursor #x00030001)
  (:sticky-keys #x00030002)
  (:sticky-mouse-buttons #x00030003)
  (:system-keys #x00030004)
  (:key-repeat #x00030005)
  (:auto-poll-events #x00030006))

  ;; glfwWaitThread wait modes

(defcenum (wait-mode)
  (:wait #x00040001)
  (:no-wait #x00040002))

  ;; glfwGetJoystickParam tokens

(defcenum (joystick-param)
  (:present #x00050001)
  (:axes #x00050002)
  (:buttons #x00050003))

  ;; glfwReadImage/glfwLoadTexture2D flags

(defconstant +no-rescale-bit+ #x00000001) ; Only for glfwReadImage

(defconstant +origin-ul-bit+ #x00000002)
(defconstant +build-mipmaps-bit+ #x00000004) ; Only for glfwLoadTexture2D

(defconstant +alpha-map-bit+ #x00000008)

  ;; Time spans longer than this (seconds) are considered to be infinity

(defconstant +infinity+ 100000d0)

(cffi:defcfun ("glfwInit" %init) boolean)

(defun init ()
  (when (%init) t))

(cffi:defcfun ("glfwTerminate" %terminate) :void)

(defun terminate ()
  (%terminate))

(defcfun ("glfwGetVersion" %get-version) :void
  (major :pointer)
  (minor :pointer)
  (rev :pointer))
(defun get-version ()
  (with-foreign-objects ((major :int) (minor :int) (rev :int))
    (%get-version major minor rev)
    (list (mem-ref major :int)
	  (mem-ref minor :int)
	  (mem-ref rev :int))))

(defmacro with-init (&body forms)
  "Call glfw:init, execute forms and clean-up with glfw:terminate once finished.
This makes a nice wrapper to an application higher-level form.
Signals an error on failure to initialize. Wrapped in a block named glfw:with-init."
  `(if (glfw:init)
       (unwind-protect
	    (block with-init ,@forms)
	 (glfw:terminate))
       (error "Error initializing glfw.")))

(defcfun ("glfwOpenWindow" %open-window) boolean
  (width :int) (height :int)
  (redbits :int) (greenbits :int) (bluebits :int) (alphabits :int)
  (depthbits :int) (stencilbits :int) (mode :int))

(declaim (inline open-window))
(defun open-window (&key (width 0) (height 0)
                    (redbits 0) (greenbits 0) (bluebits 0) (alphabits 0)
                    (depthbits 0) (stencilbits 0) (mode +window+)
		    title
		    ;;The hints
		    (refresh-rate 0 refresh-rate-p)
		    (accum-red-bits 0 accum-red-bits-p)
		    (accum-green-bits 0 accum-green-bits-p)
		    (accum-blue-bits 0 accum-blue-bits-p)
		    (accum-alpha-bits 0 accum-alpha-bits-p)
		    (aux-buffers 0 aux-buffers-p)
		    (stereo nil stereo-p)
		    (window-no-resize nil window-no-resize-p)
		    (fsaa-samples 0 fsaa-samples-p)
		    (opengl-version-major 1 opengl-version-major-p)
		    (opengl-version-minor 1 opengl-version-minor-p)
		    (opengl-forward-compat nil opengl-forward-compat-p)
		    (opengl-debug-context nil opengl-debug-context-p)
		    (opengl-profile 0 opengl-profile-p)
		    (opengl-core-profile 0 opengl-core-profile-p)
		    (opengl-compat-profile 0 opengl-compat-profile-p))

  (if cl-glfw::refresh-rate-p
      (open-window-hint :refresh-rate refresh-rate))
  (if cl-glfw::accum-red-bits-p
      (open-window-hint :accum-red-bits accum-red-bits))
  (if cl-glfw::accum-green-bits-p
      (open-window-hint :accum-green-bits accum-green-bits))
  (if cl-glfw::accum-blue-bits-p
      (open-window-hint :accum-blue-bits accum-blue-bits))
  (if cl-glfw::accum-alpha-bits-p
      (open-window-hint :accum-alpha-bits accum-alpha-bits))
  (if cl-glfw::aux-buffers-p
      (open-window-hint :aux-buffers aux-buffers))
  (if cl-glfw::fsaa-samples-p
      (open-window-hint :fsaa-samples fsaa-samples))
  (if cl-glfw::opengl-version-major-p
      (open-window-hint :opengl-version-major opengl-version-major))
  (if cl-glfw::opengl-version-minor-p
      (open-window-hint :opengl-version-minor opengl-version-minor))
  (if cl-glfw::opengl-profile-p
      (open-window-hint :opengl-profile opengl-profile))
  (if cl-glfw::opengl-core-profile-p
      (open-window-hint :opengl-core-profile opengl-core-profile))
  (if cl-glfw::opengl-compat-profile-p
      (open-window-hint :opengl-compat-profile
			opengl-compat-profile))
  (if cl-glfw::stereo-p
      (open-window-hint :stereo
			(if stereo
			    +true+
			    +false+)))
  (if cl-glfw::window-no-resize-p
      (open-window-hint :window-no-resize
			(if window-no-resize
			    +true+
			    +false+)))
  (if cl-glfw::opengl-forward-compat-p
      (open-window-hint :opengl-forward-compat
			(if opengl-forward-compat
			    +true+
			    +false+)))
  (if cl-glfw::opengl-debug-context-p
      (open-window-hint :opengl-debug-context
			(if opengl-debug-context
			    +true+
			    +false+)))
  (if (%open-window width height redbits greenbits bluebits alphabits depthbits stencilbits mode)
      (when title (set-window-title title))
      (error "Error initializing glfw window.")))

(defcfun ("glfwOpenWindowHint" open-window-hint) :void
  (target window-hint)
  (hint :int))

(defcfun ("glfwCloseWindow" close-window) :void)

(defmacro with-open-window ((&rest open-window-keys)
			    &body forms)
  "Wraps forms such that there is an open window for them to execute in and cleans up the
window afterwards. An error is signalled if there was an error opening the window.
Takes the same parameters as open-window, with the addition of 'title' which will
set the window title after opening.
Wrapped in a block named glfw:with-open-window."
  `(progn
     (open-window ,@open-window-keys)
     (unwind-protect
	  (block with-open-window ,@forms)
       (when (= +true+ (glfw:get-window-param :opened))
	 (close-window)))))

(defmacro with-init-window ((&rest open-window-keys)
			    &body forms)
  "Wraps forms in with-init, with-open-window. Passes through the other arguments to open-window."
  `(with-init
     (with-open-window (,@open-window-keys)
       ,@forms)))

(defmacro do-open-window ((&rest open-window-keys)
		     (&body setup-forms)
		     &body forms)
  "High-level convenience macro for opening a window (given the optional window parameters),
setting the title given,
running setup-forms and then running forms in a loop, with calls to swap-buffers after each loop iteration.
The loop is in a block named do-open-window [so can be exited by a call to (return-from glfw:do-window)].
If the window is closed, the loop is also exited."
  `(with-open-window (,@open-window-keys)
     ,@setup-forms
     (loop named do-open-window do
	  (progn
	    ,@forms
	    (glfw:swap-buffers)
	    (unless (= +true+ (glfw:get-window-param :opened))	
	      (return-from do-open-window))))))

(defmacro do-window ((&rest open-window-keys)
		     (&body setup-forms)
		     &body forms)
  "High-level convenience macro for initializing glfw, opening a window (given the optional window parameters),
setting the title given,
running setup-forms and then running forms in a loop, with calls to swap-buffers after each loop iteration.
The loop is in a block named do-window [so can be exited by a call to (return-from glfw:do-window)].
If the window is closed, the loop is also exited."
  `(with-init 
     (do-open-window (,@open-window-keys) (,@setup-forms) ,@forms)))


(defcfun ("glfwSetWindowTitle" set-window-title) :void
  (title :string))
(defcfun ("glfwSetWindowSize" set-window-size) :void
  (width :int)
  (height :int))
(defcfun ("glfwSetWindowPos" set-window-pos) :void
  (x :int)
  (y :int))
(defcfun ("glfwGetWindowSize" %get-window-size) :void
  (width :pointer)
  (height :pointer))
(defun get-window-size ()
  (cffi:with-foreign-objects ((width :int)
			      (height :int))
    (%get-window-size width height)
    (values (mem-ref width :int)
	    (mem-ref height :int))))

(defcfun ("glfwIconifyWindow" iconify-window) :void)
(defcfun ("glfwRestoreWindow" restore-window) :void)
(defcfun ("glfwGetWindowParam" get-window-param) :int
  (param window-hint))
(defcfun ("glfwSwapBuffers" swap-buffers) :void)
(defcfun ("glfwSwapInterval" swap-interval) :void
  (interval :int))

(defcstruct vidmode
  (width :int)
  (height :int)
  (redbits :int)
  (bluebits :int)
  (greenbits :int))

(defcfun ("glfwGetVideoModes" %get-video-modes) :int
  (list :pointer)
  (maxcount :int))

(defun get-video-modes (maxcount)
  (declare (optimize (debug 3)))
  (with-foreign-object (list 'vidmode maxcount)
    (let ((count (%get-video-modes list maxcount)))
      (loop for i below count
	 collecting
	 (let ((mode (cffi:mem-aref list 'vidmode i)))
	   (list (foreign-slot-value mode 'vidmode 'width)
		 (foreign-slot-value mode 'vidmode 'height)
		 (foreign-slot-value mode 'vidmode 'redbits)
		 (foreign-slot-value mode 'vidmode 'greenbits)
		 (foreign-slot-value mode 'vidmode 'bluebits)))))))

(defcfun ("glfwGetDesktopMode" %get-desktop-mode) :void
  (mode :pointer))
(defun get-desktop-mode ()
  (with-foreign-object (mode 'vidmode)
    (%get-desktop-mode mode)
    (list (foreign-slot-value mode 'vidmode 'width)
	  (foreign-slot-value mode 'vidmode 'height)
	  (foreign-slot-value mode 'vidmode 'redbits)
	  (foreign-slot-value mode 'vidmode 'greenbits)
	  (foreign-slot-value mode 'vidmode 'bluebits))))

(defcfun ("glfwPollEvents" poll-events) :void)
(defcfun ("glfwWaitEvents" wait-events) :void)


(defcfun ("glfwGetKey" get-key) key-action
  (key key))

(defcfun ("glfwGetMouseButton" get-mouse-button) :int
  (button mouse))

(defcfun ("glfwGetMousePos" %get-mouse-pos) :void
  (xpos :pointer)
  (ypos :pointer))
(defun get-mouse-pos ()
  (with-foreign-objects ((xpos :int) (ypos :int))
    (%get-mouse-pos xpos ypos)
    (values (mem-ref xpos :int)
	    (mem-ref ypos :int))))
(defcfun ("glfwSetMousePos" set-mouse-pos) :void
  (xpos :int)
  (ypos :int))
(defcfun ("glfwGetMouseWheel" get-mouse-wheel) :int)
(defcfun ("glfwSetMouseWheel" set-mouse-wheel) :void
  (pos :int))

(defcfun ("glfwGetJoystickParam" get-joystick-param) :int
  (joy joystick)
  (param joystick-param))

(defcfun ("glfwGetJoystickPos" %get-joystick-pos) :int
  (joy joystick)
  (pos :pointer)
  (numaxes :int))

(defun get-joystick-pos (joy numaxes)
  (with-foreign-object (pos :float numaxes)
    (let ((numaxes (%get-joystick-pos joy pos numaxes)))
      (loop for i below numaxes collecting (mem-aref pos :float i)))))


(defcfun ("glfwGetJoystickButtons" %get-joystick-buttons) :int
  (joy joystick)
  (buttons :pointer)
  (numbuttons :int))
(defun get-joystick-buttons (joy numbuttons)
  (with-foreign-object (buttons :unsigned-char numbuttons)
    (let ((numbuttons (%get-joystick-buttons joy buttons numbuttons)))
      (loop for i below numbuttons collecting (mem-aref buttons :unsigned-char i)))))


(defcfun ("glfwGetTime" get-time) :double)
(defcfun ("glfwSetTime" set-time) :void
  (time :double))

(defcfun ("glfwSleep" sleep) :void
  (time :double))

(defcstruct image
  (width :int)
  (height :int)
  (format :int)
  (bytes-per-pixel :int)
  (data :pointer))
(defcfun ("glfwReadImage" read-image) boolean
  (name :string)
  (img image)
  (flags :int))
(defcfun ("glfwReadMemoryImage" read-memory-image) boolean
  (data :pointer)
  (size :long)
  (img image)
  (flags :int))
(defcfun ("glfwFreeImage" free-image) :void
  (img image))
(defcfun ("glfwLoadTexture2D" load-texture-2d) boolean
  (name :string)
  (flags :int))
(defcfun ("glfwLoadMemoryTexture2D" load-memory-texture-2d) boolean
  (data :pointer)
  (size :long)
  (flags :int))
(defcfun ("glfwLoadTextureImage2D" load-texture-image-2d) boolean
  (img image)
  (flags :int))
(defcfun ("glfwExtensionSupported" extension-supported) boolean
  (extension :string))
(defcfun ("glfwGetProcAddress" get-proc-address) :pointer
  (procname :string))


(defcfun ("glfwGetGLVersion" %get-gl-version) :void
  (major :pointer)
  (minor :pointer)
  (revision :pointer))
(defun get-gl-version ()
  (with-foreign-objects ((major :int) (minor :int) (revision :int))
    (%get-gl-version major minor revision)
    (list (mem-ref major :int)
	  (mem-ref minor :int)
	  (mem-ref revision :int))))

(defctype thread :int)
(defctype threadfun :pointer)
(defctype mutex :pointer)
(defctype cond :pointer)

(defcfun ("glfwCreateThread" create-thread) thread
  (fun threadfun)
  (arg :pointer))
(defcfun ("glfwDestroyThread" destroy-thread) :void
  (id thread))
(defcfun("glfwWaitThread" wait-thread) boolean
  (id thread)
  (waitmode wait-mode))
(defcfun ("glfwGetThreadID" get-thread-id) thread)
(defcfun ("glfwCreateMutex" create-mutex) mutex)
(defcfun ("glfwDestroyMutex" destroy-mutex) :void
  (mutex mutex))
(defcfun ("glfwLockMutex" lock-mutex) :void
  (mutex mutex))
(defcfun ("glfwUnlockMutex" unlock-mutex) :void
  (mutex mutex))

(defmacro with-lock-mutex (mutex &body forms)
  (let ((smutex (gensym "MUTEX-")))
    `(let ((,smutex ,mutex))
       (glfw:lock-mutex ,smutex)
       (unwind-protect (progn ,@forms)
	 (glfw:unlock-mutex ,smutex)))))

(defcfun ("glfwCreateCond" create-cond) cond)
(defcfun ("glfwDestroyCond" destroy-cond) :void
  (cond cond))
(defcfun ("glfwWaitCond" wait-cond) :void
  (cond cond)
  (mutex mutex)
  (timeout :double))
(defcfun ("glfwSignalCond" signal-cond) :void
  (cond cond))
(defcfun ("glfwBroadcastCond" broadcast-cond) :void
  (cond cond))
(defcfun ("glfwGetNumberOfProcessors" get-number-of-processors) :int)
(defcfun ("glfwEnable" enable) :void
  (token enable-disable))
(defcfun ("glfwDisable" disable) :void
  (token enable-disable))
