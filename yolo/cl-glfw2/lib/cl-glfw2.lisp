(in-package #:cl-glfw)

#+ecl
(ffi:load-foreign-library "glfw" :system-library t)

#-ecl
(progn
  (cffi:define-foreign-library libglfw
    (:darwin (:or "libglfw.dylib" (:framework "GLFW")))
    (:unix (:or "libglfw.so" "libglfw.so.2"  #P"/usr/lib/x86_64-linux-gnu/libglfw.so.2"))
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

  ;; glfwGetWindowParam tokens

(defconstant +opened+ #x00020001)
(defconstant +active+ #x00020002)
(defconstant +iconified+ #x00020003)
(defconstant +accelerated+ #x00020004)
(defconstant +red-bits+ #x00020005)
(defconstant +green-bits+ #x00020006)
(defconstant +blue-bits+ #x00020007)
(defconstant +alpha-bits+ #x00020008)
(defconstant +depth-bits+ #x00020009)
(defconstant +stencil-bits+ #x0002000a)

  ;; The following constants are used for both glfwGetWindowParam
  ;; and glfwOpenWindowHint

(defconstant +refresh-rate+ #x0002000b)
(defconstant +accum-red-bits+ #x0002000c)
(defconstant +accum-green-bits+ #x0002000d)
(defconstant +accum-blue-bits+ #x0002000e)
(defconstant +accum-alpha-bits+ #x0002000f)
(defconstant +aux-buffers+ #x00020010)
(defconstant +stereo+ #x00020011)
(defconstant +window-no-resize+ #x00020012)
(defconstant +fsaa-samples+ #x00020013)
(defconstant +opengl-version-major+ #x00020014)
(defconstant +opengl-version-minor+ #x00020015)
(defconstant +opengl-forward-compat+ #x00020016)
(defconstant +opengl-debug-context+ #x00020017)
(defconstant +opengl-profile+ #x00020018)

(defconstant +opengl-core-profile+ #x00050001)
(defconstant +opengl-compat-profile+ #x00050002)

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
  "width
      The width of the window. If width is zero, it will be calculated as width = 4/3 height, if height is
      not zero. If both width and height are zero, then width will be set to 640.
height
      The height of the window. If height is zero, it will be calculated as height = 3/4 width, if width is
      not zero. If both width and height are zero, then height will be set to 480.
redbits, greenbits, bluebits
      The number of bits to use for each color component of the color buffer (0 means default color
      depth). For instance, setting redbits=5, greenbits=6, and bluebits=5 will generate a 16-bit color
      buffer, if possible.
alphabits
      The number of bits to use for the alpha buffer (0 means no alpha buffer).
depthbits
      The number of bits to use for the depth buffer (0 means no depth buffer).
stencilbits
      The number of bits to use for the stencil buffer (0 means no stencil buffer).
mode
      Selects which type of OpenGL window to use. mode can be either +WINDOW+, which
      will generate a normal desktop window, or +FULLSCREEN+ which will generate a
      window which covers the entire screen. When +FULLSCREEN+ is selected, the video
      mode will be changed to the resolution that closest matches the width and height parameters.

Return values
If the function succeeds, t is returned.
If the function fails, nil is returned.

Description

The function opens a window that best matches the parameters given to
the function. How well the resulting window matches the desired window
depends mostly on the available hardware and OpenGL drivers. In
general, selecting a fullscreen mode has better chances of generating
a close match than does a normal desktop window, since GLFW can freely
select from all the available video modes. A desktop window is
normally restricted to the video mode of the desktop.

Notes

For additional control of window properties, see glfw::OpenWindowHint.
In fullscreen mode the mouse cursor is hidden by default, and any system screensavers are prohibited
from starting. In windowed mode the mouse cursor is visible, and screensavers are allowed to start. To
change the visibility of the mouse cursor, use glfwEnable or glfwDisable with the argument
+MOUSE_CURSOR+
In order to determine the actual properties of an opened window, use glfw::GetWindowParam and
glfw::GetWindowSize (or glfw::SetWindowSizeCallback).
"
  (macrolet ((output-int-hints (&rest names)
	       `(progn
		  ,@(loop for name in names collect
			 `(when ,(intern (format nil "~a-P" name) '#:cl-glfw)
			    (open-window-hint ,(intern (format nil "+~A+" name) '#:cl-glfw)
					      ,name)))))
	     (output-boolean-hints (&rest names)
	       `(progn
		  ,@(loop for name in names collect
			 `(when ,(intern (format nil "~a-P" name) '#:cl-glfw)
			    (open-window-hint ,(intern (format nil "+~A+" name) '#:cl-glfw)
					      (if ,name +true+ +false+)))))))
    (output-int-hints refresh-rate 
		      accum-red-bits accum-green-bits accum-blue-bits
		      accum-alpha-bits
		      aux-buffers
		      fsaa-samples
		      opengl-version-major
		      opengl-version-minor
		      opengl-profile
		      opengl-core-profile
		      opengl-compat-profile)
    (output-boolean-hints stereo
			  window-no-resize
			  opengl-forward-compat
			  opengl-debug-context))
  (if (%open-window width height redbits greenbits bluebits alphabits depthbits stencilbits mode)
      (when title (set-window-title title))
      (error "Error initializing glfw window.")))

 

(defcfun ("glfwOpenWindowHint" open-window-hint) :void
  (target :int)
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
       (when (= +true+ (glfw:get-window-param glfw:+opened+))
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
	    (unless (= +true+ (glfw:get-window-param glfw:+opened+))	
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

(defmacro define-callback-setter (c-name callback-prefix return-type (&body args) &key before-form after-form documentation)
  (let* ((callback-name (intern (format nil "~A-CALLBACK" callback-prefix)))
         (special-name (intern (format nil "*~S*" callback-name)))
         (setter-name (intern (format nil "SET-~S" callback-name)))
         (internal-setter-name (intern (format nil "%~S" setter-name))))
    `(progn
       (defparameter ,special-name nil)
       (cffi:defcallback ,callback-name ,return-type ,args
         (when ,special-name
           (prog2
               ,before-form
               (funcall ,special-name ,@(mapcar #'car args))
             ,after-form)))
       (cffi:defcfun (,c-name ,internal-setter-name) :void (cbfun :pointer))
       (defun ,setter-name (callback)
         ,(format nil "GENERAL CL-GLFW CALLBACK NOTES

All callback setting functions can take either a pointer to a C function,
a function object, a function symbol, or nil to clear the callback function.

THIS CALLBACK FUNCTION

~a" documentation)
         (cl:cond
           ((null callback)
            (,internal-setter-name (cffi:null-pointer)))
           ((symbolp callback)
            (setf ,special-name callback)
            (,internal-setter-name (cffi:callback ,callback-name)))
           ((functionp callback)
            (setf ,special-name callback)
            (,internal-setter-name (cffi:callback ,callback-name)))
           ((cffi:pointerp callback)
            (,internal-setter-name callback))
           (t (error "Not an acceptable callback. Must be foreign pointer, function object, function's symbol, or nil.")))))))


(define-callback-setter "glfwSetWindowCloseCallback" #:window-close :int ()
                        :documentation
                        "
Function that will be called when a user requests that the window should be
closed, typically by clicking the window close icon (e.g. the cross in the upper right corner of a
window under Microsoft Windows). The function should have the following type:
(function () integer)

The return value of the callback function indicates whether or not the window close action should continue. If the function returns
gl:+true+, the window will be closed. If the function returns gl:+false+, the window will not
be closed. If you give a CFFI callback returning glfw:boolean, you can use t and nil as return types.

Notes
Window close events are recorded continuously, but only reported when glfwPollEvents,
glfwWaitEvents or glfwSwapBuffers is called.
The OpenGL context is still valid when this function is called.
Note that the window close callback function is not called when glfwCloseWindow is called, but only
when the close request comes from the window manager.
Do not call glfwCloseWindow from a window close callback function. Close the window by returning
gl:+true+ from the function.
")


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
  "The function is used for determining the size of an opened window. The returned values are dimensions
of the client area of the window (i.e. excluding any window borders and decorations).
(list width height)"
  (cffi:with-foreign-objects ((width :int)
			      (height :int))
    (%get-window-size width height)
    (list (mem-ref width :int)
	  (mem-ref height :int))))

(define-callback-setter "glfwSetWindowSizeCallback" #:window-size :void ((width :int) (height :int))
                        :documentation
                        "
Function that will be called every time the window size changes. The
function should takes the arguments (width height) giving the new width and height of the window client area.

A window has to be opened for this function to have any effect.
Notes
Window size changes are recorded continuously, but only reported when glfwPollEvents,
glfwWaitEvents or glfwSwapBuffers is called. ")

(defcfun ("glfwIconifyWindow" iconify-window) :void)
(defcfun ("glfwRestoreWindow" restore-window) :void)
(defcfun ("glfwGetWindowParam" get-window-param) :int
  (param :int))
(defcfun ("glfwSwapBuffers" swap-buffers) :void)
(defcfun ("glfwSwapInterval" swap-interval) :void
  (interval :int))


(define-callback-setter "glfwSetWindowRefreshCallback" #:window-refresh :void ()
                        :documentation
                        "
Function that will be called when the window client area needs to be
refreshed. The function takes no arguments and returns nothing (void).

Description

The function selects which function to be called upon a window refresh
event, which occurs when any part of the window client area has been
damaged, and needs to be repainted (for instance, if a part of the
window that was previously occluded by another window has become
visible).  A window has to be opened for this function to have any
effect.

Notes
Window refresh events are recorded continuously, but only reported when glfwPollEvents,
glfwWaitEvents or glfwSwapBuffers is called.
")

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
  "Parameters
maxcount
      Maximum number of video modes that list vector can hold.

Return values
The function returns the number of detected video modes (this number will never exceed maxcount).
The list vector is filled out with the video modes that are supported by the system.

Description
The function returns a list of supported video modes. Each video mode is represented by a
list of the form:
(width height redbits greenbits bluebits)

Notes
The returned list is sorted, first by color depth (RedBits + GreenBits + BlueBits), and then by
resolution (Width * Height), with the lowest resolution, fewest bits per pixel mode first. "
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
  "Parameters
mode
       Pointer to a GLFWvidmode structure, which will be filled out by the function.
Return values
The GLFWvidmode structure pointed to by mode is filled out with the desktop video mode.
Description
The function returns the desktop video mode in a GLFWvidmode structure. See glfwGetVideoModes
for a definition of the GLFWvidmode structure.
Notes
The color depth of the desktop display is always reported as the number of bits for each individual color
component (red, green and blue), even if the desktop is not using an RGB or RGBA color format. For
instance, an indexed 256 color display may report RedBits = 3, GreenBits = 3 and BlueBits = 2, which
adds up to 8 bits in total.
The desktop video mode is the video mode used by the desktop, not the current video mode (which may
differ from the desktop video mode if the GLFW window is a fullscreen window).
"
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
    (list (mem-ref xpos :int)
	  (mem-ref ypos :int))))
(defcfun ("glfwSetMousePos" set-mouse-pos) :void
  (xpos :int)
  (ypos :int))
(defcfun ("glfwGetMouseWheel" get-mouse-wheel) :int)
(defcfun ("glfwSetMouseWheel" set-mouse-wheel) :void
  (pos :int))


(define-callback-setter "glfwSetKeyCallback" #:key :void ((key :int) (action :int))
                        :before-form (setf key (lispify-key key))
                        :documentation
                        "
Function that will be called every time a key is pressed or released.
Function should take the arguments (key action), where key is either a character,
if the key pressed was a member of iso-8859-1, or a keyword representing the key pressed if not.
See the GLFW manual, table 3.3 for special key identifiers. Action is either glfw:+press+ or
glfw:+release+. Use set-char-callback instead if you want to read just characters.

Description
The function selects which function to be called upon a keyboard key event. The callback function is
called every time the state of a single key is changed (from released to pressed or vice versa). The
reported keys are unaffected by any modifiers (such as shift or alt).
A window has to be opened for this function to have any effect.

Notes
Keyboard events are recorded continuously, but only reported when glfw::PollEvents, glfw::WaitEvents
or glfw::SwapBuffers is called.
")
(define-callback-setter "glfwSetCharCallback" #:char :void ((character :int) (action :int))
                        :before-form (setf character (code-char character))
                        :documentation
                        "
Function that will be called every time a printable character is generated by
the keyboard. The function should take the arguments (character action)
where character is a lisp character and action is either glfw:+press+ or glfw:+release+.

NB this makes the presumption that your lisp implementation will use Unicode for code-char.

Description
The function selects which function to be called upon a keyboard character event. The callback function
is called every time a key that results in a printable Unicode character is pressed or released. Characters
are affected by modifiers (such as shift or alt).
A window has to be opened for this function to have any effect.

Notes
Character events are recorded continuously, but only reported when glfw::PollEvents, glfw::WaitEvents
or glfw::SwapBuffers is called.
Control characters, such as tab and carriage return, are not reported to the character callback function,
since they are not part of the Unicode character set. Use the key callback function for such events (see
glfw::SetKeyCallback).
The Unicode character set supports character codes above 255, so never cast a Unicode character to an
eight bit data type (e.g. the C language char type) without first checking that the character code is less
than 256. Also note that Unicode character codes 0 to 255 are equal to ISO 8859-1 (Latin 1).
")

(define-callback-setter "glfwSetMouseButtonCallback" #:mouse-button :void ((button mouse) (action :int))
                        :before-form (setf button (lispify-mouse-button button))
                        :documentation
                        "
Function that will be called every time a mouse button is pressed or released.
The function takes the arguments (button action), where button is a keyword symbol as returned by
lispify-mouse-button and action is either glfw:+press+ or glfw:+release+.

Description
The function selects which function to be called upon a mouse button event.
A window has to be opened for this function to have any effect.

Notes
Mouse button events are recorded continuously, but only reported when glfw::PollEvents,
glfw::WaitEvents or glfw::SwapBuffers is called.
+MOUSE_BUTTON_LEFT+ is equal to +MOUSE_BUTTON_1+
+MOUSE_BUTTON_RIGHT+ is equal to +MOUSE_BUTTON_2+
+MOUSE_BUTTON_MIDDLE+ is equal to +MOUSE_BUTTON_3+
")
(define-callback-setter "glfwSetMousePosCallback" #:mouse-pos :void ((x :int) (y :int))
                        :documentation
                        "
Function that will be called every time the mouse is moved.
The function takes the arguments (x y), where x and y are the current position of the mouse.

Description
The function selects which function to be called upon a mouse motion event.
A window has to be opened for this function to have any effect.

Notes
Mouse motion events are recorded continuously, but only reported when glfw::PollEvents,
glfw::WaitEvents or glfw::SwapBuffers is called.
")

(defparameter *mouse-wheel-cumulative* nil)
(define-callback-setter "glfwSetMouseWheelCallback" #:mouse-wheel :void ((pos :int))
                        :after-form (unless *mouse-wheel-cumulative* (glfw:set-mouse-wheel 0))
                        :documentation
                        "
Function that will be called every time the mouse wheel is moved.
The function takes one argument: the position of the mouse wheel.
This DIFFERS FROM GLFW's DEFAULT behaviour in that the position is
reset after every call to this function, effectively giving the delta.
As most programs are only interested in the delta anyway, this is thought
to save others recording the state of it again.
If you wish to have the original GLFW behaviour, set cl-glfw:*mouse-wheel-cumulative* to t.

Description
The function selects which function to be called upon a mouse wheel event.
A window has to be opened for this function to have any effect.
Notes
Mouse wheel events are recorded continuously, but only reported when glfw::PollEvents,
glfw::WaitEvents or glfw::SwapBuffers is called.
")

(defcfun ("glfwGetJoystickParam" get-joystick-param) :int
  (joy joystick)
  (param joystick-param))

(defcfun ("glfwGetJoystickPos" %get-joystick-pos) :int
  (joy joystick)
  (pos :pointer)
  (numaxes :int))

(defun get-joystick-pos (joy numaxes)
  "Parameters
joy
       A joystick identifier, which should be +JOYSTICK_n+ where n is in the range 1 to 16.
numaxes
       Specifies how many axes should be returned.
Return values
       An list that will hold the positional values for all requested axes.
If the joystick is not supported or connected, the function will
return nil.

Description
The function queries the current position of one or more axes of a joystick. The positional values are
returned in an array, where the first element represents the first axis of the joystick (normally the X
axis). Each position is in the range -1.0 to 1.0. Where applicable, the positive direction of an axis is
right, forward or up, and the negative direction is left, back or down.
If numaxes exceeds the number of axes supported by the joystick, or if the joystick is not available, the
unused elements in the pos array will be set to 0.0 (zero).

Notes
The joystick state is updated every time the function is called, so there is no need to call glfw::PollEvents
or glfw::WaitEvents for joystick state to be updated.
Use glfw::GetJoystickParam to retrieve joystick capabilities, such as joystick availability and number of
supported axes.
No window has to be opened for joystick input to be valid.
"
  (with-foreign-object (pos :float numaxes)
    (let ((numaxes (%get-joystick-pos joy pos numaxes)))
      (loop for i below numaxes collecting (mem-aref pos :float i)))))


(defcfun ("glfwGetJoystickButtons" %get-joystick-buttons) :int
  (joy joystick)
  (buttons :pointer)
  (numbuttons :int))
(defun get-joystick-buttons (joy numbuttons)
  "Parameters
joy
       A joystick identifier, which should be +JOYSTICK_n+ where n is in the range 1 to 16.
numbuttons
       Specifies how many buttons should be returned.
Return values
       A list that will hold the button states for all requested buttons.
The function returns the number of actually returned buttons. This is the minimum of numbuttons and
the number of buttons supported by the joystick. If the joystick is not supported or connected, the
function will return 0 (zero).

Description
The function queries the current state of one or more buttons of a joystick. The button states are
returned in an array, where the first element represents the first button of the joystick. Each state can be
either +PRESS+ or +RELEASE+
If numbuttons exceeds the number of buttons supported by the joystick, or if the joystick is not
available, the unused elements in the buttons array will be set to +RELEASE+

Notes
The joystick state is updated every time the function is called, so there is no need to call glfw::PollEvents
or glfw::WaitEvents for joystick state to be updated.
Use glfw::GetJoystickParam to retrieve joystick capabilities, such as joystick availability and number of
supported buttons.
No window has to be opened for joystick input to be valid.
"
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
  "Parameters
mutex
      A mutex object handle.
forms
      Body of code to execute
Description
This macro will acquire a lock on the selected mutex object using glfw::LockMutex and release it afterwards
using glfw::UnlockMutex.
So, forms will not execute until an exclusive lock is held.
The lock is then released when the stack is unwound."
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
