;;;;Basics
;;;;1. Opening a window
;;;;2. Testing for button presses
;;;;3. Drawing with the fixed function pipeline
;;;;4. Setting up shaders
;;;;5. deflazy

(defpackage #:test1
  (:use :cl)
  (:export #:start))
(in-package #:test1)
;;;;In order to open a window, 
(defun start ()
  (application:main
   (lambda ()
     ;;The game loop
     (loop
	;;This function needs to be called in order
	;;to observe key board events, mouse events.
	;;without it, the window will not close.
	(application:poll-app)))
   ;;The width of the window
   :width 512
   ;;The height of the window
   :height 512
   ;;The title of the window
   :title "1. Opening a window"))

(defpackage #:test2
  (:use :cl)
  (:export #:start))
(in-package #:test2)
;;;;In order to open a window, 
(defun start ()
  (application:main
   (lambda ()
     ;;The game loop
     (loop
	;;This function needs to be called in order
	;;to observe key board events, mouse events.
	;;without it, the window will not close.
	(application:poll-app)
	;;(window:button type state button)
	;;type -> :key, :mouse
	;;state -> :down, :pressed, :released, repeat
	(when (window:button :key :down #\b)
	  (print "Key b down"))
	(when (window:button :key :pressed #\b)
	  (print "Key b pressed"))
	(when (window:button :key :released #\b)
	  (print "Key b released"))
	(when (window:button :key :repeat #\b)
	  (print "Key b repeat"))
	(when (window:button :mouse :down :left)
	  (print "Left mouse button down"))

	;;Get characters to send to a terminal emulator
	(let ((characters (control:get-input-characters)))
	  (unless (zerop (length characters))
	    (print characters)))))
   ;;The width of the window
   :width 512
   ;;The height of the window
   :height 512
   ;;The title of the window
   :title "2. Listening to button presses"))

(defpackage #:test3
  (:use :cl)
  (:export #:start))
(in-package #:test3)

(defun triangle ()
  (gl:with-primitives :triangles
    (gl:color 1.0 0.0 0.0)
    (gl:vertex 1.0 -1.0 0.5)
    (gl:color 0.0 1.0 0.0)
    (gl:vertex 0.0 1.0 0.5)
    (gl:color 0.0 0.0 1.0)
    (gl:vertex -1.0 -1.0 0.5)))

(defun start ()
  (application:main
   (lambda ()
     (gl:clear-color 0.5 0.5 0.5 0.0)
     (loop (application:poll-app)
	(gl:clear :color-buffer-bit
		  :depth-buffer-bit
		  :stencil-buffer-bit)
	(triangle)))
   :width 512
   :height 512
   :title "3. Triangle: fixed pipeline"))

(defpackage #:test4
  (:use :cl)
  (:export #:start))
(in-package #:test4)

(defun triangle (position-buffer color-buffer)
  ;;In test3 we used immediate-mode opengl, which means
  ;;vertices are sent to the CPU every time the triangle is
  ;;drawn.
  ;;Here, we send data to an intermediate buffer,
  ;;position-buffer and color-buffer.
  ;;Then we send position-buffer and color-buffer in
  ;;bulk to openGL, where it can be called many times as
  ;;a display-list or VAO
  (scratch-buffer:bind-out* ((position-buffer gl-vertex)
			     (color-buffer gl-color))
    (gl-color 1.0 0.0 0.0)
    (gl-vertex 1.0 -1.0 0.5)
    (gl-color 0.0 1.0 0.0)
    (gl-vertex 0.0 1.0 0.5)
    (gl-color 0.0 0.0 1.0)
    (gl-vertex -1.0 -1.0 0.5)))

(defun frame (shader)
  (glhelp:use-gl-program shader)
  (flet ((loop-time (n)
	    (utility:floatify (/ (fps:microseconds) n))))
    ;;Set uniforms within the shader
    (glhelp:with-uniforms uniform shader
      ;;Set the uniform referenced by :time in shader
      ;;to mix colors together.
      (gl:uniformf (uniform :time)
		   (sin (loop-time 1000000.0)))
      ;;Set the uniform referenced by :rotate in shader
      ;;to a rotation matrix that depends on time.
      (gl:uniform-matrix-4fv
       (uniform :rotate)
       (nsb-cga:rotate* 0.0 0.0 (loop-time 10000000.0)))))
  (gl:clear :color-buffer-bit
	    :depth-buffer-bit
	    :stencil-buffer-bit)
  (let ((position-buffer (scratch-buffer:my-iterator))
	(color-buffer (scratch-buffer:my-iterator)))
    ;;Draw triangle coordinates and colors to the
    ;;intermediate buffers named position-buffer and color-buffer
    (triangle position-buffer color-buffer)
    (let
	;;This becomes set to a display-list or VAO depending
	;;on the OpenGL version [as well as possible the
	;;GPU vendor]
	(drawable) 
      (scratch-buffer:flush-bind-in* ((position-buffer gl-vertex)
				      (color-buffer gl-color))
	(setf drawable
	      ;;This is a macro that generates code to
	      ;;write to either a display-list or VAO,
	      ;;depending on the OpenGL version [as well
	      ;;as possible the GPU vendor]
	      (glhelp:create-vao-or-display-list-from-specs
	       (:triangles 3)
	       ;;Attribute locations
	       ((3 (gl-color) (gl-color) (gl-color))
		(0 (gl-vertex) (gl-vertex) (gl-vertex))))))
      (glhelp:slow-draw drawable)
      ;;For the purposes of the example, delete it immediately.
      ;;Usually we'll draw it many times.
      (glhelp:slow-delete drawable))))

(defun start ()
  (application:main
   (lambda ()
     (gl:clear-color 0.5 0.5 0.5 0.0)
     (let ((shader
	    (glhelp:create-opengl-shader
	     ;;The vertex shader
	     "
uniform mat4 rotate;
out vec3 color_frag;
in vec3 position;
in vec3 color;

void main () {
gl_Position=vec4(position,1.0) * rotate;
color_frag=color;
}"
	     ;;The fragment shader
	     "
uniform float time;
in vec3 color_frag;
void main () {
vec3 color_out = mix(color_frag.zyx, color_frag.xzy,time);
gl_FragColor.xyz = color_out; 
}"
	     ;;Bind inputs to locations
	     '(("position" 0) 
	       ("color" 3))
	     ;;Unifrom name in lisp, uniform name in the shader
	     '((:time "time")
	       (:rotate "rotate")))))     
       (loop (application:poll-app)
	  (frame shader))))
   :width 512
   :height 512
   :title "4. Triangle: shaders, vaos, display-lists"))

 
(defpackage :application-example-hello-world
  (:use #:cl)
  (:export #:start))
(in-package :application-example-hello-world)

(defparameter *view* (ncurses-clone-lem-view:make-view 0 0 58 30 nil))

(defun start ()
  (application:main
   (lambda ()
     (ncurses-clone-for-lem:init)
     (loop (application:poll-app)
	(frame)))
   :width 512
   :height 512
   :title "Draw random text to the screen"))

(defun aux ()
  (alexandria:random-elt
   '("green"
     "red"
     "blue"
     "brown"
     "orange"
     "white"
     "black"
     "light blue"
     "light green")))

(defun frame ()
  (ncurses-clone-for-lem:render)
  (ncurses-clone-lem-view:redraw-view-after *view*)
  (ncurses-clone-lem-view:update-display)
  (lem.term:with-attribute (:fg (aux) :bg (aux)
				:underline
				(zerop (random 3))
				:bold
				(zerop (random 4))
				:reverse
				(zerop (random 2)))
    (ncurses-clone-lem-view:print-into-view
     *view*
     (random 50)
     (random 50)
     (prin1-to-string
      (case (random 4)
	(0 "     ")
	(1 #())
	(2 (code-char (random 2000)))
	(3 (list (random 100)))))))
  (when (window::skey-j-p (window::keyval #\))
    (application::quit)))
