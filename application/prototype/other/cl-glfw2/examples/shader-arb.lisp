(require '#:asdf)
(asdf:oos 'asdf:load-op '#:cl-glfw)
(asdf:oos 'asdf:load-op '#:cl-glfw-opengl-version_1_1)
(asdf:oos 'asdf:load-op '#:cl-glfw-glu)

(defparameter *shader-program* nil)
(defparameter *uniform-time* nil)

(glfw:do-window (:title "An ARB Extension Shader Example")
    ((gl:with-setup-projection
       (glu:perspective 45 4/3 0.1 50))
     (when (and (gl-ext:load-extension "ARB_shader_objects")
		(gl-ext:load-extension "ARB_vertex_shader")
		(gl-ext:load-extension "ARB_fragment_shader"))
       (setf *shader-program*
	     (gl:make-program-arb
	      (gl:make-shader-arb gl:+vertex-shader-arb+ "
varying vec3 colour;
void main()
{
  colour = gl_Color.rgb;
  gl_Position = ftransform();
}
")
	      (gl:make-shader-arb gl:+fragment-shader-arb+ "
uniform float time;
varying vec3 colour;
const float pi2=2.0*3.14159265;
void main()
{
  gl_FragColor = vec4(pow(sin(colour.r*pi2*4.0+mod(time*8.0,pi2)),2.0),
	              pow(sin(colour.g*pi2*4.0+mod(time*8.0,pi2)),2.0),
	              pow(sin(colour.b*pi2*4.0+mod(time*8.0,pi2)),2.0),
		      1.0);
}
"))))
     (gl:use-program-object-arb *shader-program*)
     (setf *uniform-time* (gl:get-uniform-location-arb *shader-program* "time")))
  (gl:clear gl:+color-buffer-bit+)
  (gl:load-identity)
  (gl:translate-f 0 0 -5)
  (gl:rotate-d (* 10 (glfw:get-time)) 1 1 0)
  (gl:rotate-d (* 90 (glfw:get-time)) 0 0 1)
  (gl:uniform-1f-arb *uniform-time* (glfw:get-time))
  (gl:with-begin gl:+triangles+
    (gl:color-3f 1 0 0) (gl:vertex-3f  1  0 0)
    (gl:color-3f 0 1 0) (gl:vertex-3f -1  1 0)
    (gl:color-3f 0 0 1) (gl:vertex-3f -1 -1 0)))