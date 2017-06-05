(require '#:asdf)
(asdf:oos 'asdf:load-op '#:cl-glfw)
(asdf:oos 'asdf:load-op '#:cl-glfw-opengl-version_1_1)
(asdf:oos 'asdf:load-op '#:cl-glfw-glu)

(declaim (optimize (debug 3)))

;; This example uses the gl:synchronizing-program-arb mechanism to reload+recompile+relink shader files on-file-change

(glfw:do-window (:title "An ARB Synchronized Shader Example")
    ((gl:with-setup-projection
       (glu:perspective 45 4/3 0.1 50))
     (unless  (and (gl-ext:load-extension "ARB_shader_objects")
		   (gl-ext:load-extension "ARB_vertex_shader")
		   (gl-ext:load-extension "ARB_fragment_shader"))
       (error "I need ARB_vertex_shader and ARB_fragment_shader for this example. Sorry :(")))
  (gl:clear gl:+color-buffer-bit+)
  (gl:load-identity)
  (gl:translate-f 0 0 -5)
  (gl:rotate-d (* 10 (glfw:get-time)) 1 1 0)
  (gl:rotate-d (* 90 (glfw:get-time)) 0 0 1)
  (let ((program (gl:synchronizing-program-arb 'synchronized-shader
		   (list gl:+vertex-shader-arb+ (merge-pathnames "synchronized-shader.vert" *load-truename*))
		   (list gl:+fragment-shader-arb+ (merge-pathnames "synchronized-shader.frag" *load-truename*)))))
    (gl:with-use-program-arb program
      (gl:uniform-1f-arb (gl:get-uniform-location-arb program "time") (glfw:get-time))
      (gl:with-begin gl:+triangles+
	(gl:color-3f 1 0 0) (gl:vertex-3f  1  0 0)
	(gl:color-3f 0 1 0) (gl:vertex-3f -1  1 0)
	(gl:color-3f 0 0 1) (gl:vertex-3f -1 -1 0)))))