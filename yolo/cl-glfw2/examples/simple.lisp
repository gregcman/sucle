
(glfw:do-window (:title "A Simple Example")
    ((gl:with-setup-projection
       (glu:perspective 45 4/3 0.1 50)))
  (gl:clear gl:+color-buffer-bit+)
  (gl:load-identity)
  (gl:translate-f 0 0 -5)
  (gl:rotate-f (* 10 (glfw:get-time)) 1 1 0)
  (gl:rotate-f (* 90 (glfw:get-time)) 0 0 1)
  (gl:with-begin gl:+triangles+
    (gl:color-3f 1 0 0) (gl:vertex-3f  1  0 0)
    (gl:color-3f 0 1 0) (gl:vertex-3f -1  1 0)
    (gl:color-3f 0 0 1) (gl:vertex-3f -1 -1 0)))