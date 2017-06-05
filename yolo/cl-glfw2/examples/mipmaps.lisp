(require '#:asdf)
(asdf:oos 'asdf:load-op '#:cl-glfw)
(asdf:oos 'asdf:load-op '#:cl-glfw-opengl-version_1_0)
(asdf:oos 'asdf:load-op '#:cl-glfw-glu)

(let ((frames 0)
      t0 
      t1)
  (glfw:do-window (:title "Mipmap Demo" :width 640 :height 480)
      ((glfw:enable glfw:+sticky-keys+)
       (glfw:swap-interval 0)

       (unless (glfw:load-texture-2d (namestring (merge-pathnames "mipmaps.tga" (or *load-pathname* #P"examples/")))
				     glfw:+build-mipmaps-bit+)
	 (error "Unable to load texture!"))

       (gl:tex-parameter-i gl:+texture-2d+ gl:+texture-min-filter+ gl:+linear-mipmap-linear+)
       (gl:tex-parameter-i gl:+texture-2d+ gl:+texture-mag-filter+ gl:+linear+)

       (gl:enable gl:+texture-2d+)
       (setf t1 (glfw:get-time)
	     t0 t1))
   
    (unless (zerop (glfw:get-key glfw:+key-esc+))
      (return-from glfw:do-window))

    (setf t1 (glfw:get-time))

    (when (or (> (- t1 t0) 1.0) 
	      (zerop frames))
      (glfw:set-window-title (format nil "Mipmap Demo (~,1f FPS)" (/ frames (- t1 t0))))
      (setf frames 0)
      (setf t0 t1))

    (incf frames)

    (gl:clear-color 0.0 0.0 0.0 0.0)
    (gl:clear gl:+color-buffer-bit+)

    (destructuring-bind (width height) (glfw:get-window-size)
      (setf height (max height 1))
      (gl:viewport 0 0 width height)

      (gl:with-setup-projection 
	(glu:perspective 65.0d0 (coerce (/ width height) 'double-float) 1.0d0 50.0d0))
      (gl:load-identity)
      (glu:look-at 0.0d0  3.0d0 -20.0d0
		   0.0d0 -4.0d0 -11.0d0
		   0.0d0  1.0d0   0.0d0))
    
      
    (destructuring-bind (x y) (glfw:get-mouse-pos)
      (declare (ignore y))
      (gl:rotate-f (+ (* x 0.05)
		      (* t1 5.0))
		   0 1 0))

    (gl:with-begin gl:+quads+
      (gl:tex-coord-2f -20.0  20.0) (gl:vertex-3f -50.0 0.0 -50.0)
      (gl:tex-coord-2f  20.0  20.0) (gl:vertex-3f  50.0 0.0 -50.0)
      (gl:tex-coord-2f  20.0 -20.0) (gl:vertex-3f  50.0 0.0  50.0)
      (gl:tex-coord-2f -20.0 -20.0) (gl:vertex-3f -50.0 0.0  50.0))))
