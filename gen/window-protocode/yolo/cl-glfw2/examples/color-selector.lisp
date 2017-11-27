#|
  OpenGL color selector.
  Show 3 color planes (+x=red, +y=green, and +z=blue);
  select the color at their intersection.
|#
(require '#:asdf)
(asdf:oos 'asdf:load-op '#:cl-glfw)
(asdf:oos 'asdf:load-op '#:cl-glfw-opengl-version_1_0)
(asdf:oos 'asdf:load-op '#:cl-glfw-glu)

;;;; The viewer
(defparameter *red* 0)
(defparameter *green* 0)
(defparameter *blue* 0)

(defun key-callback (key action)
  (when (eql action glfw:+press+)
    (case key
      (#\Z 
       (if (eql (glfw:get-key glfw:+key-lshift+) glfw:+press+)
           (decf *blue*)
           (incf *blue*)))
      (:esc   (glfw:close-window))
      (:up    (incf *red*))
      (:down  (decf *red*))
      (:left  (decf *green*))
      (:right (incf *green*)))))

(defun color-selector ()
 (let ((frames 0)
      t0 t1)
   (setf *red* 0
         *green* 0
         *blue* 0)
  (glfw:do-window (:title "Color Selector" :width 640 :height 480)
      ((glfw:enable glfw:+sticky-keys+)
       (glfw:enable glfw:+key-repeat+)
       (gl:disable gl:+cull-face+)
       (gl:enable gl:+depth-test+)
       (gl:depth-mask gl:+true+)
       (glfw:swap-interval 0)
       (glfw:set-key-callback 'key-callback)
       (setf t0 (glfw:get-time)
	     t1 (glfw:get-time)))    

    (when (eql (glfw:get-key glfw:+key-esc+) glfw:+press+)
      (return-from glfw:do-window))

    (setf t1 (glfw:get-time))

    (when (> (- t1 t0) 1) 
      (glfw:set-window-title (format nil "Color Selector (~,1f FPS)" (/ frames (- t1 t0))))
      (setf frames 0
	    t0 t1))

    (incf frames)
    
    (destructuring-bind (width height) (glfw:get-window-size)
      (setf height (max height 1))
      (gl:viewport 0 0 width height)

      (gl:clear-color 0 0 0 0)
      (gl:clear (logior gl:+color-buffer-bit+
                        gl:+depth-buffer-bit+))

      (gl:matrix-mode gl:+projection+)
      (gl:load-identity)
      (glu:perspective 65 (/ width height) 1 100)
      (gl:matrix-mode gl:+modelview+)
      (gl:load-identity)
      (glu:look-at 0  1 0
		   0 20 0
		   0  0 1)
      
      (gl:translate-f 0 14 0)
      
      (gl:with-push-matrix
        (gl:rotate-f 20 1 0 0)
        (gl:rotate-f 0 0 1 0)
        (gl:rotate-f -135 0 0 1)
        (gl:scale-f 5 5 5)

        (gl:with-begin gl:+quads+
          (flet ((show (r g b)
                   (gl:color-3f r g b)
                   (gl:vertex-3f r g b)))
            (macrolet ((bound (axis)
                         `(cond ((< ,axis 0) (setf ,axis 0) 0)
                                ((> ,axis 255) (setf ,axis 255) 1)
                                (t (/ ,axis 255)))))
              (let ((red (bound *red*))
                    (green (bound *green*))
                    (blue (bound *blue*)))
                ;; red (green-blue plane)
                (show red 0 0)
                (show red 0 1)
                (show red 1 1)
                (show red 1 0)
                ;; green (blue-red plane)
                (show 0 green 0)
                (show 0 green 1)
                (show 1 green 1)
                (show 1 green 0)
                ;; blue (red-green plane)
                (show 0 0 blue)
                (show 0 1 blue)
                (show 1 1 blue)
                (show 1 0 blue))))))))))

(color-selector)
