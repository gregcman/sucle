(progn (require '#:asdf)
       (asdf:oos 'asdf:load-op '#:cl-glfw)
       (asdf:oos 'asdf:load-op '#:cl-glfw-opengl-version_1_0)
       (asdf:oos 'asdf:load-op '#:cl-glfw-glu))

(defun test ()
  (let ((frames 0)
	t0 t1)
    (block nil
      (glfw:do-window (:title "Spinning Triangle")
	  ((glfw:enable glfw:+sticky-keys+)
	   (glfw:swap-interval 0)
	   (setf t0 (glfw:get-time)
		 t1 (glfw:get-time)))    
	(when (eql (glfw:get-key glfw:+key-esc+) glfw:+press+)
	  (return))

	(setf t1 (glfw:get-time))

	(when (or (> (- t1 t0) 1)
		  (= frames 0))
	  (glfw:set-window-title (format nil "Spinning Triangle (~,1f FPS)" (/ frames (- t1 t0))))
	  (setf frames 0
		t0 t1))

	(incf frames)

	(destructuring-bind (width height) (glfw:get-window-size)
	  (setf height (max height 1))
	  (gl:viewport 0 0 width height)

	  (gl:clear-color 0 0 0 0)
	  (gl:clear gl:+color-buffer-bit+)

	  (gl:matrix-mode gl:+projection+)
	  (gl:load-identity)
	  (glu:perspective 65 (/ width height) 1 100)
	  (gl:matrix-mode gl:+modelview+)
	  (gl:load-identity)
	  (glu:look-at 0  1 0
		       0 20 0
		       0  0 1)

	  (gl:translate-f 0 14 0)


	  (destructuring-bind (x y) (glfw:get-mouse-pos)
	    (declare (ignore y))
	    (gl:rotate-f (+ (* x 0.3)
			    (* t1 100))
			 0 0 1))

	  (gl:with-begin gl:+triangles+
	    (gl:color-3f 1 0 0) (gl:vertex-3f -5 0 -4)
	    (gl:color-3f 0 1 0) (gl:vertex-3f  5 0 -4)
	    (gl:color-3f 0 0 1) (gl:vertex-3f  0 0  6)))))))
