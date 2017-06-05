(require '#:asdf)
(asdf:oos 'asdf:load-op '#:cl-glfw)
(asdf:oos 'asdf:load-op '#:cl-glfw-opengl-version_1_0)

(defparameter *key-repeat* nil)
(defparameter *system-keys* t)

(defun keyfun (key action) 
  (cond 
    ((not (eql action glfw:+press+)))

    ((eql key glfw:+key-esc+)
     (format t "ESC => quit program~%")
     (glfw:close-window))

    ((eql key #\R)
     (setf *key-repeat* (not *key-repeat*))
     (funcall (if *key-repeat* #'glfw:enable #'glfw:disable) glfw:+key-repeat+)
     (format t "R => Key repeat: ~s~%" (if *key-repeat* "ON" "OFF")))

    ((eql key #\S)
     (setf *system-keys* (not *system-keys*))
     (funcall (if *system-keys* #'glfw:enable #'glfw:disable) glfw:+system-keys+)
     (format t "S => System keys: ~s~%" (if *system-keys* "ON" "OFF")))

    ((characterp key)
     (format t "character ~s~%" key))

    ((symbolp key)
     (format t "symbol ~s~%" key))

    (t 
     ;; shouldn't ever be called
     (format t "??? ~s~%" key)))
  (force-output))

(glfw:do-window (:title "Press some keys!" :width 250 :height 100)
    ((glfw:set-key-callback 'keyfun))

  (destructuring-bind (width height) (glfw:get-window-size)
    (gl:viewport 0 0 width height))

  (gl:clear-color (+ 0.5 (* 0.5 (sin (* 3 (glfw:get-time)))))
		  0 0 0)
  (gl:clear gl:+color-buffer-bit+)

  (sleep 0.04))