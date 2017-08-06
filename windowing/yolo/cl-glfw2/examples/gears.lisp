(require '#:asdf)
(asdf:oos 'asdf:load-op '#:cl-glfw)
(asdf:oos 'asdf:load-op '#:cl-glfw-opengl-version_1_0)

(defparameter *autoexit* 0)
(defparameter *view-rotx* 20)
(defparameter *view-roty* 30)
(defparameter *view-rotz* 0)
(defparameter *gear1* nil)
(defparameter *gear2* nil)
(defparameter *gear3* nil)

(defun gear (inner-radius outer-radius width teeth tooth-depth)
  (let ((r0 inner-radius)
	(r1 (- outer-radius (/ tooth-depth 2)))
	(r2 (+ outer-radius (/ tooth-depth 2)))
	(angle 0)
	(da (/ (/ (* 2 pi) teeth) 4)))
    (gl:shade-model gl:+flat+)
    (gl:normal-3f 0 0 1)

    ;; draw front face
    (gl:with-begin gl:+quad-strip+
      (dotimes (i (1+ teeth))
	(setf angle (/ (* i 2 pi) teeth))
	(gl:vertex-3f (* r0 (cos angle)) (* r0 (sin angle)) (* width 0.5))
	(gl:vertex-3f (* r1 (cos angle)) (* r1 (sin angle)) (* width 0.5))
	(when (< i teeth)
	  (gl:vertex-3f (* r0 (cos angle)) (* r0 (sin angle)) (* width 0.5))
	  (gl:vertex-3f (* r1 (cos (+ angle (* 3 da))))
			(* r1 (sin (+ angle (* 3 da))))
			(* width 0.5)))))

    ;; draw front sides of teeth
    (gl:with-begin gl:+quads+
      (dotimes (i teeth)
	(setf angle (/ (* i 2 pi) teeth))
	(gl:vertex-3f (* r1 (cos angle)) (* r1 (sin angle)) (* width 0.5))
	(gl:vertex-3f (* r2 (cos (+ angle da))) (* r2 (sin (+ angle da))) (* width 0.5))
	(gl:vertex-3f (* r2 (cos (+ angle (* 2 da)))) (* r2 (sin (+ angle (* 2 da)))) (* width 0.5))
	(gl:vertex-3f (* r1 (cos (+ angle (* 3 da)))) (* r1 (sin (+ angle (* 3 da)))) (* width 0.5))))

    (gl:normal-3f 0 0 -1)

    ;; draw back face
    (gl:with-begin gl:+quad-strip+
      (dotimes (i (1+ teeth))
	(setf angle (/ (* i 2 pi) teeth))
	(gl:vertex-3f (* r1 (cos angle)) (* r1 (sin angle)) (* (- width) 0.5))
	(gl:vertex-3f (* r0 (cos angle)) (* r0 (sin angle)) (* (- width) 0.5))
	(when (< i teeth)
	  (gl:vertex-3f (* r1 (cos (+ angle (* 3 da)))) (* r1 (sin (+ angle (* 3 da)))) (* (- width) 0.5))
	  (gl:vertex-3f (* r0 (cos angle)) (* r0 (sin angle)) (* (- width) 0.5)))))

    ;; draw back sides of teeth
    (gl:with-begin gl:+quads+
      (dotimes (i teeth)
	(setf angle (/ (* i 2 pi) teeth))
	(gl:vertex-3f (* r1 (cos (+ angle (* 3 da)))) (* r1 (sin (+ angle (* 3 da)))) (* (- width) 0.5))
	(gl:vertex-3f (* r2 (cos (+ angle (* 2 da)))) (* r2 (sin (+ angle (* 2 da)))) (* (- width) 0.5))
	(gl:vertex-3f (* r2 (cos (+ angle da))) (* r2 (sin (+ angle da))) (* (- width) 0.5))
	(gl:vertex-3f (* r1 (cos angle)) (* r1 (sin angle)) (* (- width) 0.5))))

    ;; draw outward faces of teeth
    (gl:with-begin gl:+quad-strip+
      (dotimes (i teeth)
	(setf angle (/ (* i 2 pi) teeth))
	(gl:vertex-3f (* r1 (cos angle)) (* r1 (sin angle)) (* width 0.5))
	(gl:vertex-3f (* r1 (cos angle)) (* r1 (sin angle)) (* (- width) 0.5))
	(let* ((u (- (* r2 (cos (+ angle da))) (* r1 (cos angle))))
	       (v (- (* r2 (sin (+ angle da))) (* r1 (sin angle))))
	       (len (sqrt (+ (* u u) (* v v)))))

	  (setf u (/ u len))
	  (setf v (/ v len))
	  (gl:normal-3f v (- u) 0)
	  (gl:vertex-3f (* r2 (cos (+ angle da))) (* r2 (sin (+ angle da))) (* width 0.5))
	  (gl:vertex-3f (* r2 (cos (+ angle da))) (* r2 (sin (+ angle da))) (* (- width) 0.5))
	  (gl:normal-3f (cos angle) (sin angle) 0)
	  (gl:vertex-3f (* r2 (cos (+ angle (* 2 da)))) (* r2 (sin (+ angle (* 2 da)))) (* width 0.5))
	  (gl:vertex-3f (* r2 (cos (+ angle (* 2 da)))) (* r2 (sin (+ angle (* 2 da)))) (* (- width) 0.5))
	  (setf u (- (* r1 (cos (+ angle (* 3 da)))) (* r2 (cos (+ angle (* 2 da))))))
	  (setf v (- (* r1 (sin (+ angle (* 3 da)))) (* r2 (sin (+ angle (* 2 da))))))
	  (gl:normal-3f v (- u) 0)
	  (gl:vertex-3f (* r1 (cos (+ angle (* 3 da)))) (* r1 (sin (+ angle (* 3 da)))) (* width 0.5))
	  (gl:vertex-3f (* r1 (cos (+ angle (* 3 da)))) (* r1 (sin (+ angle (* 3 da)))) (* (- width) 0.5))
	  (gl:normal-3f (cos angle) (sin angle) 0)))

      (gl:vertex-3f (* r1 (cos 0)) (* r1 (sin 0)) (* width 0.5))
      (gl:vertex-3f (* r1 (cos 0)) (* r1 (sin 0)) (* (- width) 0.5)))

    (gl:shade-model gl:+smooth+)

    ;; draw inside radius cylinder */
    (gl:with-begin gl:+quad-strip+
      (dotimes (i (1+ teeth))
	(setf angle (/ (* i 2 pi) teeth))
	(gl:normal-3f (- (cos angle)) (- (sin angle)) 0)
	(gl:vertex-3f (* r0 (cos angle)) (* r0 (sin angle)) (* (- width) 0.5))
	(gl:vertex-3f (* r0 (cos angle)) (* r0 (sin angle)) (* width 0.5))))))

(let ((frames 0)
      (angle 0)
      (t-last-report 0)
      (t0 0)
      (t1 0))

  (defun report-fps ()
    (incf frames)
    (setf t0 t1
	  t1 (glfw:get-time))
    (when (>= (- t1 t-last-report) 5)
      (let* ((seconds (- t1 t-last-report))
	     (fps (/ frames seconds)))
	(format t "~d frames in ~3$ seconds = ~3$ FPS~%" frames seconds fps))
      (setf t-last-report t1
	    frames 0))
    (when (and (not (zerop *autoexit*))
	       (>= t1 (* 0.999 *autoexit*)))
      (glfw:close-window)))
  
  (defun draw ()
    (gl:clear (logior gl:+color-buffer-bit+ gl:+depth-buffer-bit+))
    (gl:with-push-matrix
      (gl:rotate-f *view-rotx* 1 0 0)
      (gl:rotate-f *view-roty* 0 1 0)
      (gl:rotate-f *view-rotz* 0 0 1)

      (gl:with-push-matrix
	(gl:translate-f -3 -2 0)
	(gl:rotate-f angle 0 0 1)
	(gl:call-list *gear1*))

      (gl:with-push-matrix
	(gl:translate-f 3.1 -2 0)
	(gl:rotate-f (- (* -2 angle) 9) 0 0 1)
	(gl:call-list *gear2*))

      (gl:with-push-matrix
	(gl:translate-f -3.1 4.2 0)
	(gl:rotate-f (- (* -2 angle) 25) 0 0 1)
	(gl:call-list *gear3*)))
    (report-fps))


  (defun animate()
    (setf angle (coerce (+ angle (* 100 (- t1 t0))) 'single-float))))

(defun init-gl ()
  (gl:enable gl:+cull-face+)
  (gl:enable gl:+lighting+)
  (gl:enable gl:+light0+)
  (gl:enable gl:+depth-test+)

  (gl:light-fv gl:+light0+ gl:+position+ #(5.0 5.0 10.0 0.0))

  ;; make the gears
  (gl:with-new-list (setf *gear1* (gl:gen-lists 1)) gl:+compile+
    (gl:material-fv gl:+front+ gl:+ambient-and-diffuse+ #(0.8 0.1 0.0 1.0))
    (gear 1 4 1 20 0.7))

  (gl:with-new-list (setf *gear2* (gl:gen-lists 1)) gl:+compile+
    (gl:material-fv gl:+front+ gl:+ambient-and-diffuse+ #(0.0 0.8 0.2 1.0))
    (gear 0.5 2 2 10 0.7))

  (gl:with-new-list (setf *gear3* (gl:gen-lists 1)) gl:+compile+
    (gl:material-fv gl:+front+ gl:+ambient-and-diffuse+ #(0.2 0.2 1.0 1.0))
    (gear 1.3 2 0.5 10 0.7)) 
    
  (gl:enable gl:+normalize+)

  ;; did we get -info or -exit?
  (dolist (arg (or #+sbcl sb-ext:*posix-argv*
		   #+lispworks system:*line-arguments-list*
		   #+cmu extensions:*command-line-words*
		   nil))
    (cond ((string= arg "-info")
	   (format t "GL_RENDERER   = ~s~%GL_VERSION    = ~s~%GL_VENDOR     = ~s~%GL_EXTENSIONS = ~s~%"
		   (gl:get-string gl:+renderer+)
		   (gl:get-string gl:+version+)
		   (gl:get-string gl:+vendor+)
		   (gl:get-string gl:+extensions+)))
	  ((string= arg "-exit")
	   (setf *autoexit* 30)
	   (format t "Auto Exit after ~d seconds.~%" *autoexit*)))))

(defun key-callback (key action)
  (when (eql action glfw:+press+)
    (case key
      (#\Z (if (eql (glfw:get-key glfw:+key-lshift+) glfw:+press+)
	       (decf *view-rotz* 5)
	       (incf *view-rotz* 5)))    
      (:esc   (glfw:close-window))
      (:up    (incf *view-rotx* 5))
      (:down  (decf *view-rotx* 5))
      (:left  (incf *view-roty* 5))
      (:right (decf *view-roty* 5)))))

(defun window-size-callback (width height)
  (let* ((h (/ height width))
	 (znear 5)
	 (zfar 30)
	 (xmax (* znear 0.5)))

    (gl:viewport 0 0 width height)
    (gl:with-setup-projection 
      (gl:frustum (- xmax) xmax (* (- xmax) h) (* xmax h) znear zfar))

    (gl:load-identity)
    (gl:translate-f 0 0 -20)))

;; program loop
(defun test ()
  (glfw:do-window (:title "Gears" :width 300 :height 300 :depthbits 16 :mode glfw:+window+) 
      ((glfw:enable glfw:+key-repeat+)
       (glfw:swap-interval 0)
       (init-gl)
       (glfw:set-window-size-callback 'window-size-callback)
       (glfw:set-key-callback 'key-callback))
    (draw)
    (animate)
    (glfw:swap-buffers)))
