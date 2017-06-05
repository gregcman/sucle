(require '#:asdf)
(asdf:oos 'asdf:load-op '#:cl-glfw)
(asdf:oos 'asdf:load-op '#:cl-glfw-opengl-version_1_1)
(asdf:oos 'asdf:load-op '#:cl-glfw-glu)

(defparameter *use-vbo* t)

(defconstant +pi+ (coerce pi 'single-float))
(defconstant +2pi+ (* +pi+ 2))
(defconstant +pi/2+ (/ +pi+ 2))
(defconstant +pi/4+ (/ +pi+ 4))
(defconstant +pi/8+ (/ +pi+ 8))
(defconstant +pi/16+ (/ +pi+ 16))
(defconstant +pi/32+ (/ +pi+ 32))
(defconstant +pi/64+ (/ +pi+ 64))
(defconstant +pi/128+ (/ +pi+ 128))

(defparameter *vertices-vbo* nil)
(defparameter *colours-vbo* nil)
(defparameter *normals-vbo* nil)
(defparameter *triangle-indices-vbo* nil)

(defparameter *triangle-indices*  nil)
(defparameter *colours-array*     nil)
(defparameter *normals-array*     nil)
(defparameter *vertices-array*    nil)


(let ((segments 0)
      (slices 0)
      (step +pi/64+)) ;; change this to change the detail of the sphere
  (loop for phi from (- step +pi/2+) upto (- +pi/2+ step) by step
     for y from 0 do
     (incf slices)
     (loop for theta from 0.0 to +2pi+ by step
	for x from 0 do
	(setf segments x)
	(let* ((theta (+ theta (if (oddp y) (/ step 2) 0.0)))
	       (v (list (* (cos phi) (cos theta))
			(* (cos phi) (sin theta))
			(sin phi)))
	       (norm (sqrt (reduce #'+ (mapcar #'* v v))))
	       (normal (mapcar #'(lambda (e) (/ e norm))
			       v)))
	  (setf *colours-array*  (nconc *colours-array* (list (+ 0.5 (/ phi +pi+))
							      (/ theta +2pi+)
							      1.0 1.0)))
	  (setf *normals-array*  (nconc *normals-array* normal))
	  (setf *vertices-array* (nconc *vertices-array* v)))))
  (format t "~a slices~%~a segments~%" slices segments)
  (nconc *normals-array* (list 0.0 0.0 -1.0 0.0 0.0 1.0))
  (nconc *vertices-array* (list 0.0 0.0 -1.0 0.0 0.0 1.0))
  (nconc *colours-array* (list 0.0 0.5 1.0 1.0) (list 1.0 0.5 1.0 1.0))
  (setf *triangle-indices*
	(nconc
	 (loop for x upto segments nconcing 
	      (list x
		    (- (/ (length *vertices-array*) 3) 2) 
		    (mod (1+ x) segments)))
	 
	 (loop for y below (1- slices) nconcing
	      (loop for x below segments nconcing
		   (let ((v00 (+ x                     (* y (1+ segments))))
			 (v01 (+ (mod (1+ x) segments) (* y (1+ segments))))
			 (v10 (+ x                     (* (1+ y) (1+ segments))))
			 (v11 (+ (mod (1+ x) segments) (* (1+ y) (1+ segments)))))
		     (if (evenp y)
			 (list v00 v01 v10 v10 v01 v11)
			 (list v10 v00 v11 v00 v01 v11)))))
	 (loop for x upto segments nconcing 
	      (list (- (/ (length *vertices-array*) 3) 1)
		    (+ x
		       (* (1- slices) (1+ segments)))
		    (+ (mod (1+ x) segments)
		       (* (1- slices) (1+ segments))))))))

(defparameter *triangle-indices-length* (length *triangle-indices*))
(defparameter *vertices-array-length* (length *vertices-array*))

(defparameter *t0* 0.0)
(defparameter *frames* 0)

(cffi:defcallback key-press :void ((key :int) (action :int))
  (when (and (= action glfw:+press+) (= key (char-code #\V)))
    (setf *use-vbo* (and (not *use-vbo*)
			 (gl-ext:extension-available-p "ARB_vertex_buffer_object"))
	  *t0* (glfw:get-time)
	  *frames* 0)
    (glfw:set-window-title (format nil "VBO: ~a~%" (if *use-vbo* "on" "off")))))


(setf *triangle-indices* (cffi:foreign-alloc 'gl:uint :initial-contents *triangle-indices*)
      *colours-array*    (cffi:foreign-alloc 'gl:float :initial-contents *colours-array*)
      *normals-array*    (cffi:foreign-alloc 'gl:float :initial-contents *normals-array*)
      *vertices-array*   (cffi:foreign-alloc 'gl:float :initial-contents *vertices-array*))

(glfw:do-window (:title "A VBO Example" :depthbits 16)
    ((gl:enable gl:+depth-test+)
     (gl:depth-func gl:+less+)
     (gl:enable gl:+light0+)
     (gl:enable gl:+lighting+)

     (gl:light-fv gl:+light0+ gl:+position+ #(1.0 1.0 1.0 0.0))
     (gl:color-material gl:+front+ gl:+ambient-and-diffuse+)
     (gl:enable gl:+color-material+)

     (glfw:set-key-callback (cffi:callback key-press))

     (gl:with-setup-projection
       (glu:perspective 45 4/3 0.125 8))

     (when (setf *use-vbo* (and t (gl-ext:load-extension "ARB_vertex_buffer_object")))
       (let ((buffers (make-array 4)))
	 (gl:gen-buffers-arb 4 buffers)
	 (setf *vertices-vbo* (elt buffers 0)
	       *normals-vbo* (elt buffers 1)
	       *colours-vbo* (elt buffers 2)
	       *triangle-indices-vbo* (elt buffers 3)))
       (format t "Loading in ~d bytes of indices~%" (* *triangle-indices-length* (cffi:foreign-type-size 'gl:uint)) )
       (gl:with-bind-buffer-arb (gl:+element-array-buffer-arb+ *triangle-indices-vbo*)
	 (gl:buffer-data-arb gl:+element-array-buffer-arb+
			     (* *triangle-indices-length* (cffi:foreign-type-size 'gl:uint)) 
			     *triangle-indices*
			     gl:+static-draw-arb+))

       (format t "Loading in ~d bytes of vertices~%" (* *vertices-array-length* (cffi:foreign-type-size 'gl:float)) )
       (gl:with-bind-buffer-arb (gl:+array-buffer-arb+ *vertices-vbo*)
	 (gl:buffer-data-arb gl:+array-buffer-arb+
			     (* *vertices-array-length* (cffi:foreign-type-size 'gl:float))
			     *vertices-array*
			     gl:+static-draw-arb+))

       (format t "Loading in ~d bytes of normals~%" (* *vertices-array-length* (cffi:foreign-type-size 'gl:float)) )
       (gl:with-bind-buffer-arb (gl:+array-buffer-arb+ *normals-vbo*)
	 (gl:buffer-data-arb gl:+array-buffer-arb+
			     (* *vertices-array-length* (cffi:foreign-type-size 'gl:float))
			     *normals-array*
			     gl:+static-draw-arb+))

       (format t "Loading in ~d bytes of colours~%" (* *vertices-array-length* 4/3 (cffi:foreign-type-size 'gl:float)) )
       (gl:with-bind-buffer-arb (gl:+array-buffer-arb+ *colours-vbo*)
	 (gl:buffer-data-arb gl:+array-buffer-arb+
			     (* *vertices-array-length* 4/3 (cffi:foreign-type-size 'gl:float))
			     *colours-array*
			     gl:+static-draw-arb+)))
     (setf *t0* (glfw:get-time)))
  
  (let ((t1 (glfw:get-time)))
    (when (> (- t1 *t0*) 1)
      (glfw:set-window-title (format nil "~4f FPS, VBO: ~a~%" (/ *frames* (- t1 *t0*)) (if *use-vbo* "on" "off")))
      (setf *t0* t1
	    *frames* 0)))
  (gl:clear (logior gl:+color-buffer-bit+ gl:+depth-buffer-bit+))
  (gl:load-identity)
  (gl:translate-f 0 0 -5)
  (gl:rotate-d (* 10 (glfw:get-time)) 1 1 0)
  (gl:rotate-d (* 90 (glfw:get-time)) 0 0 1)
  (gl:with-push-client-attrib (gl:+client-vertex-array-bit+)
    (gl:enable-client-state gl:+color-array+)
    (gl:enable-client-state gl:+vertex-array+)
    (gl:enable-client-state gl:+normal-array+)
    (if *use-vbo*
	(progn
	  (gl:with-bind-buffer-arb (gl:+array-buffer-arb+ *colours-vbo*)
	    (gl:color-pointer 4 gl:+float+ 0 (cffi:make-pointer 0)))
	  
	  (gl:with-bind-buffer-arb (gl:+array-buffer-arb+ *normals-vbo*)
	    (gl:normal-pointer gl:+float+ 0 (cffi:make-pointer 0)))

	  (gl:with-bind-buffer-arb (gl:+array-buffer-arb+ *vertices-vbo*)
	    (gl:vertex-pointer 3 gl:+float+ 0 (cffi:make-pointer 0)))

	  (gl:with-bind-buffer-arb (gl:+element-array-buffer-arb+ *triangle-indices-vbo*)
	    (gl:draw-elements gl:+triangles+ *triangle-indices-length* gl:+unsigned-int+ (cffi:make-pointer 0))))
	(progn
	  (gl:color-pointer 4 gl:+float+ 0 *colours-array*)
	  (gl:normal-pointer gl:+float+ 0 *normals-array*)
	  (gl:vertex-pointer 3 gl:+float+ 0 *vertices-array*)
	  (gl:draw-elements gl:+triangles+ *triangle-indices-length* gl:+unsigned-int+ *triangle-indices*))))
  (incf *frames*))