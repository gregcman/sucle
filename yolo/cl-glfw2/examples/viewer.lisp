(require '#:asdf)
(asdf:oos 'asdf:load-op '#:cl-glfw)
(asdf:oos 'asdf:load-op '#:cl-glfw-opengl-version_1_0)
(asdf:oos 'asdf:load-op '#:cl-glfw-glu)

(defstruct gl-object
  (type)
  (position)
  (normal)
  (color)
  (texture)
  ;; (display-list) ; compiled
  )

(defun gl-geometry-type (keyword)
  "Enumeration of the OpenGL geometric object types"
  (ecase keyword
    (:points gl:+points+)
    (:lines gl:+lines+)
    (:line-loop gl:+line-loop+)
    (:line-strip gl:+line-strip+)
    (:triangles gl:+triangles+)
    (:triangle-strip gl:+triangle-strip+)
    (:triangle-fan gl:+triangle-fan+)
    (:quads gl:+quads+)
    (:quad-strip gl:+quad-strip+)
    (:polygon gl:+polygon+)))

(defun render-gl-object (obj)
  "Display an object"
  (gl:with-begin (gl-geometry-type (gl-object-type obj))
    (let ((position (gl-object-position obj))
          (color (gl-object-color obj)))
      (if color
          (dotimes (row (array-dimension position 0))
            (gl:color-3f (aref color row 0)
                         (aref color row 1)
                         (aref color row 2))
            (gl:vertex-3f (aref position row 0)
                          (aref position row 1)
                          (aref position row 2)))
          (dotimes (row (array-dimension position 0))
            (gl:vertex-3f (aref position row 0)
                          (aref position row 1)
                          (aref position row 2)))))))
(defun tricolor (obj)
  "set alternating vertices to red,green,blue"
  (let* ((position (gl-object-position obj))
         (color (make-array (array-dimensions position))))
    (dotimes (row (array-dimension color 0))
      (case (mod row 3)
        (0 (setf (aref color row 0) 1
                 (aref color row 1) 0
                 (aref color row 2) 0))
        (1 (setf (aref color row 0) 0
                 (aref color row 1) 1
                 (aref color row 2) 0))
        (2 (setf (aref color row 0) 0
                 (aref color row 1) 0
                 (aref color row 2) 1))))
    (setf (gl-object-color obj) color)))

;;;; Platonic solids
(defparameter *tetrahedron*
  (make-gl-object
   :type :triangle-strip
   :position (make-array '(6 3)
                         :initial-contents '((0 0 1)
                                             (0 1 0)
                                             (1 0 0)
                                             (1 1 1)
                                             (0 0 1)
                                             (0 1 0)))))
(tricolor *tetrahedron*)
                                             
(defparameter *cube*
  (make-gl-object
   :type :triangle-strip
   :position (make-array '(14 3)
                         :initial-contents '((0 0 0)
                                             (0 0 1)
                                             (0 1 0)
                                             (0 1 1)
                                             (1 1 1)
                                             (0 0 1)
                                             (1 0 1)
                                             (0 0 0)
                                             (1 0 0)
                                             (0 1 0)
                                             (1 1 0)
                                             (1 1 1)
                                             (1 0 0)
                                             (1 0 1)))))
(tricolor *cube*)

(defparameter *cube-points*
  (make-gl-object
   :type :points
   :position (make-array '(8 3)
                         :initial-contents '((0 0 0)
                                             (0 0 1)
                                             (0 1 0)
                                             (0 1 1)
                                             (1 0 0)
                                             (1 0 1)
                                             (1 1 0)
                                             (1 1 1)))))

(defparameter *octahedron*
  (make-gl-object
   :type :triangle-strip
   :position (make-array '(12 3)
                         :initial-contents '((0 0 1)
                                             (1 0 0)
                                             (0 1 0)
                                             (0 0 -1)
                                             (0 1 0)
                                             (-1 0 0)
                                             (0 0 1)
                                             (0 -1 0)
                                             (1 0 0)
                                             (0 -1 0)
                                             (0 0 -1)
                                             (-1 0 0)))))
(tricolor *octahedron*)

;; dodecahedron


(defparameter *icosahedron*
  (let* ((phi (/ (+ 1 (sqrt 5))
                 2))
         (-phi (- phi))
         (0++ `(0 1 ,phi))
         (0+- `(0 1 ,-phi))
         (0-+ `(0 -1 ,phi))
         (0-- `(0 -1 ,-phi))
         (++0 `(1 ,phi 0))
         (+-0 `(1 ,-phi 0))
         (-+0 `(-1 ,phi 0))
         (--0 `(-1 ,-phi 0))
         (+0+ `(,phi 0 1))
         (+0- `(,phi 0 -1))
         (-0+ `(,-phi 0 1))
         (-0- `(,-phi 0 -1)))
    (make-gl-object
     :type :triangle-strip
     :position (make-array
                '(30 3)
                :initial-contents `(;; up
                                    ,-0-
                                    ,0--
                                    ,0+-
                                    ,+0-
                                    ,++0
                                    ,+0+
                                    ;; down
                                    ,++0
                                    ,0++
                                    ,++0
                                    ,-+0
                                    ,0+-
                                    ,-+0
                                    ,-0-
                                    ;; up
                                    ,-+0
                                    ,-0+
                                    ,0++
                                    ,0-+
                                    ,+0+
                                    ;; down
                                    ,0-+
                                    ,+-0
                                    ,0-+
                                    ,--0
                                    ,-0+
                                    ,--0
                                    ,-0-
                                    ;; up
                                    ,--0
                                    ,0--
                                    ,+-0
                                    ,+0-
                                    ,+0+)))))
(tricolor *icosahedron*)

(defparameter *icosahedron-points*
  (let* ((phi (/ (+ 1 (sqrt 5))
                 2))
         (-phi (- phi)))
    (make-gl-object
     :type :points
     :position (make-array
                '(12 3)
                :initial-contents `((0 1 ,phi)
                                    (0 1 ,-phi)
                                    (0 -1 ,phi)
                                    (0 -1 ,-phi)
                                    (1 ,phi 0)
                                    (1 ,-phi 0)
                                    (-1 ,phi 0)
                                    (-1 ,-phi 0)
                                    (,phi 0 1)
                                    (,phi 0 -1)
                                    (,-phi 0 1)
                                    (,-phi 0 -1))))))

(defun make-grid (rows cols)
  "makes a triangle-strip with 1+r+2rc vertices;
fills (0 to rows, 0 to cols, 0)"
  (let ((v (make-array (list (+ 1 rows (* 2 rows cols))
                             3)
                       :initial-element 0))
        (i 1) ; first vertex is (0, 0)
        )
    (flet ((put (r c)
             (setf (aref v i 0) r
                   (aref v i 1) c)
             (incf i)))
      (dotimes (row rows)
        (if (= (mod row 2) 0)
            (loop for col from 0 below cols
               do (progn
                    (put (1+ row) col)
                    (put row (1+ col)))
               finally (put (1+ row) cols))
            (loop for col from cols above 0
               do (progn
                    (put (1+ row) col)
                    (put row (1- col)))
               finally (put (1+ row) 0)))))
    (make-gl-object
     :type :triangle-strip
     :position v)))

(defparameter *grid* (make-grid 4 5))
(tricolor *grid*)

;;;; The viewer
(defparameter *view-rotx* 0)
(defparameter *view-roty* 0)
(defparameter *view-rotz* 0)

(defun key-callback (key action)
  (when (eql action glfw:+press+)
    (case key 
      (#\Z
       (if (eql (glfw:get-key glfw:+key-lshift+) glfw:+press+)
           (decf *view-rotz* 5)
           (incf *view-rotz* 5)))
      (:esc   (glfw:close-window))
      (:up    (incf *view-rotx* 5))
      (:down  (decf *view-rotx* 5))
      (:left  (incf *view-roty* 5))
      (:right (decf *view-roty* 5)))))

(defun view-gl-object (obj)
  (let ((frames 0)
        t0 t1)
    (setf *view-rotx* 0
          *view-roty* 0
          *view-rotz* 0)
    (glfw:do-window (:title "Shape Viewer" :width 640 :height 480)
        ((glfw:enable glfw:+sticky-keys+)
         (glfw:enable glfw:+key-repeat+)
         (gl:enable gl:+cull-face+)
         (glfw:swap-interval 0)
         (glfw:set-key-callback 'key-callback)
         (setf t0 (glfw:get-time)
               t1 (glfw:get-time)))    

      (when (eql (glfw:get-key glfw:+key-esc+) glfw:+press+)
        (return-from glfw:do-window))

      (setf t1 (glfw:get-time))

      (when (> (- t1 t0) 1) 
        (glfw:set-window-title (format nil "Shape Viewer (~,1f FPS)" (/ frames (- t1 t0))))
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
      
        (gl:with-push-matrix
            (gl:rotate-f *view-rotx* 1 0 0)
          (gl:rotate-f *view-roty* 0 1 0)
          (gl:rotate-f *view-rotz* 0 0 1)

          (render-gl-object obj))))))

(view-gl-object *tetrahedron*)
