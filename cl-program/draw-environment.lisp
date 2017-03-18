(in-package :sandbox)

;;matrix multiplication is associative

(defparameter *temp-matrix* (cg-matrix:identity-matrix)) ;;;opengl stored matrices the transpose of sb-cga
(defparameter *temp-matrix2* (cg-matrix:identity-matrix)) ;;;opengl stored matrices the transpose of sb-cga

(defparameter *camera* nil) ;;global camera
(defparameter *fog-ratio* 0.75)

(defun render ()
  (declare (optimize (safety 3) (debug 3)))

  (setf (camera-aspect-ratio *camera*) (/ window:*width* window:*height* 1.0))
  (if vsync?
      (window::set-vsync t)
      (window::set-vsync nil))
  (when (window:mice-locked-p)
    (look-around))
  (update-matrices *camera*)
    (luse-shader :solidshader)
  (bind-default-framebuffer)
  (gl:clear
   :color-buffer-bit
   :depth-buffer-bit)
  (gl:viewport 0 0 e:*width* e:*height*)
  (draw-sky)
  (window:update-display))

(defun draw-sky ()
  (progn
    (set-matrix "projectionmodelview"
		(cg-matrix:%transpose-matrix *temp-matrix* (camera-matrix-projection-view *camera*)))
    (bind-shit :skybox)
    ;(gl:bind-texture :texture-2d *framebuffer-texture*)
    (ldrawlist :skybox))
  (progn
    (let ((time (/ (float *ticks*) 50.0)))
      (set-matrix "projectionmodelview"
		  (cg-matrix:%transpose-matrix
		   *temp-matrix*
		   (cg-matrix:matrix*
		    (camera-matrix-projection-view *camera*)
		    (cg-matrix:rotate-around
		     (cg-matrix:vec -1.0 0.0 0.0)
		     time)
		    (cg-matrix:scale* 1.0 1.0 9.0))))

      (gl:enable :blend)
      (gl:blend-func :src-alpha :src-alpha)
      (bind-shit :sun)
      (ldrawlist :sun)
      
      (bind-shit :moon)
     (ldrawlist :moon))
    (gl:disable :blend)))



(defparameter *crosshair-size* 20.0)
(defparameter *hotbar-box-size* (* 22 4))
(defparameter *avector* (cg-matrix:vec 0.0 0.0 0.0))

(defun fractionalize (x)
  (clamp x 0.0 1.0))

(defparameter *vec4* (make-array 4 :element-type 'single-float)) 
(defun vec4 (vec3)
  (setf (aref *vec4* 0) (aref vec3 0))
  (setf (aref *vec4* 1) (aref vec3 1))
  (setf (aref *vec4* 2) (aref vec3 2))
  *vec4*)

(defun set-overworld-fog (time)
  (let ((x (fractionalize (* time 0.0)))
	(y (fractionalize (* time 0.0)))
	(z (fractionalize (* time 0.0))))
    (gl:clear-color x y z 1.0)
    (setf (aref *avector* 0) x
	  (aref *avector* 1) y
	  (aref *avector* 2) z)
    (set-vec3 "fogcolor" *avector*)
    (set-vec4 "cameraPos" (camera-vec-position *camera*))
    (set-float "foglet" (/ -1.0 (camera-frustum-far *camera*) *fog-ratio*))
    (set-float "aratio" (/ 1.0 *fog-ratio*))))

(defun draw-fist (camera)
  (gl:line-width 1.0)
  (set-matrix
   "projectionmodelview"
   (cg-matrix:%transpose-matrix
    *temp-matrix*
    (cg-matrix:%matrix*
     *temp-matrix2*
     (camera-matrix-projection-view-player camera)
     (cg-matrix:%translate*
      *temp-matrix*
      (+ (coerce fist-side-x 'single-float))
      (+ (coerce fist-side-y 'single-float))
      (+ (coerce fist-side-z 'single-float))))))
  (gl:disable :cull-face :blend)
  (gl:polygon-mode :front-and-back :line)
  (ldrawlist :selected-box)
  (gl:polygon-mode :front-and-back :fill))

(defun draw-framebuffer ()
  (gl:enable :blend)
  (gl:depth-func :always)
  (gl:bind-texture :texture-2d *framebuffer-texture*)
  (ldrawlist :background))

;;;the crosshair does not belong in the hud because the blending is
;;;different
(defun draw-crosshair ()
  (bind-shit :gui)
  (gl:blend-func :one-minus-dst-color :one-minus-src-color)
  (ldrawlist :crosshair))

(defun draw-hud ()
  (bind-custom-framebuffer)
  (gl:clear-color 0.0 0.0 0.0 0.0)
;  (gl:clear :color-buffer-bit)
  (gl:enable :blend)
  (gl:blend-func :src-alpha :one-minus-src-alpha)
  (bind-shit :gui)
  (ldrawlist :gui)
  (ldrawlist :hotbar-selector))

(defun draw-chunk-meshes ()
  (gl:enable :depth-test)  
  (gl:depth-func :less)

  (gl:enable :cull-face)
  (gl:cull-face :back)
  (bind-shit :terrain)
  (name-mesh :world #'draw-world)
  (ldrawlist :world))

(defun draw-world ()
  (maphash
   (lambda (key display-list)
     (when (numberp key)
       (gl:call-list display-list)))
   *g/chunk-call-list*))

(progn
  (defparameter *g/chunk-call-list* (make-hash-table :test 'eq));;opengl call lists
  (defun get-chunk-display-list (name)
    (gethash name *g/chunk-call-list*))
  (defun set-chunk-display-list (name list-num)
    (setf (gethash name *g/chunk-call-list*) list-num))
  (defun remove-chunk-display-list (name)
    (remhash name *g/chunk-call-list*)))

(defun distance-to-player (x y z)
  (let ((dx (- *xpos* x))
	(dy (- *ypos* y))
	(dz (- *zpos* z)))
    (sqrt (+ (* dx dx) (* dy dy) (* dz dz)))))

(defparameter vsync? t)

(defun glinnit ()
  (setf *camera* (make-camera))
  (setf %gl:*gl-get-proc-address* (e:get-proc-address))
  (setf *shader-program* nil)
  (setf mesher-thread nil)
  
  (let ((width (if t 480 854))
	(height (if t 360 480)))
    (window:push-dimensions width height))
  (setf e:*resize-hook* #'on-resize)
  (set-framebuffer)

  (name-funcs)
  (texture-imageries)
  (name-shaders)

  (load-shaders)
  (load-some-images))

(defun set-render-cam-pos (camera)
  (let ((vec (camera-vec-position camera))
	(cev (camera-vec-noitisop camera)))
    (setf (aref vec 0) *xpos*)
    (setf (aref vec 1) *ypos*)
    (setf (aref vec 2) *zpos*)

    (setf (aref cev 0) (- *xpos*))
    (setf (aref cev 1) (- *ypos*))
    (setf (aref cev 2) (- *zpos*))

    (unit-pitch-yaw (camera-vec-forward *camera*)
		    (coerce *pitch* 'single-float)
		    (coerce *yaw* 'single-float))
    
    (setf (camera-fov *camera*) defaultfov)))

(defun unit-pitch-yaw (result pitch yaw)
  (let ((cos-pitch (cos pitch)))
    (setf (aref result 0) (* cos-pitch (cos yaw))
	  (aref result 1) (sin pitch)
	  (aref result 2) (* cos-pitch (sin yaw))))
  result)

(defun on-resize (w h)
  (setf *window-height* h
	*window-width* w)
  (lcalllist-invalidate :gui)
  (lcalllist-invalidate :hotbar-selector)
  (lcalllist-invalidate :crosshair)
  (clean-framebuffers)
  (set-framebuffer))

(defparameter ourdir
  (make-pathname :host (pathname-host #.(or *compile-file-truename*
					    *load-truename*))
		 :directory (pathname-directory #.(or *compile-file-truename*
						      *load-truename*))))
(defparameter dir-resource (merge-pathnames #P"res/" ourdir))
(defparameter dir-shader (merge-pathnames #P"shaders/" dir-resource))
(defparameter dir-mc-assets (merge-pathnames "moreshit/" dir-resource))

(defun shader-path (name)
  (merge-pathnames name dir-shader))

(defun img-path (name)
  (merge-pathnames name dir-mc-assets))

(defun name-mesh (display-list-name mesh-func)
  (setf (gethash display-list-name *g/call-list-backup*)
	(lambda (&optional name)
	  (declare (ignorable name))
	  (create-call-list-from-func mesh-func))))

(defun texture-imagery (texture-name image-name)
  (setf (gethash texture-name *g/texture-backup*)
	(lambda (&optional name)
	  (declare (ignorable name))
	  (pic-texture (get-image image-name)))))

(defun name-shader (shader-name vs fs attributes)
  (setf (gethash shader-name *g/shader-backup*)
	(lambda (&optional name)
	  (declare (ignorable name))
	  (make-shader-program-from-strings
	   (get-text vs) (get-text fs) attributes))))

(defun src-image (name src-path)
  (setf (gethash name *g/image-backup*)
	(lambda (&optional name)
	  (declare (ignorable name))
	  (let ((img (load-png src-path)))
	    (flip-image img)
	    img))))

;;;;flip an image in-place - three dimensions - does not conse
(defun flip-image (image)
  (destructuring-bind (height width components) (array-dimensions image)
    (dotimes (h (- height (ash height -1)))
      (dotimes (w width)
	(dotimes (c components)
	  (rotatef (aref image (- height h 1) w c)
		   (aref image h w c))))))
  image)

(defun src-text (name src-path)
  (setf (gethash name *g/text-backup*)
	(lambda (&optional name)
	  (declare (ignorable name))
	  (file-string src-path))))

(defun load-some-images ()
  (src-image "skybox/cheap.png" (img-path #P"skybox/cheap.png"))
  (src-image "terrain/sun.png" (img-path #P"terrain/sun.png"))
  (src-image "terrain/moon.png" (img-path #P"terrain/moon.png")))

(defun texture-imageries ()
  (texture-imagery :skybox "skybox/cheap.png")
  (texture-imagery :sun "terrain/sun.png")
  (texture-imagery :moon "terrain/moon.png"))
(defun name-shaders ()
  (name-shader :blockshader :bs-vs :bs-frag '(("position" . 0)
					      ("texCoord" . 2)
					      ("darkness" . 8)))
  (name-shader :solidshader :ss-vs :ss-frag '(("position" . 0)
					      ("texCoord" . 2)
					      ("darkness" . 8))))
(defun name-funcs ()
  (progn (name-mesh :skybox #'draw-skybox)
	 (name-mesh :sun #'draw-sun)
	 (name-mesh :moon #'draw-moon)
	 (name-mesh :background #'draw-background)))

(defun load-shaders ()
  (src-text :bs-vs (shader-path "blockshader/transforms.vs"))
  (src-text :bs-frag (shader-path "blockshader/basictexcoord.frag"))
  (src-text :ss-vs (shader-path "solidshader/transforms.vs"))
  (src-text :ss-frag (shader-path "solidshader/basictexcoord.frag")))

(in-package :sandbox)

(defmacro vvv (darkness u v x y z)
  `(progn (%gl:vertex-attrib-1f 8 ,darkness)
	  (%gl:vertex-attrib-2f 2 ,u ,v)
	  (%gl:vertex-attrib-3f 0 ,x ,y ,z)))

(defun draw-background ()
  (let ((distance 0.99999997))
    (gl:with-primitives :quads
      (vvv 1.0 0.0 0.0 -1.0 -1.0 distance)
      (vvv 1.0 1.0 0.0 1.0 -1.0 distance)
      (vvv 1.0 1.0 1.0 1.0 1.0 distance)
      (vvv 1.0 0.0 1.0 -1.0 1.0 distance))))

(defun draw-skybox ()
  (let ((h0 0.0)
	(h1 (/ 1.0 3.0))
	(h2 (/ 2.0 3.0))
	(h3 (/ 3.0 3.0))
	(w0 0.0)
	(w1 (/ 1.0 4.0))
	(w2 (/ 2.0 4.0))
	(w3 (/ 3.0 4.0))
	(w4 (/ 4.0 4.0)))
    (let ((neg -10.0)
	  (pos 10.0))
      (gl:with-primitives :quads
	;;j+
	(vvv 1.0 w2 h3 neg pos neg)
	(vvv 1.0 w2 h2 pos pos neg)
	(vvv 1.0 w1 h2 pos pos pos)
	(vvv 1.0 w1 h3 neg pos pos)

	;;j-
	(vvv 1.0 w2 h0 neg neg neg)
	(vvv 1.0 w1 h0 neg neg pos)
	(vvv 1.0 w1 h1 pos neg pos)
	(vvv 1.0 w2 h1 pos neg neg)

	;;k-
	(vvv 1.0 w3 h2 neg pos neg)
	(vvv 1.0 w3 h1 neg neg neg)
	(vvv 1.0 w2 h1 pos neg neg)
	(vvv 1.0 w2 h2 pos pos neg)

	;;k+
	(vvv 1.0 w1 h1 pos neg pos)
	(vvv 1.0 w0 h1 neg neg pos)
	(vvv 1.0 w0 h2 neg pos pos)
	(vvv 1.0 w1 h2 pos pos pos)
	
	;;i-
	(vvv 1.0 w3 h1 neg neg neg)
	(vvv 1.0 w3 h2 neg pos neg)
	(vvv 1.0 w4 h2 neg pos pos)
	(vvv 1.0 w4 h1 neg neg pos)

	;;i+
	(vvv 1.0 w2 h1 pos neg neg)
	(vvv 1.0 w1 h1 pos neg pos)
	(vvv 1.0 w1 h2 pos pos pos)
	(vvv 1.0 w2 h2 pos pos neg)))))

(defun draw-sun ()
  (let ((distance 1.0))
    (gl:with-primitives :quads
      (vvv 1.0 1.0 1.0  1.0 -1.0 distance)
      (vvv 1.0 1.0 0.0 -1.0 -1.0 distance)
      (vvv 1.0 0.0 0.0 -1.0  1.0 distance)
      (vvv 1.0 0.0 1.0  1.0  1.0 distance))))

(defun draw-moon ()
  (let ((distance -1.0))
    (gl:with-primitives :quads
      (vvv 1.0 1.0 1.0  -1.0 1.0 distance)
      (vvv 1.0 1.0 0.0 -1.0 -1.0 distance)
      (vvv 1.0 0.0 0.0 1.0  -1.0 distance)
      (vvv 1.0 0.0 1.0  1.0  1.0 distance))))

(defun draw-box (minx miny minz maxx maxy maxz)
  (let ((h0 0.0)
	(h1 (/ 1.0 3.0))
	(h2 (/ 2.0 3.0))
	(h3 (/ 3.0 3.0))
	(w0 0.0)
	(w1 (/ 1.0 4.0))
	(w2 (/ 2.0 4.0))
	(w3 (/ 3.0 4.0))
	(w4 (/ 4.0 4.0)))
    (gl:with-primitives :quads
      (vvv 0.0 w2 h3 minx maxy minz)
      (vvv 0.0 w2 h2 maxx maxy minz)
      (vvv 0.0 w1 h2 maxx maxy maxz)
      (vvv 0.0 w1 h3 minx maxy maxz)

      ;;j-
      (vvv 0.0 w2 h0 minx miny minz)
      (vvv 0.0 w1 h0 minx miny maxz)
      (vvv 0.0 w1 h1 maxx miny maxz)
      (vvv 0.0 w2 h1 maxx miny minz)

      ;;k-
      (vvv 0.0 w3 h2 minx maxy minz)
      (vvv 0.0 w3 h1 minx miny minz)
      (vvv 0.0 w2 h1 maxx miny minz)
      (vvv 0.0 w2 h2 maxx maxy minz)

      ;;k+
      (vvv 0.0 w1 h1 maxx miny maxz)
      (vvv 0.0 w0 h1 minx miny maxz)
      (vvv 0.0 w0 h2 minx maxy maxz)
      (vvv 0.0 w1 h2 maxx maxy maxz)
      
      ;;i-
      (vvv 0.0 w3 h1 minx miny minz)
      (vvv 0.0 w3 h2 minx maxy minz)
      (vvv 0.0 w4 h2 minx maxy maxz)
      (vvv 0.0 w4 h1 minx miny maxz)

      ;;i+
      (vvv 0.0 w2 h1 maxx miny minz)
      (vvv 0.0 w1 h1 maxx miny maxz)
      (vvv 0.0 w1 h2 maxx maxy maxz)
      (vvv 0.0 w2 h2 maxx maxy minz))))

