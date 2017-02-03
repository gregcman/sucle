(in-package :sandbox)

;;matrix multiplication is associative

(defparameter *temp-matrix* (cg-matrix:identity-matrix)) ;;;opengl stored matrices the transpose of sb-cga
(defparameter *temp-matrix2* (cg-matrix:identity-matrix)) ;;;opengl stored matrices the transpose of sb-cga

(defun update-matrices (camera)
  (let ((projection-matrix (camera-matrix-projection camera))
	(view-matrix (camera-matrix-view camera))
	(projection-view-matrix (camera-matrix-projection-view camera))
	(projection-view-player-matrix (camera-matrix-projection-view-player camera))
	(player-matrix (camera-matrix-player camera))
	(forward (camera-vec-forward camera))
	(up (camera-vec-up camera)))
    (projection-matrix
     projection-matrix
     (camera-fov camera)
     *aspect-ratio*
     (camera-frustum-near camera)
     (camera-frustum-far camera))

    (relative-lookat view-matrix 					 
		     forward
		     up)
    (cg-matrix:%translate player-matrix (camera-vec-noitisop camera))
    (cg-matrix:%matrix* projection-view-matrix
			projection-matrix
			view-matrix)
    (cg-matrix:%matrix* projection-view-player-matrix
			projection-view-matrix
			player-matrix)))

(defstruct camera
  (vec-position (cg-matrix:vec 0.0 0.0 0.0) :type cg-matrix:vec)

  (vec-up (cg-matrix:vec 0.0 1.0 0.0) :type cg-matrix:vec)
  (vec-forward (cg-matrix:vec 1.0 0.0 0.0) :type cg-matrix:vec)

  (vec-noitisop (cg-matrix:vec 0.0 0.0 0.0) :type cg-matrix:vec) ;;;the negative of position
  (matrix-player (cg-matrix:identity-matrix)) ;;positional information of camera
  (matrix-view (cg-matrix:identity-matrix))		    ;;view matrix
  (matrix-projection (cg-matrix:identity-matrix))	    ;;projection matrix
  (matrix-projection-view (cg-matrix:identity-matrix)) ;;projection * view matrix
  (matrix-projection-view-player (cg-matrix:identity-matrix))
  
  (fov (coerce (/ pi 2.0) 'single-float) :type single-float)

  (frustum-near 0.0078125 :type single-float)
  (frustum-far 128.0 :type single-float))

(defparameter *camera* nil) ;;global camera
(defparameter *fog-ratio* 0.75)

(defun render ()
  (declare (optimize (safety 3) (debug 3)))

  (setf *aspect-ratio* (/ window:*width* window:*height*))
  (if vsync?
      (window::set-vsync t)
      (window::set-vsync nil))
  (when (window:mice-locked-p)
    (look-around))
  (update-matrices *camera*)
  
  (luse-shader :blockshader)
  ;(set-overworld-fog daytime)
  (bind-default-framebuffer)
  (gl:clear
   :color-buffer-bit
   :depth-buffer-bit)
  (gl:viewport 0 0 e:*width* e:*height*)
  (gl:disable :blend)
  (set-matrix
   "projectionmodelview"
   (cg-matrix:%transpose-matrix
    *temp-matrix*
    (camera-matrix-projection-view-player *camera*)))
  ;;;static geometry with no translation whatsoever
  (draw-chunk-meshes)
  ;(draw-fistbox)
  
  ;(gl:disable :cull-face) 
  ;(luse-shader :solidshader)
  ;(set-matrix "projectionmodelview" cg-matrix:+identity-matrix+)

  
  ;(draw-framebuffer)
  ;(draw-crosshair)
  (window:update-display)
  ;(draw-hud)
  
  (designatemeshing))


(defparameter *crosshair-size* 20.0)
(defparameter *hotbar-box-size* (* 22 4))
(defparameter *avector* (cg-matrix:vec 0.0 0.0 0.0))

(defun fractionalize (x)
  (clamp x 0.0 1.0))

(defun set-overworld-fog (time)
  (let ((x (fractionalize (* time 0.68)))
	(y (fractionalize (* time 0.8)))
	(z (fractionalize (* time 1.0))))
    (gl:clear-color x y z 1.0)
    (setf (aref *avector* 0) x
	  (aref *avector* 1) y
	  (aref *avector* 2) z)
    (set-vec3 "fogcolor" *avector*)
    (set-float "foglet" (/ -1.0 *clip-distance* *fog-ratio*))
    (set-float "aratio" (/ 1.0 *fog-ratio*))))

(defun draw-fistbox ()  
  (progn (when fist?
	   (gl:line-width 1.0)
	   (set-matrix
	    "projectionmodelview"
	    (cg-matrix:%transpose-matrix
	     *temp-matrix*
	     (cg-matrix:%matrix*
	      *temp-matrix2*
	      *projection-view-player-matrix*
	      (cg-matrix:%translate*
	       *temp-matrix*
	       (+ (coerce fist-side-x 'single-float))
	       (+ (coerce fist-side-y 'single-float))
	       (+ (coerce fist-side-z 'single-float))))))
	   
	   (progn
	     (gl:disable :cull-face :blend)
	     (gl:polygon-mode :front-and-back :line)
	     (ldrawlist :selected-box)
	     (gl:polygon-mode :front-and-back :fill)))))

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
  (gl:clear :color-buffer-bit)
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

(defun update-world-vao ()
  (maphash (lambda (k v)
	     (declare (ignorable k))
	     (gl:delete-lists v 1)
	     (remove-chunk-display-list k))
	   *g/chunk-call-list*)
  (maphash
   (lambda (k v)
     (declare (ignore v))
     (dirty-push k))
   world::chunkhash))

(defparameter vsync? t)

(defun glinnit ()
  (let ((width (if t 480 854))
	(height (if t 360 480)))
    (window:push-dimensions width height))
  (setf %gl:*gl-get-proc-address* (e:get-proc-address))
  (setf e:*resize-hook* #'on-resize)
  (set-framebuffer)

  (name-funcs)
  (texture-imageries)
  (name-shaders)

  (load-shaders)
  (load-some-images)

  (setf *camera* (make-camera))
  
  (setf *shader-program* nil)
  (setf mesher-thread nil))

(defun set-render-cam-pos (camera)
  (let ((vec (camera-vec-position camera))
	(cev (camera-vec-noitisop camera)))
    (setf (aref vec 0) *xpos*)
    (setf (aref vec 1) *ypos*)
    (setf (aref vec 2) *zpos*)

    (setf (aref cev 0) (- *xpos*))
    (setf (aref cev 1) (- *ypos*))
    (setf (aref cev 2) (- *zpos*))

    (setf (camera-vec-forward *camera*)
	(unit-pitch-yaw (camera-vec-forward *camera*)
			(coerce *pitch* 'single-float)
			(coerce *yaw* 'single-float)))
    
    (setf (camera-fov *camera*) defaultfov)))

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
	  (let ((img (imagewise:load-png src-path)))
	    (imagewise:flip-image img)
	    img))))

(defun src-text (name src-path)
  (setf (gethash name *g/text-backup*)
	(lambda (&optional name)
	  (declare (ignorable name))
	  (file-string src-path))))

(defun load-some-images ()
  (src-image "gui/gui.png" (img-path #P"gui/gui.png"))
  (src-image "misc/grasscolor.png" (img-path #P"misc/grasscolor.png"))
  (src-image "skybox/cheap.png" (img-path #P"skybox/cheap.png"))
  (src-image "terrain/sun.png" (img-path #P"terrain/sun.png"))
  (src-image "terrain/moon.png" (img-path #P"terrain/moon.png"))
  (src-image "terrain.png" (img-path #P"terrain.png")))

(defun texture-imageries ()
  (texture-imagery :terrain "terrain.png")
  (texture-imagery :skybox "skybox/cheap.png")
  (texture-imagery :sun "terrain/sun.png")
  (texture-imagery :moon "terrain/moon.png")
  (texture-imagery :gui "gui/gui.png"))
(defun name-shaders ()
  (name-shader :blockshader :bs-vs :bs-frag '(("position" . 0)
					      ("texCoord" . 2)
					      ("darkness" . 8)))
  (name-shader :solidshader :ss-vs :ss-frag '(("position" . 0)
					      ("texCoord" . 2)
					      ("darkness" . 8))))
(defun name-funcs ()
  (name-mesh :skybox #'draw-skybox)
  (name-mesh :sun #'draw-sun)
  (name-mesh :moon #'draw-moon)
  (name-mesh :selected-box
	     (l () (let ((foo 0.005))
		     (let ((min (- 0.0 foo))
			   (max (+ 1.0 foo)))
		       (draw-box min min min max max max)))))
  (name-mesh :background #'draw-background)
  (name-mesh :crosshair #'mesh-crosshair)
  (name-mesh :gui #'draw-hotbar)
  (name-mesh :hotbar-selector #'draw-hotbar-selector))

(defun load-shaders ()
  (src-text :bs-vs (shader-path "blockshader/transforms.vs"))
  (src-text :bs-frag (shader-path "blockshader/basictexcoord.frag"))
  (src-text :ss-vs (shader-path "solidshader/transforms.vs"))
  (src-text :ss-frag (shader-path "solidshader/basictexcoord.frag")))
