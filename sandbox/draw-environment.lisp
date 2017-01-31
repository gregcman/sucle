(in-package :sandbox)

;;matrix multiplication is associative
(defparameter *view-matrix* (cg-matrix:identity-matrix))		    ;;view matrix
(defparameter *projection-matrix* (cg-matrix:identity-matrix))	    ;;projection matrix
(defparameter *projection-view-matrix* (cg-matrix:identity-matrix)) ;;projection * view matrix
(defparameter *temp-matrix* (cg-matrix:identity-matrix)) ;;;opengl stored matrices the transpose of sb-cga
(defparameter *temp-matrix2* (cg-matrix:identity-matrix)) ;;;opengl stored matrices the transpose of sb-cga

(defun update-projection-view-matrices ()
  (setf *projection-view-matrix* (cg-matrix:%matrix* *projection-view-matrix*
						    *projection-matrix*
						    *view-matrix*)))

(defun set-projection-matrix (fovy aspect near far)
  (let ((projection-matrix (projection-matrix *projection-matrix* fovy aspect near far)))
    (setf *projection-matrix* projection-matrix)))

(defun set-view-matrix (direction up)
  (let ((view (relative-lookat *view-matrix* direction up)))
    (setf *view-matrix* view)))


(defun draw-sky ()
  (progn
    (set-matrix "projectionmodelview"
		(cg-matrix:transpose-matrix
		 *projection-view-matrix*))
    (bind-shit :skybox)
    (gl:bind-texture :texture-2d *framebuffer-texture*)
    (ldrawlist :skybox))
  (progn
    (let ((time (daytime)))
      (set-matrix "projectionmodelview"
		  (cg-matrix:%transpose-matrix
		   *temp-matrix*
		   (cg-matrix:matrix*
		    *projection-view-matrix*
		    (cg-matrix:rotate-around
		     (cg-matrix:vec -1.0 0.0 0.0)
		     time)
		    (cg-matrix:scale* 10.0 10.0 90.0))))

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

(defun daytime ()
  (coerce (* (get-internal-run-time)
	     (/ 840.0 100000000.0))
	  'single-float))


(defstruct camera
  xpos
  ypos 
  zpos 
  upx 
  upy 
  upz 
  yaw 
  pitch 
  fov)

(defparameter *camera* nil) ;;global camera
(defparameter *player-matrix* (cg-matrix:identity-matrix)) ;;positional information of camera
(defparameter *clip-distance* (ash 1 7))
(defparameter *fog-ratio* 0.75)
(defparameter *up-vector* (cg-matrix:vec 0.0 1.0 0.0))
(defparameter *projection-view-player-matrix* (cg-matrix:identity-matrix))
(defparameter *forward* (cg-matrix:vec 1.0 0.0 0.0))

(defun render ()
  (declare (optimize (safety 3) (debug 3)))
  (when (window:mice-locked-p)
    (look-around))
  (set-render-cam-look)
  (setf *aspect-ratio* (/ window:*width* window:*height*))
  (with-slots (xpos ypos zpos fov pitch yaw) *camera*
    (let ((floaty-fov (coerce (deg-rad fov) 'single-float)))
      (set-projection-matrix
       floaty-fov
       *aspect-ratio*
       0.01
       *clip-distance*))
    
    (set-view-matrix
     (unit-pitch-yaw *forward*
		     (coerce pitch 'single-float)
		     (coerce yaw 'single-float))
     *up-vector*)

    (set-player-matrix  (coerce xpos 'single-float)
			(coerce ypos 'single-float)
			(coerce zpos 'single-float)))
  (update-projection-view-matrices)

  
  (luse-shader :blockshader)
  (set-overworld-fog daytime)
  (bind-default-framebuffer)
  (gl:clear
   :color-buffer-bit
   :depth-buffer-bit)
  (gl:viewport 0 0 e:*width* e:*height*)
  (gl:disable :blend)
  (setf *projection-view-player-matrix*
	(cg-matrix:%matrix* *projection-view-player-matrix*
			    *projection-view-matrix*
			    *player-matrix*))
  (set-matrix
   "projectionmodelview"
   (cg-matrix:%transpose-matrix
    *temp-matrix*
    *projection-view-player-matrix*))
  ;;;static geometry with no translation whatsoever
  (draw-chunk-meshes)  
  (progn (when fist?
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
	     (gl:polygon-mode :front-and-back :fill))))

  (gl:disable :cull-face)
 
  (luse-shader :solidshader)
  (set-matrix "projectionmodelview"
		       cg-matrix:+identity-matrix+)
  (gl:enable :blend)
  (gl:depth-func :always)
  (gl:bind-texture :texture-2d *framebuffer-texture*)
  (ldrawlist :background)
  
  (draw-crosshair)
  (window:update-display)  
  (draw-hud)
  
  (designatemeshing))

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
  (if vsync?
      (window::set-vsync t)
      (window::set-vsync nil))

  (setf *camera* (make-camera
		  :xpos 0
		  :ypos 0
		  :zpos 0
		  :upx 0
		  :upy 1
		  :upz 0
		  :yaw 0
		  :pitch 0
		  :fov 70))
  
  (setf *shader-program* nil)
  (setf mesher-thread nil))

(defun set-render-cam-pos ()
  (setf (camera-xpos *camera*) *xpos* 
	(camera-ypos *camera*) *ypos* 
	(camera-zpos *camera*) *zpos* 
	(camera-pitch *camera*) *pitch*
	(camera-yaw *camera*) *yaw*
	(camera-fov *camera*) defaultfov))

(defun set-render-cam-look ()
  (setf (camera-pitch *camera*) *pitch*
	(camera-yaw *camera*) *yaw*))

(defun on-resize (w h)
  (setf *window-height* h
	*window-width* w)
  (lcalllist-invalidate :gui)
  (lcalllist-invalidate :hotbar-selector)
  (lcalllist-invalidate :crosshair)
  (clean-framebuffers)
  (set-framebuffer))

(defun set-player-matrix (x y z)
  (let ((player-matrix (cg-matrix:%translate* *player-matrix* (- x) (- y) (- z))))
    (setf *player-matrix* player-matrix)))

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
