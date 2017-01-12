(in-package :sandbox)

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

;;matrix multiplication is associative

(defparameter *camera* nil) ;;global camera
(defparameter *view-matrix* nil) ;;view matrix
(defparameter *projection-matrix* nil) ;;projection matrix
(defparameter *projection-view-matrix* nil) ;;projection * view matrix
(defparameter *player-matrix* nil) ;;positional information of camera

(defparameter *window-height* nil)
(defparameter *window-width* nil)
(defparameter *aspect-ratio* nil)

(defun glinnit ()
  ;(lpic-ltexture "gui/items.png" :items)
  ;(lpic-ltexture "misc/grasscolor.png" :grasscolor)
  ;(lpic-ltexture "misc/foliagecolor.png" :foliagecolor)
  (lpic-ltexture "terrain.png" :terrain)
  (lpic-ltexture "skybox/cheap.png" :skybox)
  (lpic-ltexture "terrain/sun.png" :sun)
  (lpic-ltexture "terrain/moon.png" :moon)
  ;(lpic-ltexture "font/default.png" :default)
  ;(lpic-ltexture "pack.png" :pack)
  ;(lpic-ltexture "environment/clouds.png" :clouds)

  (let ((vsync? (lget *g/args* :vsync)))    
    (cond (vsync? (window::set-vsync t)
		  (setf render-delay 0))
	  (t (window::set-vsync nil)
	     (setf render-delay (if t 0 (/ 1000000.0 59.88))))))

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
  
  (setf glshader:*shader-program* nil)
  (setf mesher-thread nil)
  (load-shader-programs)
  (update-world-vao))

(defun render ()
  "responsible for rendering the world"
  (when (window:mice-locked-p)
    (look-around))
  (set-render-cam-look)
  (setf *aspect-ratio* (/ window:*width* window:*height*))
  (with-slots (xpos ypos zpos fov pitch yaw) *camera*
    (set-projection-matrix
     (coerce (deg-rad fov) 'single-float)
     *aspect-ratio*
     0.01
     128)
    
    (set-view-matrix
     (unit-pitch-yaw (coerce pitch 'single-float)
		     (coerce yaw 'single-float))
     (sb-cga:vec 0.0 1.0 0.0))

    (set-player-matrix  (coerce xpos 'single-float)
			(coerce ypos 'single-float)
			(coerce zpos 'single-float)))
  (update-projection-view-matrix)
  (gl:viewport 0 0 window:*width* window:*height*)
  (luse-shader :blockshader)

  (glshader:set-matrix
   "projectionmodelview"
   (sb-cga:transpose-matrix
    (sb-cga:matrix* *projection-view-matrix* *player-matrix*)))
  ;;;static geometry with no translation whatsoever
  (draw-chunk-meshes)

  ;;;;draw the fist hitbox
  (with-slots ((minx aabbcc::minx) (miny aabbcc::miny) (minz aabbcc::minz)
	       (maxx aabbcc::maxx) (maxy aabbcc::maxy) (maxz aabbcc::maxz))
      fist-aabb
    (draw-box (+ minx fistx -0) (+  miny fisty -0) (+  minz fistz -0)
	      (+ maxx fistx -0) (+  maxy fisty -0) (+  maxz fistz -0)))
  
  (window:update-display)
  (draw-sky)
  (designatemeshing)
  (glshader:set-float "timeday" daytime)
  (set-overworld-fog daytime)
  (gl:clear
   ;color-buffer-bit
   :depth-buffer-bit))

(defun update-projection-view-matrix ()
  (setf *projection-view-matrix* (sb-cga:matrix* *projection-matrix* *view-matrix*)))

(defun set-projection-matrix (fovy aspect near far)
  (let ((projection-matrix (projection-matrix fovy aspect near far)))
    (setf *projection-matrix* projection-matrix)))

(defun set-view-matrix (direction up)
  (let ((view (relative-lookat direction up)))
    (setf *view-matrix* view)))

(defun set-player-matrix (x y z)
  (let ((player-matrix (sb-cga:translate* (- x) (- y) (- z))))
    (setf *player-matrix* player-matrix)))

(defun luse-shader (name)
  (glshader:use-program (lget *g/shader* name)))

(defun draw-chunk-meshes ()
  (gl:enable :depth-test)
  (gl:depth-func :less)

  (gl:enable :cull-face)
  (gl:cull-face :back)
  (bind-shit (case 0
	       (0 :terrain)
	       (1 :pack)
	       (2 :default)))
  (maphash
     (lambda (key display-list)
       (declare (ignore key))
       (gl:call-list display-list))
    *g/call-list*))

(defun update-world-vao ()
  "updates all of the vaos in the chunkhash. takes a long time"
  (maphash (lambda (k v)
	     (declare (ignorable k))
		   (gl:delete-lists v 1))
	   *g/call-list*)
  (lclear *g/call-list*)
  (maphash
   (lambda (k v)
     (declare (ignore v))
     (dirty-push k))
   world::chunkhash))

(defun bind-shit (name)
  "bind a texture located in the texture library"
  (let ((num (lget *g/texture* name)))
    (if num
	(gl:bind-texture :texture-2d num)
	(print "error-tried to use NIL texture"))))

;;;turn a picture which is in the image library into an
;;;opengl texture which is in the texture library
(defun lpic-ltexture (image-name &optional (texture-name image-name))
  (let ((thepic (lget *g/image* image-name)))
    (if thepic
	(destructuring-bind (h w c) (array-dimensions thepic)
	  (let ((type (case c
			(3 :rgb)
			(4 :rgba))))
	    (let ((new-texture (create-texture (imagewise:array-flatten thepic) w h type)))
	      (lset *g/texture* texture-name new-texture)
	      new-texture))))))

(defun load-shader-programs ()
  (load-block-shader)
  (load-simple-shader))

(defun load-block-shader ()
  (let ((old (lget *g/shader* :blockshader)))
    (when old
      (gl:delete-program old)))
  (lset *g/shader*
	:blockshader
	(glshader:make-shader-program-from-strings
	 (lget *g/text* :bs-vs)
	 (lget *g/text* :bs-frag)
	 '(("position" . 0)
	   ("texCoord" . 2)
	   ("darkness" . 8)
	   ;;    ("blockLight" . 8)
	   ;;   ("skyLight" . 12)
	   ))))

(defun load-simple-shader ()
  (let ((old (lget *g/shader* :simpleshader)))
    (when old
      (gl:delete-program old)))
  (lset *g/shader*
	:simpleshader
	(glshader:make-shader-program-from-strings
	 (lget *g/text* :ss-vs)
	 (lget *g/text* :ss-frag)
	 '(("position" . 0)
	   ("texCoord" . 2)
	   ("color" . 3)))))

(defun glActiveTexture (num)
  "sets the active texture"
  (gl:active-texture (+ num (glinfo:get-gl-constant :texture0))))

(defun create-texture (tex-data width height &optional (type :rgba))
  "creates an opengl texture from data"
  (let ((the-shit (car (gl:gen-textures 1))))
    (gl:bind-texture :texture-2d the-shit)
    (gl:tex-parameter :texture-2d :texture-min-filter :nearest; :nearest-mipmap-nearest
		      )
    (gl:tex-parameter :texture-2d :texture-mag-filter :nearest)
    (gl:tex-parameter :texture-2d :texture-wrap-s :repeat)
    (gl:tex-parameter :texture-2d :texture-wrap-t :repeat)
    (gl:tex-parameter :texture-2d :texture-border-color '(0 0 0 0))
   ; (gl:tex-parameter :texture-2d :generate-mipmap :true)
    (gl:tex-image-2d
     :texture-2d 0
     type width height 0 type :unsigned-byte tex-data)
 ;   (gl:generate-mipmap :texture-2d)
    the-shit))

