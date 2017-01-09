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

(defparameter *camera* nil)
(defparameter *view-matrix* nil)
(defparameter *projection-matrix* nil)

(defun glinnit ()
  (lpic-ltexture "gui/items.png" :items)
  (lpic-ltexture "misc/grasscolor.png" :grasscolor)
  (lpic-ltexture "misc/foliagecolor.png" :foliagecolor)
  (lpic-ltexture "terrain.png" :terrain)
  (lpic-ltexture "font/default.png" :default)
  (lpic-ltexture "pack.png" :pack)
  (lpic-ltexture "environment/clouds.png" :clouds)

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
  (setf worldlist nil)
  (setf mesher-thread nil)
  (load-block-shader)
  (load-simple-shader)
  (update-world-vao))

(defun render ()
  "responsible for rendering the world"

  (if (in:ismousecaptured)
      (look-around))
  (with-slots (xpos ypos zpos fov pitch yaw) *camera*
    (set-projection-matrix
     (coerce (deg-rad fov) 'single-float)
     (/ out::pushed-width out::pushed-height)
     0.01
     128)
    (set-view-matrix
     (sb-cga:vec (coerce xpos 'single-float)
		 (coerce ypos 'single-float)
		 (coerce zpos 'single-float))
     (unit-pitch-yaw (coerce pitch 'single-float)
		     (coerce yaw 'single-float))
     (sb-cga:vec 0.0 1.0 0.0)))

  (luse-shader :blockshader)
  (glshader:set-matrix
   "projectionmodelview"
   (sb-cga:transpose-matrix
    (sb-cga:matrix* *projection-matrix* *view-matrix*)))
  
  (draw-chunk-meshes)
  (window:update-display)

  
  (set-render-cam-look)
  (if (in:key-p :g)
      (update-world-vao))
  (if (in:key-p :5)
      (load-shaders))
  
  (designatemeshing)
  (glshader:set-float "timeday" daytime)
  (set-overworld-fog daytime)
  (gl:clear
;   :color-buffer-bit
   :depth-buffer-bit))

(defun set-projection-matrix (fovy aspect near far)
  (let ((projection-matrix (projection-matrix fovy aspect near far)))
    (setf *projection-matrix* projection-matrix)))

(defun set-view-matrix (camera-position direction up)
  (let ((view (relative-lookat camera-position direction up)))
    (setf *view-matrix* view)))

(defun luse-shader (name)
  (glshader:use-program (lget *g/shader* name)))

(defun genworldcallist ()
  (let ((ourlist (gl:gen-lists 1)))
    (gl:new-list ourlist :compile)
    (maphash
     (lambda (key display-list)
       (declare (ignore key))
       (gl:call-list display-list))
    *g/call-list*)
    (gl:end-list)
    ourlist))

(defun draw-chunk-meshes ()
  (gl:enable :depth-test)

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
    *g/call-list*)
  (progno
    (if worldlist
	(gl:call-list worldlist))))

(defparameter worldlist nil)

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
	(destructuring-bind (h w &rest c) (array-dimensions thepic)
	  (declare (ignore c))
	  (let ((new-texture (gltexture:create-texture (imagewise:array-flatten thepic) w h)))
	    (lset *g/texture* texture-name new-texture)
	    new-texture)))))
