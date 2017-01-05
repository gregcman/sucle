(in-package :sandbox)

(defstruct simplecam
  (pos (mat:onebyfour '(0.0 0.0 0.0 1)))
  (up (mat:onebyfour '(0.0 1.0 0.0 0)))
  (yaw 0)
  (pitch 0)
  (fov 100))
(defparameter ourcam (make-simplecam))

(defun render ()
  "responsible for rendering the world"

  (gl:clear
   :color-buffer-bit
   :depth-buffer-bit)

  (if (in:key-p :g)
      (update-world-vao))
  (when (in:key-p :5)
    (load-shaders)
    (load-block-shader))

  (luse-shader :blockshader)
  (setupmatrices ourcam)
  (designatemeshing)
  (glshader:set-float "timeday" daytime)
  (set-overworld-fog daytime)
  
  (draw-chunk-meshes)
  (window:update-display))

(defun luse-shader (name)
  (glshader:use-program (lget *g/shader* name)))

(defun setupmatrices (camera)
  (let ((modelview (mat:easy-lookat
		    (mat:add (mat:onebyfour
			      (list 0
				    (if isneaking (- 1.5 1/8) 1.5)
				    0 0))
			     (simplecam-pos camera))
		    (simplecam-pitch camera)
		    (simplecam-yaw camera)))
	(projection (mat:projection-matrix
		     (deg-rad (simplecam-fov camera))
		     (/ out::pushed-width out::pushed-height) 0.01 128)))
    (glshader:set-matrix "projectionmodelview"
			 (mat:to-flat (mat:mmul projection modelview)))))

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
  (if worldlist
      (gl:call-list worldlist)))

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
	     (setf render-delay (/ 1000000.0 59.88)))))

  (setf glshader:*shader-program* nil)
  (setf worldlist nil)
  (setf mesher-thread nil)
  (load-block-shader)
  (load-simple-shader)
  (update-world-vao))

;;;turn a picture which is in the image library into an
;;;opengl texture which is in the texture library
(defun lpic-ltexture (image-name &optional (texture-name image-name))
  (let ((thepic (lget *g/image* image-name)))
    (if thepic
	(destructuring-bind (h w &rest c) (array-dimensions thepic)
	  (declare (ignore c))
	  (let ((new-texture (create-texture-wot (imagewise:array-flatten thepic) w h)))
	    (lset *g/texture* texture-name new-texture)
	    new-texture)))))
