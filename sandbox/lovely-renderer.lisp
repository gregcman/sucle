(in-package :sandbox)

(defstruct simplecam
  (pos (mat:onebyfour '(0.0 0.0 0.0 1)))
  (up (mat:onebyfour '(0.0 1.0 0.0 0)))
  (yaw 0)
  (pitch 0)
  (fov 100))
(defparameter ourcam (make-simplecam))
(defparameter shaderProgram nil)

(defun render ()
  "responsible for rendering the world"

  (gl:clear :depth-buffer-bit :color-buffer-bit)
  (if (in:key-p :g)
      (update-world-vao))
  (if (in:key-p :5)
      (load-block-shader))

  (use-program :blockshader)
  (setupmatrices ourcam)
  (designatemeshing)
  (set-float "timeday" daytime)
  (set-overworld-fog daytime)
    
  (draw-chunk-meshes)
  (window:update-display))

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
    (set-matrix "projectionmodelview"
		(mat:mmul projection modelview))))

(defun genworldcallist ()
  (let ((ourlist (gl:gen-lists 1)))
    (gl:new-list ourlist :compile)
    (maphash
     (lambda (key vao)
       (declare (ignore key))
       (if t
	   (gl:call-list vao)
	   (draw-vao vao)))
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
  (lclear *g/call-list*)
  (maphash
   (lambda (k v)
     (declare (ignore v))
     (dirty-push k))
   world::chunkhash))

(defun load-block-shader ()
  (lset *g/shader*
   :blockshader
   (make-shader-program-from-strings
    (lget *g/text* :bs-vs)
    (lget *g/text* :bs-frag)
    '(("position" . 0)
      ("texCoord" . 2)
      ("color" . 3)
      ("blockLight" . 8)
      ("skyLight" . 12)))))

(defun load-simple-shader ()
  (lset *g/shader*
   :simpleshader
   (make-shader-program-from-strings
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

(defun use-program (name)
  (let ((ourprog (lget *g/shader* name)))
    (unless (eql ourprog shaderProgram)
      (setq shaderProgram ourprog)
      (gl:use-program ourprog))))


(defun glinnit ()
  (lpic-ltexture "gui/items.png" :items)
  (lpic-ltexture "misc/grasscolor.png" :grasscolor)
  (lpic-ltexture "misc/foliagecolor.png" :foliagecolor)
  (lpic-ltexture "terrain.png" :terrain)
  (lpic-ltexture "font/default.png" :default)
  (lpic-ltexture "pack.png" :pack)
  (lpic-ltexture "environment/clouds.png" :clouds)
  
  (setf worldlist nil)
  (setf mesher-thread nil)
  (setf shaderProgram nil)
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
	  (let ((new-texture (create-texture-wot (array-flatten thepic) w h)))
	    (lset *g/texture* texture-name new-texture)
	    new-texture)))))
