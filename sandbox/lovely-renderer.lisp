(in-package :sandbox)

;;why in the world is everything lumped in here?
;;meshing
;;vaos
;;gpu data
;;vbos and vaos
;;meshing data
;;main controls
;;handling libraries
;;handling environment
;;handling blocks

;;vaohash holds all the vaos which correlate to each chunk
(defparameter vaohash (make-hash-table :test #'equal))
(defparameter drawmode nil)

(defstruct simplecam
  (pos (mat:onebyfour '(0.0 0.0 0.0 1)))
  (up (mat:onebyfour '(0.0 1.0 0.0 0)))
  (yaw 0)
  (pitch 0)
  (fov 100))
(defparameter ourcam (make-simplecam))

(defun render ()
  "responsible for rendering the world"

  (gl:clear :depth-buffer-bit :color-buffer-bit)
  (if (in:key-p :g)
      (update-world-vao))
  (if (in:key-p :5)
      (load-block-shader))

  (use-program "blockshader")
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
     vaohash)
    (gl:end-list)
    ourlist))

(defun draw-chunk-meshes ()
  (gl:enable :depth-test)

  (gl:enable :cull-face)
  (gl:cull-face :back)
  (bind-shit (case 2
	       (0 "terrain.png")
	       (1 "pack.png")
	       (2 "default.png")))
  (if worldlist
      (gl:call-list worldlist)))

(defparameter worldlist nil)

(defun update-world-vao ()
  "updates all of the vaos in the chunkhash. takes a long time"
  (clrhash vaohash)
  (maphash
   (lambda (k v)
     (declare (ignore v))
     (dirty-push k))
   world::chunkhash))

(defun glinnit ()
  "initializing things"
  (loadletextures)
  (load-into-texture-library "items.png")
  (load-into-texture-library "grasscolor.png")
  (load-into-texture-library "foliagecolor.png")
  (load-into-texture-library "terrain.png")
  (load-into-texture-library "default.png")
  (load-into-texture-library "pack.png")
  (load-into-texture-library "clouds.png")
  (bind-shit "terrain.png")
  (setf worldlist nil)
  (setf mesher-thread nil)
  (setf shaderProgram nil)
  (clrhash vaohash)
  (clrhash shaderhash)
  (load-block-shader)
  (load-simple-shader)
  (update-world-vao))

(defun load-block-shader ()
  (load-a-shader
   "blockshader"
   "blockshader/transforms.vs"
   "blockshader/basictexcoord.frag"
   '(("position" . 0)
     ("texCoord" . 2)
     ("color" . 3)
     ("blockLight" . 8)
     ("skyLight" . 12))))

(defun load-simple-shader ()
  (load-a-shader
   "simpleshader"
   "simpleshader/transforms.vs"
   "simpleshader/basictexcoord.frag"
   '(("position" . 0)
     ("texCoord" . 2)
     ("color" . 3))))

(defun bind-shit (name)
  "bind a texture located in the texture library"
  (let ((num (gethash name texture-library)))
    (gl:bind-texture :texture-2d num)))
