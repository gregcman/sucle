(in-package :sandbox)

;;matrix multiplication is associative
 ;;;opengl stored matrices the transpose of sb-cga
(defparameter *camera* nil) ;;global camera
(defparameter vsync? t)

(defconstant +mat4-identity+ (cg-matrix:identity-matrix))

(defun render ()
  (declare (optimize (safety 3) (debug 3)))
  (setf (camera-aspect-ratio *camera*) (/ window:*width* window:*height* 1.0))
  (if vsync?
      (window::set-vsync t)
      (window::set-vsync nil))
  (update-matrices *camera*)
  (luse-shader :solidshader)
  (gl:uniform-matrix-4fv
   (gl:get-uniform-location *shader-program* "projectionmodelview")
   (camera-matrix-projection-view-player *camera*)
   nil)
  (gl:enable :depth-test)
  (bind-default-framebuffer)
  (gl:clear
   :color-buffer-bit
   :depth-buffer-bit)
  (gl:viewport 0 0 e:*width* e:*height*)
  (bind-shit :font)
  (ldrawlist :background)
  (ldrawlist :skybox)
  (gl:uniform-matrix-4fv
   (gl:get-uniform-location *shader-program* "projectionmodelview")
   +mat4-identity+)
  (ldrawlist :background)
  (window:update-display))

(defun glinnit ()
  (setf *camera* (make-camera))
  (setf %gl:*gl-get-proc-address* (e:get-proc-address))
  (setf *shader-program* nil)
  
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
  (clean-framebuffers)
  (set-framebuffer))

(defparameter dir-resource (merge-pathnames #P"res/" ourdir))
(defparameter dir-shader (merge-pathnames #P"shaders/" dir-resource))

(defun shader-path (name)
  (merge-pathnames name dir-shader))

(defun img-path (name)
  (merge-pathnames name dir-resource))

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
  (let ((dims (array-dimensions image)))
    (let ((height (pop dims))
	  (width (pop dims)))
      (if dims
	  (let ((components (car dims)))
	    (dobox ((h 0 (- height (ash height -1)))
		    (w 0 width)
		    (c 0 components))
		   (rotatef (aref image (- height h 1) w c)
			    (aref image h w c))))
	  (dobox ((h 0 (- height (ash height -1)))
		  (w 0 width))
	      (rotatef (aref image (- height h 1) w)
		       (aref image h w))))))
  image)

(defun src-text (name src-path)
  (setf (gethash name *g/text-backup*)
	(lambda (&optional name)
	  (declare (ignorable name))
	  (file-string src-path))))

(defun load-some-images ()
  (src-image :font-image (img-path #P"font/codepage-437-vga-9x16.png")))

(defun texture-imageries ()
  (texture-imagery :font :font-image))
(defun name-shaders ()
  (name-shader :blockshader :bs-vs :bs-frag '(("position" . 0)
					      ("texCoord" . 2)
					      ("darkness" . 8)))
  (name-shader :solidshader :ss-vs :ss-frag '(("position" . 0)
					      ("texCoord" . 2)
					      ("darkness" . 8))))
(defun name-funcs ()
  (name-mesh :background #'draw-background)
  (name-mesh :skybox #'draw-skybox))

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

