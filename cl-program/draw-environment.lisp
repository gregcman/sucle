(in-package :sandbox)

;;matrix multiplication is associative
 ;;;opengl stored matrices the transpose of sb-cga
(defparameter *camera* nil) ;;global camera
(defparameter vsync? t)

(defparameter *mat4-identity* (cg-matrix:identity-matrix))

(defun render ()
  (declare (optimize (safety 3) (debug 3)))
  (setf (camera-aspect-ratio *camera*) (/ window:*width* window:*height* 1.0))
  (if vsync?
      (window::set-vsync t)
      (window::set-vsync nil))
  (update-matrices *camera*)
  (luse-shader :solidshader)


  
  (progn
   (bind-custom-framebuffer *framebuffer*)
   (gl:viewport 0 0 *framebuffer-width* *framebuffer-height*))
  
  (gl:uniform-matrix-4fv
   (gl:get-uniform-location *shader-program* "projectionmodelview")
   *mat4-identity*)
  (bind-shit :font)
  (gl:clear-color 0f0 1f0 0f0 1f0)
  
  (progn
    (gl:clear :color-buffer-bit :depth-buffer-bit)
    (gl:disable :depth-test)
  ;  (lcalllist-invalidate :string)
    (name-mesh :string (lambda ()
			 (gl:with-primitives :quads
			   (draw-string-raster-char foo (floor 256 9) (floor 256 16) 0 32 0.5))))
    (ldrawlist :string))

  (progn
   (gl:uniform-matrix-4fv
    (gl:get-uniform-location *shader-program* "projectionmodelview")
    (camera-matrix-projection-view-player *camera*)
    nil)
   (gl:enable :depth-test)
   
   (gl:clear-color 1.0 0.0 0.0 1.0)
   
   (bind-default-framebuffer)
   (gl:viewport 0 0 e:*width* e:*height*)
   (gl:clear :depth-buffer-bit :color-buffer-bit
	     )
   (gl:bind-texture :texture-2d *framebuffer-texture*)
   (ldrawlist :background)
   (bind-shit :font)
   (ldrawlist :skybox))
  
  
  (window:update-display))

(defparameter foo
  "
(lambda (x) (* 2 x))
(lambda (x) (print x))
(lambda (x) (* x x))
(lambda (x) (values x x x x))")

(defun glinnit ()
  (setf *camera* (make-camera))
  (setf %gl:*gl-get-proc-address* (e:get-proc-address))
  (setf *shader-program* nil)
  
  (let ((width (if t 480 854))
	(height (if t 360 480)))
    (window:push-dimensions width height))
  (setf e:*resize-hook* #'on-resize)
  
  (setf (values *framebuffer-texture* *framebuffer*)
	(create-framebuffer *framebuffer-width* *framebuffer-height*))
  (bind-custom-framebuffer *framebuffer*)
  (gl:clear-color 0f0 1f0 0f0 1f0)
  (gl:clear :color-buffer-bit)

  (name-funcs)
  (texture-imageries)
  (name-shaders)

  (load-shaders)
  (load-some-images))

(defparameter *framebuffer-width* 512)
(defparameter *framebuffer-height* 512)
(defparameter *framebuffer* nil)
(defparameter *framebuffer-texture* nil)

(defun clean-framebuffers ()
  (gl:delete-framebuffers-ext (list *framebuffer*))
  (gl:delete-textures (list *framebuffer-texture*)))

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
	*window-width* w))

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
  (src-image :font-image (img-path #P"font/codepage-437-vga-9x16.png"))
  (src-image :cursor-image (img-path #P"cursor/windos-cursor.png")))

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

(declaim (inline vpc))
(defun vpc (u v x y z)
  (%gl:vertex-attrib-1f 8 1.0)
  (%gl:vertex-attrib-2f 2 u v)
  (%gl:vertex-attrib-3f 0 x y z)
  )
(declaim (notinline vpc))

(defun draw-background ()
  (let ((distance 0.99999997))
    (locally (declare (inline vpc))
      (gl:with-primitives :quads
	(vpc 0.0 0.0 -1.0 -1.0 distance)
	(vpc 1.0 0.0 1.0 -1.0 distance)
	(vpc 1.0 1.0 1.0 1.0 distance)
	(vpc 0.0 1.0 -1.0 1.0 distance)))))

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
	(progn
	  (vpc w2 h3 neg pos neg)
	  (vpc w2 h2 pos pos neg)
	  (vpc w1 h2 pos pos pos)
	  (vpc w1 h3 neg pos pos))
	(progn 	;;j-
	  (vpc w2 h0 neg neg neg)
	  (vpc w1 h0 neg neg pos)
	  (vpc w1 h1 pos neg pos)
	  (vpc w2 h1 pos neg neg))
	(progn 	;;k-
	  (vpc w3 h2 neg pos neg)
	  (vpc w3 h1 neg neg neg)
	  (vpc w2 h1 pos neg neg)
	  (vpc w2 h2 pos pos neg))
	(progn 	;;k+
	  (vpc w1 h1 pos neg pos)
	  (vpc w0 h1 neg neg pos)
	  (vpc w0 h2 neg pos pos)
	  (vpc w1 h2 pos pos pos))
	(progn 	;;i-
	  (vpc w3 h1 neg neg neg)
	  (vpc w3 h2 neg pos neg)
	  (vpc w4 h2 neg pos pos)
	  (vpc w4 h1 neg neg pos))
	(progn 	;;i+
	  (vpc w2 h1 pos neg neg)
	  (vpc w1 h1 pos neg pos)
	  (vpc w1 h2 pos pos pos)
	  (vpc w2 h2 pos pos neg))))))

(defconstant +single-float-one-sixteenth+ (coerce 1/16 'single-float))

(defun draw-raster-char-cell (code-point width height x y z)
  (let ((width-unit (/ 1.0 width))
	(height-unit (/ 1.0 height))
	(xstart (- (/ x width) 1.0))
	(ystart (- (/ y height) 1.0)))
    (draw-raster-char code-point xstart	ystart (+ xstart width-unit) (+ ystart height-unit) z)))

(defun draw-raster-char (code-point x1 y1 x2 y2 z)
  (multiple-value-bind (vfoo ufoo) (floor code-point 16)
    (let ((u (/ ufoo 16.0))
	  (v (- 1.0 +single-float-one-sixteenth+ (/ vfoo 16.0))))
      (let ((maxv (+ v +single-float-one-sixteenth+))
	    (maxu (+ u +single-float-one-sixteenth+)))
	(vpc u v x1 y1 z)
	(vpc maxu v x2 y1 z)
	(vpc maxu maxv x2 y2 z)
	(vpc u maxv x1 y2 z)))))

(defun draw-string-raster-char (string width height x y z)
  (let ((xoffset 0)
	(yoffset 0))
    (dotimes (position (length string))
      (let ((char (aref string position)))
	(if (char= char #\Newline)
	    (progn
	      (setf xoffset 0)
	      (decf yoffset))
	    (progn
	      (draw-raster-char-cell (char-code char) width height (+ xoffset x) (+ yoffset y) z)
	      (incf xoffset)))))))
