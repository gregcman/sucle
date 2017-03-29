(in-package :sandbox)

;;matrix multiplication is associative
 ;;;opengl stored matrices the transpose of sb-cga
(defparameter *camera* nil) ;;global camera
(defparameter vsync? t)

(defparameter *mat4-identity* (cg-matrix:identity-matrix))

(defconstant +single-float-just-less-than-one+ 0.99999997)

(defun render ()
  (setf (camera-aspect-ratio *camera*) (/ window:*width* window:*height* 1.0))
  (if vsync?
      (window::set-vsync t)
      (window::set-vsync nil))
  (update-matrices *camera*)
  (luse-shader :blockshader)
  (set-overworld-fog *daytime*)


  (bind-default-framebuffer)
  (gl:uniform-matrix-4fv
   (gl:get-uniform-location *shader-program* "projectionmodelview")
   *mat4-identity*)
  (gl:viewport 0 0 e:*width* e:*height*)
  (setf *aspect-ratio* (/ e:*height* e:*width*))
  (bind-shit :font)
  (gl:enable :depth-test)
  (set-sky-color)
  
  (gl:clear :color-buffer-bit :depth-buffer-bit)
  (lcalllist-invalidate :string)

  (let ((scale 32.0))
    (name-mesh :string (lambda ()
			 (gl-draw-quads 
			  (lambda (tex-buf pos-buf lit-buf)
			    (draw-string-raster-char
			     pos-buf tex-buf lit-buf
			     foo
			     (/ scale e:*width* 2.0)
			     (/ scale e:*height*)
			     -1.0 0.0
			     (- +single-float-just-less-than-one+)))))))
  (ldrawlist :string)

  (gl:uniform-matrix-4fv
   (gl:get-uniform-location *shader-program* "projectionmodelview")
   (camera-matrix-projection-view-player *camera*)
   nil)
  (set-sky-color)
  
  (bind-shit :ocean)
    
  (ldrawlist :skybox)
     
  (window:update-display))

(defun set-sky-color ()
  (let ((r (* *daytime* (aref *sky-color* 0)))
	(g (* *daytime* (aref *sky-color* 1)))
	(b (* *daytime* (aref *sky-color* 2))))
    (gl:clear-color r g b 1.0)))
(defparameter *daytime* 0.4)
(defparameter *sky-color* (vector 1.0 0.8 0.68))
(defparameter *fog-ratio* 0.75)
(defun set-overworld-fog (time)
  (flet ((fractionalize (x)
	   (clamp x 0.0 1.0)))
    (let ((x (fractionalize (* (aref *sky-color* 0) time)))
	  (y (fractionalize (* (aref *sky-color* 1) time)))
	  (z (fractionalize (* (aref *sky-color* 2) time))))
      (%gl:uniform-3f (gl:get-uniform-location *shader-program* "fogcolor")
		      x y z)
      (gl:uniformfv (gl:get-uniform-location *shader-program* "cameraPos")
		    (camera-vec-position *camera*))
      (%gl:uniform-1f (gl:get-uniform-location *shader-program* "foglet")
		      (/ -1.0 (camera-frustum-far *camera*) *fog-ratio*))
      (%gl:uniform-1f (gl:get-uniform-location *shader-program* "aratio")
		      (/ 1.0 *fog-ratio*)))))

(defparameter foo "Hello World")


(defun glinnit ()
  (setf *camera* (make-camera))
  (setf %gl:*gl-get-proc-address* (e:get-proc-address))
  (setf *shader-program* nil)
  
  (let ((width (if t 480 854))
	(height (if t 360 480)))
    (window:push-dimensions width height))
  (setf e:*resize-hook* #'on-resize)
  
 
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
  (src-image :cursor-image (img-path #P"cursor/windos-cursor.png"))
  (src-image :ocean-image (img-path #P"skybox/first-fancy-skybox.png")))

(defun texture-imageries ()
  (texture-imagery :font :font-image)
  (texture-imagery :ocean :ocean-image))
(defun name-shaders ()
  (name-shader :blockshader :bs-vs :bs-frag '(("position" . 0)
					      ("texCoord" . 2)
					      ("darkness" . 8)))
  (name-shader :solidshader :ss-vs :ss-frag '(("position" . 0)
					      ("texCoord" . 2)
					      ("darkness" . 8))))
(defun name-funcs ()
  (name-mesh :background
	     (lambda ()
	       (gl-draw-quads (function draw-background))))
  (name-mesh :skybox (lambda ()
		       (gl-draw-quads (function draw-skybox)))))

(defun gl-draw-quads (func)
  (let ((iter *attrib-buffer-iterators*))
    (reset-attrib-buffer-iterators iter)
    (let ((tex-buf (aref iter 2))
	  (pos-buf (aref iter 0))
	  (lit-buf (aref iter 8)))
      (let ((times (funcall func tex-buf pos-buf lit-buf)))
	(gl:with-primitives :quads
	  (reset-attrib-buffer-iterators iter)
	  (mesh-test42 times lit-buf tex-buf pos-buf))))))

(defun load-shaders ()
  (src-text :bs-vs (shader-path "blockshader/transforms.vs"))
  (src-text :bs-frag (shader-path "blockshader/basictexcoord.frag"))
  (src-text :ss-vs (shader-path "solidshader/transforms.vs"))
  (src-text :ss-frag (shader-path "solidshader/basictexcoord.frag")))

(in-package :sandbox)

(eval-always
 (defun duaq (start clockwise-winding form)
   (destructuring-bind (x- x+ y- y+) form
     (if (member start '(1 2 3 4))
	 (let ((vec (vector x+ y+
			    x- y+
			    x- y-
			    x+ y-))
	       (i 0)
	       (ii 2)
	       (iii 4)
	       (iv 6))
	   (declare (dynamic-extent vec))
	   (when clockwise-winding
	     (rotatef ii iv))
	   (let ((end-test (* 2 (1- start))))
	     (do ()
		 ((= end-test i)
		  (list (aref vec i)
			(aref vec (1+ i))
			(aref vec ii)
			(aref vec (1+ ii))
			(aref vec iii)
			(aref vec (1+ iii))
			(aref vec iv)
			(aref vec (1+ iv))))
	       (rotatef i ii iii iv))))
	 (error "~s is not a plane quadrant" start)))))

(eval-always
 (defun aalgnqd (start value subform)
   (if (member start '(0 1 2))
       (let ((places (vector nil nil nil))
	     (count 0))
	 (dolist (form subform)
	   (push form (aref places count))
	   (setf count (- 1 count)))
	 (setf (aref places 2) (list value value value value))
	 (rotatef (aref places 2) (aref places start))
	 (let (acc)
	   (dotimes (x 4)
	     (push (pop (aref places 2)) acc)
	     (push (pop (aref places 1)) acc)
	     (push (pop (aref places 0)) acc))
	   acc))
       (error "~s is not 0 1 or 2" start))))

(defmacro etouq (form)
  (eval form))

(fuktard:eval-always
 (progn
   (defun quadi+ (i form) ;;jk
     (aalgnqd 0 i (duaq 1 t form)))
   (defun quadi- (i form)
     (aalgnqd 0 i (duaq 3 nil form)))
   (defun quadj+ (j form) ;;ik
     (aalgnqd 1 j (duaq 1 t form)))
   (defun quadj- (j form) 
     (aalgnqd 1 j (duaq 3 nil form)))
   (defun quadk+ (k form) ;;ji
     (aalgnqd 2 k (duaq 1 nil form)))
   (defun quadk- (k form)
     (aalgnqd 2 k (duaq 3 t form)))

   (defun preach (value form)
     (mapcar (lambda (x)
	       (list value x))
	     form))

   (defun raps (form times)
     (make-list times :initial-element form))))

(defun ngorp (&rest forms)
  (cons (quote progn)
	(apply (function nconc) forms)))

(defun draw-background (tex-buf pos-buf lit-buf)
  (declare (optimize (safety 0) (speed 3)))
  (declare (type iter-ator:iter-ator tex-buf pos-buf lit-buf))
  (iter-ator:wasabios ((etex tex-buf)
		       (epos pos-buf)
		       (elit lit-buf))
    (let ((distance +single-float-just-less-than-one+))
      (etouq (ngorp (preach 'etex (duaq 1 nil '(0.0 1.0 0.0 1.0)))
		     (preach 'epos (quadk+ 'distance '(-1.0 1.0 -1.0 1.0)))
		     (preach 'elit (raps 1f0 4))))))
  4)

(let ((%skybox-pos nil)
      (%skybox-tex nil))
  (declare (type (or null simple-vector)
		 %skybox-pos %skybox-tex))
  (setf (values %skybox-pos %skybox-tex)
	(let ((h0 0.0)
	      (h1 (/ 1.0 3.0))
	      (h2 (/ 2.0 3.0))
	      (h3 1.0)
	      (w0 0.0)
	      (w1 0.25)
	      (w2 0.5)
	      (w3 0.75)
	      (w4 1.0))
	  (let ((neg -10.0)
		(pos 10.0))
	    (values (etouq (cons 'vector
				    (let ((npnp (quote (neg pos neg pos))))
				      (nconc (quadi+ 'neg npnp)
					     (quadi- 'pos npnp)
					     (quadj+ 'neg npnp)
					     (quadj- 'pos npnp)
					     (quadk+ 'neg npnp)
					     (quadk- 'pos npnp)))))
		    (etouq (cons 'vector
				 (nconc (duaq 2 nil '(w2 w3 h1 h2))
					(duaq 3 nil '(w0 w1 h1 h2))
					(duaq 2 nil '(w1 w2 h0 h1))
					(duaq 1 nil '(w1 w2 h2 h3))
					(duaq 1 nil '(w3 w4 h1 h2))
					(duaq 4 nil '(w1 w2 h1 h2)))))))))
  (defun draw-skybox (tex-buf pos-buf lit-buf)
    (declare (optimize (safety 0) (space 3)))
    (declare (type iter-ator:iter-ator tex-buf pos-buf lit-buf))
    (iter-ator:wasabios ((etex tex-buf)
			 (epos pos-buf)
			 (elit lit-buf))
      (dotimes (x (length %skybox-tex))
 	(etex (aref %skybox-tex x)))
      (dotimes (x (length %skybox-pos))
	(epos (aref %skybox-pos x)))
      (dotimes (x 24)
	(elit 1f0)))
    24))

(progn
  (defconstant +single-float-one-sixteenth+ (coerce 1/16 'single-float))
  (progn
    (declaim (ftype (function ((unsigned-byte 8))
			      (values single-float single-float single-float single-float))
		    sixteen-by-sixteen-texture-ref))
    (defun sixteen-by-sixteen-texture-ref (num)
      (multiple-value-bind (vfoo ufoo) (floor num 16)
	(let ((u 
		(/ ufoo 16.0))
	      (v
		(- 1.0 +single-float-one-sixteenth+ (/ vfoo 16.0))))
	  (values u v
		 (+ u +single-float-one-sixteenth+) 
		 (+ v +single-float-one-sixteenth+)))))))

(defun draw-string-raster-char (pos-buf tex-buf lit-buf string
				char-width char-height x y z)
  (declare (type iter-ator:iter-ator pos-buf tex-buf lit-buf)
	   (type single-float x y z char-width char-height)
	   (optimize (speed 3) (safety 0))
	   (type (simple-array character) string))
  (iter-ator:wasabios ((epos pos-buf)
		       (etex tex-buf)
		       (elit lit-buf))
    (let ((len (array-total-size string))
	  (times 0))
      (declare (type fixnum times))
      (let ((xoffset 0f0)
	    (yoffset 0f0))
	(declare (type single-float xoffset yoffset))
	(dotimes (position len)
	  (let ((char (row-major-aref string position)))
	    (if (char= char #\Newline)
		(progn
		  (setf xoffset 0f0)
		  (decf yoffset))
		(progn
		  (incf times 4)
		  (etouq (ngorp (preach 'elit '(1f0 1f0 1f0 1f0))))
		  (let ((code (char-code char)))
		    (multiple-value-bind (x0 y0 x1 y1)
			(sixteen-by-sixteen-texture-ref code)
		      (etouq (ngorp (preach 'etex (duaq 1 nil '(x0 x1 y0 y1)))))))
		  (let ((xstart (+ x (* xoffset char-width)))
			(ystart (+ y (* yoffset char-height))))
		    (etouq (ngorp
			    (preach
			     'epos
			     (quadk+ 'z '(xstart
					  (+ xstart char-width)
					  ystart
					  (+ ystart char-height)))))))
		  (incf xoffset))))))
      times)))

(defparameter +gl-primitives+
  (vector
   :points
   :lines
   :line-strip
   :line-loop
   :triangles
   :triangle-strip
   :triangle-fan
   :quads
   :quad-strip
   :polygon))

(progn
  (defparameter *framebuffer-width* 512)
  (defparameter *framebuffer-height* 512)
  (defparameter *framebuffer* nil)
  (defparameter *framebuffer-texture* nil)
  (bind-custom-framebuffer *framebuffer*)
  (gl:clear-color 0f0 1f0 0f0 1f0)
  (gl:clear :color-buffer-bit)

  (setf (values *framebuffer-texture* *framebuffer*)
	(create-framebuffer *framebuffer-width* *framebuffer-height*))
  (progno
   (progno
    (bind-custom-framebuffer *framebuffer*)
    (gl:viewport 0 0 *framebuffer-width* *framebuffer-height*))
   (progn
     (bind-default-framebuffer)))
  (defun clean-framebuffers ()
    (gl:delete-framebuffers-ext (list *framebuffer*))
    (gl:delete-textures (list *framebuffer-texture*))))
 

