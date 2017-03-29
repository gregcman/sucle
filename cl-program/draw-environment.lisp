(in-package :sandbox)


(defparameter vsync? t)

(defparameter *mat4-identity* (cg-matrix:identity-matrix))

(defconstant +single-float-just-less-than-one+ 0.99999997)

(defun render ()
  (if vsync?
      (window::set-vsync t)
      (window::set-vsync nil))
  (luse-shader :solidshader)


  (bind-default-framebuffer)
  (gl:uniform-matrix-4fv
   (gl:get-uniform-location *shader-program* "pmv")
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
			     -1.0 1.0
			     (- +single-float-just-less-than-one+)))))))
  (ldrawlist :string)

  (window:update-display))

(defun glinnit ()
  (setf %gl:*gl-get-proc-address* (e:get-proc-address))
  (setf *shader-program* nil)
  
  (let ((width (if t 480 854))
	(height (if t 360 480)))
    (window:push-dimensions width height))
  (setf e:*resize-hook* #'on-resize)
  

  (progn
    (name-mesh :background
	       (lambda ()
		 (gl-draw-quads (function draw-background))))
    (name-mesh :skybox (lambda ()
			 (gl-draw-quads (function draw-skybox)))))
  (progn    
    (name-shader :solidshader :ss-vs :ss-frag '(("pos" . 0)
						("tex" . 2)
						("col" . 8)))
    (src-text :ss-vs (shader-path "pos4f-col4f-tex2f.vs"))
    (src-text :ss-frag (shader-path "fcol4f-ftex2f-no0a.frag")))
  
  (progn
    (texture-imagery :font :font-image)
    (texture-imagery :ocean :ocean-image))
  (progn
    (src-image :font-image (img-path #P"font/codepage-437-vga-9x16-alpha.png"))
    (src-image :cursor-image (img-path #P"cursor/windos-cursor.png"))
    (src-image :ocean-image (img-path #P"skybox/first-fancy-skybox.png"))))

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

   (defun raps (times form)
     (make-list times :initial-element form))

   (defun ngorp (&rest forms)
     (cons (quote progn)
	   (apply (function nconc) forms)))))

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
      (let ((xoffset x)
	    (yoffset y))
	(declare (type single-float xoffset yoffset))
	(dotimes (position len)
	  (let ((char (row-major-aref string position)))
	    (let ((next-x (+ xoffset char-width))
		  (next-y (- yoffset char-height)))
	      (if (char= char #\Newline)
		  (setf xoffset x
			yoffset next-y)
		  (progn
		    (unless (char= #\space char)
		      (incf times 4)
		      (dotimes (x 4)
			(etouq (ngorp (preach 'elit '(1f0 1f0 1f0 1f0)))))
		      (let ((code (char-code char)))
			(multiple-value-bind (x0 y0 x1 y1)
			    (sixteen-by-sixteen-texture-ref code)
			  (etouq (ngorp (preach 'etex (duaq 1 nil '(x0 x1 y0 y1)))))))
		      (etouq (ngorp
			      (preach
			       'epos
			       (quadk+ 'z '(xoffset
					    next-x
					    next-y
					    yoffset))))))
		    (setf xoffset next-x)))))))
      times)))

(defparameter foo
  (let ((a (write-to-string
	    '(defun render ()
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
	      
	      (window:update-display)))))
    (map-into a
	      (lambda (x) (char-downcase x)) a)))
