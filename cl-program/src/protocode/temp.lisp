(in-package :sandbox)

(progno

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
       24)))

 (progno
  (set-sky-color)
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
			(/ 1.0 *fog-ratio*))))))

 (progno
  
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
 
 (progno
  (gl:uniform-matrix-4fv
   (gl:get-uniform-location *shader-program* "projectionmodelview")
   (camera-matrix-projection-view-player *camera*)
   nil)
  
  (bind-shit :ocean)
  
  (ldrawlist :skybox))
 )

(progno
  (setf *camera* (make-camera))
   (setf (camera-aspect-ratio *camera*) (/ window:*width* window:*height* 1.0))
   (update-matrices *camera*)
   (defparameter *camera* nil)
   (progno
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
      result)))

(progno
     (name-shader :blockshader :bs-vs :bs-frag '(("position" . 0)
						 ("texCoord" . 2)
						 ("darkness" . 8)))
     (src-text :bs-vs (shader-path "blockshader/transforms.vs"))
     (src-text :bs-frag (shader-path "blockshader/basictexcoord.frag")))

(progno
    (name-mesh :background
	       (lambda ()
		 (gl-draw-quads (function draw-background))))
    (name-mesh :skybox (lambda ()
			 (gl-draw-quads (function draw-skybox)))))x

(progno
     (texture-imagery :ocean :ocean-image)
     (src-image :ocean-image (img-path #P"skybox/first-fancy-skybox.png")))


(progno
;;;;player-controls?
 ;;;;separate physics from keyboard+ mouse and other factors
 (defparameter noclip nil)

 (defparameter *xpos* 0f0)
 (defparameter *ypos* 0f0)
 (defparameter *zpos* 0f0)

 (defparameter *xvel* 0)
 (defparameter *yvel* 0)
 (defparameter *zvel* 0)

 (defparameter fly t)

 (defparameter *yaw* 0f0)
 (defparameter *pitch* 0f0)
 
 (defparameter defaultfov (* 70 +single-float-pi+ 1/180))

 (defparameter air-friction 0.98)
 (defparameter walking-friction (* 0.6 0.9))

 (defparameter gravity nil)
 (defparameter *speed* 0.01)

 (defparameter onground nil)
 (defparameter *fist-function* (constantly nil))

 (defun controls ()
   (setf net-scroll (clamp (+ net-scroll e:*scroll-y*) -1.0 1.0))
   (when (e:key-p :space)
     (incf *yvel* *speed*))
   (when (e:key-p :left-shift)
     (decf *yvel* *speed*))    
   (let ((dir 0))
     (when (e:key-p :w)
       (incf dir #C(-1 0)))
     (when (e:key-p :a)
       (incf dir #C(0 1)))
     (when (e:key-p :s)
       (incf dir #C(1 0)))
     (when (e:key-p :d)
       (incf dir #C(0 -1)))
     (unless (zerop dir)
       (let ((rot-dir (* dir (cis *yaw*))))
	 (let ((normalized (/ rot-dir (complex-modulus rot-dir))))
	   (incf *xvel* (* *speed* (realpart normalized)))
	   (incf *zvel* (* *speed* (imagpart normalized))))))))

 (defun mouse-looking ()
   (multiple-value-bind (dx dy) (delta)
     (let ((x (* mouse-sensitivity (/ dx 360.0)))
	   (y (* mouse-sensitivity (/ dy 360.0))))
       (multiple-value-bind (dyaw dpitch) (%sphere-mouse-help x y)
	 (setf *yaw* (mod (+ *yaw* dyaw) +single-float-two-pi+))
	 (setf *pitch* (clamp (+ *pitch* dpitch)
			      (* -0.99 +single-float-half-pi+)
			      (* 0.99 +single-float-half-pi+)))))))

 (defun %sphere-mouse-help (x y)
   (if (zerop x)
       (if (zerop y)
	   (values 0.0 0.0)
	   (values 0.0 y))
       (if (zerop y)
	   (values x 0.0)
	   (new-direction (coerce x 'single-float)
			  (coerce y 'single-float)))))

 (defparameter *new-dir* (cg-matrix:vec 0.0 0.0 0.0))
 (defun new-direction (dx dy)
   (let ((size (sqrt (+ (* dx dx) (* dy dy)))))
     (let ((dir *x-unit*))
       (let ((rot (cg-matrix:%rotate-around*
		   *temp-matrix3*
		   0.0 (/ (- dx) size) (/ dy size) size)))
	 (let ((new-dir (cg-matrix:%transform-direction *new-dir* dir rot)))
	   (multiple-value-bind (p y) (extract-polar-coords new-dir)
	     (values y p)))))))

 ;;return the pitch and yaw of a unit direction vector
 (defun extract-polar-coords (vec)
   (let ((zero (aref vec 0))
	 (two (aref vec 2)))
     (values (asin (aref vec 1))
	     (atan two zero))))

 (progno
  (progno
   (when (e:key-j-p :v) (toggle noclip))
   (when (e:key-j-p :g) (toggle gravity))
   (when (e:key-j-p :f) (toggle fly))
   
   
   
   (mouse-looking))
  (setf air-friction 0.9)
  (incf *xpos* *xvel*)
  (incf *ypos* *yvel*)
  (incf *zpos* *zvel*)

  (setf *xvel* (* *xvel* air-friction))
  (setf *zvel* (* *zvel* air-friction))    
  (setf *yvel* (* *yvel* air-friction))))

(progno
 (defparameter susie (flhat:make-flhat))
 (defparameter george (flhat:make-flhat))
 (defparameter pauline (flhat:make-flhat))

 (defun reset-susie ()
   (setf susie (flhat:make-flhat)))

 (defun test6 (times)
   (declare (optimize (speed 3) (safety 0)))
   (let ((spine (flhat:make-flhat-iterator susie)))
     (declare (type iter-ator:iter-ator spine))
     (dotimes (x 1 (flhat:iterator-position spine))
       (flhat:reset-iterator spine)
       (iateor:wasabios ((next spine))
	 (dotimes (x times)
	   (next x))))))

 (defun test69 ()
   (declare (optimize (speed 3) (safety 0)))
   (let ((spine (flhat:make-flhat-iterator susie))
	 (spine2 (flhat:make-flhat-iterator george))
	 (spine3 (flhat:make-flhat-iterator pauline)))
     (declare (type iter-ator:iter-ator spine spine2 spine3))
     (dotimes (x 25)
       (deach flhat:reset-iterator spine spine2 spine3)
       (iateor:wasabios ((emit spine)
			 (emit2 spine2)
			 (emit3 spine3))
	 (dotimes (x (floor (expt 10 7) 3))
	   (emit x)
	   (emit2 x)
	   (emit3 x)))))))

(progno
 (defparameter *save* #P"third/")

 (defparameter *saves-dir* (merge-pathnames #P"saves/" ourdir))

 (defun save (filename &rest things)
   (let ((path (merge-pathnames filename *saves-dir*)))
     (with-open-file (stream path :direction :output :if-does-not-exist :create :if-exists :supersede)
       (dolist (thing things)
	 (prin1 thing stream)))))

 (defun save2 (thingfilename &rest things)
   (apply #'save (merge-pathnames (format nil "~s" thingfilename) *save*) things))

 (defun myload2 (thingfilename)
   (myload (merge-pathnames (format nil "~s" thingfilename) *save*)))

 (defun myload (filename)
   (let ((path (merge-pathnames filename *saves-dir*)))
     (let ((things nil))
       (with-open-file (stream path :direction :input :if-does-not-exist nil)
	 (tagbody rep
	    (let ((thing (read stream nil nil)))
	      (when thing
		(push thing things)
		(go rep)))))
       (nreverse things)))))

(progno
 (defun totally-destroy-package (package)
   (do-symbols (symbol package)
     (let ((home (symbol-package symbol)))
       (when (eql package home)
	 (when (fboundp symbol)
	   (fmakunbound symbol))
	 (when (boundp symbol)
	   (makunbound symbol)))))
   (delete-package package))

 (defparameter shit nil)

 (defun test ()
   (dotimes (x (expt 10 4))
     (let ((package (make-package (gensym))))
       (push package shit))))

 (defun test2 (symbol)
   (eval
    `(progn
       (declaim (inline ,symbol))
       (defun ,symbol (package)
	 (do-symbols (symbol package)
	   (let ((home (symbol-package symbol)))
	     (when (eq package home)
	       (when (fboundp symbol)
		 (fmakunbound symbol))
	       (when (boundp symbol)
		 (makunbound symbol)))))
	 (print package)
	 (print package)
	 (print package)
	 (print package)
	 (print package)
	 (print package)
	 (print package)
	 (print package)
	 (print package)
	 (write package))
       (declaim (notinline ,symbol))
       
       (values
	(lambda (x)
	  (locally (declare (inline ,symbol))
	    (,symbol x)))
	(quote,symbol)))))

 (defparameter foo nil)

 (defun wot ()
   (setf foo nil)
   (do-symbols (symbol :cl)
     (if (or (boundp symbol) (fboundp symbol))
	 (push (symbol-name symbol) foo)))
   (setf foo (sort foo #'string<)))

 (defun simple-defun-p (form)
   (and (eq (quote defun) (pop form))
	(symbolp (pop form)))))

(progno
 (defparameter *skybox-tilemap* (regular-enumeration 4 3)))

(progno
 (defun glActiveTexture (num)
   "sets the active texture"
   (gl:active-texture (+ num (get-gl-constant :texture0))))

 (defun sizeof (type-keyword)
   "gets the size of a foreign c type"
   (cffi:foreign-type-size type-keyword))

 (defun get-gl-constant (keyword)
   "gets a gl-constant"
   (cffi:foreign-enum-value '%gl:enum keyword)))

(progno
 (defun luse-shader (name)
   (use-program (get-shader name)))

 (defun bind-shit (name)
   "bind a texture located in the texture library"
   (let ((num (get-texture name)))
     (if num
	 (gl:bind-texture :texture-2d num)
	 (print "error-tried to use NIL texture")))))

(progno
 (defun bind-custom-framebuffer (framebuffer)
   (gl:bind-framebuffer-ext :framebuffer-ext framebuffer))

 (defun bind-default-framebuffer ()
   (gl:bind-framebuffer-ext :framebuffer-ext 0))

 (defun create-framebuffer (w h)
   (let ((framebuffer (first (gl:gen-framebuffers-ext 1)))
	 (depthbuffer (first (gl:gen-renderbuffers-ext 1)))
	 (texture (first (gl:gen-textures 1))))
     ;; setup framebuffer
     (gl:bind-framebuffer-ext :framebuffer-ext framebuffer)

     ;; setup texture and attach it to the framebuffer
     (gl:bind-texture :texture-2d texture)
     (gl:tex-parameter :texture-2d :texture-min-filter :nearest)
     (gl:tex-parameter :texture-2d :texture-mag-filter :nearest)
     (gl:tex-image-2d :texture-2d 0 :rgba w h 0 :rgba :unsigned-byte (cffi:null-pointer))
     (gl:generate-mipmap-ext :texture-2d)
     (gl:bind-texture :texture-2d 0)
     (gl:framebuffer-texture-2d-ext :framebuffer-ext
				    :color-attachment0-ext
				    :texture-2d
				    texture
				    0)

     ;; setup depth-buffer and attach it to the framebuffer
     (gl:bind-renderbuffer-ext :renderbuffer-ext depthbuffer)
     (gl:renderbuffer-storage-ext :renderbuffer-ext :depth-component24 w h)
     (gl:framebuffer-renderbuffer-ext :framebuffer-ext
				      :depth-attachment-ext
				      :renderbuffer-ext
				      depthbuffer)

     ;; validate framebuffer
     (let ((framebuffer-status (gl:check-framebuffer-status-ext :framebuffer-ext)))
       (unless (gl::enum= framebuffer-status :framebuffer-complete-ext)
	 (error "Framebuffer not complete: ~A." framebuffer-status)))
     
     (gl:clear :color-buffer-bit
	       :depth-buffer-bit)
     (gl:enable :depth-test :multisample)
     (values texture framebuffer))))

(progno
 (defun ldrawlist (name)
   (let ((the-list (get-display-list name)))
     (if the-list
	 (gl:call-list the-list)
	 (print "error")))))

(progno
 (defun lpic-ltexture (image-name &optional (texture-name image-name))
   (let ((thepic (get-image image-name)))
     (when thepic
       (set-texture texture-name 
		    (pic-texture thepic))))))

(progno
 (defun create-texture (tex-data width height &optional (type :rgba))
   "creates an opengl texture from data"
   (let ((the-shit (gl:gen-texture)))
     (gl:bind-texture :texture-2d the-shit)
     (gl:tex-parameter :texture-2d :texture-min-filter  :nearest
		       )
     (gl:tex-parameter :texture-2d :texture-mag-filter :nearest)
     (gl:tex-parameter :texture-2d :texture-wrap-s :clamp)
     (gl:tex-parameter :texture-2d :texture-wrap-t :clamp)
     (gl:tex-parameter :texture-2d :texture-border-color '(0 0 0 0))
					; (gl:tex-parameter :texture-2d :generate-mipmap :true)
     (gl:tex-image-2d :texture-2d 0 type width height 0 type :unsigned-byte tex-data)
					;(gl:generate-mipmap :texture-2d)
     the-shit)))

(progno
 (defun lcalllist-invalidate (name)
   (let ((old (get-display-list name)))
     (remhash name *g/call-list*)
     (when old (gl:delete-lists old 1)))))

(progno
 (namexpr backup :ss-frag
	  (lambda () (file-string (shader-path "fcol3f-ftex2f-no0a.frag"))))
 (namexpr backup :ss-vs
	  (lambda () (file-string (shader-path "pos4f-col3f-tex2f.vs")))))

(progno
 (defun draw-mouse (pos-buf tex-buf lit-buf
		    lookup value char-width char-height x y z)
   (declare (type iter-ator:iter-ator pos-buf tex-buf lit-buf)
	    (type single-float x y z char-width char-height)
	    (type simple-vector lookup)
	    (optimize (speed 3) (safety 0))
	    (type fixnum value))
   (iter-ator:wasabios ((epos pos-buf)
			(etex tex-buf)
			(elit lit-buf))

     (dotimes (x 4)
       (etouq (ngorp (preach 'elit '(1f0
				     1f0
				     1f0)))))
     (multiple-value-bind (x0 y0 x1 y1) (index-quad-lookup lookup value)
       (etouq (ngorp (preach 'etex (duaq 1 nil '(x0 x1 y0 y1))))))
     (etouq (ngorp
	     (preach
	      'epos
	      (quadk+ 'z '(x
			   (+ char-width x)
			   (- y char-height)
			   y)))))4))
 (defun render-mouse ()
   (progn
     (gl:uniform-matrix-4fv
      (gl:get-uniform-location (get-stuff :textshader *stuff* *backup*) "pmv")
      (cg-matrix:%matrix* *temp-matrix2*
			  *screen-scaled-matrix*
			  (cg-matrix:%translate* *temp-matrix* cursor-x cursor-y 0.0))
      nil)
     (progn
       (gl:bind-texture :texture-2d (get-stuff :cursor *stuff* *backup*))
       (progn (let ((scale 64.0))
		(namexpr *backup* :cursor-list
			 (lambda ()
			   (create-call-list-from-func
			    (lambda ()
			      (gl-draw-quads 
			       (lambda (tex-buf pos-buf lit-buf)
				 (draw-mouse
				  pos-buf tex-buf lit-buf
				  *4x4-tilemap* 0
				  scale 
				  scale
				  -0.0 0.0
				  (- +single-float-just-less-than-one+))))))))))
       (gl:call-list (get-stuff :cursor-list *stuff* *backup*))))))

(progno
 (defun gl-draw-quads (func)
   (let ((iter *attrib-buffer-iterators*))
     (reset-attrib-buffer-iterators iter)
     (let ((pos-buf (aref iter 0))
	   (lit-buf (aref iter 3))
	   (tex-buf (aref iter 8)))
       (let ((times (funcall func tex-buf pos-buf lit-buf)))
	 (gl:with-primitives :quads
	   (reset-attrib-buffer-iterators iter)
	   (mesh-test42 times lit-buf tex-buf pos-buf))))))

 (defun mesh-test42 (times lit tex pos)
   (declare (type iter-ator:iter-ator tex pos))
   (iter-ator:wasabiis ((d lit)
			(uv tex)
			(xyz pos))
     (dotimes (x times)
       (%gl:vertex-attrib-2f 8 (uv) (uv))
       (%gl:vertex-attrib-3f 3 (d) (d) (d))
       (%gl:vertex-attrib-3f 0 (xyz) (xyz) (xyz))))))
