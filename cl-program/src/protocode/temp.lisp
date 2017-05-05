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

(progno
     (gl:uniformf (getuniform solidshader-uniforms :bg)
		  0f0 0f0 1f0)
     (gl:uniformf (getuniform solidshader-uniforms :fg)
		  0f0 1f0 0f0))

(progno
 (let ((xmax (float e:*width*))
	      (ymax (float e:*height*)))
	  (setf cursor-x (min (- xmax 4) (max (- xmax) (+ cursor-x delx))))
	  (setf cursor-y (min (+ ymax 2) (max (- ymax) (- cursor-y dely))))))


(progno
 (defun draw-string-raster-char (pos-buf tex-buf
				 lookup string char-width char-height
				 x y z)
   (declare (type iter-ator:iter-ator pos-buf tex-buf)
	    (type single-float x y z char-width char-height)
	    (type simple-vector lookup)
	    (optimize (speed 3) (safety 0))
	    (type (vector character) string))
   (iter-ator:wasabios ((epos pos-buf)
			(etex tex-buf))
     (let ((len (length string))
	   (times 0))
       (declare (type fixnum times))
       (let ((xoffset x)
	     (yoffset y))
	 (declare (type single-float xoffset yoffset))
	 (let ((wonine (if t 0.0 (/ char-width 8.0))))
	   (dotimes (position len)
	     (let ((char (row-major-aref string position)))
	       (let ((next-x (+ xoffset char-width wonine))
		     (next-y (- yoffset char-height)))
		 (cond ((char= char #\Newline)
			(setf xoffset x
			      yoffset next-y))
		       (t (incf times 4)
			  (let ((code (char-code char)))
			    (multiple-value-bind (x0 y0 x1 y1) (index-quad-lookup lookup code)
			      (etouq (ngorp (preach 'etex (duaq 1 nil '(x0 x1 y0 y1)))))))
			  (etouq (ngorp
				  (preach
				   'epos
				   (quadk+ 'z '(xoffset
						(- next-x wonine)
						next-y
						yoffset)))))
			  (setf xoffset next-x))))))))
       times))))

(progno
      (gl:bind-texture :texture-2d (get-stuff :font *stuff* *backup*))
      (namexpr *backup* :string
	       (lambda ()
		 (create-call-list-from-func
		  (lambda ()
		    (gl-draw-quads 
		     (lambda (tex-buf pos-buf fg-buf bg-buf)
		       (let ((times (draw-string-raster-char
				     pos-buf tex-buf
				     *16x16-tilemap* foo
				     18.0
				     32.0
				     0.0 0.0 +single-float-just-less-than-one+)))
			 (iter-ator:wasabios ((efg fg-buf)
					      (ebg bg-buf))
			   (dotimes (x times)
			     (etouq (ngorp (preach 'efg '(0f0 1f0 0f0))))
			     (etouq (ngorp (preach 'ebg '(0f0 0f0 (random 1f0)))))))
			 times)))))))
      (gl:call-list (get-stuff :string *stuff* *backup*)))

(progno
 (let ((newlen (length e:*chars*))
       (changed nil))
   (dotimes (pos newlen)
     (vector-push-extend (aref e:*chars* pos) foo))
   (cond ((e:key-j-p (cffi:foreign-enum-value (quote %glfw::key) :enter))
	  (multiple-value-bind (data p) (read-string foo nil)
	    (cond (p (print data)
		     (setf (fill-pointer foo) 0)
		     (with-output-to-string (var foo)
		       (prin1
			(handler-bind ((condition (lambda (c)
						    (declare (ignorable c))
						    (invoke-restart
						     (find-restart (quote continue))))))
			  (restart-case
			      (eval data)
			    (continue () data)))
			var))
		     )
		  (t (vector-push-extend #\Newline foo))))
	  (setf changed t)))
   (cond ((e:key-j-p (cffi:foreign-enum-value (quote %glfw::key) :backspace))
	  (setf (fill-pointer foo) (max 0 (1- (fill-pointer foo))))
	  (setf changed t)))
   (when (or changed (not (zerop newlen)))
     (let ((list (get-stuff :string *stuff* *backup*)))
       (when list
	 (gl:delete-lists list 1))
       (remhash :string *stuff*))))

 (defun goo (c)
   (declare (ignorable c))
   (let ((restart (find-restart (quote goober))))
     (when restart (invoke-restart restart))))

 (defun read-string (string otherwise)
   (handler-bind ((end-of-file #'goo))
     (restart-case
	 (values (read-from-string string nil otherwise) t)
       (goober ()
	 :report "wtf"
	 (values otherwise nil))))))


(progno
    (namexpr *backup* :chunks
	     (quad-mesh 
	      (lambda (tex-buf pos-buf fg-buf bg-buf)
		(let ((times (draw-box-char
			      pos-buf tex-buf
			      *16x16-tilemap* *chunks*
			      0 *chunk-width* 0 *chunk-height*
			      *block-width*
			      *block-height*
			      +single-float-just-less-than-one+)))
		  
		  (attrib-repeat fg-buf times (map-into *vec3-scratch* (lambda () (random 1f0))))
		  (attrib-repeat bg-buf times (map-into *vec3-scratch* (lambda () (random 1f0))))
		  times))))
    (gl:call-list (get-stuff :chunks *stuff* *backup*)))

(progno
 ((eql +press+ old)
  (cond ((eql new +release+) +release+)
	((eql new +press+) t)
	((eql new +repeat+) +repeat+)))
 ((eql +release+ old)
  (cond ((eql new +release+) nil)
	((eql new +press+) +press+)
	((eql new +repeat+) (print "huh") +press+)))
 ((eql +repeat+ old)
  (cond ((eql new +release+) +release+)
	((eql new +press+) (print "huh") +press+)
	((eql new +repeat+) +repeat+))))
(progno
 (defun next-key-state (old new)
  (cond ((eq nil old)
	 (cond ((eql new +release+) nil)
	       ((eql new +press+) +press+)
	       ((eql new +repeat+) (print "wtf") +press+)))
	((eq t old)
	 (cond ((eql new +release+) +release+)
	       ((eql new +press+) t)
	       ((eql new +repeat+) +repeat+))))))

(progno
 (defun bar (d b)
   (sqrt (* -2 d (log (- 1 b))))))

(progno
 (with-unsafe-speed
  (defun itsafixnum (x)
    (+ 1 (the fixnum x)))))

(progno (let ((pos-buf (aref bufs 0))
		 (tex-buf (aref bufs 1))
		 (lit-buf (aref bufs 2)))
	     (declare (type iter-ator:iter-ator pos-buf tex-buf lit-buf)))
	   (iter-ator:wasabios ((epos pos-buf)
				(etex tex-buf)
				(elit lit-buf))))
(progno (let ((pos (aref bufs 0))
		  (tex (aref bufs 1))
		  (fg (aref bufs 2))
		  (bg (aref bufs 3)))
	      (declare (type iter-ator:iter-ator tex pos)))
	    (iter-ator:wasabiis ((uv tex)
				 (xyz pos)
				 (eft fg)
				 (ebg bg))))

(progno (let ((pos (aref bufs 0))
		    (tex (aref bufs 1))
		    (col (aref bufs 2)))
		(declare (type iter-ator:iter-ator tex pos col)))
	      (iter-ator:wasabiis ((uv tex)
				   (xyz pos)
				   (eft col))))

(progno
	 (let ((pos-buf (aref bufs 0))
	       (tex-buf (aref bufs 1))
	       (fg-buf (aref bufs 2))
	       (bg-buf (aref bufs 3))))
	 (declare (type iter-ator:iter-ator pos-buf tex-buf fg-buf bg-buf))
	 (iter-ator:wasabios ((epos pos-buf)
			      (etex tex-buf)
			      (efg fg-buf)
			      (ebg bg-buf))))

(progno
 (sb-ext:run-program "/usr/bin/ssh"
		     (list "-t" "terminal256@0.0.0.0")
		     :wait nil
		     :output :stream
		     :input :stream
		     :external-format :utf-8))

(progno
 (when (skey-r-or-p :up) (incf *cursor-y*) (setf *cursor-moved* *ticks*))
 (when (skey-r-or-p :down) (decf *cursor-y*) (setf *cursor-moved* *ticks*))
 (when (skey-r-or-p :left) (decf *cursor-x*) (setf *cursor-moved* *ticks*))
 (when (skey-r-or-p :right) (incf *cursor-x*) (setf *cursor-moved* *ticks*))

 (progno
  (typing-insert (logior
		  (char-code char)
		  *white-black-color*)
		 *cursor-x* *cursor-y*)			      
  (incf *cursor-x*)


  )

 (progno
  (when (skey-r-or-p :u)
    (toggle *scroll-sideways*))
  (let ((scroll (ceiling e:*scroll-y*)))
    (unless (zerop scroll)
      (if *scroll-sideways*
	  (incf *camera-x* (* 1 scroll))
	  (incf *camera-y* (* 1 scroll)))
      (setf *cursor-moved* *ticks*))))
 (progno
  (typing-delete *cursor-x* *cursor-y*)
  (decf *cursor-x*)
  (setf *cursor-moved* *ticks*))
 (progno (when (smice-p :left)
	   (setf *cursor-x* (+ *camera-x* (floor *mouse-x* *block-width*))
		 *cursor-y* (+ *camera-y* (floor *mouse-y* *block-height*)))
	   (setf *cursor-moved* *ticks*))))
(progno
   (unless (zerop (fill-pointer foo))
     (setf (values *print-head-x* *print-head-y*)
	   (copy-string-to-world *print-head-x* *print-head-y*
				 0 foo
				 (strip-char (or (pix:get-obj (pix:xy-index *cursor-x* *cursor-y*)
							      *chunks*)
						 0))
				 (lambda (x) (mod (1+ x) 64))
				 (lambda (y) (mod (1- y) 32)))))
   (setf (fill-pointer foo) 0))

(progno
    (when (or (skey-j-p :left-shift)
	      (skey-j-p :right-shift))
      (enter "[C"))
    (when (or (skey-j-p :left-alt)
	      (skey-j-p :right-alt))
      (enter "[C"))
    (when (or (skey-j-p :left-control)
	      (skey-j-p :right-control))
      (enter "[C"))
    (when (or (skey-j-p :left-super)
	      (skey-j-p :right-super))
      (enter "[C")))

   (progno
    (let ((len (length e:*chars*)))
      (unless (zerop len) (setf *cursor-moved* *ticks*))
      (dotimes (x len)
	(let ((char (vector-pop e:*chars*)))
	  (enter (string char))))))

(progno
 (with-unsafe-speed
   (defun wow (x)
     (declare (type (complex fixnum) x))
     (the fixnum (+ (realpart x)
		    (imagpart x)))))

 (with-unsafe-speed
   (defun wow2 (x)
     (declare (type (cons fixnum fixnum) x))
     (the fixnum (+ (car x)
		    (cdr x))))))

(progno
  (declaim (ftype (function (vector
			     simple-vector
			     simple-vector
			     fixnum fixnum fixnum fixnum
			     single-float single-float single-float)
			    fixnum)
		  draw-box-char))
  (with-unsafe-speed
    (defun draw-box-char (bufs
			  lookup world
			  bx0 bx1 by0 by1
			  char-width char-height
			  z)
      (let ((nope 0)
	    (index 0))
	(with-iterators (epos etex efg ebg) bufs iter-ator:wasabios iter-ator:iter-ator
	  (dobox ((ix bx0 bx1)
		  (iy by0 by1))
		 (let ((obj (aref world index)))
		   (if obj
		       (let ((value (get-char-num obj)))
			 (declare (type fixnum value))
			 (with-char-colors (xfg-r xfg-g xfg-b xbg-r xbg-g xbg-b) value
			   (let ((fg-r (aref +byte-fraction-lookup+ xfg-r))
				 (fg-g (aref +byte-fraction-lookup+ xfg-g))
				 (fg-b (aref +byte-fraction-lookup+ xfg-b))
				 (bg-r (aref +byte-fraction-lookup+ xbg-r))
				 (bg-g (aref +byte-fraction-lookup+ xbg-g))
				 (bg-b (aref +byte-fraction-lookup+ xbg-b)))
			     (dotimes (x 4)
			       (etouq (ngorp (preach 'efg '(fg-r fg-g fg-b))))
			       (etouq (ngorp (preach 'ebg '(bg-r bg-g bg-b)))))))
			 (multiple-value-bind (x0 y0 x1 y1) (index-quad-lookup lookup (mod value 256))
			   (etouq (ngorp (preach 'etex (duaq 1 nil '(x0 x1 y0 y1))))))
			 (let ((foox0 (* (float ix) char-width))
			       (fooy0 (* (float iy) char-height)))
			   (let ((x1 (+ foox0 char-width))
				 (y1 (+ fooy0 char-height)))
			     (etouq (ngorp (preach 'epos (quadk+ 'z '(foox0 x1 fooy0 y1))))))))
		       (incf nope)))
		 (incf index)))
	(* 4 (- (* (- bx1 bx0) (- by1 by0)) nope))))))

(progno
 '(if nil
   
   (let ((foox0 (* (float ix) char-width))
	 (fooy0 (* (float iy) char-height)))
     (let ((x1 (+ foox0 char-width))
	   (y1 (+ fooy0 char-height)))
       (etouq (ngorp (preach 'epos (quadk+ 'z '(foox0 x1 fooy0 y1)))))))))

(progno
 (defmacro with-iterators ((&rest bufvars) buf func type &body body)
  (let* ((letargs nil)
	 (counter 0)
	 (syms (mapcar (lambda (x) (gensym (string x))) bufvars))
	 (bindings nil)
	 (decl `(declare (type ,type ,@syms))))
    (dolist (sym syms)
      (push `(,sym (aref ,buf ,counter)) letargs)
      (push `(,(pop bufvars) ,sym) bindings)
      (incf counter))
    `(let ,letargs ,decl
	  (,func ,bindings
		 ,@body)))))

(progno
 (with-unsafe-speed
   (defun lol (n s)
     (declare (values fixnum)
	      (type fixnum n s))
     (sb-kernel:shift-towards-end n s))))

(progno
 (with-unsafe-speed
   (defun lol (n s)
     (declare (values fixnum)
	      (type fixnum n s))
     (dpb -1 (byte s 8) n))))

(progno
 (progn
  (declaim (ftype (function (fixnum fixnum simple-vector) t)
		  get2)
	   (inline get2))
  (with-unsafe-speed
    (defun get2 (x y world)
      (let ((subarray
	     (let ((index (multiple-value-bind (xbig ybig) (page x y 4 4)
			    (index xbig ybig 8 8))))
	       (aref world index))))
	(declare (type (or null simple-vector) subarray))
	(if subarray
	    (aref subarray
		  (let ((sub-index (index x y 4 4)))
		    sub-index))))))))

(progno
 (progn
  (declaim (ftype (function (fixnum fixnum simple-vector t) t)
		  set2))
  (declaim (inline set2))
  (with-unsafe-speed
    (defun set2 (x y world value)
      (let ((subarray
	     (let ((index
		    (multiple-value-bind (xbig ybig) (page x y 4 4)
		      (index xbig ybig 8 8))))
	       (let ((sub (aref world index)))
		 (if sub
		     sub
		     (setf (aref world index)
			   (make-chunk)))))))
	(declare (type simple-vector subarray))
	(let ((sub-index (index x y 4 4)))
	  (setf (aref subarray sub-index) value)))))))

(progno
 (defun typing-insert (value x y)
   (let ((start (pix:xy-index x y)))
     (let ((old-value (pix:get-obj start *chunks*)))
       (set-char-with-update start value)
       (if old-value
	   (typing-insert old-value (1+ x) y)))))

 (defun typing-delete (x y)
   (let ((start (pix:xy-index x y)))
     (let ((old-value (pix:get-obj start *chunks*))
	   (prev (pix:xy-index (1- x) y)))
       (cond (old-value 
	      (set-char-with-update prev old-value)
	      (typing-delete (1+ x) y))
	     (t (set-char-with-update prev nil))))))
 (progn
   (declaim (ftype (function (fixnum fixnum fixnum (vector character) fixnum
				     (function (fixnum) fixnum)
				     (function (fixnum) fixnum))
			     (values fixnum fixnum))
		   copy-string-to-world))
   (defun copy-string-to-world (x y newline-start string color next-x-func next-y-func)
     (let ((len (length string)))
       (dotimes (index len)
	 (let ((char (aref string index)))
	   (cond ((char= char #\Newline)
		  (setf x newline-start y (funcall next-y-func y)))
		 (t		     
		  (set-char-with-update (pix:xy-index x y)
					(logior (char-code char) color))
		  (setf x (funcall next-x-func x))))))
       (values x y)))))

(progno (defparameter *achar* 0))

(progno (defparameter foo
	  (make-array 0 :adjustable t :fill-pointer 0
		      :element-type (quote character))))

(progno (defparameter *scroll-sideways* nil))

(progno
  (defparameter *print-head-x* 0)
  (defparameter *print-head-y* 127))

(progno
 (defparameter *mat4-identity* (cg-matrix:identity-matrix)))

(progno      (draw-window *hud-rectangle* *chunks* *chunk-call-lists* 16 16 *hud-x* *hud-y*))

(progno
  (defparameter *terminal-start-x* 0)
  (defparameter *terminal-start-y* 0))


(progno (when (skey-j-p :escape) (window:toggle-mouse-capture)))
