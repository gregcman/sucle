(in-package :sandbox)

(progno
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

(progno
 (progn
  (declaim (ftype (function (iter-ator:iter-ator fixnum (simple-array t)))
		  attrib-repeat))
  (with-unsafe-speed
    (defun attrib-repeat (buf times vector)
      (iter-ator:wasabios ((ebuf buf))
	(let ((len (length vector)))
	  (dotimes (x times)
	    (dotimes (index len)
	      (let ((value (aref vector index)))
		(ebuf value))))))))))

(progno
 (defun create-call-list-from-func (func &optional (the-list (gl:gen-lists 1)))
   (gl:with-new-list (the-list :compile)
     (funcall func))
   the-list))

(progno
 (defparameter *one-over-window-width* 0.0)
 (defparameter *one-over-window-height* 0.0))

(progno  (setf *one-over-window-width* (/ 1.0 e:*width*)
		 *one-over-window-height* (/ 1.0 e:*height*)))

(progno
  (defparameter *chunk-width* 16)
  (defparameter *chunk-height* 16))

(progno
  (defparameter *window-block-height* 0.0)
  (defparameter *window-block-width* 0.0))

(progno
   (setf *window-block-width* (/ e:*width* *block-width*)
	*window-block-height* (/ e:*height* *block-height*)))

(progno
 (let ((chunk (gethash chunk-id *chunk-call-lists*)))
    (when chunk
      (gl:delete-lists chunk 1)
      (remhash chunk-id *chunk-call-lists*))))

(progno
     (declaim
      (ftype (function (fixnum hash-table) t) get-obj)
      (ftype (function (fixnum t hash-table) t) set-obj))
     (defun set-obj (place value world)
       (with-chunk-or-null (chunk hash-id) (place world)
	 (unless chunk
	   (let ((new-chunk (make-chunk)))
	     (setf (gethash hash-id world) new-chunk)
	     (setf chunk new-chunk)))
	 (setf (aref chunk (chunk-ref place)) value)
	 hash-id))
     (defun get-obj (place world)
       (with-chunk-or-null (chunk) (place world)
	 (if chunk
	     (aref chunk (chunk-ref place)))))
     (progn
       (declaim (inline (setf get-obj)))
       (defun (setf get-obj) (value place hash-table)
	 (set-obj place value hash-table))))
(progno
 (defun chunk-ref (place)
      (let* ((num (logand place +index-mask+))
	     (num2 (ash num +right-shift+))
	     (num3 (logand +xy-bitmask+ (logior num num2))))
	num3)))

(progno
   (defmacro with-chunk-or-null ((chunk &optional (hash-id (gensym))) (place hash) &body body)
     `(let* ((,hash-id (logand ,place +hash-mask+))
	     (,chunk (gethash ,hash-id ,hash)))
	(declare (type (or null simple-vector) ,chunk))
	,@body)))

(progno (ftype (function (fixnum) fixnum) chunk-ref)
	(inline get-obj set-obj chunk-ref))

(progno (defconstant +not-n-bits+ (let ((array (make-array +available-bits+)))
				    (dotimes (x (length array))
				      (setf (aref array x) (fixnum-not
							    (aref +n-bits+ x))))
				    array)))

(progno
   (let ((place (xy-index x y)))
     (let* ((hash-id (logand place +hash-mask+))
	    (chunk (gethash hash-id world)))
       (declare (type (or null simple-vector) chunk))  
       (unless chunk
	 (let ((new-chunk (make-chunk)))
	   (setf (gethash hash-id world) new-chunk)
	   (setf chunk new-chunk)))
       (let* ((num (logand place +index-mask+))
	      (num2 (ash num +right-shift+))
	      (num3 (logand +xy-bitmask+ (logior num num2))))
	 (values chunk num3)))))
x
(progno
 (defconstant +x-bits-start+ (floor +available-bits+ 2))
 (defconstant +x-chunk-bits+ 4)
 (defconstant +x-chunk-size+ (ash 1 +x-chunk-bits+))
 (defconstant +x-bitmask+ (1- +x-chunk-size+))
 (defconstant +y-chunk-bits+ 4)
 (defconstant +y-chunk-size+ (ash 1 +y-chunk-bits+))
 (defconstant +y-bitmask+ (1- +y-chunk-size+))
 (defconstant +xy-bitmask+ (1- (* +y-chunk-size+ +x-chunk-size+)))
 (defconstant +index-mask+ (logior (ash +x-bitmask+ +x-bits-start+)
				   +y-bitmask+))
 (defconstant +hash-mask+ (logxor +index-mask+ most-positive-fixnum))
 (defconstant +right-shift+ (- +y-chunk-bits+ +x-bits-start+))
 (defconstant +y-mask+ (1- (ash 1 +x-bits-start+)))
 (defconstant +chunk-capacity+ (* +x-chunk-size+ +y-chunk-size+)))

(progno
 (dotimes (foobar 10)
   (let ((char (get-char x y *chunks*)))
     (when char
       (set-char-with-update x y (case 0
				   (0 nil)
				   (2 (logior *white-black-color* (logand char 255)))
				   (1 (logior (acolor val4 val2 val1 val3 val1 val2)
					      (if nil (char-code #\space) (random 256)))))
			     *chunks*)))
   (incf val1 1)
   (incf val2 2)
   (incf val3 3)
   (incf val4 4)
   (if (zerop (random 2))
       (incf x (if (< mousex x)
		   -1
		   1))
       (incf y (if (< mousey y)
		   -1
		   1)))
   (progno (setf x (mod x 128)
		 y (mod y 128)))))

(progno
 (defun lol (x)
   (let ((buf (let ((buf (load-time-value (make-array 0 :adjustable t :fill-pointer 0))))
		(setf (fill-pointer buf) 0)
		buf)))
     (dotimes (b (random x))
       (vector-push-extend b buf))
     (print buf))))
(progno
 ((square-p char)
  (multiple-value-bind (tripointx tripointy) (deref-triangle char)
    (if (and (= tripointx squareoffsetx)
	     (= tripointy squareoffsety))
	(print "a1")
	(print "a2")))))

(progno

 (defun update-elastic (xtri ytri data)
   (multiple-value-bind (xloc yloc) (deref-triangle data)
     (let ((squares (let ((buf (load-time-value (make-array 0 :adjustable t :fill-pointer 0))))
		      (setf (fill-pointer buf) 0)
		      buf))
	   (triangles (let ((buf (load-time-value (make-array 0 :adjustable t :fill-pointer 0))))
			(setf (fill-pointer buf) 0)
			buf))
	   (new-triangles (let ((buf (load-time-value (make-array 0 :adjustable t :fill-pointer 0))))
			    (setf (fill-pointer buf) 0)
			    buf)))
       (let ((num (get-char xloc yloc *chunks*)))
	 (when num
	   (when (typep num (quote fixnum))
	     (let ((move-p t)
		   (last-one t))
	       (flet ((neighbor (dx dy)
			(let ((neighborx (+ dx xloc))
			      (neighbory (+ dy yloc))
			      (trioffsetx (+ dx xtri))
			      (trioffsety (+ dy ytri)))
			  (let ((char (get-char neighborx neighbory *chunks*)))
			    (cond ((square-p char)
			      ;;;;reverse square unless transferring to the correct location
				   (multiple-value-bind (squarexto squareyto) (deref-square char)
				     (if (and (= squarexto trioffsetx)
					      (= squareyto trioffsety))
					 ;;its transferring the correct way, make sure to notify on move
					 (progn
					   (flet ((add-item (x)
						    (vector-push-extend x squares)))
					     (add-item neighborx) ;;;where it is
					     (add-item neighbory)
					     (add-item char) ;;where it points
					     ))
					 ;;else reverse it
					 (progn
					;;;TODO
					   (reverse-square neighborx neighbory char)
					   (setf move-p nil)
					   ))))
				  ((triangle-p char)
			      ;;;;wait for it to get out of the way, or add it to the active cell states
				   ;;  (print "NOOOOO")
				   (progno (setf (car char) :square)
					   (chunk-update neighborx neighbory *chunks*))
				   
				   (setf move-p nil)
				   )
				  (t
			      ;;;; its data
				   (setf last-one nil)
				   (let ((new-pos (get-char trioffsetx trioffsety *chunks*)))
				     (if new-pos
					 (progn
					   (cond ((triangle-p new-pos)
						  (multiple-value-bind (xoldtri yoldtri)
						      (deref-triangle new-pos)
						    (if (and (= xoldtri neighborx)
							     (= yoldtri neighbory))
							(progn
							  ;;new-pos dummy already there
							  )
							(setf move-p nil))))
						 ((square-p new-pos)
						  ;;	 (print 34434)
						  (multiple-value-bind (xoldsquare yoldsquare)
						      (deref-square new-pos)
						    (if (and (= xoldsquare neighborx)
							     (= yoldsquare neighbory))
							(reverse-square trioffsetx trioffsety new-pos)
							
							(setf move-p nil))))
						 (t ;;(print new-pos)
						  (setf move-p nil)))) ;;cannot transfer, data in the way
					 (progn
				      ;;;;space for new triangles, remember
				      ;;;the new space and the place it points to
				      ;;;so a new triangle can be created
					   (flet ((add-item (x)
						    (vector-push-extend x new-triangles)))
					     (add-item trioffsetx) ;;where it will be
					     (add-item trioffsety) 
					     (add-item neighborx) ;;where it will point
					     (add-item neighbory)))))
				   ))))))
		 (etouq
		  (let ((start 56))
		    (let ((left (ash 1 (+ 0 start)))
			  (down (ash 1 (+ 1 start)))
			  (right (ash 1 (+ 2 start)))
			  (up (ash 1 (+ 3 start))))
		      `(progn
			 (when (logtest num ,left)
			   (neighbor -1 0))		
			 (when (logtest num ,down)
			   (neighbor 0 -1))		
			 (when (logtest num ,right)
			   (neighbor 1 0))		
			 (when (logtest num ,up)
			   (neighbor 0 1)))))))

	       ;;   (print move-p)
	       (when move-p
	      ;;;;move the item
		 (progn (set-char-with-update xtri ytri num *chunks*))
		 (let ((new-tri-count (fill-pointer new-triangles)))
		   (dobox ((offset 0 new-tri-count :inc 4))
			  (etouq
			   (with-vec-params '((offset tx ty nx ny))
			     '(new-triangles)
			     '(set-char-with-update tx ty
			       (make-triangle nx ny) *chunks*))))
		   (if last-one
		    ;;;dont leave a square, because theres no one left,
		    ;;;and update the sqaures left behind so they can be cleaned up 
		       (progn
			 (set-char-with-update xloc yloc nil *chunks*) ;;;don't leave anything
			 )

		    ;;;;leave a square, and create the new triangles
		       (progn
			 (set-char-with-update xloc yloc (make-square xtri ytri) *chunks*);;;deposit square
			 )
		       ))
		 (dobox ((offset 0 (fill-pointer squares) :inc 3))
			(etouq
			 (with-vec-params '((offset sx sy data))
			   '(squares)
			   '(if (zerop (update-square sx sy data))
			     (set-char-with-update sx sy nil *chunks*)))))
		 (dobox ((offset 0 (fill-pointer triangles) :inc 3))
			(etouq
			 (with-vec-params '((offset sx sy data))
			   '(squares)
			   '(if (zerop (update-square sx sy data))
			     (set-char-with-update sx sy nil *chunks*)))))))))))))

 (defun make-square (x y)
   (cons :square (pix::xy-index x y)))

 (defun make-triangle (x y)
   (cons :triangle (pix::xy-index x y)))

 (defun square-p (x)
   (when (listp x)
     (eq :square (car x))))

 (defun deref-square (x)
   (pix::index-xy (cdr x)))
 (defun deref-triangle (x)
   (pix::index-xy (cdr x)))

 (defun triangle-p (x)
   (when (listp x)
     (eq :triangle (car x))))

 (defun update-square (xsquare ysquare data)
   (multiple-value-bind (xloc yloc) (deref-square data)
     (let ((num (get-char xloc yloc *chunks*)))
       (let ((in-use-p 0))
	 (when num
	   (when (typep num (quote fixnum))
	     (flet ((neighbor (dx dy)
		      (let ((neighborx (+ dx xloc))
			    (neighbory (+ dy yloc))
			    (squareoffsetx (+ dx xsquare))
			    (squareoffsety (+ dy ysquare)))
			(let ((char (get-char neighborx neighbory *chunks*)))
			  (cond ((square-p char)
				;;;it can be moved
				 (multiple-value-bind (quadx quady) (deref-square char)
				   (if (and (= quadx squareoffsetx)
					    (= quady squareoffsety))
				       (progn (incf in-use-p)
					      ;;points toward, in use
					      )
				       (progn ;;;nothing
					 ))))
				((triangle-p char)
				;;;if the triangle points back to the square, its still used
				;;;if it points away, its not
				 (multiple-value-bind (trix triy) (deref-triangle char)
				   (if (and (= trix squareoffsetx)
					    (= triy squareoffsety))
				       (progn (incf in-use-p)
					      ;;points toward, in use
					      )
				       (progn ;;;nothing
					 )))
				 )
				(t ;;;its connected
			;;;	(print char)
					;	(setf in-use-p t)
				 ))))))
	       (etouq
		(let ((start 56))
		  (let ((left (ash 1 (+ 0 start)))
			(down (ash 1 (+ 1 start)))
			(right (ash 1 (+ 2 start)))
			(up (ash 1 (+ 3 start))))
		    `(progn
		       (when (logtest num ,left)
			 (neighbor -1 0))		
		       (when (logtest num ,down)
			 (neighbor 0 -1))		
		       (when (logtest num ,right)
			 (neighbor 1 0))		
		       (when (logtest num ,up)
			 (neighbor 0 1)))))))))
	 in-use-p))))

 (defun reverse-square (xsquare ysquare data)
;;;  (print (list xsquare ysquare))
   (setf (car data) :triangle)
   (chunk-update xsquare ysquare *chunks*)
   (multiple-value-bind (xloc yloc) (deref-square data)
     (let ((num (get-char xloc yloc *chunks*)))
       (when num
	 (when (typep num (quote fixnum))
	   (flet ((neighbor (dx dy)
		    (let ((neighborx (+ dx xloc))
			  (neighbory (+ dy yloc))
			  (squareoffsetx (+ dx xsquare))
			  (squareoffsety (+ dy ysquare)))
		      (let ((char (get-char neighborx neighbory *chunks*)))
			(cond ((triangle-p char)
			       (multiple-value-bind (tripointx tripointy) (deref-triangle char)
				 (if (and (= tripointx squareoffsetx)
					  (= tripointy squareoffsety))
				     (progn (setf (car char) :square)
					;     (print "b2")
					    (chunk-update neighborx neighbory *chunks*)))
				 ;;   (print "AHHHH")
				 
				 )))))))
	     (etouq
	      (let ((start 56))
		(let ((left (ash 1 (+ 0 start)))
		      (down (ash 1 (+ 1 start)))
		      (right (ash 1 (+ 2 start)))
		      (up (ash 1 (+ 3 start))))
		  `(progn
		     (when (logtest num ,left)
		       (neighbor -1 0))		
		     (when (logtest num ,down)
		       (neighbor 0 -1))		
		     (when (logtest num ,right)
		       (neighbor 1 0))		
		     (when (logtest num ,up)
		       (neighbor 0 1))))))))))))


 (progn
   (defparameter *qux* (make-array 0 :adjustable t :fill-pointer 0))
   (defun wow ()
     (map-box (lambda (x y)
		(let ((char (get-char x y *chunks*)))
		  (cond ((triangle-p char)		   
			 (when (zerop (random 10))
			   (vector-push-extend (pix:xy-index x y) *qux*)
			   (update-elastic x y char)))))))))


 (defparameter *yolo* (copy-seq *qux*))
 (defparameter *baggins* (copy-seq *baz*))

 (defun test-reset ()
   (progn
     (klear)
     (damn-test3 0 0 32 5)
     (dobox ((offset 0 12 :inc 4))
	    (etouq (with-vec-params '((offset x y woffset hoffset)) '(*baggins*)
				    '(set-char-with-update x y
				      (make-triangle woffset hoffset) *chunks*))))
     (defparameter *offset* 0)
     (dotimes (x 24)
       (test-step))))

 (defun test-step ()
   (progn (multiple-value-bind (x y) (pix::index-xy (aref *yolo* *offset*))
	    (update-elastic x y (get-char x y *chunks*)))
	  (incf *offset*)))
 )

(progno (when (skey-p :space)
		   (if nil (test-step)
		       (dotimes (x 10)
			 (wow)))))

(progno (progn (when (skey-j-p :e)
			  (if nil
			      (test-reset)
			      (progn
				(setf (fill-pointer *qux*) 0)
				(klear)
				(test3))))))

(progno
 (defun print-sexp (sexp)
   (if (listp sexp)
       (if sexp
	   (progn
	     (princ "(")
	     (print-cells sexp))
	   (princ nil))
       (princ sexp)))

 (defun emit-spaces (times)
   (dotimes (x times)
     (princ " ")))

 (defun print-cells (sexp)
   (let ((cdr (cdr sexp))
	 (car (car sexp)))
     (if (listp car)
	 (if car
	     (progn
	       (princ "(")
	       (print-cells car))
	     (princ nil))
	 (prin1 car))
     (if (listp cdr)
	 (if cdr
	     (progn
	       (princ " ")
	       (print-cells cdr))
	     (princ ")"))
	 (progn
	   (princ " . ")
	   (prin1  cdr)
	   (princ ")"))))))


(progno
  (defparameter wombo nil)
  (defparameter hello nil)
  (defun test ()
    (setf wombo (vector-circular-node "wombo "))
    (setf hello (vector-nodes2 "hello "))
    (node-splice
     (nthcdr 5 hello)
     wombo)
    (nodes-vector hello)))

(progno
 (defparameter *postexcol* (quote (("POS" . 0)	
				   ("TEX" . 8)
				   ("COL" . 9)))))

(progno	   (:fgindirection . "fgindirection")
	   (:bgindirection . "bgindirection"))

(progno (fgindirection (getuniform solidshader-uniforms :fgindirection))
	(bgindirection (getuniform solidshader-uniforms :bgindirection)))

(progno
 (gl:uniformi fgindirection 2)
 (gl:active-texture (+ 2 +gltexture0+))    
 (gl:bind-texture :texture-2d (get-stuff :text-scratch-fg *stuff* *backup*))
 
 (gl:uniformi bgindirection 3)
 (gl:active-texture (+ 3 +gltexture0+))    
 (gl:bind-texture :texture-2d (get-stuff :text-scratch-bg *stuff* *backup*)))

(progno
 (progn
   
   
   (gl:bind-texture :texture-2d (get-stuff :text-scratch *stuff* *backup*))
   (gl:tex-sub-image-2d :texture-2d 0 0 0 width height :rgba :unsigned-byte a))
 (progn
   (let ((b (load-time-value (cffi:foreign-alloc :uint8 :count (* 256 256 4)))))
     (dotimes (x (* width height))
       (let ((offset (* 4 x)))
	 (progn
	   (setf (cffi:mem-aref b :uint8 (+ offset 0)) (random 256))
	   (setf (cffi:mem-aref b :uint8 (+ offset 1)) (random 256))
	   (setf (cffi:mem-aref b :uint8 (+ offset 2)) (random 256))
	   (setf (cffi:mem-aref b :uint8 (+ offset 3)) (random 256)))))
     (gl:bind-texture :texture-2d (get-stuff :text-scratch-bg *stuff* *backup*))
     (gl:tex-sub-image-2d :texture-2d 0 0 0 width height :rgba :unsigned-byte b)))
 (progn
   (let ((c (load-time-value (cffi:foreign-alloc :uint8 :count (* 256 256 4)))))
     (dotimes (x (* width height))
       (let ((offset (* 4 x)))
	 (progn
	   (setf (cffi:mem-aref c :uint8 (+ offset 0)) (random 256))
	   (setf (cffi:mem-aref c :uint8 (+ offset 1)) (random 256))
	   (setf (cffi:mem-aref c :uint8 (+ offset 2)) (random 256))
	   (setf (cffi:mem-aref c :uint8 (+ offset 3)) (random 256)))))
     (gl:bind-texture :texture-2d (get-stuff :text-scratch-fg *stuff* *backup*))
     (gl:tex-sub-image-2d :texture-2d 0 0 0 width height :rgba :unsigned-byte c))))

(when (skey-p :k)
  (let ((width *window-block-width*)
	(height *window-block-height*))
    (let ((b (get-stuff :glyph-screen *other-stuff* *backup*)))
      (dotimes (x (* width height))
	(let ((offset (* 4 x)))
	  (progn
	    (setf (cffi:mem-aref b :uint8 (+ offset 0)) (if nil 65 (random 256)))
	    (setf (cffi:mem-aref b :uint8 (+ offset 1)) (if nil 189 (random 256)))
	    (setf (cffi:mem-aref b :uint8 (+ offset 2)) (if nil 222 (random 256)))
					;	 (setf (cffi:mem-aref b :uint8 (+ offset 3)) (random 256))
	    )))
      (progn
	(gl:bind-texture :texture-2d (get-stuff :text-scratch *stuff* *backup*))
	(gl:tex-sub-image-2d :texture-2d 0 0 0 width height :rgba :unsigned-byte b)))))

(progno
 (defparameter *uint-lookup*
   (let ((ans (make-array (length *16x16-tilemap*) :element-type '(unsigned-byte 8))))
     (map-into ans (lambda (x) (round (* x 255))) *16x16-tilemap*))))

(progno
 (defun keyword-ascii (keyword &optional (value (gethash keyword e:*keypress-hash*)))
	  (when value
	    (let ((code (gethash keyword *keyword-ascii*)))
	      (when code
		(let ((mods (ash value (- e::+mod-key-shift+))))
		  (multiple-value-bind (char esc) (convert-char code mods)
		    (values char esc))))))))



(progno
 (defconstant +single-float-just-less-than-one+ 0.99999997)
 (defparameter +byte-fraction-lookup+
   (let ((array (make-array 256 :element-type 'single-float)))
     (dotimes (x 256)
       (setf (aref array x) (/ (float x) 255.0)))
     array)))

(progno
 (defparameter *chunk-call-lists* (make-eq-hash)))

(progno
 (defparameter *snapshot* (make-array (list 1024 1024 4) :element-type '(unsigned-byte 8)))

 (dobox ((x 0 1024)
	 (y 0 1024))
	(let ((value (get-char-num (get-char x y))))
	  (setf (aref *snapshot* x y 0) (ldb (byte 8 0) value))
	  (setf (aref *snapshot* x y 1) (ldb (byte 8 8) value))
	  (setf (aref *snapshot* x y 2) (ldb (byte 8 16) value))
	  (setf (aref *snapshot* x y 3) (ldb (byte 8 24) value))))

 (opticl:write-png-file (saves-path "asnapshot") *snapshot*))

(progno
 (defparameter *x-zoom-offset* 0)
 (defparameter *y-zoom-offset* 0))

(progno (defun set-char-with-update (x y value)
	    (multiple-value-bind (chunk offset) (pix::area2 x y pix::*world*)
	      (setf (aref chunk offset) value)
	      (setf (aref chunk (* 16 16)) *ticks*))))

(progno  (when (skey-p :t)
	     (map-box
	      *cam-rectangle*
	      (lambda (x y)
		(let ((value (get-char x y)))
		  (if (and (listp value) value)
		      (setf (car value) (random most-positive-fixnum)))))))
	   (when (skey-p :q)
	     (map-box
	      *cam-rectangle*
	      (lambda (x y)
		(let ((value (get-char x y))
		      (value2 (get-char x (1- y))))
		  (when (> (get-char-num value2) (get-char-num value))
		    (scwu value x (1- y))
		    (scwu value2 x y)))))
	     (map-box
	      *cam-rectangle*
	      (lambda (x y)
		(let ((value (get-char x y))
		      (value2 (get-char (1- x) y)))
		  (when (> (get-char-num value2) (get-char-num value))
		    (scwu value (1- x) y)
		    (scwu value2 x y))))))
	   (when (skey-p :r)
	     (etouq
	      (with-vec-params
		  (vec-slots :rectangle (quote ((x :x1) (y :y1))))
		'(*point-rectangle*)
		(quote
		 (let ((xstart (- x 0))
		       (ystart (- y 0))
		       (counter 0))
		   (dobox ((x xstart (+ xstart 16))
			   (y ystart (+ ystart 16)))
			  (scwu (logior counter
					(ash (logior counter
						     (ash (- 256 counter) 8)) 8)) x y)
			  (incf counter)))))))
	   (when (skey-p :l)
	     (dobox ((x 0 228)
		     (y 0 70))
		    (scwu (random most-positive-fixnum) x y)))
	   (when (skey-p :v)
	     (etouq
	      (with-vec-params
		  (vec-slots :rectangle (quote ((cx1 :x1) (cy1 :y1) (cx0 :x0) (cy0 :y0))))
		(quote (*point-rectangle*))
		(quote
		 (progn
		   (progn (setf (fill-pointer foobar) 0)
			  (with-output-to-string (out foobar)
			    (print *mouse-rectangle* out))
			  (copy-string-to-world cx1 cy1 foobar))
		   (progn (setf (fill-pointer foobar) 0)
			  (with-output-to-string (out foobar)
			    (print *cursor-rectangle* out))
			  (copy-string-to-world cx1 (+ 1 cy1) foobar))
		   (progn (setf (fill-pointer foobar) 0)
			  (with-output-to-string (out foobar)
			    (print (list *point-x* *point-y*) out))
			  (copy-string-to-world cx1 (+ 2 cy1) foobar))))))))

(progno
 (let ((xstart cx0)
       (ystart cy0))
   (let ((value (list (random most-positive-fixnum))))
     (scwu value xstart ystart)
     (if (> xstart cx1)
	 (rotatef xstart cx1))
     (if (> ystart cy1)
	 (rotatef ystart cy1))
     (dobox ((x xstart (1+ cx1))
	     (y ystart (1+ cy1)))
	    (scwu value x y)))))


(progn
  (defparameter *mandelbrot-iteration-cap* 1024)
  (with-unsafe-speed
    (defun mandelbrot (x y &optional (cap *mandelbrot-iteration-cap*))
      (let ((a (complex x y)))
	(labels ((rec (value count)
		   (declare (type (complex double-float) value))
		   (if (> count cap)
		       nil
		       (if (>= (abs value) pi)
			   count
			   (rec (+ a (expt value pi)) (1+ count))))))
	  (rec a 0))))))
(defun mandel-color (x)
  (if x
      (sxhash x)
      0))
(defun square (x)
  (vector (- x) (- x) x x))

(progn
 (defparameter *snapshot* (make-array (list 1024 1024 4) :element-type '(unsigned-byte 8)))

 (dobox ((x 0 1024)
	 (y 0 1024))
	(let ((value (get-char-num (get-char (- x 512) (- y 512)))))
	  (setf (aref *snapshot* x y 0) (ldb (byte 8 0) value))
	  (setf (aref *snapshot* x y 1) (ldb (byte 8 8) value))
	  (setf (aref *snapshot* x y 2) (ldb (byte 8 16) value))
	  (setf (aref *snapshot* x y 3) (ldb (byte 8 24) value))))

 (opticl:write-png-file (saves-path "amandel2") *snapshot*))
