(in-package :sandbox)

(defstruct camera
  xpos
  ypos 
  zpos 
  upx 
  upy 
  upz 
  yaw 
  pitch 
  fov)

;;matrix multiplication is associative

(defparameter *camera* nil) ;;global camera
(defparameter *view-matrix* nil) ;;view matrix
(defparameter *projection-matrix* nil) ;;projection matrix
(defparameter *projection-view-matrix* nil) ;;projection * view matrix
(defparameter *player-matrix* nil) ;;positional information of camera

(defparameter *window-height* nil)
(defparameter *window-width* nil)
(defparameter *aspect-ratio* nil)

(defun glinnit ()
  (setf %gl:*gl-get-proc-address* (e:get-proc-address))
  (setf e:*resize-hook* #'on-resize)
  (set-framebuffer)
  ;(lpic-ltexture "gui/items.png" :items)
  ;(lpic-ltexture "misc/grasscolor.png" :grasscolor)
  ;(lpic-ltexture "misc/foliagecolor.png" :foliagecolor)
  (lpic-ltexture "terrain.png" :terrain)
  (lpic-ltexture "skybox/cheap.png" :skybox)
  (lpic-ltexture "terrain/sun.png" :sun)
  (lpic-ltexture "terrain/moon.png" :moon)
  (lpic-ltexture "gui/gui.png" :gui)
  ;(lpic-ltexture "font/default.png" :default)
  ;(lpic-ltexture "pack.png" :pack)
  ;(lpic-ltexture "environment/clouds.png" :clouds)

  (let ((vsync? (lget *g/args* :vsync)))    
    (cond (vsync? (window::set-vsync t)
		  (setf render-delay 0))
	  (t (window::set-vsync nil)
	     (setf render-delay (if t 0 (/ 1000000.0 59.88))))))

  (setf *camera* (make-camera
		  :xpos 0
		  :ypos 0
		  :zpos 0
		  :upx 0
		  :upy 1
		  :upz 0
		  :yaw 0
		  :pitch 0
		  :fov 70))
  
  (setf glshader:*shader-program* nil)
  (setf mesher-thread nil)
  (load-shader-programs)
  (update-world-vao))

(defun set-framebuffer ()
  (setf (values *framebuffer-texture* *framebuffer*)
	(create-framebuffer e:*width* e:*height*)))

(defmacro ltexture-bind-ensure (name func-form)
  `(unless (lget *g/call-list* ,name)
     (lcreate-call-list ,func-form ,name)))

(defun render ()
  "responsible for rendering the world"
  (when (window:mice-locked-p)
    (look-around))
  (set-render-cam-look)
  (setf *aspect-ratio* (/ window:*width* window:*height*))
  (with-slots (xpos ypos zpos fov pitch yaw) *camera*
    (let ((floaty-fov (coerce (deg-rad fov) 'single-float)))
      (set-projection-matrix
       floaty-fov
       *aspect-ratio*
       0.01
       (ash 1 7)))
    
    (set-view-matrix
     (unit-pitch-yaw (coerce pitch 'single-float)
		     (coerce yaw 'single-float))
     (sb-cga:vec 0.0 1.0 0.0))

    (set-player-matrix  (coerce xpos 'single-float)
			(coerce ypos 'single-float)
			(coerce zpos 'single-float)))
  (update-projection-view-matrices)

  
  (luse-shader :blockshader)
  (set-overworld-fog daytime)
  (bind-default-framebuffer)
  (gl:clear
   :color-buffer-bit
   :depth-buffer-bit)
  (gl:viewport 0 0 e:*width* e:*height*)
  (gl:disable :blend)
  (glshader:set-matrix
   "projectionmodelview"
   (sb-cga:transpose-matrix
    (sb-cga:matrix* *projection-view-matrix* *player-matrix*)))
  ;;;static geometry with no translation whatsoever
  (draw-chunk-meshes)  
  (progn (when fist?
	   (glshader:set-matrix
	    "projectionmodelview"
	    (sb-cga:transpose-matrix
	       (sb-cga:matrix* *projection-view-matrix* *player-matrix*
			       (sb-cga:translate* (+ (coerce fist-side-x 'single-float))
						  (+ (coerce fist-side-y 'single-float))
						  (+ (coerce fist-side-z 'single-float))))))
	     (ltexture-bind-ensure :selected-box
				   (l () (let ((foo 0.005))
					   (let ((min (- 0.0 foo))
						 (max (+ 1.0 foo)))
					     (draw-box min min min max max max)))))
	     
	     (progn
	       (gl:disable :cull-face :blend)
	       (gl:polygon-mode :front-and-back :line)
	       (ldrawlist :selected-box)
	       (gl:polygon-mode :front-and-back :fill))))

  (gl:disable :cull-face)
 
  (luse-shader :solidshader)
  (glshader:set-matrix "projectionmodelview"
		       sb-cga:+identity-matrix+)
  (gl:enable :blend)
  (gl:depth-func :always)
  (gl:bind-texture :texture-2d *framebuffer-texture*)
  (ltexture-bind-ensure :background #'draw-background)
  (ldrawlist :background)
  
  (bind-shit :gui)
  (gl:blend-func :one-minus-dst-color :one-minus-src-color)
  (ldrawlist :crosshair)
  (window:update-display)

  (bind-custom-framebuffer)
  (gl:clear-color 0.0 0.0 0.0 0.0)
  (gl:clear :color-buffer-bit)
  
  (gl:enable :blend)
  (gl:blend-func :src-alpha :one-minus-src-alpha)
  (bind-shit :gui)
  (ltexture-bind-ensure :gui #'draw-hotbar)
  (ltexture-bind-ensure :hotbar-selector #'draw-hotbar-selector)
  (ltexture-bind-ensure :crosshair #'draw-crosshair)
  (ldrawlist :gui)
  (ldrawlist :hotbar-selector)
  
  (designatemeshing))

(defun on-resize (w h)
  (setf *window-height* h
	*window-width* w)
  (lcalllist-invalidate :gui)
  (lcalllist-invalidate :hotbar-selector)
  (lcalllist-invalidate :crosshair)
  (clean-framebuffers)
  (set-framebuffer))

(defun clean-framebuffers ()
  (gl:delete-framebuffers-ext (list *framebuffer*))
  (gl:delete-textures (list *framebuffer-texture*)))

(defun lcalllist-invalidate (name)
  (let ((old (lget *g/call-list* name)))
    (lremove *g/call-list* name)
    (when old
      (gl:delete-lists old 1))))

(defun create-call-list-from-func (func)
  (let ((the-list (gl:gen-lists 1)))
    (gl:new-list the-list :compile)
    (funcall func)
    (gl:end-list)
    the-list))

(defun lcreate-call-list (func name)
  (let ((the-list (create-call-list-from-func func)))
    (let ((old (lget *g/call-list* name)))
      (setf (lget *g/call-list* name) the-list)
      (when old
	(gl:delete-lists old 1)))))

(defun ldrawlist (name)
  (let ((the-list (lget *g/call-list* name)))
    (if the-list
	(gl:call-list the-list)
	(print "error"))))

(defun update-projection-view-matrices ()
  (setf *projection-view-matrix* (sb-cga:matrix* *projection-matrix* *view-matrix*)))

(defun set-projection-matrix (fovy aspect near far)
  (let ((projection-matrix (projection-matrix fovy aspect near far)))
    (setf *projection-matrix* projection-matrix)))

(defun set-view-matrix (direction up)
  (let ((view (relative-lookat direction up)))
    (setf *view-matrix* view)))

(defun set-player-matrix (x y z)
  (let ((player-matrix (sb-cga:translate* (- x) (- y) (- z))))
    (setf *player-matrix* player-matrix)))

(defun luse-shader (name)
  (glshader:use-program (lget *g/shader* name)))

(defun draw-chunk-meshes ()
  (gl:enable :depth-test)  
  (gl:depth-func :less)

  (gl:enable :cull-face)
  (gl:cull-face :back)
  (bind-shit :terrain)
  (progno
   (gl:read-pixels 0 0 256 256 :rgba :unsigned-byte))
  (ltexture-bind-ensure :world #'draw-world)
  (ldrawlist :world))

(defun draw-world ()
  (maphash
   (lambda (key display-list)
     (when (numberp key)
       (gl:call-list display-list)))
   *g/call-list*))

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

(defun bind-shit (name)
  "bind a texture located in the texture library"
  (let ((num (lget *g/texture* name)))
    (if num
	(gl:bind-texture :texture-2d num)
	(print "error-tried to use NIL texture"))))

;;;turn a picture which is in the image library into an
;;;opengl texture which is in the texture library
(defun lpic-ltexture (image-name &optional (texture-name image-name))
  (let ((thepic (lget *g/image* image-name)))
    (if thepic
	(destructuring-bind (h w c) (array-dimensions thepic)
	  (let ((type (case c
			(3 :rgb)
			(4 :rgba))))
	    (let ((new-texture (create-texture (imagewise:array-flatten thepic) w h type)))
	      (setf (lget *g/texture* texture-name) new-texture)
	      new-texture))))))

(defun load-shader-programs ()
  (load-block-shader)
  (load-solid-shader))

(defun load-block-shader ()
  (let ((old (lget *g/shader* :blockshader)))
    (when old
      (gl:delete-program old)))
  (setf (lget *g/shader*
	      :blockshader)
	(glshader:make-shader-program-from-strings
	 (lget *g/text* :bs-vs)
	 (lget *g/text* :bs-frag)
	 '(("position" . 0)
	   ("texCoord" . 2)
	   ("darkness" . 8)))))

(defun load-solid-shader ()
  (let ((old (lget *g/shader* :solidshader)))
    (when old
      (gl:delete-program old)))
  (setf (lget *g/shader*
	      :solidshader)
	(glshader:make-shader-program-from-strings
	 (lget *g/text* :ss-vs)
	 (lget *g/text* :ss-frag)
	 '(("position" . 0)
	   ("texCoord" . 2)
	   ("darkness" . 8)))))

(defun glActiveTexture (num)
  "sets the active texture"
  (gl:active-texture (+ num (glinfo:get-gl-constant :texture0))))

(defun create-texture (tex-data width height &optional (type :rgba))
  "creates an opengl texture from data"
  (let ((the-shit (car (gl:gen-textures 1))))
    (gl:bind-texture :texture-2d the-shit)
    (gl:tex-parameter :texture-2d :texture-min-filter  :nearest-mipmap-nearest
		      )
    (gl:tex-parameter :texture-2d :texture-mag-filter :nearest)
    (gl:tex-parameter :texture-2d :texture-wrap-s :repeat)
    (gl:tex-parameter :texture-2d :texture-wrap-t :repeat)
    (gl:tex-parameter :texture-2d :texture-border-color '(0 0 0 0))
    (gl:tex-parameter :texture-2d :generate-mipmap :true)
    (gl:tex-image-2d
     :texture-2d 0
     type width height 0 type :unsigned-byte tex-data)
    (gl:generate-mipmap :texture-2d)
    the-shit))

(defparameter *framebuffer* nil)
(defparameter *framebuffer-texture* nil)

(defun bind-default-framebuffer ()
  (gl:bind-framebuffer-ext :framebuffer-ext 0))

(defun bind-custom-framebuffer ()
  (gl:bind-framebuffer-ext :framebuffer-ext *framebuffer*))

(defun create-framebuffer (w h)
  (let ((framebuffer (first (gl:gen-framebuffers-ext 1)))
        (depthbuffer (first (gl:gen-renderbuffers-ext 1)))
        (texture (first (gl:gen-textures 1))))
    ;; setup framebuffer
    (gl:bind-framebuffer-ext :framebuffer-ext framebuffer)

    ;; setup texture and attach it to the framebuffer
    (gl:bind-texture :texture-2d texture)
    (gl:tex-parameter :texture-2d :texture-min-filter :nearest-mipmap-nearest)
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
    (values texture framebuffer)))
