(in-package :sandbox)

(defparameter *window-height* nil)
(defparameter *window-width* nil)
(defparameter *aspect-ratio* nil)

(progn
  (defparameter *g/call-list* (make-hash-table :test 'eq));;opengl call lists
  (hook:add-hook init-hook :call-list (lambda () (clrhash *g/call-list*)))
  (defun get-display-list (name)
    (let ((display-list (gethash name *g/call-list*)))
      (if display-list
	  display-list
	  (get-display-list-backup name))))
  (defun set-display-list (name list-num)
    (setf (gethash name *g/call-list*) list-num))
  (defun remove-display-list (name)
    (remhash name *g/call-list*)))
(progn
  (defparameter *g/call-list-backup* (make-hash-table :test 'eq))
  (defun get-display-list-backup (name)
    (let ((display-list-func (gethash name *g/call-list-backup*)))
      (when (functionp display-list-func)
	(let ((ans (funcall display-list-func name)))
	  (when ans
	    (set-display-list name ans)))))))

(progn
  (defparameter *g/texture* (make-hash-table :test 'eq)) ;;opengl textures
  (hook:add-hook init-hook :texture (lambda () (clrhash *g/texture*)))
  (defun get-texture (name)
    (let ((texture (gethash name *g/texture*)))
      (if texture
	  texture
	  (get-texture-backup name))))
  (defun set-texture (name texture-num)
    (setf (gethash name *g/texture*) texture-num))
  (defun remove-texture (name)
    (remhash name *g/texture*)))
(progn
  (defparameter *g/texture-backup* (make-hash-table :test 'eq))
  (defun get-texture-backup (name)
    (let ((image-data-func (gethash name *g/texture-backup*)))
      (when (functionp image-data-func)
	(let ((ans (funcall image-data-func name)))
	  (when ans
	    (set-texture name ans)))))))

(progn
  (defparameter *g/shader* (make-hash-table :test 'eq)) ;;opengl shaders
  (hook:add-hook init-hook :shader (lambda () (clrhash *g/shader*)))
  (defun get-shader (name)
    (let ((shader-prog (gethash name *g/shader*)))
      (if shader-prog
	  shader-prog
	  (get-shader-backup name))))
  (defun set-shader (name shader-num)
    (setf (gethash name *g/shader*) shader-num))
  (defun remove-shader (name)
    (remhash name *g/shader*)))
(progn
  (defparameter *g/shader-backup* (make-hash-table :test 'eq))
  (defun get-shader-backup (name)
    (let ((shader-make-func (gethash name *g/shader-backup*)))
      (when (functionp shader-make-func)
	(let ((ans (funcall shader-make-func name)))
	  (when ans
	    (set-shader name ans)))))))

(defun set-framebuffer ()
  (setf (values *framebuffer-texture* *framebuffer*)
	(create-framebuffer e:*width* e:*height*)))

(defun clean-framebuffers ()
  (gl:delete-framebuffers-ext (list *framebuffer*))
  (gl:delete-textures (list *framebuffer-texture*)))

(defun lcalllist-invalidate (name)
  (let ((old (get-display-list name)))
    (remove-display-list name)
    (when old (gl:delete-lists old 1))))

(defun create-call-list-from-func (func)
  (let ((the-list (gl:gen-lists 1)))
    (gl:new-list the-list :compile)
    (funcall func)
    (gl:end-list)
    the-list))

(defun ldrawlist (name)
  (let ((the-list (get-display-list name)))
    (if the-list
	(gl:call-list the-list)
	(print "error"))))

(defun luse-shader (name)
  (use-program (get-shader name)))

(defun bind-shit (name)
  "bind a texture located in the texture library"
  (let ((num (get-texture name)))
    (if num
	(gl:bind-texture :texture-2d num)
	(print "error-tried to use NIL texture"))))

;;;turn a picture which is in the image library into an
;;;opengl texture which is in the texture library
(defun lpic-ltexture (image-name &optional (texture-name image-name))
  (let ((thepic (get-image image-name)))
    (when thepic
      (set-texture texture-name 
		   (pic-texture thepic)))))

(defun pic-texture (thepic)
  (destructuring-bind (h w c) (array-dimensions thepic)
	  (let ((type (case c
			(3 :rgb)
			(4 :rgba))))
	    (let ((new-texture (create-texture (imagewise:array-flatten thepic) w h type)))
	      new-texture))))

(defun glActiveTexture (num)
  "sets the active texture"
  (gl:active-texture (+ num (get-gl-constant :texture0))))

(defun sizeof (type-keyword)
  "gets the size of a foreign c type"
  (cffi:foreign-type-size type-keyword))

(defun get-gl-constant (keyword)
  "gets a gl-constant"
  (cffi:foreign-enum-value '%gl:enum keyword))


(defun create-texture (tex-data width height &optional (type :rgba))
  "creates an opengl texture from data"
  (let ((the-shit (car (gl:gen-textures 1))))
    (gl:bind-texture :texture-2d the-shit)
    (gl:tex-parameter :texture-2d :texture-min-filter  :nearest-mipmap-nearest)
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

;;;opengl can only use one shaderprogram at once,
;;;so there is a global *shader-program* variable
(defparameter *shader-program* nil)

;;;check if the 
(defun use-program (ourprog)
  (unless (eql ourprog *shader-program*)
    (setq *shader-program* ourprog)
    (gl:use-program ourprog)))

;;;attribs is an alist with a string in the car representing an attribute
;;;and a number representing the location in the cdr
(defun make-shader-program-from-strings
    (vertex-shader-string fragment-shader-string attribs)
  "makes a shader program from strings. makes noises if something goes wrong"
  (block nil
    (let ((vertexShader (gl:create-shader :vertex-shader))
	  (fragmentShader (gl:create-shader :fragment-shader))
	  (shaderProgram (gl:create-program)))
      (dolist (val attribs)
	(gl:bind-attrib-location shaderProgram
				 (cdr val)
				 (car val)))
      (gl:shader-source vertexShader vertex-shader-string)
      (gl:compile-shader vertexShader)
      (let ((success (gl:get-shader-info-log vertexShader)))
	(unless (zerop (length success))
	  (return (print success))))
      (gl:shader-source fragmentShader fragment-shader-string)
      (gl:compile-shader fragmentShader)
      (let ((success (gl:get-shader-info-log fragmentShader)))
	(unless (zerop (length success))
	  (return (print success))))
      (gl:attach-shader shaderProgram vertexShader)
      (gl:attach-shader shaderProgram fragmentShader)
      (gl:link-program shaderProgram)
      (let ((success (gl:get-program-info-log shaderProgram)))
	(unless (zerop (length success))
	  (return (print success))))
      (gl:delete-shader vertexShader)
      (gl:delete-shader fragmentShader)
      shaderProgram)))

;;;various functions for setting uniforms 
(defun set-matrix (name matrix)
  (gl:uniform-matrix-4fv
   (gl:get-uniform-location *shader-program* name)
   matrix))

(defun set-int (name thenumber)
  (gl:uniformi
   (gl:get-uniform-location *shader-program* name)
   thenumber))

(defun set-vec4 (name thevec4)
  (gl:uniformfv
   (gl:get-uniform-location *shader-program* name)
   thevec4))

(defun set-vec3 (name thevec3)
  (gl:uniformfv
   (gl:get-uniform-location *shader-program* name)
   thevec3))

(defun set-float (name thefloat)
  (gl:uniformf
   (gl:get-uniform-location *shader-program* name)
   thefloat))
