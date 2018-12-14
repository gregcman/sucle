(defpackage #:glhelp
  (:use #:cl))

(in-package :glhelp)

(defparameter *gl-version* "2.0") ;;abitrary, gets overwritten
(defparameter *gl-version-substring* "2.0") ;;also arbitrary
(defparameter *version-data*
  (quote (("2.0" 110 :display-list)
	  ("2.1" 120 :display-list)
	  ("3.0" 130 :display-list)
	  ("3.1" 140 :vertex-array-object)
	  ("3.2" 150 :vertex-array-object)
	  ("3.3" 330 :vertex-array-object)
	  ("4.0" 400 :vertex-array-object)
	  ("4.1" 410 :vertex-array-object)
	  ("4.2" 420 :vertex-array-object)
	  ("4.3" 430 :vertex-array-object)
	  ("4.4" 440 :vertex-array-object)
	  ("4.5" 450 :vertex-array-object)
	  ("4.6" 460 :vertex-array-object))))
(defparameter *slow-draw-type* :display-list)

;;;;in opengl horizontal lines must be multiples of 4 bytes
(defun array-flatten (array)
  (make-array (array-total-size array)
	      :displaced-to array
	      :element-type (array-element-type array)))

(defun create-texture (tex-data width height format &optional (type :unsigned-byte))
  (let ((tex (car (gl:gen-textures 1))))
    (gl:bind-texture :texture-2d tex)
    (gl:tex-image-2d :texture-2d 0 :rgba width height 0 format type tex-data)
    tex))


(defparameter *default-tex-params* (quote ((:texture-min-filter . :nearest)
					   (:texture-mag-filter . :nearest)
;					   (:texture-wrap-s . :repeat)
;					   (:texture-wrap-t . :repeat)
					   )))

;;;;tex-parameters is an alist of pairs (a . b) with
;;;;(lambda (a b) (gl:tex-parameter :texture-2d a b))
(defun pic-texture (thepic &optional type)
  (let ((dims (array-dimensions thepic)))
    (let ((h (pop dims))
	  (w (pop dims))
	  (channels (pop dims)))
      (unless type
	(setf
	 type
	 (case channels
	   ((nil) :red)
	   (2 (error "2 components?"))
	   (3 :rgb)
	   (4 :rgba))))
      (when (eq nil channels)
	(setf channels 1))
      (let* ((byte-width (* channels w))
	     (foured (ash (ash byte-width -2) 2)))
	(if (= byte-width foured)
	    (create-texture (array-flatten thepic) w h type)
	    (progn
	      (incf foured 4)
	      (let ((array (make-array (* foured h)
				       :element-type (array-element-type thepic))))
;		(declare (dynamic-extent array))
		(dotimes (hi h)
		  (let ((base1 (* hi byte-width))
			(base2 (* hi foured)))
		    (dotimes (wi byte-width)
		      (setf (aref array (+ wi base2))
			    (row-major-aref thepic (+ wi base1))))))
		(create-texture array w h type))))))))

(export '(apply-tex-params))
(defun apply-tex-params (tex-parameters)
  (dolist (param tex-parameters)
    (gl:tex-parameter :texture-2d (car param) (cdr param))))

(defun compile-string-into-shader (shader string)
  (gl:shader-source shader string)
  (gl:compile-shader shader)
  (let ((success (gl:get-shader-info-log shader)))
    (unless (gl-success success)
      (print success)
      (error "~S" success))))

(defun string-match? (a b)
  (let ((log (remove "" (split-sequence:split-sequence #\Newline a)
		     :test #'string=)))
    (string= (car (last log))
	     b)))

(defun gl-success (success)
  (or (zerop (length success))
      (string-match? success "No errors.")))

;;;;attribs is an alist of pairs (a . b) with
;;;;(lambda (a b) (gl:bind-attrib-location ...[the program].. a b)
(defun make-shader-program-from-strings (vs-string fs-string attribs)
  (let ((vert (gl:create-shader :vertex-shader))
	(frag (gl:create-shader :fragment-shader))
	(program (gl:create-program)))

    (compile-string-into-shader frag fs-string)
    (compile-string-into-shader vert vs-string)
    
    (gl:attach-shader program vert)
    (gl:attach-shader program frag)

    (dolist (val attribs)
      (gl:bind-attrib-location program
			       (cdr val)
			       (car val)))
    
    (gl:link-program program)
    (let ((success (gl:get-program-info-log program)))
      (unless (gl-success success)
	(print success)
	(error "~S" success)))
    (gl:detach-shader program vert)
    (gl:detach-shader program frag)
    
    (gl:delete-shader vert)
    (gl:delete-shader frag)
    program))

(export (quote (pic-texture make-shader-program-from-strings)))


(defun sizeof (type-keyword)
  "gets the size of a foreign c type"
  (cffi:foreign-type-size type-keyword))

(defun get-gl-constant (keyword)
  "gets a gl-constant"
  (cffi:foreign-enum-value '%gl:enum keyword))

(progn
  (defconstant +gltexture0+ (cffi:foreign-enum-value (quote %gl:enum) :texture0))
  (defun set-active-texture (num)
    (gl:active-texture (+ num +gltexture0+))))

(defun bind-default-framebuffer ()
  (gl:bind-framebuffer :framebuffer 0))


(defun create-framebuffer (w h)
  (let ((framebuffer (first (gl:gen-framebuffers 1)))
        (depthbuffer (first (gl:gen-renderbuffers 1)))
        (texture (first (gl:gen-textures 1))))
    ;; setup framebuffer
    (gl:bind-framebuffer :framebuffer framebuffer)

    (progn
      ;; setup texture and attach it to the framebuffer
      (gl:bind-texture :texture-2d texture)
      (gl:tex-parameter :texture-2d :texture-min-filter :nearest)
      (gl:tex-parameter :texture-2d :texture-mag-filter :nearest)
      (gl:tex-image-2d :texture-2d 0 :rgba w h 0 :rgba :unsigned-byte (cffi:null-pointer))
      (gl:bind-texture :texture-2d 0)
      (gl:framebuffer-texture-2d :framebuffer
				 :color-attachment0
				 :texture-2d
				 texture
				 0))
    (progn
      ;; setup depth-buffer and attach it to the framebuffer
      (gl:bind-renderbuffer :renderbuffer depthbuffer)
      (gl:renderbuffer-storage :renderbuffer :depth-component24 w h)
      (gl:framebuffer-renderbuffer
       :framebuffer
       :depth-attachment
       :renderbuffer
       depthbuffer))

    ;; validate framebuffer
    (let ((framebuffer-status (gl:check-framebuffer-status :framebuffer)))
      (unless (gl::enum= framebuffer-status :framebuffer-complete)
        (error "Framebuffer not complete: ~A." framebuffer-status)))

    #+nil
    (gl:clear-color 0.0 0.0 0.0 0.0)
    #+nil
    (gl:clear :color-buffer-bit
	      :depth-buffer-bit)
    #+nil
    (gl:enable :depth-test ;:multisample
	       )
    (values texture framebuffer depthbuffer)))

(defun glsl-gl-version (&optional (version *gl-version-substring*))
  (second
   (assoc
    version
    *version-data*
    :test 'string=)))

(defun gl-slow-draw-type (&optional (version *gl-version-substring*))
  (third
   (assoc
    version
    *version-data*
    :test 'string=)))

;;;;vertex array objects and layout

(defun simple-vertex-array-layout (spec)
  "spec is a list of (attribute-index size) starting from zero"
  (let ((total-size (reduce '+ spec :key 'second))
	(acc)
	(index 0))
    (dolist (item spec)
      (destructuring-bind (attribute-index size) item
	(push (list attribute-index size index) acc)
	(incf index size)))
    (make-vertex-array-layout
     :total-size total-size
     :attributes (nreverse acc))))

(struct-to-clos:struct->class
 (defstruct vertex-array-layout
   total-size
   attributes ;;(attr size start)
   ))

(defun gl-vertex-attributes (vertex-array-layout)
  (let ((float-size (glhelp::sizeof :float))
	(total-size (vertex-array-layout-total-size vertex-array-layout))
	(attributes (vertex-array-layout-attributes vertex-array-layout)))
    (dolist (spec attributes)
      (destructuring-bind (attr size start) spec
	(gl:enable-vertex-attrib-array attr)
	(gl:vertex-attrib-pointer
	 attr size
	 :float
	 ;; Using a null pointer as the data source indicates that we want
	 ;; the vertex data to come from the currently bound array-buffer.
	 nil
	 (* float-size total-size)
	 (* float-size start))))))

(defun fill-vertex-array-object (vertex-array vertex-buffer index-buffer verts indices layout)  
  (gl:bind-buffer :array-buffer vertex-buffer)
  (let ((len (length verts)))
    (let ((arr (gl:alloc-gl-array :float len)))
      (dotimes (i len)
	(setf (gl:glaref arr i) (aref verts i)))
      (gl:buffer-data :array-buffer :static-draw arr)
      (gl:free-gl-array arr)))
  ;; 0 is always reserved as an unbound object.
  (gl:bind-buffer :array-buffer 0)

  ;; An element array buffer stores vertex indices. We fill it in the
  ;; same way as an array buffer.
  (gl:bind-buffer :element-array-buffer index-buffer)
  (let ((len (length indices)))
    (let ((arr (gl:alloc-gl-array :unsigned-int len)))
      (dotimes (i len)
	(setf (gl:glaref arr i) (aref indices i)))
      (gl:buffer-data :element-array-buffer :static-draw arr)
      (gl:free-gl-array arr)))
  (gl:bind-buffer :element-array-buffer 0)

  ;; Vertex array objects manage which vertex attributes are
  ;; associated with which data buffers. 
  (gl:bind-vertex-array vertex-array)

  ;; To associate our VBO data with this VAO, we bind it, specify
  ;; which vertex attribute we want to associate it with, and specify
  ;; where the data comes from.
  (gl:bind-buffer :array-buffer vertex-buffer)
  
  (gl-vertex-attributes layout)

  ;; To associate an element array with this VAO, all we need to do is
  ;; bind the element array buffer we want to use.
  (gl:bind-buffer :element-array-buffer index-buffer)

  ;; Once we're done, we can unbind the VAO, and rebind it when we want to render it.
  (gl:bind-vertex-array 0))

(defun %draw-vertex-array (vao length &optional (type :triangles))
  (gl:bind-vertex-array vao)   
  ;; This call actually does the rendering. The vertex data comes from
  ;; the currently-bound VAO. If the input array is null, the indices
  ;; will be taken from the element array buffer bound in the current
  ;; VAO.
  (gl:draw-elements
   type
   (gl:make-null-gl-array :unsigned-int)
   :count
   length
   :offset 0))

;;;

(defun draw-display-list (display-list)
  (gl:call-list display-list))
