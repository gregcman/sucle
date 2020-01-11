(defpackage #:glhelp
  (:use #:cl))

(in-package :glhelp)

(defparameter *gl-version* "2.0") ;;abitrary, gets overwritten
(defparameter *gl-version-substring* "2.0") ;;also arbitrary
(defparameter *glsl-version* 110) ;;arbitrary, overwritten
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

(defun wrap-opengl-texture (texture)
  (make-instance
   'glhelp::gl-texture
   :handle
   texture))

(defmethod create-opengl-texture-from-data ((data array))
  (pic-texture data))

(defun create-texture (tex-data width height &key (format :rgba) (type :unsigned-byte))
  (let ((tex (gl:gen-texture)))
    (gl:bind-texture :texture-2d tex)
    (gl:tex-image-2d :texture-2d 0 :rgba width height 0 format type tex-data)
    (apply-tex-params *default-tex-params*)
    (gl:bind-texture :texture-2d 0)
    tex))

(defparameter *default-tex-params* (quote ((:texture-min-filter . :nearest)
					   (:texture-mag-filter . :nearest)
					   (:texture-wrap-s . :repeat)
					   (:texture-wrap-t . :repeat)
					   )))

;;;;in opengl horizontal lines must be multiples of 4 bytes

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
	    (flet ((array-flatten (array)
		     (make-array
		      (array-total-size array)
		      :displaced-to array
		      :element-type (array-element-type array))))
	      (create-texture (array-flatten thepic) w h :format type))
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
		(create-texture array w h :format type))))))))

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
	(push (make-instance
	       'va-section
	       :attr attribute-index
	       :size size
	       :start index)
	      acc)
	(incf index size)))
    (make-vertex-array-layout
     :total-size total-size
     :attributes (nreverse acc))))

(struct-to-clos:struct->class
 (defstruct vertex-array-layout
   total-size
   attributes ;;(attr size start)
   ))

(struct-to-clos:struct->class
 (defstruct va-section
   attr
   size
   start))

(set-pprint-dispatch
 'vertex-array-layout
 (lambda (stream object)
   (format stream "[~%size: ~a ~%attr: ~a~%]"
	   (vertex-array-layout-total-size object)
	   (vertex-array-layout-attributes object))))

(set-pprint-dispatch
 'va-section
 (lambda (stream object)
   (format stream "[attr: ~a size: ~a start: ~a]"
	   (va-section-attr object)
	   (va-section-size object)
	   (va-section-start object))))

(defmethod make-load-form ((obj va-section) &optional env)
  `(make-instance
    'va-section
    :start ',(va-section-start obj)
    :size ',(va-section-size obj)
    :attr ',(va-section-attr obj)))
(defmethod make-load-form ((obj vertex-array-layout) &optional env)
  `(make-instance
    'vertex-array-layout
    :attributes ',(vertex-array-layout-attributes obj)
    :total-size ',(vertex-array-layout-total-size obj))
  )

(defun gl-vertex-attributes (vertex-array-layout)
  (let ((float-size (glhelp::sizeof :float))
	(total-size (vertex-array-layout-total-size vertex-array-layout))
	(attributes (vertex-array-layout-attributes vertex-array-layout)))
    (dolist (spec attributes)
      (with-slots (attr size start) spec
	(gl:enable-vertex-attrib-array attr)
	(gl:vertex-attrib-pointer
	 attr size
	 :float
	 ;; Using a null pointer as the data source indicates that we want
	 ;; the vertex data to come from the currently bound array-buffer.
	 nil
	 (* float-size total-size)
	 (* float-size start))))))

(defun associate-vbos-with-vao (vertex-array vertex-buffer index-buffer layout)
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
   (load-time-value (gl:make-null-gl-array :unsigned-int))
   :count
   length
   :offset 0))

;;;

(defun draw-display-list (display-list)
  (gl:call-list display-list))

;;;;Shader programs

(defun create-gl-program2 (src)
  ;;FIXME::add ability to rename varyings so
  ;;vertex shader and fragment shader can have different variable names
  (let ((raw-attributes (getf src :attributes))
	(uniform-data (getf src :uniforms))
	(frag (getf src :frag))
	(vs (getf src :vs)))
    (let ((inst
	   (make-instance 'gl-program :src src))
	  (obj (make-shader-program-from-strings
		(fixup-shader-for-version :vs vs)
		(fixup-shader-for-version :frag frag)
		raw-attributes)))
      (setf (handle inst) obj)
      (setf (gl-program-object-uniforms inst)
	    (cache-program-uniforms
	     obj
	     uniform-data))
      inst)))

(defun create-opengl-shader (vert-text frag-text attributes uniforms)
  (create-gl-program2
   (list :vs vert-text
	 :frag frag-text
	 :attributes (fixup-list-to-alist attributes)
	 :uniforms (fixup-list-to-alist uniforms))))

(defun fixup-list-to-alist (list)
  (mapcar (lambda (x)
	    (cons (first x)
		  (second x)))
	  list))

(defun concatenate-strings (&rest strings)
  (with-output-to-string (str)
    (labels ((rec (node)
	       (if (listp node)
		   (dolist (string node)
		     (rec string))
		   (write-string (string node) str))))
      (rec strings))))

(defparameter *test*
  "
in vec2 texcoord_out;
uniform vec2 size = 10;

void main () {
//rg = fraction
//ba = text lookup

vec2 foo = floor(texcoord_out * size) / vec2(255.0);
vec2 bar = fract(texcoord_out * size);
vec4 pixcolor; //font lookup
pixcolor.rg = bar; //fraction
pixcolor.ba = foo; // text lookup

gl_FragColor = pixcolor; 
}")
(defparameter *newline* (format nil "~%"))
(defparameter *gl-fragcolor-replacement*
  ;;FIXME::procedurally generate a name that definitely does not clash with
  ;;any glsl names or other names
  "roloCgarF_lg")
(defun fixup-shader-for-version (&optional (shader-type (or :frag :vs))
				   (fragment-shader *test*)
				   (version *glsl-version*))
  (assert (or (eq shader-type :frag)
	      (eq shader-type :vs)))
  (let* ((ast
	  (glsl-toolkit:parse fragment-shader))
	 (new-ast
	  (glsl-toolkit:walk
	   ast
	   (lambda (ast context environment)
	     (declare (ignorable context))
	     (block out
	       (flet ((walk-next (foo)
			(return-from out foo)))
		 (when (and (glsl-toolkit:function-identifier-p ast environment)
			    (string= ast "texture2D")
			    (>= version 150))
		   (walk-next "texture"))
		 (when (eq shader-type :frag)
		   (when (and (glsl-toolkit:identifier-p ast environment)
			      (> version 120)
			      (string= ast "gl_FragColor"))
		     (walk-next *gl-fragcolor-replacement*)))
		 (when (and (consp ast)
			    (eq (first ast)
				'glsl-toolkit:variable-declaration))
		   (let
		       (;;FIXME::This assumes the type-qualifiers are in the second position
			(type-qualifier-data (second ast)))
		     (when (consp type-qualifier-data)
		       (symbol-macrolet ((type-qualifiers (cdr type-qualifier-data)))
			 (flet ((replace-qualifer (new old)
				  (setf type-qualifiers
					(nsubst new old type-qualifiers))))
			   (when (member :uniform type-qualifiers)
			     (unless (>= version 120)
			       ;;FIXME::this represents the variable declaration.
			       ;;How to actually refer? ask shinmera?
			       ;;This hack code removes the optional init form.
			       ;;glsl version 120 and greater allow initialization
			       (setf (cdr (cdr (cdr (cdr (cdr ast)))))
				     nil)))
			   (unless (> version 120)
			     (when (member :in type-qualifiers)
			       (replace-qualifer
				(ecase shader-type
				  (:frag "varying")
				  (:vs "attribute"))
				:in))
			     (when (member :out type-qualifiers)
			       (ecase shader-type
				 ;;FIXME out in the fragment shader?
				 #+nil
				 (:frag (add-qualifier "varying"))
				 (:vs (replace-qualifer :out
							"varying"))))
			     #+nil ;;FIXME:: varying does not occur
			     (when (member :varying type-qualifiers))))))))
		 (walk-next ast)))))))
    (concatenate-strings
     (list
      "#version "
      (with-standard-io-syntax
	(write-to-string version))
      *newline*)

     (when (= version 100)
       (list
	"precision mediump float;"
	*newline*))
     (when (eq shader-type :frag)
       (when (> version 120)
	 (list "out vec4 " *gl-fragcolor-replacement* ";" *newline*)))
     
     (glsl-toolkit:serialize new-ast))))

;;;;
(defun set-uniform-to-texture (uniform-location texture num)
  (gl:uniformi uniform-location num)
  (glhelp::set-active-texture num)
  (gl:bind-texture :texture-2d texture))

;;FIXME::is a macro really necessary here? To prevent consing?
(defmacro set-uniforms-to-textures (&rest specs)
  (cons 'progn
	(mapcar (lambda (spec number)
		  (destructuring-bind (location texture) spec
		    `(set-uniform-to-texture ,location ,texture ,number)))
		specs
		(alexandria:iota (length specs)))))

(defmacro vertex-attrib-f (index &rest forms)
  (ecase (length forms)
    (1 `(%gl:vertex-attrib-1f ,index ,@forms))
    (2 `(%gl:vertex-attrib-2f ,index ,@forms))
    (3 `(%gl:vertex-attrib-3f ,index ,@forms))
    (4 `(%gl:vertex-attrib-4f ,index ,@forms))))

;;FIXME::make sure 0 comes last, because that completes each vertex?
(defmacro vertex-attrib-f* ((&rest forms))
  `(progn ,@(mapcar (lambda (x) `(vertex-attrib-f ,@x)) forms)))

(export '(vertex-attrib-f vertex-attrib-f*))
