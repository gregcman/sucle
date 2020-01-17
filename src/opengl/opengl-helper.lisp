(defpackage #:glhelp
  (:use :cl)
  (:import-from
   #:gl
   #:enum=
   #:make-gl-array-from-pointer))
(in-package :glhelp)
;;;;************************************************************************;;;;
;;;;<CONTENTS>
;; contents
;; context
;; version
;; parent object
;; setup
;; utilities
;; vao
;; display-list
;; texture
;; program
;; shader
;; legacy
;; switch between display lists and vaos
;; viewport
;;;;</CONTENTS>
;;;;************************************************************************;;;;
;;;;<CONTEXT>
;;;;holds a token representing a valid gl context. objects which are not current
;;;;have old tokens.
(defparameter *gl-context* nil)
;;;;</CONTEXT>
;;;;************************************************************************;;;;
;;;;<VERSION>
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
(defun get-version-data (&optional (version *gl-version-substring*))
  (assoc
   version
   *version-data*
   :test 'string=))
(defun glsl-gl-version (&optional (version-data (get-version-data *gl-version-substring*)))
  (second version-data))
(defun gl-slow-draw-type (&optional (version-data (get-version-data *gl-version-substring*)))
  (third version-data))

(defparameter *slow-draw-type* :display-list)
;;;;</VERSION>
;;;;************************************************************************;;;;
;;;;<PARENT OBJECT>
(defclass gl-object ()
  ((handle :accessor handle
	   :initarg :handle)
   (context :accessor context
	    :initform *gl-context*)))
(defgeneric gl-delete* (obj))
(defmethod gl-delete* :after ((obj gl-object))
  (slot-makunbound obj 'handle))

(defun alive-p (obj)
  (eq *gl-context*
      (context obj)))
(export '(alive-p))

;;;;</PARENT OBJECT>
;;;;************************************************************************;;;;
;;;;<SETUP>

(deflazy:deflazy gl-context ()
  (unless *gl-context*
    (error "no opengl context you idiot!")))

;;[FIXME]does code involving deflazy belong here at all?
(export '(deflazy-gl))
(defmacro deflazy-gl (name (&rest deps) &rest gen-forms)
  "for objects that should be forgotten because they were
not made in the current OpenGL context, so they are garbage"
  `(deflazy:deflazy ,name (,@deps gl-context)
     (declare (ignorable gl-context))
     ,@gen-forms))

(defmethod deflazy:cleanup-node-value ((object gl-object))
  (when (alive-p object)
    (gl-delete* object)))

(defmacro with-gl-context ((gl-proc-address) &body body)
  `(unwind-protect (progn
		     (setf %gl:*gl-get-proc-address* ,gl-proc-address) ;;[FIXME]is this needed?
		     (setf *gl-context* (cons "gl-context" "token"))
		     (setf *gl-version* (gl:get-string :version))
		     (setf *gl-version-substring*
			   (subseq *gl-version* 0 3))
		     (setf *glsl-version* (glsl-gl-version))
		     (setf *slow-draw-type* (gl-slow-draw-type))
		     (deflazy:refresh 'gl-context t)
		     ,@body)
     (setf *gl-context* nil)))

(export '(with-gl-context))

;;;;</SETUP>
;;;;************************************************************************;;;;
;;;;<UTILITIES>

(defun string-match? (a b)
  (let ((log (remove "" (split-sequence:split-sequence #\Newline a)
		     :test #'string=)))
    (string= (car (last log))
	     b)))
(defun gl-success (success)
  (or (zerop (length success))
      (string-match? success "No errors.")))

(defun sizeof (type-keyword)
  "gets the size of a foreign c type"
  (cffi:foreign-type-size type-keyword))

(deftype +gluint+ () '(unsigned-byte 32))
(defparameter *gl-primitives*
  (list
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

;;;;</UTILITIES>
;;;;************************************************************************;;;;
;;;;<VAO>
;;;;vertex array objects
(defclass vao (gl-object)
  ((vbuff :accessor vertex-buffer)
   (ibuff :accessor index-buffer)

   ;;control whether the vertex buffer or index buffer are cleaned up
   ;;upon removal of the vao.
   (v-delete-p :accessor v-delete-p :initform t)
   (i-delete-p :accessor i-delete-p :initform t)
   
   (va :accessor vertex-array)
   (indices :accessor indices :initform 0)
   (render-type :accessor render-type :initform :triangles)))
(defun delete-vao (vao)
  (gl:delete-vertex-arrays (list (vertex-array vao)))

  ;;share the index buffer because it is repetitive in the case of converting from quads
  ;;to triangles
  (when (v-delete-p vao)
    (gl:delete-buffers (list (vertex-buffer vao))))
  (when (i-delete-p vao)
    (gl:delete-buffers (list (index-buffer vao)))))
(defmethod gl-delete* ((obj vao))
  (delete-vao obj))

(defmacro bind-to-array-buffer ((vertex-buffer) &body body)
  `(progn
     (gl:bind-buffer :array-buffer ,vertex-buffer)
     (multiple-value-prog1
	 (locally ,@body)
       ;; 0 is always reserved as an unbound object.
       (gl:bind-buffer :array-buffer 0))))
(defmacro bind-to-element-array-buffer ((index-buffer) &body body)
  `(progn
     (gl:bind-buffer :element-array-buffer ,index-buffer)
     (multiple-value-prog1
	 (locally ,@body)
       ;; 0 is always reserved as an unbound object.
       (gl:bind-buffer :element-array-buffer 0))))

(defun use-array-buffer (vertex-buffer array &optional (type :static-draw))
  (bind-to-array-buffer (vertex-buffer)
    (gl:buffer-data :array-buffer type array)))
(defun use-element-array-buffer (index-buffer array &optional (type :static-draw))
  (bind-to-element-array-buffer (index-buffer)
    (gl:buffer-data :element-array-buffer type array)))

#+nil
(defun make-vertex-array (verts indices layout type)
  (let* ((value (allocate-vertex-array))
	 (vertex-array (vertex-array value))
	 (vertex-buffer (vertex-buffer value))
	 (index-buffer (index-buffer value)))
    
    (let ((len (length verts)))
      (gl:with-gl-array (arr :float :count len)
	(dotimes (i len)
	  (setf (gl:glaref arr i) (aref verts i)))
	(use-array-buffer vertex-buffer arr)))
    
    ;; An element array buffer stores vertex indices. We fill it in the
    ;; same way as an array buffer.
    (let ((len (length indices)))
      (gl:with-gl-array (arr :unsigned-int :count len)
	(dotimes (i len)
	  (setf (gl:glaref arr i) (aref indices i)))
	(use-array-buffer index-buffer arr)))
    
    (associate-vbos-with-vao vertex-array vertex-buffer index-buffer layout)
    (setf (indices value) (length indices))
    (setf (render-type value) type)
    value))

(defun assemble-vao (gl-vertbuf gl-indexbuf layout length type)
  "assume that we have a gl-buffer for the vertices and indices, 
we know the layout, the length, and the type (points, triangles, etc...)
just put together a new vao"
  (let ((vao (make-instance 'vao))
	(vertex-array (gl:gen-vertex-array)))

    (setf (index-buffer vao) gl-indexbuf)
    (setf (vertex-buffer vao) gl-vertbuf)
    (setf (indices vao) length)
    (setf (render-type vao) type)
    (setf (vertex-array vao) vertex-array)
    (associate-vbos-with-vao vertex-array gl-vertbuf gl-indexbuf layout)
    
    vao))
#+nil
(defun allocate-vertex-array ()
  (let ((vao (make-instance 'vao)))
    (setf (vertex-buffer vao)
	  (gl:gen-buffer))
    (setf (index-buffer vao)
	  (gl:gen-buffer))
    (setf (vertex-array vao)
	  (gl:gen-vertex-array))
    vao))

(defun draw-vertex-array (vao)
  (%draw-vertex-array
   (vertex-array vao)
   (indices vao)
   (render-type vao)))

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
  (let ((float-size (sizeof :float))
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
;;;;</VAO>
;;;;************************************************************************;;;;
;;;;<DISPLAY-LIST>
;;;;display lists
(defclass gl-list (gl-object)
  ())
(defmethod gl-delete* ((obj gl-list))
  (gl:delete-lists (handle obj) 1))
;;;
(defmacro with-gl-list (&body body)
  (let ((list-sym (gensym)))
    `(let ((,list-sym (gl:gen-lists 1)))
       (unwind-protect
	    (progn (gl:new-list ,list-sym :compile)
		   ,@body)
	 (gl:end-list))
       ,list-sym)))
(export '(with-gl-list))
;;;

(defun draw-display-list (display-list)
  (gl:call-list display-list))

;;;;</DISPLAY-LIST>
;;;;************************************************************************;;;;
;;;;<TEXTURE>
;;;;textures
(defclass gl-texture (gl-object)
  ())
(defmethod gl-delete* ((obj gl-texture))
  (gl:delete-texture (handle obj)))
;;;
(defun wrap-opengl-texture (texture)
  (make-instance
   'gl-texture
   :handle
   texture))
;;;
(defmethod create-opengl-texture-from-data ((data array))
  (pic-texture data))

(defun create-texture (tex-data width height &key (format :rgba) (type :unsigned-byte))
  (let ((tex (gl:gen-texture)))
    (gl:bind-texture :texture-2d tex)
    (gl:tex-image-2d :texture-2d 0 :rgba width height 0 format type tex-data)
    (apply-tex-params *default-tex-params*)
    (gl:bind-texture :texture-2d 0)
    tex))
;;;
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
;;;
(progn
  (defconstant +gltexture0+ (cffi:foreign-enum-value (quote %gl:enum) :texture0))
  (defun set-active-texture (num)
    (gl:active-texture (+ num +gltexture0+))))
;;;;</TEXTURE>
;;;;************************************************************************;;;;
;;;;<FRAMEBUFFER>
;;;;framebuffers
(defclass gl-framebuffer (gl-object)
  ((texture :accessor texture)
   (depth :accessor depth)
   (x :accessor x)
   (y :accessor y)))

(defun make-gl-framebuffer (width height)
  (let ((inst (make-instance 'gl-framebuffer)))
    (with-slots (x y handle texture depth) inst
      (setf x width
	    y height)
      (setf (values texture handle depth)
	    (create-framebuffer width height)))
    inst))

(defmethod gl-delete* ((obj gl-framebuffer))
  (destroy-gl-framebuffer obj))
(defun destroy-gl-framebuffer (gl-framebuffer)
  (gl:delete-renderbuffers (list (depth gl-framebuffer)))
  (gl:delete-framebuffers (list (handle gl-framebuffer)))
  (gl:delete-textures (list (texture gl-framebuffer))))
;;;
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
      (unless (enum= framebuffer-status :framebuffer-complete)
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

;;;;</FRAMEBUFFER>
;;;;************************************************************************;;;;
;;;;<PROGRAM>
;;;;program objects
(defun cache-program-uniforms (program uniforms)
  (mapcar
   (lambda (args)
     (destructuring-bind (id . string) args
       (cons
	id
	(gl:get-uniform-location program string))))
   uniforms))
(defun getuniform (shader-info name)
  (cdr (assoc name shader-info :test 'eq)))
(defmacro with-uniforms (name program-object &body body)
  (let ((uniforms-var (gensym)))
    `(let ((,uniforms-var (gl-program-object-uniforms ,program-object)))
       (macrolet ((,name (id)
		    (list 'getuniform ',uniforms-var id)))
	 ,@body))))

(export '(getuniform cache-program-uniforms make-uniform-cache with-uniforms))
(defclass gl-program (gl-object)
  ((src :accessor gl-program-object-src
	:initarg :src)
   (uniforms :accessor gl-program-object-uniforms)))
;;[FIXME]fix exports
(export 'use-gl-program)
(defun use-gl-program (src)
  (gl:use-program (handle src)))

(defmethod gl-delete* ((obj gl-program))
  (gl:delete-program (handle obj)))
;;;;
(defun set-uniform-to-texture (uniform-location texture num)
  (gl:uniformi uniform-location num)
  (set-active-texture num)
  (gl:bind-texture :texture-2d texture))

;;[FIXME]is a macro really necessary here? To prevent consing?
(defmacro set-uniforms-to-textures (&rest specs)
  (cons 'progn
	(mapcar (lambda (spec number)
		  (destructuring-bind (location texture) spec
		    `(set-uniform-to-texture ,location ,texture ,number)))
		specs
		(alexandria:iota (length specs)))))
;;;;</PROGRAM>
;;;;************************************************************************;;;;
;;;;<SHADER>
;;;;Shader programs
(defun compile-string-into-shader (shader string)
  (gl:shader-source shader string)
  (gl:compile-shader shader)
  (let ((success (gl:get-shader-info-log shader)))
    (unless (gl-success success)
      (print success)
      (error "~S" success))))

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
;;;;
(defun create-gl-program2 (src)
  ;;[FIXME]add ability to rename varyings so
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
(export 'create-opengl-shader)
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
  ;;[FIXME]procedurally generate a name that definitely does not clash with
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
		       (;;[FIXME]This assumes the type-qualifiers are in the second position
			(type-qualifier-data (second ast)))
		     (when (consp type-qualifier-data)
		       (symbol-macrolet ((type-qualifiers (cdr type-qualifier-data)))
			 (flet ((replace-qualifer (new old)
				  (setf type-qualifiers
					(nsubst new old type-qualifiers))))
			   (when (member :uniform type-qualifiers)
			     (unless (>= version 120)
			       ;;[FIXME]this represents the variable declaration.
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
				 ;;[FIXME] out in the fragment shader?
				 #+nil
				 (:frag (add-qualifier "varying"))
				 (:vs (replace-qualifer :out
							"varying"))))
			     #+nil ;;[FIXME] varying does not occur
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

;;;;</SHADER>
;;;;************************************************************************;;;;
;;;;<LEGACY>

(defmacro vertex-attrib-f (index &rest forms)
  (ecase (length forms)
    (1 `(%gl:vertex-attrib-1f ,index ,@forms))
    (2 `(%gl:vertex-attrib-2f ,index ,@forms))
    (3 `(%gl:vertex-attrib-3f ,index ,@forms))
    (4 `(%gl:vertex-attrib-4f ,index ,@forms))))

;;[FIXME]make sure 0 comes last, because that completes each vertex?
(defmacro vertex-attrib-f* ((&rest forms))
  `(progn ,@(mapcar (lambda (x) `(vertex-attrib-f ,@x)) forms)))

(export '(vertex-attrib-f vertex-attrib-f*))
;;;;</LEGACY>
;;;;************************************************************************;;;;
;;;;<SWITCH BETWEEN DISPLAY LISTS AND VAOS>
(defun quads-triangles-index-buffer (n)
  "Convert N quads into an element buffer of triangles"
  ;;0->3 quad
  ;;0 1 2 triangle
  ;;0 2 3 triangle
  (let ((array (make-array (* 6 n) :element-type '(unsigned-byte 32))))
    (dotimes (i n)
      (let ((base (* i 6))
	    (quad-base (* i 4)))
	(flet ((foo (a b)
		 (setf (aref array (+ base a))
		       (+ quad-base b))))
	  (foo 0 0)
	  (foo 1 1)
	  (foo 2 2)
	  (foo 3 0)
	  (foo 4 2)
	  (foo 5 3))))
    array))

(export 'quads-triangles-index-buffer)
;;[FIXME]rename from gl-list to display-list?
(defmacro create-gl-list-from-specs ((type times) form)
  (utility:with-gensyms (fixed-times fixed-type)
    ;;[FIXME] the prefix "fix" is unrelated for fixed-type,fixed-times vs fixnum
    `(let ((,fixed-type ,type)
	   (,fixed-times ,times))
       (declare (type fixnum ,fixed-times))
       (with-gl-list
	 (gl:with-primitives ,fixed-type
	   (loop :repeat ,fixed-times :do 
	      (vertex-attrib-f* ,form)))))))

(export 'create-gl-list-from-specs)


(defparameter *quad-to-triangle-index-buffer-quad-count*
  ;;the number of quads in a 16x16x16 chunk if each block has 6 faces showing.
  ;;around 300k of memory, with 32 bit unsigned ints.
  (* 16 16 16 6 3))
(deflazy-gl
 shared-quad-to-triangle-index-buffer ()
 (let ((index-buffer (gl:gen-buffer))
       (indices (quads-triangles-index-buffer *quad-to-triangle-index-buffer-quad-count*)))
   (let ((len (length indices)))
     (gl:with-gl-array (arr :unsigned-int :count len)
       (dotimes (i len)
	 (setf (gl:glaref arr i) (aref indices i)))
       (use-element-array-buffer index-buffer arr)))
   index-buffer))
(defparameter *plain-index-buffer-count*
	      (* 16 16 16 6 3))
(deflazy-gl
 shared-plain-index-buffer ()
 (let ((index-buffer (gl:gen-buffer))
       (indices (quads-triangles-index-buffer *plain-index-buffer-count*)))
   (let ((len (length indices)))
     (gl:with-gl-array (arr :unsigned-int :count len)
       (dotimes (i len)
	 (setf (gl:glaref arr i) i))
       (use-element-array-buffer index-buffer arr)))
   index-buffer))

(defun get-fixed-type-and-index-buffer-for-type (type times)
  ;;convert quads to tris for new opengl.
  (case type
    (:quads
     (values :triangles (deflazy:getfnc 'shared-quad-to-triangle-index-buffer) (* 6/4 times)))
    (otherwise
     (values type (deflazy:getfnc 'shared-plain-index-buffer) times))))

(defmacro create-vao-from-specs ((type-form times-form) form)
  (let* ((data 
	  (mapcar (lambda (n)
		    (destructuring-bind (index &rest forms) n
		      (list index (length forms))))
		  form))
	 (layout
	  (simple-vertex-array-layout data))
	 (forms (apply 'concatenate 'list (mapcar 'rest form))))

    (let ((len (vertex-array-layout-total-size layout)))
      (assert (= len (length forms)))
      (values
       ;;data
       ;;forms
       ;;layout
       (utility:with-gensyms (times type vertex-buffer array-count arr index add)
	 `(let* ((,type ,type-form)
		 (,times ,times-form)
		 (,vertex-buffer (gl:gen-buffer))
		 (,array-count (* ,len ,times)))
	    (declare (type fixnum ,times ,array-count))
	    (cffi:with-foreign-object (,arr :float ,array-count)
	      (let ((,index 0))
		(declare (type fixnum ,index))
		(flet ((,add (n)
			 (setf (cffi:mem-aref ,arr :float ,index) n)
			 (incf ,index)))
		  (loop :repeat ,times :do
		     ,@(mapcar (lambda (form) `(,add ,form)) forms))))
	      (let ((glarray (make-gl-array-from-pointer ,arr :float ,array-count)))
		(use-array-buffer ,vertex-buffer glarray)))
	
	    (let
		((vao
		  (multiple-value-bind (fixed-type index-buffer fixed-times)
		      (get-fixed-type-and-index-buffer-for-type ,type ,times)		  
		    (assemble-vao
		     ,vertex-buffer
		     index-buffer
		     ',layout
		     ;;[FIXME] is it the total count of primitives, or points?
		     fixed-times
		     fixed-type))))
	      (setf (i-delete-p vao) nil)
	      (values vao))))))))

(defmacro create-vao-or-display-list-from-specs ((type times) form)
  `(ecase *slow-draw-type*
     (:display-list
      (create-gl-list-from-specs (,type ,times) ,form))
     (:vertex-array-object
      (create-vao-from-specs (,type ,times) ,form))))
(export 'create-vao-or-display-list-from-specs)

#+nil
(create-vao-or-display-list-from-specs
 (:quads 10)
 ((2 (xyz) (xyz) (xyz))
    ;;why???
    (8 0.06 0.06)
    (1 0.0 0.0 0.0 0.0)
    ;;zero always comes last?
  (0 0.0 0.0 0.0 0.0)))


(declaim (inline slow-draw))
(defun slow-draw (gl-thing)
  ;;;dispatch on either display-list or vao
  (declare (optimize (speed 3) (safety 0)))
  (typecase gl-thing
    (+gluint+ (draw-display-list gl-thing))
    (vao (draw-vertex-array gl-thing))
    (gl-list (draw-display-list (handle gl-thing))))
  )

(defun slow-delete (gl-thing)
  ;;;dispatch on either display-list or vao
  (declare (optimize (speed 3) (safety 0)))
  (typecase gl-thing
    (+gluint+ (gl:delete-lists gl-thing 1))
    (vao (delete-vao gl-thing))
    (gl-list (gl:delete-lists (handle gl-thing) 1)))
  )
(export '(slow-draw slow-delete))

;;;;</SWITCH BETWEEN DISPLAY LISTS AND VAOS>
;;;;************************************************************************;;;;
;;;;<VIEWPORT>
(export '(set-render-area))
(defun set-render-area (x y width height)
  (gl:viewport x y width height)
  (gl:scissor x y width height))

;;;;</VIEWPORT>
;;;;************************************************************************;;;;

