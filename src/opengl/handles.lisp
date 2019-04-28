(in-package :glhelp)

;;;;holds a token representing a valid gl context. objects which are not current
;;;;have old tokens.
(defparameter *gl-context* nil)

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
	    (glhelp::create-framebuffer width height)))
    inst))

(defmethod gl-delete* ((obj gl-framebuffer))
  (destroy-gl-framebuffer obj))
(defun destroy-gl-framebuffer (gl-framebuffer)
  (gl:delete-renderbuffers (list (depth gl-framebuffer)))
  (gl:delete-framebuffers (list (handle gl-framebuffer)))
  (gl:delete-textures (list (texture gl-framebuffer))))


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

(defun use-gl-program (src)
  (gl:use-program (handle src)))

(defun create-gl-program (src)
  (let ((inst
	 (make-instance 'gl-program :src src))
	(obj (glslgen::gl-dump-shader src)))
    (setf (handle inst) obj)
    (setf (gl-program-object-uniforms inst)
	  (cache-program-uniforms
	   obj
	   (glslgen::shader-program-data-raw-uniform src)))
    inst))

(defun create-gl-program2 (src)
  (let ((raw-attributes (getf src :attributes))
	(uniform-data (getf src :uniforms))
	(frag (getf src :frag))
	(vs (getf src :vs)))
    (let ((inst
	   (make-instance 'gl-program :src src))
	  (obj (glhelp:make-shader-program-from-strings
		vs
		frag
		raw-attributes)))
      (setf (handle inst) obj)
      (setf (gl-program-object-uniforms inst)
	    (cache-program-uniforms
	     obj
	     uniform-data))
      inst)))

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
uniform vec2 size;

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
				   (version glslgen::*glsl-version*))
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

(defmethod gl-delete* ((obj gl-program))
  (gl:delete-program (handle obj)))

;;;;textures

(defclass gl-texture (gl-object)
  ())
(defmethod gl-delete* ((obj gl-texture))
  (gl:delete-textures (list (handle obj))))

;;;;call lists

(defclass gl-list (gl-object)
  ())
(defmethod gl-delete* ((obj gl-list))
  (gl:delete-lists (handle obj) 1))

;;;;vertex array objects

(defclass vao (gl-object)
  ((vbuff :accessor vertex-buffer)
   (ibuff :accessor index-buffer)
   (va :accessor vertex-array)
   (indices :accessor indices :initform 0)
   (render-type :accessor render-type :initform :triangles)))
(defmethod gl-delete* ((obj vao))
  (gl:delete-vertex-arrays (list (vertex-array obj)))
  (gl:delete-buffers (list (vertex-buffer obj) (index-buffer obj))))

(defun make-vertex-array (vertbuf indexbuf layout type)
  (let ((value (glhelp::allocate-vertex-array)))
    (glhelp::fill-vertex-array-object
     (glhelp::vertex-array value)
     (glhelp::vertex-buffer value)
     (glhelp::index-buffer value)
     vertbuf
     indexbuf
     layout)
    (setf (glhelp::indices value)
	  (length indexbuf)
	  (glhelp::render-type value)
	  type)
    value))

(defun allocate-vertex-array ()
  (let ((w (make-instance 'vao)))
    (let ((buffers (gl:gen-buffers 2)))
      (setf (vertex-buffer w) (elt buffers 0)
	    (index-buffer w) (elt buffers 1)))
    (setf (vertex-array w) (gl:gen-vertex-array))
    w))

(defun draw-vertex-array (vao)
  (%draw-vertex-array
   (glhelp::vertex-array vao)
   (glhelp::indices vao)
   (render-type vao)))

;;;;
(defgeneric slow-draw (gl-thing)) ;;;dispatch on either display-list or vao
(defmethod slow-draw ((thing vao)) 
  (draw-vertex-array thing))
(defmethod slow-draw ((thing gl-list)) 
  (glhelp::draw-display-list (glhelp::handle thing)))
