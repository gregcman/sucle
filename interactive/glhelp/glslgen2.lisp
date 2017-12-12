(defpackage #:glslgen2
  (:use :cl))
(in-package :glslgen2) 

(defparameter *operators* 
  `
  ((dump-infix
    *
    /
    %
    <<
    >>
    <
    >
    <=
    >=
    ==
    =! 
    &
    ^
    |\||
    &&
    ^^
    ||
    =		    ;; assignemnts
    += -=		    ;;
    *= /=		    ;;
    %= <<= >>= &= ^= |\|=| ;;;
    |,|
    )
   (dump-infix-otherwise-unary
    + ;;unary and infix
    - ;unary and infix
    )
   (dump-unary
    ~ ;;unary
    !
    -- ;;pre or post
    ++ ;;prefix or post
    )
   (dump-postfix
    --/**/ ;;pre or post
    ++/**/ ;;prefix or post
    );;unary
   (dump-ternary
    |?\:|) ;;;special
   (dump-vref
    []);special

 ;  |()|;;special
   (dump-accessor
    |.|)
    ))

(defparameter *op-string-hash*
  (let ((hash (make-hash-table :test 'equal)))
    (dolist (common *operators*)
      (destructuring-bind (dump-fun &rest ops) common
	(dolist (sym ops)
	  (let ((name (symbol-name sym)))
	    (setf (gethash name hash) dump-fun)))))
    hash))

(defun dump-infix-otherwise-unary (op &rest args)
  (case (length args)
    (1 (apply #'dump-unary op args))
    (2 (apply #'dump-infix op args))))

(defun paren (&rest rest)
  (list "(" rest ")"))
(defun dump-infix (op &rest rest)
  (paren (pop rest) " " op " " (car rest)))
(defun dump-unary (op &rest rest)
  (paren op (car rest)))
(defun dump-postfix (op &rest rest)
  (paren (car rest) op))
(defun dump-ternary (op &rest rest)
  (declare (ignore op))
  (destructuring-bind (if then else) rest
    (paren if " ? " then " : " else)))
(defun dump-accessor (op &rest rest)
  (paren (pop rest) op (car rest)))
(defun dump-vref (op &rest rest)
  (declare (ignorable op))
  (paren (pop rest) "[" (car rest) "]"))

(defparameter *funs*
  '
  (
   int
   float
   bool
   ivec2
   ivec3
   ivec4
   bvec2
   bvec3
   bvec4
   vec2
   vec3
   vec4
   mat2
   mat3
   mat4))
(defparameter *control* 
  '
  (if else for while do-while continue break return discard))
'
(defparameter *builtin-vars*
  '
  ((gl-position "gl_Position" vec4)
   (gl-point-size "gl_PointSize" float)
   
   (gl-frag-color "gl_FragColor" vec4)
   (gl-frag-coord "gl_FragCoord" vec4)
   (gl-frag-data "gl_FragData" vec4 [])
   (gl-front-facing "gl_FrontFacing" bool)
   (gl-point-coord "gl_PointCoord" vec2)))

;;constants
(defparameter *some-constants*
  '((gl-max-vertex-attribs "gl_MaxVertexAttribs")
    (gl-max-vertex-uniform-vectors "gl_MaxVertexUniformVectors")
    (gl-max-varying-vectors "gl_MaxVaryingVectors")
    (gl-max-vertex-texture-image-units "gl_MaxVertexTextureImageUnits")
    (gl-max-combined-texture-image-units "gl_MaxCombinedTextureImageUnits")
    (gl-max-texture-image-units "gl_MaxTextureImageUnits")
    (gl-max-fragments-uniform-vectors "gl_MaxFragmentUniformVectors")
    (gl-max-draw-buffers "gl_MaxDrawbuffers")))

(defparameter *more-funs*
  '
  (radians
   degrees
   sin
   cos
   tan
   asin
   acos
   atan

   pow
   exp
   log
   exp2
   log2
   sqrt
   inversesqrt
   abs
   sign
   floor
   ceil
   fract
   mod
   min
   max
   clamp
   mix
   step
   smoothstep
   length
   distance
   dot
   cross
   normalize
   faceforward
   reflect
   refract
   (matrix-comp-mult "matrixCompMult")
   (less-than "lessThan")
   (less-than-equal "lessThanEqual")
   (greater-than "greaterThan")
   (greater-than-equal "greaterThanEqual")
   equal
   (not-equal "notEqual")
   any
   all
   not
   (texture2d "texture2D")
   (texture2d-proj "texture2DProj")
   (texture2d-lod "texture2DLod")
   (texture2d-proj-lod "texture2DProjLod")
   (texture-cube "textureCube")
   (texture-cube-lod "textureCubeLod")))

(defun gen-tables (&rest lists)
  (let ((hash (make-hash-table :test 'equal)))
    (labels
      ((build-item (item)
	 (cond ((atom item)
		(let ((namestring (symbol-name item)))
		  (add-item namestring item)))
	       (t (destructuring-bind (name glsl-name &rest rest) item
		    (declare (ignore rest))
		    (add-item (symbol-name name) glsl-name)))))
       (add-item (name glsl-name)
	 (setf (gethash (glslify-name name) hash) glsl-name))
       (do-items (list)
	 (dolist (item list)
	   (build-item item))))
      (dolist (item lists)
	(do-items item))
      hash)))

(defparameter *funs-string-hash*
  (gen-tables *more-funs* *funs*))
(defun get-fun-name (sym)
  (let ((name (glslify-name (symbol-name sym))))
    (or (gethash name *funs-string-hash*)
	name)))
(defparameter *vars-string-hash*
  (gen-tables *builtin-vars* *some-constants*))
(defun get-var-name (sym)
  (let ((name (glslify-name (symbol-name sym))))
    (or (gethash name *vars-string-hash*)
	name)))


(defun glslify-name (name)
  (substitute #\_ #\- (string-downcase name)))

(defun output-stuff (list)
  (glslgen:funglsl
   (glslify-name (car list))
   (mapcar
    (lambda (x)
      (cond ((atom x) (get-var-name x))
	    (t (output-stuff x))))
    (cdr list))))

(defun dispatch-fun ()
  )
