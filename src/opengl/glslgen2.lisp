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
    |.|)))

(defparameter *op-string-hash*
  (let ((hash (make-hash-table :test 'equal)))
    (dolist (common *operators*)
      (destructuring-bind (dump-fun &rest ops) common
	(dolist (sym ops)
	  (let ((name (string sym)))
	    (setf (gethash name hash) dump-fun)))))
    hash))

(defun dump-infix-otherwise-unary (op args)
  (case (length args)
    (1 (dump-unary op args))
    (2 (dump-infix op args))))

(defun paren (&rest rest)
  (list "(" rest ")"))
(defun dump-infix (op rest)
  (paren (pop rest) " " op " " (car rest)))
(defun dump-unary (op rest)
  (paren op (car rest)))
(defun dump-postfix (op rest)
  (paren (car rest) op))
(defun dump-ternary (op rest)
  (declare (ignore op))
  (destructuring-bind (if then else) rest
    (paren if " ? " then " : " else)))
(defun dump-accessor (op rest)
  (paren (pop rest) op (car rest)))
(defun dump-vref (op rest)
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

(defparameter *builtin-vars*
  '
  ("gl_Position" ;vec4
   "gl_PointSize"; float
   
   "gl_FragColor"; vec4
   "gl_FragCoord"; vec4
   "gl_FragData" ;vec4 []
   "gl_FrontFacing"; bool
   "gl_PointCoord" ;vec2
   ))

;;constants
(defparameter *some-constants*
  '("gl_MaxVertexAttribs"
    "gl_MaxVertexUniformVectors"
    "gl_MaxVaryingVectors"
    "gl_MaxVertexTextureImageUnits"
    "gl_MaxCombinedTextureImageUnits"
    "gl_MaxTextureImageUnits"
    "gl_MaxFragmentUniformVectors"
    "gl_MaxDrawbuffers"))

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
   "matrixCompMult"
   "lessThan"
   "lessThanEqual"
   "greaterThan"
   "greaterThanEqual"
   equal
   "notEqual"
   any
   all
   not
   "texture2D"
   "texture2DProj"
   "texture2DLod"
   "texture2DProjLod"
   "textureCube"
   "textureCubeLod"))

(defparameter *more-glsl-words*
  '(void
    "sampler2D"
    "texture2D"))

(defun gen-tables (&rest lists)
  (let ((hash (make-hash-table :test 'equal)))
    (labels
      ((build-item (item)
	 (cond ((atom item)
		(let ((namestring (string item)))
		  (add-item namestring (if (symbolp item)
					   (string-downcase namestring)
					   namestring))))))
       (add-item (name glsl-name)
	 (setf (gethash name hash) glsl-name))
       (do-items (list)
	 (dolist (item list)
	   (build-item item))))
      (dolist (item lists)
	(do-items item))
      hash)))

(defparameter *vars-string-hash*
  (gen-tables *builtin-vars* *some-constants* *more-glsl-words*
	      *more-funs* *funs*))
(defun get-var-name (name)
  (gethash name *vars-string-hash*))

(defun dump-normal-args (args &optional (reserved *reserved*))
  (mapcar
   (lambda (x)
     (cond ((atom x) (dispatch-var x reserved))
	   (t (output-stuff x reserved))))
   args))

(defparameter *reserved* nil)
(defun output-stuff (list &optional (reserved *reserved*))
  (let ((name (dispatch-var (car list))))
    (multiple-value-bind (dispatch-fun type) (dispatch-fun name)
      (case type
	(function-expression
	 (funcall
	  dispatch-fun
	  name
	  (dump-normal-args (cdr list) reserved)))
	(special-expression
	 (funcall dispatch-fun name (cdr list)))))))

(defun dispatch-fun (name)
  (let ((fun (gethash name *op-string-hash*)))
    (if fun
	(values fun 'function-expression)
	(let ((special (gethash name *special-op-string-hash*)))
	  (if special
	      (values special 'special-expression)
	      (values #'glslgen:funglsl 'function-expression))))))

(defun dispatch-var (node &optional (reserved '()))
  (if (member node reserved)
      node
      (typecase node
	(symbol (let ((newname (string node)))
		  (or
		   (get-var-name newname)
		   newname)))
	(otherwise (write-to-string node :escape nil :pretty nil :base 10 :readably nil)))))

(defun dump-test (list)
  (glslgen::dump-string #'identity (output-stuff list)))

(defun make-shader-stage (&key in out temp program)
  (let ((*reserved* (cons :gl-frag-color (mapcar #'first (append in out temp)))))
    (let ((dedumped (output-stuff program)))
;;      (print dedumped)
 ;;     (print (glslgen::dump-string #'identity dedumped))
      
      (glslgen::make-shader-vars :out out
				 :in in
				 :program dedumped
				 :temp temp))))

(defun comma-separated-list (args)
  (list "(" (glslgen::spaces args ", ") ")"))

(defun glsl-progn (op args)
  (declare (ignorable op))
  (glslgen::brackets 
   (mapcar (lambda (x) (list x glslgen::*semicolon*))
	   (dump-normal-args args))))
(defun noop (op args)
  (declare (ignore op))
  (glslgen:spaces (dump-normal-args args)))
(defparameter *special-operators*
  '
  ((noop
    /**/)
   (glsl-progn
    progn)
   (glsl-func-def
    defun)
   (glsl-if
    if)))
(defun glsl-if (op args)
  (declare (ignore op))
  (destructuring-bind (test first &optional else) args
    (let ((end (if else
		   (list "else"
			 (glslgen::brackets (output-stuff else)))
		   nil)))
      (list*
       "if"
       (paren (output-stuff test))
       (glslgen::brackets (glslgen::blogn (output-stuff first)))
       end))))

(defun glsl-func-def (op args)
  (declare (ignore op))
  (destructuring-bind (name type params &rest body) args
    (list
     (get-var-name (string type))
     " "
     (string name)
     (comma-separated-list params)
     (glsl-progn nil body))))
(defparameter *special-op-string-hash*
  (let ((hash (make-hash-table :test 'equal)))
    (dolist (common *special-operators*)
      (destructuring-bind (dump-fun &rest ops) common
	(dolist (sym ops)
	  (let ((name (string sym)))
	    (setf (gethash name hash) dump-fun)))))
    hash))


#+nil
(defun glslify-name (name)
  (substitute #\_ #\- (string-downcase name)))
