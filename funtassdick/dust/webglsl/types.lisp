(in-package :webglsl)

(defmacro glsl (&rest args)
  (cons 'progn args))

(glsl 100
      (deftype %void () 'nil)
      (deftype %bool () 'bit)
      (deftype %int () '(signed-byte 32))
      (deftype %float () 'single-float))

(deftype %array (type size) `(simple-array ,type ,size))
(deftype %bool-array (&optional n) `(%array %bool (,n)))
(deftype %int-array (&optional n) `(%array %int (,n)))
(deftype %float-array (&optional n) `(%array %float (,n)))

(glsl 100
      (deftype %vec2 () '(%float-array 2))
      (deftype %vec3 () '(%float-array 3))
      (deftype %vec4 () '(%float-array 4))
      (deftype %bvec2 () '(%bool-array 2))
      (deftype %bvec3 () '(%bool-array 3))
      (deftype %bvec4 () '(%bool-array 4))
      (deftype %ivec2 () '(%int-array 2))
      (deftype %ivec3 () '(%int-array 3))
      (deftype %ivec4 () '(%int-array 4)))

(deftype %square-float-matrix (n) `(%float-array ,(* n n)))

(glsl 100
      (deftype %mat2 () '(%square-float-matrix 2))
      (deftype %mat3 () '(%square-float-matrix 3))
      (deftype %mat4 () '(%square-float-matrix 4))
      (deftype %sampler2d () t)
      (deftype %samplercube () t))

(defmacro %if (test-form then-form &optional else-form)
  `(if (zerop ,test-form)
       ,else-form
       ,then-form))

(defun %int (value)
  (typecase value
    (%bool value)
    (%int value)
    (%float (truncate value))
    (%bool-array (sbit value 0))
    (%int-array (aref value 0))
    (%float-array (truncate (aref value 0)))))

(defun %float (value)
  (typecase value
    (%bool (float value))
    (%int (float value))
    (%float value)
    (%bool-array (float (sbit value 0)))
    (%int-array (float (aref value 0)))
    (%float-array (aref value 0))))

(defun %bool (value)
  (typecase value
    (%bool value)
    (%int (%if value 0 1))
    (%float (%if value 0 1))
    (%bool-array (sbit value 0))
    (%int-array (%if (aref value 0) 0 1))
    (%float-array (%if (aref value 0) 0 1))))

(defun %%vec2 ()
  (make-array 2 :element-type '%float))
(defun %%vec3 ()
  (make-array 3 :element-type '%float))
(defun %%vec4 ()
  (make-array 4 :element-type '%float))
(defun %%bvec2 ()
  (make-array 2 :element-type '%bool))
(defun %%bvec3 ()
  (make-array 3 :element-type '%bool))
(defun %%bvec4 ()
  (make-array 4 :element-type '%bool))
(defun %%ivec2 ()
  (make-array 2 :element-type '%int))
(defun %%ivec3 ()
  (make-array 3 :element-type '%int))
(defun %%ivec4 ()
  (make-array 4 :element-type '%int))

(defun %%mat2 ()
  (make-array 4 :element-type '%float))
(defun %%mat3 ()
  (make-array 9 :element-type '%float))
(defun %%mat4 ()
  (make-array 16 :element-type '%float))

(defconstant +newline-string+ (make-string 1 :initial-element #\newline))
(defconstant +null-string+ "")
(defconstant +semicolon-string+ ";")

(defun emit-newline (stream)
  (format stream +newline-string+))
(defun emit-semicolon (stream)
  (format stream +semicolon-string+))
(defun emit-right-paren (stream)
  (format stream ")"))
(defun emit-left-paren (stream)
  (format stream "("))
(defun emit-right-bracket (stream)
  (format stream "]"))
(defun emit-left-bracket (stream)
  (format stream "["))
(defun emit-right-brace (stream)
  (format stream "}"))
(defun emit-left-brace (stream)
  (format stream "{"))
(defun emit-comma (stream)
  (format stream ","))
(defun emit-space (stream)
  (format stream " "))
(defun emit-dot (stream)
  (format stream "."))
(defun emit-dash (stream)
  (format stream "-"))
(defun emit-bang (stream)
  (format stream "!"))
(defun emit-right-angle (stream)
  (format stream ">"))
(defun emit-left-angle (stream)
  (format stream "<"))
(defun emit-plus (stream)
  (format stream "+"))
(defun emit-caret (stream)
  (format stream "^"))
(defun emit-percent (stream)
  (format stream "%"))
(defun emit-slash (stream)
  (format stream "/"))
(defun emit-equal (stream)
  (format stream "="))
(defun emit-colon (stream)
  (format stream ":"))
(defun emit-tilde (stream)
  (format stream "~~"))
(defun emit-star (stream)
  (format stream "*"))
(defun emit-ampersand (stream)
  (format stream "&"))
(defun emit-question (stream)
  (format stream "?"))
(defun emit-vertical-bar (stream)
  (format stream "|"))

(defun emit-uniform (stream func)
  (format stream "uniform ")
  (funcall func stream)
  (emit-semicolon stream)
  (emit-newline stream))

(defun emit-attribute (stream func)
  (format stream "attribute ")
  (funcall func stream)
  (emit-semicolon stream)
  (emit-newline stream))

(defun emit-varying (stream func)
  (format stream "varying ")
  (funcall func stream)
  (emit-semicolon stream)
  (emit-newline stream))

(defun emit-version (stream)
  (format stream "#version 100")
  (emit-newline stream))

(defun literal (literal-string)
  (lambda (stream)
    (princ literal-string stream)))

(defun literals (&rest literal-strings)
  (mapcar #'literal literal-strings))

(defun bar (stream)
  (emit-version stream)
  (emit-newline stream)
  (emit-attributes stream (list (literal "vec4 position")
				(literal "vec2 texCoord")
				(literal "float darkness")))
  (emit-newline stream)
  (emit-uniforms stream (list (literal "mat4 projectionmodelview")
			      (literal "sampler2D ourTexture")
			      (literal "vec3 cameraPos")
			      (literal "float foglet")
			      (literal "float aratio")))
  (emit-newline stream)
  (emit-varyings stream (list (literal "lowp vec2 TexCoord")
			      (literal "lowp float mycolor")
			      (literal "lowp float fogratio")))
  (emit-newline stream)
  (emit-call stream
	     (literal "void main")
	     nil)
  (emit-newline stream)
  (emit-block-with-statements
   stream
   (lambda (stream)
     (walk-expr stream  '(basic-assignment
			  "gl_Position"
			  (multiplication
			   "projectionmodelview"
			   "position"))))
   (lambda (stream)
     (walk-expr stream  '(basic-assignment
			  "mycolor"
			  "darkness")))
   (lambda (stream)
     (walk-expr stream  '(basic-assignment
			  "TexCoord"
			  "texCoord")))
   (lambda (stream)
     (walk-expr stream  '(basic-assignment
			  "fogratio"
			  ("min" 1.0
			   (addition
			    (multiplication
			     ("distance"
			      "cameraPos"
			      (structure-reference "position" "xyz"))
			     "foglet")
			    "aratio")))))))

"void main()
{

gl_Position = projectionmodelview * position;

mycolor = darkness;
TexCoord = texCoord;

fogratio = min(distance(position.xyz, cameraPos)*foglet+aratio, 1.0);

}"

(defun emit-block-with-statements (stream &rest statements)
  (emit-block stream
	      (lambda (stream)
		(emit-statements stream
				 statements))))

(defun emit-statements (stream statements)
  (dolist (statement statements)
    (emit-statement stream statement)))

(defun emit-statement (stream statement)
  (funcall statement stream)
  (emit-semicolon stream)
  (emit-newline stream))

(defun emit-block (stream body-func)
  (emit-braces stream
	       (lambda (stream)
		 (emit-newline stream)
		 (funcall body-func stream))))

(defun emit-attributes (stream funcs)
  (dolist (func funcs)
    (emit-attribute stream func)))
(defun emit-uniforms (stream funcs)
  (dolist (func funcs)
    (emit-uniform stream func)))
(defun emit-varyings (stream funcs)
  (dolist (func funcs)
    (emit-varying stream func)))

(defparameter foo (make-string-output-stream))
(defun test ()
  (progn (princ (bar foo)) (values))
  (get-output-stream-string foo))
(defun emit-parens (stream body-func)
  (emit-left-paren stream)
  (funcall body-func stream)
  (emit-right-paren stream))
(defun emit-braces (stream body-func)
  (emit-left-brace stream)
  (funcall body-func stream)
  (emit-right-brace stream))
(defun emit-brackets (stream body-func)
  (emit-left-bracket stream)
  (funcall body-func stream)
  (emit-right-bracket stream))

(defun emit-comma-separated (stream list)
  (tagbody rep
     (let ((first (pop list)))
       (when first (funcall first stream))
       (when list
	 (emit-comma stream)
	 (emit-space stream)
	 (go rep)))))

(defun emit-call (stream function-name args)
  (funcall function-name stream)
  (emit-parens stream
	       (lambda (stream)
		 (emit-comma-separated stream args))))


(defstruct (operator (:constructor make-op
			     (operator precedence associativity arity glsl-string print-func)))
  operator
  precedence
  associativity
  arity
  glsl-string
  print-func)

(defconstant +void+ (lambda (&rest args) (declare (ignore args)) (values)))

(defmacro infix-binop-lambda (stream-name &rest body)
  (let ((a (gensym))
	(b (gensym)))
    `(lambda (,stream-name ,a ,b)
       (funcall ,a ,stream-name)
       (emit-space ,stream-name)
       ,@body
       (emit-space ,stream-name)
       (funcall ,b ,stream-name))))

(defmacro infix-binop-lambda-spaceless (stream-name &rest body)
  (let ((a (gensym))
	(b (gensym)))
    `(lambda (,stream-name ,a ,b)
       (funcall ,a ,stream-name)
       ,@body
       (funcall ,b ,stream-name))))

(defmacro twice (&rest body)
  `(progn
     ,@body
     ,@body))

(defparameter *operator-data*
  `((1 nil
       (parentheses "()" 1
		    ,(lambda (stream arg)
			     (emit-parens stream arg))))
    (2 :l
       (subscript "[]" 2
		  ,(lambda (stream a b)
			   (funcall a stream)
			   (emit-brackets stream b)))
       (function-call "()" 2
		      ,(lambda (stream a b)
			       (emit-call stream a b)))
       (structure-reference "." 2
			    ,(infix-binop-lambda-spaceless s (emit-dot s)))
       (increment-postfix "++" 1
			  ,(lambda (stream arg)
				   (funcall arg stream)
				   (emit-plus stream)
				   (emit-plus stream)
				 ))
       (decrement-postfix "--" 1
			  ,(lambda (stream arg)
				   (funcall arg stream)
				   (emit-dash stream)
				   (emit-dash stream)
				 ))) 
    (3 :r
       (increment-prefix "++" 1
			 ,(lambda (stream arg)
				
				  (emit-plus stream)
				  (emit-plus stream)
				  (funcall arg stream)))
       (decrement-prefix "--" 1
			 ,(lambda (stream arg)
				 
				  (emit-dash stream)
				  (emit-dash stream)
				  (funcall arg stream)))
       (unary-plus "+" 1
		   ,(lambda (stream arg)

			    (emit-plus stream)
			    (funcall arg stream)))
       (unary-minus "-" 1
		    ,(lambda (stream arg)
			   
			     (emit-dash stream)
			     (funcall arg stream)))
       (logical-negation "!" 1
			 ,(lambda (stream arg)
				 
				  (emit-bang stream)
				  (funcall arg stream)))
       (bitwise-not "~" 1
		    ,(lambda (stream arg)
			    
			     (emit-tilde stream)
			     (funcall arg stream))))	   
    (4 :l
       (multiplication "*" 2 ,(infix-binop-lambda s (emit-star s)))
       (division "/" 2 ,(infix-binop-lambda s (emit-slash s)))
       (modulo "%" 2 ,(infix-binop-lambda s (emit-percent s)))) 
    (5 :l
       (addition "+" 2 ,(infix-binop-lambda s (emit-plus s)))
       (subtraction "-" 2 ,(infix-binop-lambda s (emit-dash s))))
    (6 :l
       (bitwise-left-shift "<<" 2 ,(infix-binop-lambda s (twice (emit-left-angle s))))
       (bitwise-right-shift ">>" 2 ,(infix-binop-lambda s (twice (emit-right-angle s)))))
    (7 :l
       (less-than "<" 2 ,(infix-binop-lambda s (emit-left-angle s)))
       (greater-than ">" 2 ,(infix-binop-lambda s (emit-right-angle s)))
       (less-than-or-equal-to "<=" 2 ,(infix-binop-lambda s
							(emit-left-angle s)
							(emit-equal s)))
       (greater-than-or-equal-to ">=" 2 ,(infix-binop-lambda s
							   (emit-right-angle s)
							   (emit-equal s))))
    (8 :l
       (equal-to "==" 2 ,(infix-binop-lambda s (twice (emit-equal s))))
       (not-equal-to "!=" 2
		     ,(infix-binop-lambda s
					  (emit-bang s)
					  (emit-equal s))))
    (9 :l
       (bitwise-and "&" 2 ,(infix-binop-lambda s (emit-ampersand s))))
    (10 :l
	(bitwise-xor "^" 2 ,(infix-binop-lambda s (emit-caret s)))) 
    (11 :l
	(bitwise-or "|" 2 ,(infix-binop-lambda s (emit-vertical-bar s))))
    (12 :l
	(logical-and "&&" 2 ,(infix-binop-lambda s (twice (emit-ampersand s)))))
    (13 :l
	(double-caret "^^" 2 ,(infix-binop-lambda s (twice (emit-caret s)))))
    ;;;what is this thing?
    (14 :l
	(logical-or "||" 2 ,(infix-binop-lambda s (twice (emit-vertical-bar s)))))
    (15 :r
	(ternary-conditional "?:" 3
			     ,(lambda (stream test then else)
				      (funcall test stream)
				      (emit-space stream)
				      (emit-question stream)
				      (emit-space stream)
				      (funcall then stream)
				      (emit-space stream)
				      (emit-colon stream)
				      (emit-space stream)
				      (funcall else stream))))
    (16 :r
	(basic-assignment "=" 2
			  ,(infix-binop-lambda s (emit-equal s)))
	(addition-assignment "+=" 2
			     ,(infix-binop-lambda s
						  (emit-plus s)
						  (emit-equal s)))
	(subtraction-assignment "-=" 2
				,(infix-binop-lambda s
						     (emit-dash s)
						     (emit-equal s)))
	(multiplication-assignment "*=" 2
				   ,(infix-binop-lambda s (emit-star s)
							(emit-equal s)))
	(division-assignment "/=" 2 
 			     ,(infix-binop-lambda s
						  (emit-slash s)
						  (emit-equal s)))
	(modulo-assignment "%=" 2
			   ,(infix-binop-lambda s
						(emit-percent s)
						(emit-equal s)))
	(bitwise-left-shift-assignment "<<=" 2
				       ,(infix-binop-lambda s
							    (twice (emit-left-angle s))
							    (emit-equal s)))
	(bitwise-right-shift-assignment ">>=" 2
					,(infix-binop-lambda s
							     (twice (emit-right-angle s))
							     (emit-equal s)))
	(bitwise-and-assignment "&=" 2
				,(infix-binop-lambda s
						     (emit-ampersand s)
						     (emit-equal s)))
	(bitwise-xor-assignment "^=" 2
				,(infix-binop-lambda s
						     (emit-caret s)
						     (emit-equal s)))
	(bitwise-or-assignment "|=" 2
			       ,(infix-binop-lambda s
						    (emit-vertical-bar s)
						    (emit-equal s))))
    (17 :l
	(comma "," 2
	       ,(infix-binop-lambda s (emit-comma s))))
    (31 nil
	(nil "" 0 ,+void+))))

;;:l is left to right :r is right to left nil is NA
(defparameter *operators*
  (let ((tot nil)
	(ops *operator-data*))
    (dolist (zopz ops)
      (destructuring-bind (precedence associativity &rest thops) zopz
	(dolist (op thops)
	  (destructuring-bind (operator glsl-string arity print-func) op
	      (push (make-op operator precedence associativity arity glsl-string print-func) tot)))))
    tot))

(defparameter *ophash*
  (let ((table (make-hash-table :test 'eq)))
    (dolist (op *operators*)
      (setf (gethash (operator-operator op) table) op))
    table))

(defparameter *nilop* (gethash nil *ophash*))

(defun getop (op)
  (gethash op *ophash*))

(defparameter *parens-always* nil)

(defun parensp (curr left-info right-info)
  (or *parens-always*
      (let ((curr-prec (operator-precedence curr)))
	(let ((left-prec (operator-precedence left-info)))
	  (if (< left-prec curr-prec)
	      (return-from parensp 0)
	      (when (and (= left-prec curr-prec)
			 (eq (operator-associativity left-info) :l))
		(return-from parensp 1))))
	(let ((right-prec (operator-precedence right-info)))
	  (if (< right-prec curr-prec)
	      (return-from parensp 2)
	      (when (and (= right-prec curr-prec)
			 (eq (operator-associativity right-info) :r))
		(return-from parensp 3)))))))

(defun walk-binop (stream curr tree left right)
  (let ((tail (cdr tree)))
    (let* ((l (pop tail))
	   (r (pop tail)))
      (let ((parensp (parensp curr left right))
	    (print-func (operator-print-func curr)))
	(if parensp
	    (progn
	      (emit-parens stream (lambda (stream)
				    (funcall print-func stream
					     (lambda (stream)
					       (walk stream l *nilop* curr))
					     (lambda (stream)
					       (walk stream r curr *nilop*))))))
	    (progn
	      (funcall print-func stream
		       (lambda (stream)
			 (walk stream l left curr))
		       (lambda (stream)
			 (walk stream r curr right)))))))))

(defun walk-unary (stream curr tree left right)
  (let ((u (second tree)))
    (let ((parensp (parensp curr left right))
	  (print-func (operator-print-func curr)))
      (if parensp
	  (progn
	    (emit-parens stream (lambda (stream)
				  (funcall print-func stream
					   (lambda (stream)
					     (walk stream u *nilop* *nilop*))))))
	  (progn
	    (funcall print-func stream
		     (lambda (stream)
		       (walk stream u left right))))))))

(defun walk-ternary (stream curr tree left right)
  (let ((tail (cdr tree)))
    (let* ((l (pop tail))
	   (m (pop tail))
	   (r (pop tail)))
      (let ((parensp (parensp curr left right))
	    (print-func (operator-print-func curr)))
	(if parensp
	    (progn
	      (emit-parens stream (lambda (stream)
				    (funcall print-func stream
					     (lambda (stream)
					       (walk stream l *nilop* curr))
					     (lambda (stream)
					       (walk stream m *nilop* *nilop*))
					     (lambda (stream)
					       (walk stream r curr *nilop*))))))
	    (progn
	      (funcall print-func stream
		       (lambda (stream)
			 (walk stream l left curr))
		       (lambda (stream)
			 (walk stream m *nilop* *nilop*))
		       (lambda (stream)
			 (walk stream r curr right)))))))))

(defun walk (stream tree left right)
	     (if (consp tree)
		 (let ((curr (getop (car tree))))
		   (if curr
		       (let ((arity (operator-arity curr)))
			 (case arity
			   (1 (walk-unary stream curr tree left right))
			   (2 (walk-binop stream curr tree left right))
			   (3 (walk-ternary stream curr tree left right))))
		       (let ((curr (getop 'function-call)))
			 (let ((print-func (operator-print-func curr)))
			   (funcall print-func stream
				    (literal (car tree))
				    (mapcar (lambda (x)
					      (lambda (stream)
						(walk-expr stream x))) (cdr tree)))))))
		 (format stream "~a" tree)))


(defun walk-expr (stream ast)
  (walk stream ast *nilop* *nilop*))

(defun llist (&rest args)
  args)

(defparameter wtf
  (let ((a (cons nil nil)))
    (let ((b (cons a a)))
      (setf (car a) b)
      (setf (cdr a) b))))

 
