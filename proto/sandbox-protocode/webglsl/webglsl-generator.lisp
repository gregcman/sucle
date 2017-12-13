(in-package :webglsl)

(defparameter *preprocessor-directives*
  (list
   "#"
   "#define"
   "#undef"

   "#if"
   "#ifdef"
   "#ifndef"
   "#else"
   "#elif"
   "#endif"
   
   "#error"
   "#pragma"

   "#extension"
   "#version" ;;;must be 100

   "#line"))

(defstruct (preprocessor-op (:constructor preop (operator precedence associativity operator-class)))
  operator
  precedence
  associativity
  operator-class)

(defparameter *preprocessor-operators*
  (list
   (preop "(" 1 nil :parenthetical-grouping)
   (preop ")" 1 nil :parenthetical-grouping)
   (preop "defined" 2 :r-l :unary) ;;;defined (identifier) ;;;defined identifier
   (preop "+" 2 :r-l :unary)
   (preop "-" 2 :r-l :unary)
   (preop "~" 2 :r-l :unary)
   (preop "!" 2 :r-l :unary)
   (preop "*" 3 :l-r :multiplicative)
   (preop "/" 3 :l-r :multiplicative)
   (preop "%" 3 :l-r :multiplicative)
   (preop "+" 4 :l-r :additive)
   (preop "-" 4 :l-r :additive)
   (preop "<<" 5 :l-r :bitwise-shift)
   (preop ">>" 5 :l-r :bitwise-shift)
   (preop "<" 6 :l-r :relational)
   (preop ">" 6 :l-r :relational)
   (preop "<=" 6 :l-r :relational)
   (preop ">=" 6 :l-r :relational)
   (preop "==" 7 :l-r :equality)
   (preop "!=" 7 :l-r :equality)
   (preop "&" 8 :l-r :bitwise-and)
   (preop "^" 9 :l-r :bitwise-xor)
   (preop "|" 10 :l-r :bitwise-ior)
   (preop "&&" 11 :l-r :logical-and)
   (preop "||" 12 :l-r :logical-ior)))

(defparameter *preprocessor-predefined-macros*
  (list
   "__LINE__"
   "__FILE__"
   "__VERSION__"
   "GL_ES"))

(defparameter *preprocessor-pragmas*
  (list
   "STDGL"
   
   "optimize(on)"
   "optimize(off)"

   "debug(on)"
   "debug(off)"))

(defparameter *preprocessor-extension-behaviors*
  (list
   "require"
   "enable"
   "warn"
   "disable"))

(defparameter *whitespace*
  '(#\Space #\NewLine #\Tab #\Linefeed #\Page #\Return))
(defparameter *numbers*
  '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
(defparameter *letters-lowercase* 
  '(#\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m
    #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z))
(defparameter *letters-uppercase*
  '(#\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M
    #\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z))
(defconstant +number-sign+ #\#)
(defconstant +underscore+ #\_)
(defparameter *symbols*
  '(#\. #\+ #\- #\/ #\* #\% #\<
    #\> #\[ #\] #\( #\) #\{ #\} #\^
    #\| #\& #\~ #\= #\! #\: #\; #\,
    #\?))

(defun char-whitespace-p (char)
  (member char *whitespace*))
(defun char-number-p (char)
  (member char *numbers*))
(defun char-letter-uppercase-p (char)
  (member char *letters-lowercase*))
(defun char-letter-lowercase-p (char)
  (member char *letters-uppercase*))
(defun char-symbol-p (char)
  (member char *symbols*))
(defun char-number-sign-p (char)
  (char= char +number-sign+))
(defun char-underscore-p (char)
  (char= char +underscore+))

(defun char-letter-p (char)
  (or (char-letter-lowercase-p char)
      (char-letter-uppercase-p char)))

(defparameter *hexadecimal-digits*
  (list
   #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9
   #\a #\b #\c #\d #\e #\f
   #\A #\B #\C #\D #\E #\F)) ;;leading 0X or 0x's

(defparameter *octal-digits*
  (list
   #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7)) ;;leading zeroes

(defparameter *tokens*
  (list
   "keyword"
   "identifier"
   "integer-constant"
   "floating-constant"
   "operator"))

(defparameter *keywords*
  (list
   "attribute" "const" "uniform" "varying"
   "break" "continue" "do" "for" "while"
   "if" "else"
   "in" "out" "inout"
   "float" "int" "void" "bool" "true" "false"
   "lowp" "mediump" "highp" "precision" "invariant"
   "discard" "return"
   "mat2" "mat3" "mat4"
   "vec2" "vec3" "vec4" "ivec2" "ivec3" "ivec4" "bvec2" "bvec3" "bvec4"
   "sampler2D" "samplerCube"
   "struct"))

(defparameter *keywords-reserved*
  (list
   "asm"
   "class" "union" "enum" "typedef" "template" "this" "packed"
   "goto" "switch" "default"
   "inline" "noinline" "volatile" "public" "static" "extern" "external" "interface" "flat"
   "long" "short" "double" "half" "fixed" "unsigned" "superp"
   "input" "output"
   "hvec2" "hvec3" "hvec4" "dvec2" "dvec3" "dvec4" "fvec2" "fvec3" "fvec4"
   "sampler1D" "sampler3D"
   "sampler1DShadow" "sampler2dShadow"
   "sampler2DRect" "sampler3dRect" "sampler2DRectShadow"
   "sizeof" "cast"
   "namespace" "using"))

(defun keyword-reserved-p (keyword)
  (when (<= 2 (length keyword))
    (or (and (char= #\_ (aref keyword 0))
	     (char= #\_ (aref keyword 1)))
	(member keyword *keywords-reserved* :test #'equal))))

(defun identifier-reserved-p (identifier)
  (when (<= 3 (length identifier))
    (and (char= #\g (aref identifier 0))
	 (char= #\l (aref identifier 1))
	 (char= #\_ (aref identifier 2)))))

(defun char-nondigit-p (char)
  (or (char-letter-p char)
      (char-underscore-p char)))

(defun identifier-p (identifier)
  (let ((len (length identifier)))
    (unless (zerop len)
      (unless (char-nondigit-p (aref identifier 0))
	(return-from identifier-p nil))
      (dotimes (x (1- len))
	(let ((char (aref identifier (1+ x))))
	  (unless (or (char-nondigit-p char)
		      (char-number-p char))
	    (return-from identifier-p nil))))
      t)))

(defparameter *basic-data-types*
  (list
   "void"
   "bool"
   "int"
   "float"
   "vec2"
   "vec3"
   "vec4"
   "bvec2"
   "bvec3"
   "bvec4"
   "ivec2"
   "ivec3"
   "ivec4"
   "mat2"
   "mat3"
   "mat4"
   "sampler2D"
   "samplerCube"))

(defparameter *conditionals*
  (list
   "if"
   "for"
   "?:"
   "while"
   "do-while"))

;;;remove parentheses when operators fit according to precedence?

;;;('texture2D '(a b)) -> texture2D(a, b); = '(texture2D "(" a ", " b ")")
;;;put the function in the front unless its an operator.
;;;if the function is an operator use parentheses and shit
;;;put commas between the arguments, with a space after each comma
;;;terminate with a semicolon
(defun lisp-glsl-function-call (function args) 
  `(,function . ,(glsl-lambda-list args)))

(defun glsl-lambda-list (list)
  (let ((tot (list "(")))
    (dolist (element list)
      (push element tot)
      (push ", " tot))
    (setf (car tot) ")")
    (nreverse tot)))

(defun flatten (obj)
	   (do* ((result (cons obj nil))
		 (node result))
		((null node) (delete nil result))
	     (if (consp (car node))
		 (progn (when (cdar node) 
			  (setf (cdr node) 
				(cons (cdar node) 
				      (cdr node))))
			(setf (car node) (caar node)))
		 (setf node 
		       (cdr node)))))

(defun list-of-strings-to-string (list)
  (let ((total 0))
    (dolist (str list)
      (incf total (length str)))
    (let ((concat (make-string total))
	  (index 0))
      (dolist (str list)
	(let ((len (length str)))
	  (dotimes (charplace len)
	    (setf (aref concat index) (aref str charplace))
	    (incf index))))
      concat)))

(defun list-of-stuff-to-list-of-strings (list)
  (mapcar
   (lambda (x)
     (if (stringp x)
	 x
	 (write-to-string x)))
   list))


(defun walk-paren-flatten (expr)
  (let ((flat nil))
    (labels ((flatten-paren (expr)
		 (if (consp expr)
		     (destructuring-bind (parensp op left right) expr
		       (when parensp (push ")" flat))
		       (flatten-paren right)
		       (push op flat)
		       (flatten-paren left)
		       (when parensp (push "(" flat)))
		     (push expr flat))))
      (flatten-paren expr))
    flat))

;(defun stringify-funcs)

(defun binop-converter (expr)
  (list-of-strings-to-string
   (list-of-stuff-to-list-of-strings
    (walk-paren-flatten
     (walk-expr expr)))))

(defparameter *foo* ;; (89 / (23 * 4)) + (67 - 76)
  '("+"
    ("/" 89
     ("*" 23 4)) 
    ("-" 67 76)))

(defparameter *foo2* ;; ((23 * 4) / 89) + (67 - 76)
  '("+"
    ("/"
     ("*" 23 4)
     89) 
    ("-" 67 76)))
(defun foo ()
  (maphash (lambda (k v)
	     (multiple-value-bind (loopp table) (walk-dependencies k)
	       (print (list loopp (hash-table-count table) k))))
	   *GLSL-GRAMMAR-RULES-TABLE*))

(defparameter *lexical-tokens*
  '(ATTRIBUTE CONST BOOL FLOAT INT
    BREAK CONTINUE DO ELSE FOR IF DISCARD RETURN
    BVEC2 BVEC3 BVEC4 IVEC2 IVEC3 IVEC4 VEC2 VEC3 VEC4
    MAT2 MAT3 MAT4 IN OUT INOUT UNIFORM VARYING
    SAMPLER2D SAMPLERCUBE
    STRUCT VOID WHILE

    IDENTIFIER TYPE-NAME FLOATCONSTANT INTCONSTANT BOOLCONSTANT
    FIELD-SELECTION
    LEFT-OP RIGHT-OP
    INC-OP DEC-OP LE-OP GE-OP EQ-OP NE-OP
    AND-OP OR-OP XOR-OP MUL-ASSIGN DIV-ASSIGN ADD-ASSIGN
    MOD-ASSIGN LEFT-ASSIGN RIGHT-ASSIGN AND-ASSIGN XOR-ASSIGN OR-ASSIGN
    SUB-ASSIGN

    LEFT-PAREN RIGHT-PAREN LEFT-BRACKET RIGHT-BRACKET LEFT-BRACE RIGHT-BRACE DOT
    COMMA COLON EQUAL SEMICOLON BANG DASH TILDE PLUS STAR SLASH PERCENT
    LEFT-ANGLE RIGHT-ANGLE VERTICAL-BAR CARET AMPERSAND QUESTION

    INVARIANT
    HIGH-PRECISION MEDIUM-PRECISION LOW-PRECISION PRECISION))

;;;dependencies in the car of the cons dependents in the cdr 
(defparameter *glsl-grammar-rules-dependencies*
  (let ((table (make-hash-table :test 'eq)))
    (maphash
     (lambda (k v)
       (let ((thecons (or (gethash k table)
			  (cons nil nil))))
	 (setf (gethash k table) thecons)
	 (when (consp v)
	   (dolist (option v)
	     (dolist (value option)
	       (push value (car thecons))
	       (let ((othercons (or (gethash value table)
				    (cons nil nil))))
		 (setf (gethash value table) othercons)
		 (push k (cdr othercons))))))))
     *glsl-grammar-rules-table*)
    table))

(defparameter *scratch-eq-hash* (make-hash-table :test 'eq))
(defun walk-dependencies (start)
  (let ((queue (q:make-uniq-q))
	(visited (clrhash *scratch-eq-hash*)))
    (q:uniq-push start queue)
    (setf (gethash start visited) 0)
    (tagbody
     rep
       (let ((val (q:uniq-pop queue)))
	 (when val
	   (print val)
	   (let ((connections (gethash val *glsl-grammar-rules-table*)))
	     (if (consp connections)
		 (dolist (dependencies connections)
		   (dolist (support dependencies)
		     (if (eq support start)
			 (incf (gethash support visited))
			 (unless (gethash support visited)
			   
			   (setf (gethash support visited) t)
			   (q:uniq-push support queue)
			   ))))
					;(print connections)
		 ))
	   (go rep))))
    (values (gethash start visited) visited)))

(defun sort-by-breadth ()
  (let ((list nil))
    (maphash
     (lambda (k v)
       (declare (ignorable v))
       (multiple-value-bind (loopp table) (walk-dependencies k)
	 (let ((breadth (hash-table-count table))
	       (self-loop-p loopp)
	       (rule-name k))
	   (push (list breadth self-loop-p rule-name) list))))
     *GLSL-GRAMMAR-RULES-TABLE*)
    (sort list #'> :key 'car)))

;;;ast node = cons cell with car name and cdr data

(defparameter *foofo*
  '((162 1 TRANSLATION-UNIT);just a list of 1 or more external-declarations
    (161 0 EXTERNAL-DECLARATION) ;;function-definition or %declaration
    (160 0 FUNCTION-DEFINITION)
    (159 1 ITERATION-STATEMENT);
    (159 1 SELECTION-REST-STATEMENT);
    (159 1 SELECTION-STATEMENT);
    (159 3 STATEMENT-LIST);
    (159 1 COMPOUND-STATEMENT-NO-NEW-SCOPE);
    (159 4 STATEMENT-WITH-SCOPE);
    (159 1 COMPOUND-STATEMENT-WITH-SCOPE);
    (159 2 SIMPLE-STATEMENT);
    (159 4 STATEMENT-NO-NEW-SCOPE);
    (137 0 FOR-INIT-STATEMENT)
    (135 0 DECLARATION-STATEMENT)
    (134 0 %DECLARATION)
    (129 0 FUNCTION-PROTOTYPE)
    (128 0 FUNCTION-DECLARATOR)
    (127 1 FUNCTION-HEADER-WITH-PARAMETERS);
    (124 0 PARAMETER-DECLARATION)
    (122 0 FOR-REST-STATEMENT)
    (121 0 CONDITIONOPT)
    (120 3 INIT-DECLARATOR-LIST);
    (119 0 %CONDITION)
    (119 0 SINGLE-DECLARATION)
    (118 0 FUNCTION-HEADER)
    (117 0 FULLY-SPECIFIED-TYPE)
    (111 0 PARAMETER-TYPE-SPECIFIER)
    (111 0 PARAMETER-DECLARATOR)
    (110 2 STRUCT-DECLARATION);
    (110 3 STRUCT-DECLARATION-LIST);
    (110 1 STRUCT-SPECIFIER);
    (110 2 TYPE-SPECIFIER-NO-PREC);
    (110 1 TYPE-SPECIFIER);
    (98 0 JUMP-STATEMENT)
    (95 1 STRUCT-DECLARATOR-LIST);
    (94 0 EXPRESSION-STATEMENT)
    (94 0 STRUCT-DECLARATOR)
    (93 0 INITIALIZER)
    (93 0 CONSTANT-EXPRESSION)
    (92 4 EXPRESSION);
    (92 3 ASSIGNMENT-EXPRESSION);
    (92 1 CONDITIONAL-EXPRESSION);
    (92 3 LOGICAL-OR-EXPRESSION);
    (92 3 LOGICAL-XOR-EXPRESSION);
    (92 3 LOGICAL-AND-EXPRESSION);
    (92 3 INCLUSIVE-OR-EXPRESSION);
    (92 3 EXCLUSIVE-OR-EXPRESSION);
    (92 3 AND-EXPRESSION);
    (92 4 EQUALITY-EXPRESSION);
    (92 7 RELATIONAL-EXPRESSION);
    (92 7 SHIFT-EXPRESSION);
    (92 5 ADDITIVE-EXPRESSION);
    (92 6 MULTIPLICATIVE-EXPRESSION);
    (92 8 UNARY-EXPRESSION);
    (92 2 FUNCTION-CALL-HEADER-WITH-PARAMETERS);
    (92 1 FUNCTION-CALL-GENERIC);
    (92 1 FUNCTION-CALL);
    (92 1 INTEGER-EXPRESSION);
    (92 5 POSTFIX-EXPRESSION);
    (92 1 PRIMARY-EXPRESSION);
    (23 0 FUNCTION-CALL-HEADER-NO-PARAMETERS)
    (21 0 FUNCTION-CALL-HEADER)
    (19 0 FUNCTION-IDENTIFIER)
    (17 0 CONSTRUCTOR-IDENTIFIER)
    (12 0 ASSIGNMENT-OPERATOR)
    (6 0 TYPE-QUALIFIER)
    (5 0 PARAMETER-QUALIFIER)
    (5 0 UNARY-OPERATOR)
    (4 0 PRECISION-QUALIFIER)
    (2 0 VARIABLE-IDENTIFIER)
    (1 0 NOTHINGEMPTY)
    (1 0 PRECISION)
    (1 0 LOW-PRECISION)
    (1 0 MEDIUM-PRECISION)
    (1 0 HIGH-PRECISION)
    (1 0 INVARIANT)
    (1 0 QUESTION)
    (1 0 AMPERSAND)
    (1 0 CARET)
    (1 0 VERTICAL-BAR)
    (1 0 RIGHT-ANGLE)
    (1 0 LEFT-ANGLE)
    (1 0 PERCENT)
    (1 0 SLASH)
    (1 0 STAR)
    (1 0 PLUS)
    (1 0 TILDE)
    (1 0 DASH)
    (1 0 BANG)
    (1 0 SEMICOLON)
    (1 0 EQUAL)
    (1 0 COLON)
    (1 0 COMMA)
    (1 0 DOT)
    (1 0 RIGHT-BRACE)
    (1 0 LEFT-BRACE)
    (1 0 RIGHT-BRACKET)
    (1 0 LEFT-BRACKET)
    (1 0 RIGHT-PAREN)
    (1 0 LEFT-PAREN)
    (1 0 SUB-ASSIGN)
    (1 0 OR-ASSIGN)
    (1 0 XOR-ASSIGN)
    (1 0 AND-ASSIGN)
    (1 0 RIGHT-ASSIGN)
    (1 0 LEFT-ASSIGN)
    (1 0 MOD-ASSIGN)
    (1 0 ADD-ASSIGN)
    (1 0 DIV-ASSIGN)
    (1 0 MUL-ASSIGN)
    (1 0 XOR-OP)
    (1 0 OR-OP)
    (1 0 AND-OP)
    (1 0 NE-OP)
    (1 0 EQ-OP)
    (1 0 GE-OP)
    (1 0 LE-OP)
    (1 0 DEC-OP)
    (1 0 INC-OP)
    (1 0 RIGHT-OP)
    (1 0 LEFT-OP)
    (1 0 FIELD-SELECTION)
    (1 0 BOOLCONSTANT)
    (1 0 INTCONSTANT)
    (1 0 FLOATCONSTANT)
    (1 0 TYPE-NAME)
    (1 0 IDENTIFIER)
    (1 0 WHILE)
    (1 0 VOID)
    (1 0 STRUCT)
    (1 0 SAMPLERCUBE)
    (1 0 SAMPLER2D)
    (1 0 VARYING)
    (1 0 UNIFORM)
    (1 0 INOUT)
    (1 0 OUT)
    (1 0 IN)
    (1 0 MAT4)
    (1 0 MAT3)
    (1 0 MAT2)
    (1 0 VEC4)
    (1 0 VEC3)
    (1 0 VEC2)
    (1 0 IVEC4)
    (1 0 IVEC3)
    (1 0 IVEC2)
    (1 0 BVEC4)
    (1 0 BVEC3)
    (1 0 BVEC2)
    (1 0 RETURN)
    (1 0 DISCARD)
    (1 0 IF)
    (1 0 FOR)
    (1 0 ELSE)
    (1 0 DO)
    (1 0 CONTINUE)
    (1 0 BREAK)
    (1 0 INT)
    (1 0 FLOAT)
    (1 0 BOOL)
    (1 0 CONST)
    (1 0 ATTRIBUTE)))

(defun walk-the-tree (tree func)
  (if (consp tree)
      (destructuring-bind (root &rest branch) tree
	(declare (ignore root))
	(dolist (b branch)
	  (walk-the-tree b func)))
      (funcall func tree)))

(defun collect-values-flat (tree)
  (let ((ans))
    (flet ((put-leaf (x)
	     (if (symbolp x)
		 (push (gethash x *GLSL-GRAMMAR-RULES-TABLE*) ans)
		 (push x ans))))
      (walk-the-tree tree #'put-leaf))
    (nreverse ans)))

(defun write-to-program (tree)
  (list-of-strings-to-string
   (spacify
    (list-of-stuff-to-list-of-strings
     (collect-values-flat tree)))))

(defparameter *version-string* (let ((ans (string " #version 100  ")))
				 (setf (aref ans 0) #\newline)
				 (setf (aref ans (1- (length ans))) #\newline)
				 (setf (aref ans (- (length ans) 2)) #\newline)
				 ans))
;;;put newlines after semicolons
;;;
(defun spacify (list)
  (let ((ans nil))
    (dolist (val list)
      (cond ((equal val ";")
	     (setf (car ans) +null-string+)
	     (push val ans)
	     (push +newline-string+ ans))
	    (t
	     (push val ans)
	     (push +null-string+ ans))))
    (nreverse ans)))

(defun typelist-eq (a b)
  (do ((aval a (cdr aval))
       (bval b (cdr bval)))
      ((and (null aval) (null bval)) t)
    (unless (eq (car aval) (car bval))
      (return-from typelist-eq nil))))

(defun typelist-eq-other (a b)
  (do ((aval a (cdr aval))
       (bval b (cdr bval)))
      ((and (null aval) (null bval)) t)
    (unless (eq (caar aval) (car bval))
      (return-from typelist-eq-other nil))))

(defun valid-grammar (node)
  (let ((options (gethash (car node) *glsl-grammar-rules-table*))
	(rest (cdr node)))
    (dolist (option options)
      (let ((ans (typelist-eq-other rest option)))
	(when ans (return-from valid-grammar t))))
    nil))

;;;;
;;;;
;;;;

(defun toplevel (translation-unit)
  (list 'toplevel *version-string* translation-unit))

;;(162 1 TRANSLATION-UNIT);just a list of 1 or more external-declarations
(defun %translation-unit (declaration &optional unit)
  (if unit
      `(translation-unit ,unit ,declaration)
      `(translation-unit ,declaration)))

;;convert a list of external declarations into a chain of translation-unit nodes
(defun translation-units (&rest declarations)
  (let ((ans (%translation-unit (pop declarations))))
    (dolist (arg declarations) 
      (setf ans (%translation-unit arg ans)))
    ans))

;;(161 0 EXTERNAL-DECLARATION) ;;function-definition or %declaration
(defun %external-declaration (x)
  `(external-declaration ,x))

;;(160 0 FUNCTION-DEFINITION)
(defun %function-definition (function-prototype compound-statement-no-new-scope)
  `('function-definition ,function-prototype ,compound-statement-no-new-scope))

;;(159 1 ITERATION-STATEMENT)
(defun %iteration-statement (x)
  `(iteration-statement ,x))

;;(159 1 SELECTION-REST-STATEMENT);
(defun %selection-rest-statement (then &optional else)
  (if else
      `(selection-statement ,then 'ELSE ,else)
      `(selection-statement ,then)))

;(159 1 SELECTION-STATEMENT);
(defun %selection-statement (expression selection-rest-statement)
  `(selection-statement IF LEFT-PAREN ,expression RIGHT-PAREN ,selection-rest-statement))

;;(159 3 STATEMENT-LIST);
(defun %statement-list (statement-no-new-scope &optional statement-list)
  (if statement-list
      `(statement-list ,statement-list ,statement-no-new-scope)
      `(statement-list ,statement-no-new-scope)))
(defun statement-lists (&rest statement-no-new-scopes)
  (let ((ans (%statement-list (pop statement-no-new-scopes))))
    (dolist (arg statement-no-new-scopes) 
      (setf ans (%statement-list arg ans)))
    ans))

;;(159 1 COMPOUND-STATEMENT-NO-NEW-SCOPE);
(defun compound-statement-no-new-scope (&optional statement-list)
  (if statement-list
      (list 'compound-statement-no-new-scope 'LEFT-BRACE statement-list 'RIGHT-BRACE)
      (list 'compound-statement-no-new-scope 'LEFT-BRACE 'RIGHT-BRACE)))

;;(159 4 STATEMENT-WITH-SCOPE);
(defun %statement-with-scope (x)
  `(statement-with-scope ,x))

;;(159 1 COMPOUND-STATEMENT-WITH-SCOPE);
(defun compound-statement-with-scope (&optional statement-list)
  (if statement-list
      (list 'compound-statement-with-scope 'LEFT-BRACE statement-list 'RIGHT-BRACE)
      (list 'compound-statement-with-scope 'LEFT-BRACE 'RIGHT-BRACE)))

;;(159 2 SIMPLE-STATEMENT);
(defun %simple-statement (x)
  `(simple-statement ,x))

;;(159 4 STATEMENT-NO-NEW-SCOPE);
(defun %statement-no-new-scope (x)
  `(statement-no-new-scope ,x))

;;(137 0 FOR-INIT-STATEMENT)
(defun %for-init-statement (x)
  `(for-init-statement ,x))

;;(135 0 DECLARATION-STATEMENT)
(defun %declaration-statement (x)
  `(declaration-statement ,x))

;;(134 0 %DECLARATION)
(defun %declaration (x)
  `(declaration ,x))

(defun precision (precision-qualifier type-specifier-no-prec) ;;declare the precision of types
  `(%declaration PRECISION ,precision-qualifier ,type-specifier-no-prec SEMICOLON))

;;(129 0 FUNCTION-PROTOTYPE)
(defun %function-prototype (function-declarator)
  `(function-prototype ,function-declarator RIGHT-PAREN))

;;(128 0 FUNCTION-DECLARATOR)
(defun %function-declarator (x)
  `(function-declarator ,x))

;;(127 1 FUNCTION-HEADER-WITH-PARAMETERS);
(defun %function-header-with-parameters (x)
  `(function-header-with-parameters ,x))

(defun function-header-with-parameters (function-header &rest parameter-declarations)
  (let* ((rev parameter-declarations)
	 (ans `(function-header-with-parameters ,function-header ,(pop rev))))
    (dolist (param rev)
      (setf ans `(function-header-with-parameters ,ans COMMA ,param)))
    ans))

;;(23 0 FUNCTION-CALL-HEADER-NO-PARAMETERS)
(defun %function-call-header-no-parameters (function-call-header)
  `(function-call-header-no-parameters ,function-call-header))

;;(21 0 FUNCTION-CALL-HEADER)
(defun %function-call-header (function-identifier)
  `(function-call-header ,function-identifier LEFT-PAREN))

;;(19 0 FUNCTION-IDENTIFIER)
(defun %function-identifier (x)
  `(%function-identifier ,x))

;;(124 0 PARAMETER-DECLARATION)
(defun %parameter-declaration (parameter-qualifier decl &optional type-qualifier)
  (if type-qualifier
      `(parameter-declaration ,type-qualifier ,parameter-qualifier ,decl)
      `(parameter-declaration ,parameter-qualifier ,decl)))

;;(122 0 FOR-REST-STATEMENT)
(defun %for-rest-statement (conditionopt &optional expression)
  (if expression
      `(for-rest-statement conditionopt SEMICOLON expression)
      `(for-rest-statement conditionopt SEMICOLON)))

;;(121 0 CONDITIONOPT)
(defun %conditionopt (&optional condition)
  (if condition
      `(conditionopt ,condition)
      `(conditionopt NOTHINGEMPTY)))
(coge:progno
 (120 3 INIT-DECLARATOR-LIST);

 (119 0 %CONDITION)
 (119 0 SINGLE-DECLARATION))
;;(118 0 FUNCTION-HEADER)
(defun %function-header (fully-specified-type idenifier)
  `(function-header ,fully-specified-type ,idenifier LEFT-PAREN))

;;(111 0 PARAMETER-TYPE-SPECIFIER)
(defun %parameter-type-specifier (type-specifier &optional constant-expression)
  (if constant-expression
      `(parameter-type-specifier ,type-specifier LEFT-BRACKET ,constant-expression RIGHT-BRACKET)
      `(parameter-type-specifier ,type-specifier)))

;;(111 0 PARAMETER-DECLARATOR)
(defun %parameter-declarator (type-specifier identifier &optional constant-expression)
  (if constant-expression
      `(parameter-declarator ,type-specifier LEFT-BRACKET ,constant-expression RIGHT-BRACKET)
      `(parameter-declarator ,type-specifier)))

(coge:progno
 (110 2 STRUCT-DECLARATION);
 (110 3 STRUCT-DECLARATION-LIST);
 (110 1 STRUCT-SPECIFIER);
 (95 1 STRUCT-DECLARATOR-LIST));

;;(117 0 FULLY-SPECIFIED-TYPE);;qualifier - precision - type
(defun fully-specified-type (qualifier precision type)
  `(fully-specified-type
    (type-qualifier
     ,qualifier)
    (type-specifier
     (precision-qualifier
      ,precision)
     (type-specifier-no-prec
      ,type))))
;;(6 0 TYPE-QUALIFIER) ;;
;;(110 1 TYPE-SPECIFIER) ;;tack on precision or not
(defun type-specifier (precision type)
  (let ((ans (type-specifier-no-prec type)))
    (if precision
	(cons 'type-specifier
	      (list (cons 'precision-qualifier
			  (list precision))
		    ans))
	ans)))
;;(11 0 2 TYPE-SPECIFIER-NO-PREC) ;;the type
(defun type-specifier-no-prec (type)
  `(type-specifier-no-prec ,type))

;;;(98 0 JUMP-STATEMENT)
(defun jump-continue ()
  `(jump-statement CONTINUE SEMICOLON))
(defun jump-break ()
  `(jump-statement BREAK SEMICOLON))
(defun jump-return ()
 `(jump-statement RETURN SEMICOLON))
(defun jump-discard ()
  `(jump-statement DISCARD SEMICOLON))
(defun jump-return-expr (expr)
  `(jump-statement RETURN ,expr SEMICOLON))

;;(94 0 EXPRESSION-STATEMENT)
(defun expression-statement (&optional expr)
  (if expr
      (list 'expression-statement expr 'SEMICOLON)
      (list 'expression-statement 'SEMICOLON)))

(coge:progno
 (94 0 STRUCT-DECLARATOR)
 (93 0 INITIALIZER)
 (93 0 CONSTANT-EXPRESSION)

;;;cyclic bnf chain
 (92 4 EXPRESSION);
 (92 3 ASSIGNMENT-EXPRESSION);
 (92 1 CONDITIONAL-EXPRESSION);
 (92 3 LOGICAL-OR-EXPRESSION);
 (92 3 LOGICAL-XOR-EXPRESSION);
 (92 3 LOGICAL-AND-EXPRESSION);
 (92 3 INCLUSIVE-OR-EXPRESSION);
 (92 3 EXCLUSIVE-OR-EXPRESSION);
 (92 3 AND-EXPRESSION);
 (92 4 EQUALITY-EXPRESSION);
 (92 7 RELATIONAL-EXPRESSION);
 (92 7 SHIFT-EXPRESSION);
 (92 5 ADDITIVE-EXPRESSION);
 (92 6 MULTIPLICATIVE-EXPRESSION);
 (92 8 UNARY-EXPRESSION);
 (92 2 FUNCTION-CALL-HEADER-WITH-PARAMETERS);
 (92 1 FUNCTION-CALL-GENERIC);
 (92 1 FUNCTION-CALL);
 (92 1 INTEGER-EXPRESSION);
 (92 5 POSTFIX-EXPRESSION);
 (92 1 PRIMARY-EXPRESSION);

 (17 0 CONSTRUCTOR-IDENTIFIER)
 (12 0 ASSIGNMENT-OPERATOR)
 (5 0 PARAMETER-QUALIFIER)
 (5 0 UNARY-OPERATOR)
 (4 0 PRECISION-QUALIFIER)
 (2 0 VARIABLE-IDENTIFIER))
