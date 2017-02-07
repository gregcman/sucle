(defpackage #:webglsl
  (:use :cl))

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

(defstruct (op (:constructor make-op (operator precedence associativity))
	       (:print-object printop))
  operator
  precedence
  associativity)

(defun printop (op stream)
  (format stream "~a" (op-operator op)))

;;:l is left to right :r is right to left nil is NA
(defparameter *operators*
  (let ((tot nil)
	(ops
	 '(
	   ;(1 nil "(" ")") ;not operators?
	   (2 :l "[]" ;;binop
	    "()" ;;variadic
	    "." ;;binop -infix
	    "post++" "post--") ;;unary
	   (3 :r "++pre" "--pre" "+unary" "-unary" "!"
	    "~") ;;reserved
	   
	   (4 :l "*" "/"
	    "%") ;;reserved
	   (5 :l "+" "-")
	   (6 :l "<<" ">>") ;;reserved
	   (7 :l "<" ">" "<=" ">=")
	   (8 :l "==" "!=")
	   (9 :l "&") ;;reserved
	   (10 :l "^") ;;reserved
	   (11 :l "|") ;;reserved
	   (12 :l "&&")
	   (13 :l "^^")
	   (14 :l "||")
	   (15 :r "?:") ;;not a binop
	   (16 :r "=" "+=" "-=" "*=" "/="
	    "%=" "<<=" ">>=" "&=" "^=" "|=");;reserved
	   (17 :l ",")
	   (31 nil nil))))
    (dolist (zopz ops)
      (destructuring-bind (precedence associativity &rest thops) zopz
	  (dolist (op thops)
	    (push (make-op op precedence associativity) tot))))
    tot))

(defparameter *ophash*
  (let ((table (make-hash-table :test 'equal)))
    (dolist (op *operators*)
      (setf (gethash (op-operator op) table) op))
    table))

(defun walk-expr (ast)
  (labels ((walk (tree left right)
	     (if (consp tree)
		 (destructuring-bind (currop l r) tree
		   (let ((curr (gethash currop *ophash*)))
		     ;(print (list left curr right))
		     (let ((parensp (parensp curr left right)))
		       ;(print parensp)
		       (list (when parensp t) curr
			     (walk l left curr)
			     (walk r curr right)))))
		 tree))
	   (parensp (curr left-info right-info)
	     (let ((curr-prec (op-precedence curr)))
	       (let ((left-prec (op-precedence left-info)))
		 (if (< left-prec curr-prec)
		     (return-from parensp 0)
		     (when (and (= left-prec curr-prec)
				(eq (op-associativity left-info) :l))
		       (return-from parensp 1))))
	       (let ((right-prec (op-precedence right-info)))
		 (if (< right-prec curr-prec)
		     (return-from parensp 2)
		     (when (and (= right-prec curr-prec)
				(eq (op-associativity right-info) :r))
		       (return-from parensp 3)))))))
    (walk ast (gethash nil *ophash*) (gethash nil *ophash*))))

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

(defparameter variable-identifier
  '((IDENTIFIER)))
(defparameter primary-expression
  '((variable-identifier)
    (INTCONSTANT)
    (FLOATCONSTANT)
    (BOOLCONSTANT)
    (LEFT-PAREN expression RIGHT-PAREN)))
(defparameter postfix-expression
  '((primary-expression)
    (postfix-expression LEFT-BRACKET integer-expression RIGHT-BRACKET)
    (function-call)
    (postfix-expression DOT FIELD-SELECTION)
    (postfix-expression INC-OP)
    (postfix-expression DEC-OP)))
(defparameter integer-expression
  '((expression)))
(defparameter function-call
  '((function-call-generic)))
(defparameter function-call-generic
  '((function-call-header-with-parameters RIGHT-PAREN)
    (function-call-header-no-parameters LEFT-PAREN)))
(defparameter function-call-header-no-parameters
  '((function-call-header VOID)
    (function-call-header)))
(defparameter function-call-header-with-parameters
  '((function-call-header assignment-expression)
    (function-call-header-with-parameters COMMA assignment-operator)))
(defparameter function-call-header
  '((function-identifier LEFT-PAREN)))
(defparameter function-identifier
  '((constructor-identifier)
    (IDENTIFIER)))
(defparameter constructor-identifier
  '((FLOAT)
    (INT)
    (BOOL)
    (VEC2)
    (VEC3)
    (VEC4)
    (BVEC2)
    (BVEC3)
    (BVEC4)
    (IVEC2)
    (IVEC3)
    (IVEC4)
    (MAT2)
    (MAT3)
    (MAT4)
    (TYPE-NAME)))
(defparameter unary-expression
  '((postfix-expression)
    (INC-OP unary-expression)
    (DEC-OP unary-expression)
    (unary-operator unary-expression)))
(defparameter unary-operator
  '((PLUS)
    (DASH)
    (BANG)
    (TILDE)))
(defparameter multiplicative-expression
  '((unary-expression)
    (multiplicative-expression STAR unary-expression)
    (multiplicative-expression SLASH unary-expression)
    (multiplicative-expression PERCENT unary-expression)))
(defparameter additive-expression
  '((multiplicative-expression)
    (additive-expression PLUS multiplicative-expression)
    (additive-expression DASH multiplicative-expression)))
(defparameter shift-expression
  '((additive-expression)
    (shift-expression LEFT-OP additive-expression)
    (shift-expression RIGHT-OP additive-expression)))
(defparameter relational-expression
  '((shift-expression)
    (relational-expression LEFT-ANGLE shift-expression)
    (relational-expression RIGHT-ANGLE shift-expression)
    (relational-expression LE-OP shift-expression)
    (relational-expression GE-OP shift-expression)))
(defparameter equality-expression
  '((relational-expression)
    (equality-expression EQ-OP relational-expression)
    (equality-expression NE-OP relational-expression)))
(defparameter and-expression
  '((equality-expression)
    (and-expression AMPERSAND equality-expression)))
(defparameter exclusive-or-expression
  '((and-expression)
    (exclusive-or-expression CARET and-expression)))
(defparameter inclusive-or-expression
  '((exclusive-or-expression)
    (inclusive-or-expression VERTICAL-BAR exclusive-or-expression)))
(defparameter logical-and-expression
  '((inclusive-or-expression)
    (logical-and-expression AND-OP inclusive-or-expression)))
(defparameter logical-xor-expression
  '((logical-and-expression)
    (logical-xor-expression XOR-OP logical-and-expression)))
(defparameter logical-or-expression
  '((logical-xor-expression)
    (logical-or-expression OR-OP logical-xor-expression)))
(defparameter conditional-expression
  '((logical-or-expression)
    (logical-or-expression QUESTION expression COLON assignment-operator)))
(defparameter assignment-expression
  '((conditional-expression)
    (unary-expression assignment-operator assignment-expression)))
(defparameter assignment-operator
  '((EQUAL)
    (MUL-ASSIGN)
    (DIV-ASSIGN)
    (MOD-ASSIGN)
    (ADD-ASSIGN)
    (SUB-ASSIGN)
    (LEFT-ASSIGN)
    (RIGHT-ASSIGN)
    (AND-ASSIGN)
    (XOR-ASSIGN)
    (OR-ASSIGN)))
(defparameter expression
  '((assignment-operator)
    (expression COMMA assignment-expression)))
(defparameter constant-expression
  '((conditional-expression)))
(defparameter declaration
  '((function-prototype SEMICOLON)
    (init-declarator-list SEMICOLON)
    (PRECISION precision-qualifier type-specifier-no-prec SEMICOLON)))
(defparameter function-prototype
  '((function-declarator RIGHT-PAREN)))
(defparameter function-declarator
  '((function-header)
    (function-header-with-parameters)))
(defparameter function-header-with-parameters
  '((function-header parameter-declaration)
    (function-header-with-parameters COMMA parameter-declaration)))
(defparameter function-header
  '((fully-specified-type IDENTIFIER LEFT-PAREN)))
(defparameter parameter-declarator
  '((type-specifier IDENTIFIER)
    (type-specifier IDENTIFIER LEFT-BRACKET constant-expression RIGHT-BRACKET)))
(defparameter parameter-declaration
  '((type-qualifier parameter-qualifier parameter-declarator)
    (parameter-qualifier parameter-declarator)
    (type-qualifier parameter-qualifier parameter-type-specifier)
    (parameter-qualifier parameter-type-specifier)))
(defparameter parameter-qualifier
  '(("/* empty */")
    (IN)
    (OUT)
    (INOUT)))
(defparameter parameter-type-specifier
  '((type-specifier)
    (type-specifier LEFT-BRACKET constant-expression RIGHT-BRACKET)))
(defparameter init-declarator-list
  '((single-declaration)
    (init-declarator-list COMMA IDENTIFIER)
    (init-declarator-list COMMA IDENTIFIER LEFT-BRACKET constant-expression RIGHT-BRACKET)
    (init-declarator-list COMMA IDENTIFIER EQUAL initializer)))
(defparameter single-declaration
  '((fully-specified-type)
    (fully-specified-type IDENTIFIER)
    (fully-specified-type IDENTIFIER LEFT-BRACKET constant-expression RIGHT-BRACKET)
    (fully-specified-type IDENTIFIER EQUAL initializer)
    (INVARIANT IDENTIFIER)))
(defparameter fully-specified-type
  '((type-specifier)
    (type-qualifier type-specifier)))
(defparameter type-qualifier
  '((CONST)
    (ATTRIBUTE)
    (VARYING)
    (INVARIANT VARYING)
    (UNIFORM)))
(defparameter type-specifier
  '((type-specifier-no-prec)
    (precision-qualifier type-specifier-no-prec)))
(defparameter type-specifier-no-prec
  '((VOID)
    (FLOAT)
    (INT)
    (BOOL)
    (VEC2)
    (VEC3)
    (VEC4)
    (BVEC2)
    (BVEC3)
    (BVEC4)
    (IVEC2)
    (IVEC3)
    (IVEC4)
    (MAT2)
    (MAT3)
    (MAT4)
    (SAMPLER2D)
    (SAMPLERCUBE)
    (struct-specifier)
    (TYPE-NAME)))
(defparameter precision-qualifier
  '((HIGH-PRECISION)
    (MEDIUM-PRECISION)
    (LOW-PRECISION)))
(defparameter struct-specifier
  '((STRUCT IDENTIFIER LEFT-BRACE struct-declaration-list RIGHT-BRACE)
    (STRUCT LEFT-BRACE struct-declaration-list RIGHT-BRACE)))
(defparameter struct-declaration-list
  '((struct-declaration)
    (struct_declaration_list struct_declaration)))
(defparameter struct-declaration
  '((type-specifier struct-declarator-list SEMICOLON)))
(defparameter struct-declarator-list
  '((struct-declarator)
    (struct-declarator-list COMMA struct-declarator)))
(defparameter struct-declarator
  '((IDENTIFIER)
    (IDENTIFIER LEFT-BRACKET constant-expression RIGHT-BRACKET)))
(defparameter initializer
  '((assignment-expression)))
(defparameter declaration-statement
  '((declaration)))
(defparameter statement-no-new-scope
  '((compound-statement-with-scope)
    (simple-statement)))
(defparameter simple-statement
  '((declaration-statement)
    (expression-statement)
    (selection-statement)
    (iteration-statement)
    (jump-statement)))
(defparameter compound-statement-with-scope
  '((LEFT-BRACE RIGHT-BRACE)
    (LEFT-BRACE statement-list RIGHT-BRACE)))
(defparameter statement-with-scope
  '((compound-statement-no-new-scope)
    (simple-statement)))
(defparameter compound-statement-no-new-scope
  '((LEFT-BRACE RIGHT-BRACE)
    (LEFT-BRACE statement-list RIGHT-BRACE)))
(defparameter statement-list
  '((statement-no-new-scope)
    (statement-list statement-no-new-scope)))
(defparameter expression-statement
  '((SEMICOLON)
    (expression SEMICOLON)))
(defparameter selection-statement
  '((IF LEFT-PAREN expression RIGHT-PAREN selection-rest-statement)))
(defparameter selection-rest-statement
  '((statement-with-scope ELSE statement-with-scope)
    (statement-with-scope)))
(defparameter condition
  '((expression)
    (fully-specified-type IDENTIFIER EQUAL initializer)))
(defparameter iteration-statement
  '((WHILE LEFT-PAREN condition RIGHT-PAREN statement-no-new-scope)
    (DO statement-with-scope WHILE LEFT-PAREN expression RIGHT-PAREN SEMICOLON)
    (FOR LEFT-PAREN for-init-statement for-rest-statement RIGHT-PAREN statement-no-new-scope)))
(defparameter for-init-statement
  '((expression-statement)
    (declaration-statement)))
(defparameter conditionopt
  '((condition)
    ("/* empty */")))
(defparameter for-rest-statement
  '((conditionopt SEMICOLON)
    (conditionopt SEMICOLON expression)))
(defparameter jump-statement
  '((CONTINUE SEMICOLON)
    (BREAK SEMICOLON)
    (RETURN SEMICOLON)
    (RETURN expression SEMICOLON)
    (DISCARD SEMICOLON)))
(defparameter translation-unit
  '((external-declaration)
    (translation-unit external-declaration)))
(defparameter external-declaration
  '((function-definition)
    (declaration)))
(defparameter function-definition
  '((function-prototype compound-statement-no-new-scope)))
