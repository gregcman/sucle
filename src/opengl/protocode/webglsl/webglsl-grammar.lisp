(in-package :webglsl)

(defparameter *glsl-grammar-rules*
  '((variable-identifier
     ((IDENTIFIER)))
    (primary-expression
     ((variable-identifier)
      (INTCONSTANT)
      (FLOATCONSTANT)
      (BOOLCONSTANT)
      (LEFT-PAREN expression RIGHT-PAREN)))
    (postfix-expression
     ((primary-expression)
      (postfix-expression LEFT-BRACKET integer-expression RIGHT-BRACKET)
      (function-call)
      (postfix-expression DOT FIELD-SELECTION)
      (postfix-expression INC-OP)
      (postfix-expression DEC-OP)))
    (integer-expression
     ((expression)))


    ;;;;the only rule seen beyond this block is "function call"
    (function-call
     ((function-call-generic)))
    (function-call-generic
     ((function-call-header-with-parameters RIGHT-PAREN)
      (function-call-header-no-parameters RIGHT-PAREN)))
    (function-call-header-no-parameters
     ((function-call-header VOID)
      (function-call-header)))
    (function-call-header-with-parameters
     ((function-call-header assignment-expression)
      (function-call-header-with-parameters COMMA assignment-expression)))
    (function-call-header
     ((function-identifier LEFT-PAREN)))
    
    (function-identifier
     ((constructor-identifier)
      (IDENTIFIER)))
    (constructor-identifier
     ((FLOAT)
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
    
    (unary-expression
     ((postfix-expression)
      (INC-OP unary-expression)
      (DEC-OP unary-expression)
      (unary-operator unary-expression)))
    (unary-operator
     ((PLUS)
      (DASH)
      (BANG)
      (TILDE)))
    (multiplicative-expression
     ((unary-expression)
      (multiplicative-expression STAR unary-expression)
      (multiplicative-expression SLASH unary-expression)
      (multiplicative-expression PERCENT unary-expression)))
    (additive-expression
     ((multiplicative-expression)
      (additive-expression PLUS multiplicative-expression)
      (additive-expression DASH multiplicative-expression)))
    (shift-expression
     ((additive-expression)
      (shift-expression LEFT-OP additive-expression)
      (shift-expression RIGHT-OP additive-expression)))
    (relational-expression
     ((shift-expression)
      (relational-expression LEFT-ANGLE shift-expression)
      (relational-expression RIGHT-ANGLE shift-expression)
      (relational-expression LE-OP shift-expression)
      (relational-expression GE-OP shift-expression)))
    (equality-expression
     ((relational-expression)
      (equality-expression EQ-OP relational-expression)
      (equality-expression NE-OP relational-expression)))
    (and-expression
     ((equality-expression)
      (and-expression AMPERSAND equality-expression)))
    (exclusive-or-expression
     ((and-expression)
      (exclusive-or-expression CARET and-expression)))
    (inclusive-or-expression
     ((exclusive-or-expression)
      (inclusive-or-expression VERTICAL-BAR exclusive-or-expression)))
    (logical-and-expression
     ((inclusive-or-expression)
      (logical-and-expression AND-OP inclusive-or-expression)))
    (logical-xor-expression
     ((logical-and-expression)
      (logical-xor-e xpression XOR-OP logical-and-expression)))
    (logical-or-expression
     ((logical-xor-expression)
      (logical-or-expression OR-OP logical-xor-expression)))
    (conditional-expression
     ((logical-or-expression)
      (logical-or-expression QUESTION expression COLON assignment-operator)))
    (assignment-expression
     ((conditional-expression)
      (unary-expression assignment-operator assignment-expression)))
    (assignment-operator
     ((EQUAL)
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
    (expression
     ((assignment-operator)
      (expression COMMA assignment-expression)))
    (constant-expression
     ((conditional-expression)))
    
    (%declaration
     ((function-prototype SEMICOLON)
      (init-declarator-list SEMICOLON)
      (PRECISION precision-qualifier type-specifier-no-prec SEMICOLON)))
    
    (function-prototype
     ((function-declarator RIGHT-PAREN)))
    (function-declarator
     ((function-header)
      (function-header-with-parameters)))
    (function-header-with-parameters ;;recursive
     ((function-header parameter-declaration)
      (function-header-with-parameters COMMA parameter-declaration)))
    (function-header
     ((fully-specified-type IDENTIFIER LEFT-PAREN)))
    (parameter-declarator
     ((type-specifier IDENTIFIER)
      (type-specifier IDENTIFIER LEFT-BRACKET constant-expression RIGHT-BRACKET)))
    (parameter-declaration
     ((type-qualifier parameter-qualifier parameter-declarator)
      (parameter-qualifier parameter-declarator)
      (type-qualifier parameter-qualifier parameter-type-specifier)
      (parameter-qualifier parameter-type-specifier)))
    (parameter-qualifier
     ((NOTHINGEMPTY)
      (IN)
      (OUT)
      (INOUT)))
    (parameter-type-specifier
     ((type-specifier)
      (type-specifier LEFT-BRACKET constant-expression RIGHT-BRACKET)))
    
    (init-declarator-list
     ((single-declaration)
      (init-declarator-list COMMA IDENTIFIER)
      (init-declarator-list COMMA IDENTIFIER LEFT-BRACKET constant-expression RIGHT-BRACKET)
      (init-declarator-list COMMA IDENTIFIER EQUAL initializer)))
    (single-declaration
     ((fully-specified-type)
      (fully-specified-type IDENTIFIER)
      (fully-specified-type IDENTIFIER LEFT-BRACKET constant-expression RIGHT-BRACKET)
      (fully-specified-type IDENTIFIER EQUAL initializer)
      (INVARIANT IDENTIFIER)))
    
    (fully-specified-type
     ((type-specifier)
      (type-qualifier type-specifier)))
    (type-qualifier
     ((CONST)
      (ATTRIBUTE)
      (VARYING)
      (INVARIANT VARYING)
      (UNIFORM)))
    (type-specifier
     ((type-specifier-no-prec)
      (precision-qualifier type-specifier-no-prec)))
    (type-specifier-no-prec
     ((VOID)
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
    (precision-qualifier
     ((HIGH-PRECISION)
      (MEDIUM-PRECISION)
      (LOW-PRECISION)))
    
    (struct-specifier
     ((STRUCT IDENTIFIER LEFT-BRACE struct-declaration-list RIGHT-BRACE)
      (STRUCT LEFT-BRACE struct-declaration-list RIGHT-BRACE)))
    (struct-declaration-list
     ((struct-declaration)
      (struct-declaration-list struct-declaration)))
    (struct-declaration
     ((type-specifier struct-declarator-list SEMICOLON)))
    (struct-declarator-list
     ((struct-declarator)
      (struct-declarator-list COMMA struct-declarator)))
    (struct-declarator
     ((IDENTIFIER)
      (IDENTIFIER LEFT-BRACKET constant-expression RIGHT-BRACKET)))
    
    (initializer
     ((assignment-expression)))
    (declaration-statement
     ((%declaration)))
    (statement-no-new-scope
     ((compound-statement-with-scope)
      (simple-statement)))
    (simple-statement
     ((declaration-statement)
      (expression-statement)
      (selection-statement)
      (iteration-statement)
      (jump-statement)))
    (compound-statement-with-scope
     ((LEFT-BRACE RIGHT-BRACE)
      (LEFT-BRACE statement-list RIGHT-BRACE)))
    (statement-with-scope
     ((compound-statement-no-new-scope)
      (simple-statement)))
    (compound-statement-no-new-scope
     ((LEFT-BRACE RIGHT-BRACE)
      (LEFT-BRACE statement-list RIGHT-BRACE)))
    (statement-list 
     ((statement-no-new-scope)
      (statement-list statement-no-new-scope)))
    (expression-statement
     ((SEMICOLON)
      (expression SEMICOLON)))
    (selection-statement
     ((IF LEFT-PAREN expression RIGHT-PAREN selection-rest-statement)))
    (selection-rest-statement
     ((statement-with-scope ELSE statement-with-scope)
      (statement-with-scope)))
    (%condition
     ((expression)
      (fully-specified-type IDENTIFIER EQUAL initializer)))
    (iteration-statement
     ((WHILE LEFT-PAREN %condition RIGHT-PAREN statement-no-new-scope)
      (DO statement-with-scope WHILE LEFT-PAREN expression RIGHT-PAREN SEMICOLON)
      (FOR LEFT-PAREN for-init-statement for-rest-statement RIGHT-PAREN statement-no-new-scope)))
    (for-init-statement
     ((expression-statement)
      (declaration-statement)))
    (conditionopt
     ((%condition)
      (NOTHINGEMPTY)))
    (for-rest-statement
     ((conditionopt SEMICOLON)
      (conditionopt SEMICOLON expression)))
    (jump-statement
     ((CONTINUE SEMICOLON)
      (BREAK SEMICOLON)
      (RETURN SEMICOLON)
      (RETURN expression SEMICOLON)
      (DISCARD SEMICOLON)))
    (translation-unit
     ((external-declaration)
      (translation-unit external-declaration)))
    (external-declaration
     ((function-definition)
      (%declaration)))
    (function-definition
     ((function-prototype compound-statement-no-new-scope)))))

(defparameter *token-strings*
  '((ATTRIBUTE "attribute") (CONST "const") (BOOL "bool") (FLOAT "float") (INT "int")
    (BREAK "break") (CONTINUE "continue") (DO "do") (ELSE "else")
    (FOR "for") (IF "if") (DISCARD "discard") (RETURN "return")
    (BVEC2 "bvec2") (BVEC3 "bvec3") (BVEC4 "bvec4")
    (IVEC2 "ivec2") (IVEC3 "ivec3") (IVEC4 "ivec4")
    (VEC2 "vec2") (VEC3 "vec3") (VEC4 "vec4")
    (MAT2 "mat2") (MAT3 "mat3") (MAT4 "mat4")
    (IN "in") (OUT "out") (INOUT "inout")
    (UNIFORM "uniform") (VARYING "varying")
    (SAMPLER2D "sampler2D") (SAMPLERCUBE "sampler2D")
    (STRUCT "struct") (VOID "void") (WHILE "while")
    (IDENTIFIER t)
    (TYPE-NAME t)
    (FLOATCONSTANT t) (INTCONSTANT t) (BOOLCONSTANT t)
    (FIELD-SELECTION t) ;;swizzling
    (LEFT-OP "<<") (RIGHT-OP ">>") (INC-OP "++")
    (DEC-OP "--") (LE-OP "<=") (GE-OP ">=") (EQ-OP "==") (NE-OP "!=")
    (AND-OP "&&") (OR-OP "||") (XOR-OP "^^")
    (MUL-ASSIGN "*=") (DIV-ASSIGN "/=") (ADD-ASSIGN "+=")
    (MOD-ASSIGN "%=") (LEFT-ASSIGN "<<=") (RIGHT-ASSIGN ">>=")
    (AND-ASSIGN "&=") (XOR-ASSIGN "^=") (OR-ASSIGN "|=")
    (SUB-ASSIGN "-=")
    (LEFT-PAREN "(") (RIGHT-PAREN ")") (LEFT-BRACKET "[") (RIGHT-BRACKET "]")
    (LEFT-BRACE "{") (RIGHT-BRACE "}") (DOT ".") (COMMA ",") (COLON ":")
    (EQUAL "=") (SEMICOLON ";") (BANG "!") (DASH "-") (TILDE "~") (PLUS "+")
    (STAR "*") (SLASH "/")
    (PERCENT "%") (LEFT-ANGLE "<") (RIGHT-ANGLE ">") (VERTICAL-BAR "|")
    (CARET "^") (AMPERSAND "&") (QUESTION "?") (INVARIANT "invariant")
    (HIGH-PRECISION "highp") (MEDIUM-PRECISION "mediump") (LOW-PRECISION "lowp")
    (PRECISION "precision")

    (NOTHINGEMPTY "/* empty */")))

(defparameter *glsl-grammar-rules-table*
  (let ((table (make-hash-table :test 'eq)))
    (dolist (rule *glsl-grammar-rules*)
      (destructuring-bind (name substitutions) rule
	(setf (gethash name table) substitutions)))
    (dolist (token *token-strings*)
      (destructuring-bind (name substitution) token
	(setf (gethash name table) substitution)))
    table))
