;; A small self applicable scheme->llvm compiler.
;; ,  Tobias Nurmiranta
;;
;; -- To Use It --
;; Reads scheme-code from standard input.
;; cat code.ss|mzscheme --script compile.ss|llvm-as -f -o=test.bc;lli test.bc
;;
;;
;; mzscheme --script compile.ss.heap|llvm-as|opt -lowergc -f -o=test2.bc|llvm-link test2.bc ~/llvm/llvm/runtime/GC/SemiSpace/BytecodeObj/semispace.bc -f -o=test3.bc
;;
;;
;; The compiler is painfully slow since it for testing purposes
;; compiles all help functions as well (see variable bootstrap). 
;; It extends standard scheme with a subset of available
;; llvm-instructions, and adds the new special-form llvm-define.
;;
;; - llvm-define -
;; llvm-define defines a llvm-function, which parameters are the
;; llvm-function's parameters, so it doesn't use an environment for
;; variable lookup. llvm-define's are used to implement basic
;; functionality of the language, and can only call other llvm-defined
;; functions.
;;
;; -- Example of Self Application --
;; bash-2.05b$ mzscheme --script compile.ss > hello.ll
;; (display "you can't h4xx0r a cake if you not kr4xx0r an egg")
;; bash-2.05b$ cat compile.ss|mzscheme --script compile.ss|llvm-as -o=ccomp.bc
;; bash-2.05b$ lli ccomp.bc > hello2.ll                                
;; (display "you can't h4xx0r a cake if you not kr4xx0r an egg")       
;; bash-2.05b$ llvm-as hello2.ll
;; bash-2.05b$ lli hello2.bc
;; you can't h4xx0r a cake if you not kr4xx0r an egg
;; bash-2.05b$ diff hello.ll hello2.ll
;; bash-2.05b$
;;
;; -- Implemented Types --
;; 30 bit immediate fixnums (also used as chars)
;; symbols
;; strings
;; vectors (which are also used as conscells)
;; functions (fixed and arbitrary number of arguments)
;;
;; All objects are represented with a 32 bit uint, with 2 bits reserved 
;; for a type tag.
;;
;; -- The Implementation --

(begin
  
(define (error func str)
  (display func) (display " ") 
  (display str) (newline))

;; Abstract syntax

(define (tagged-list? exp tag) (if (pair? exp) (eq? (car exp) tag) (= 1 0)))
(define (self-evaluating? exp) (or (number? exp) (string? exp)))
(define (variable? exp) (symbol? exp))
(define (assignment? exp) (tagged-list? exp 'set!))
(define (definition? exp) (tagged-list? exp 'define))
(define (if? exp) (tagged-list? exp 'if))
(define (cond? exp) (tagged-list? exp 'cond))
(define (lambda? exp) (tagged-list? exp 'lambda))
(define (begin? exp) (tagged-list? exp 'begin))
(define (quote? exp) (tagged-list? exp 'quote))
(define (let? exp) (tagged-list? exp 'let))
(define (application? exp) (pair? exp))

(define (llvm-definition? exp) (tagged-list? exp 'llvm-define))
(define (llvm-instruction? exp) (assoc (operator exp) llvm-instructions))

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (first-arg exp) (cadr exp))
(define (second-arg exp) (caddr exp))
(define (third-arg exp) (cadddr exp))

(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))

(define (text-of-quotation exp) (cadr exp))

(define (definition-variable exp)
  (if (symbol? (cadr exp)) (cadr exp) (caadr exp)))
(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp) (cons 'lambda (cons (cdadr exp) (cddr exp)))))

(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp) (if (not (null? (cdddr exp))) (cadddr exp) 0))
(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

(define (lambda-parameters exp) 
  (define (fix-list lst)
    (cond ((not (pair? lst)) (cons lst '()))
          (else (cons (car lst) (fix-list (cdr lst))))))
  (if (list? (cadr exp)) (cadr exp) (fix-list (cadr exp))))
(define (lambda-arbitrary-args? exp) (not (list? (cadr exp))))
(define (lambda-body exp) (cddr exp))

;; Code transformation

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (car seq))
        (else (make-begin seq))))
(define (make-begin seq) (cons 'begin seq))

(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause) (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))
(define (cond->if exp) (expand-clauses (cond-clauses exp)))
(define (expand-clauses clauses)
  (if (null? clauses) 0
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest) (sequence->exp (cond-actions first))
                (error 'expand-clauses "else clause not last"))
            (make-if (cond-predicate first)
                     (sequence->exp (cond-actions first))
                     (expand-clauses rest))))))

(define (let-vars exp) (map (lambda (x) (car x)) (cadr exp)))
(define (let-vals exp) (map (lambda (x) (cadr x)) (cadr exp)))
(define (let-body exp) (cddr exp))
(define (let->lambda exp)
  (cons (cons 'lambda (cons (let-vars exp) (let-body exp))) (let-vals exp)))

;; Code constructors

(define (append-code2 instrs)
    (cond ((null? instrs) instrs)
          ((null? (car instrs))
           (append-code2 (cdr instrs)))
          ((pair? (car instrs))
           (append (car instrs) (append-code2 (cdr instrs))))
          (else (cons (car instrs) (append-code2 (cdr instrs))))))
(define (append-code . instrs) (append-code2 instrs)) 
(define (make-code target . instrs) (cons target (append-code2 instrs)))

;; Generate variables, labels, string constants and functions.

(define c
  (lambda strs
    (define (str-app str1 rest)
      (if (null? rest) str1
          (string-append str1 (str-app (car rest) (cdr rest)))))
    (str-app (car strs) (cdr strs))))
;(define c string-append) 

(define (init-generators)
  (set! var-counter 0)
  (set! label-counter 0)
  (set! function-counter 0)
  (set! llvm-function-list '())
  (set! llvm-primitive-functions ;; functions implemented in llvm assembler. 
        '(llvm-read-char 
          print-number print-string/symbol
          allocate-object
          object-size
          object-ref object-set!
          allocate-string/symbol
          string-ref string-set!
          mem-cpy
          apply-procedure
          number? vector? string/symbol? procedure? null?
          make-null make-true
          exit))
  (set! llvm-string-list '()))

(define var-counter 0)
(define (make-reg)
  (set! var-counter (+ 1 var-counter))
  (list 'reg (c "%r" (number->string var-counter))))

(define (stack-reg? exp) (or (tagged-list? exp 'stack-reg) (tagged-list? exp 'stack-reg-not-used)))
(define (stack-reg-used? exp) (tagged-list? exp 'stack-reg))
(define (set-stack-reg-used! sreg) (set-car! sreg 'stack-reg))
(define (construct-stack-reg reg-name) (list 'stack-reg reg-name))

(define (convert-to-stack-reg exp) (list 'convert-to-stack-reg exp))
(define (convert-to-stack-reg? exp) (tagged-list? exp 'convert-to-stack-reg))
(define (convert-to-stack-reg-body exp) (cadr exp))

(define (make-stack-reg)
  (set! var-counter (+ 1 var-counter))
  (list 'stack-reg-not-used (c "%r" (number->string var-counter))))

(define label-counter 0)
(define (make-label)
  (set! label-counter (+ 1 label-counter))
  (c "label" (number->string label-counter)))

(define function-counter 0)
(define (make-function-name)
  (set! function-counter (+ 1 function-counter))
  (c "%function" (number->string function-counter)))

(define llvm-primitive-functions '())
(define (add-llvm-function-name f-name)
  (set! llvm-primitive-functions (cons f-name llvm-primitive-functions)))

(define llvm-function-list '())
(define (add-llvm-function f-name f-params f-body f-target)
  (define (build-params params)
    (if (null? params) ""
        (c "uint* " (llvm-repr (car params))
           (if (null? (cdr params)) "" ", ")
           (build-params (cdr params)))))
  (set! llvm-function-list
	(append 
         llvm-function-list
         (list (append-code 
                (c "uint " (llvm-repr f-name) "(" (build-params f-params) ") {")
                f-body
                (llvm-ret f-target)
                (c "}"))))))

(define (fix-string-format str) ;; some extra work since we have no char-type.
  (define (str-ref-int str pos) (char->integer (string-ref str pos)))
  (define (esc-char hex1 hex2 rest)
    (cons (integer->char 92) 
          (cons (integer->char hex1) (cons (integer->char hex2) rest))))
  (define (fix-str-format str pos end)
    (cond ((= pos end) '())
          ((eq? (str-ref-int str pos) 34) 
           (esc-char 50 50 (fix-str-format str (+ pos 1) end)))
          ((eq? (str-ref-int str pos) 92)
           (esc-char 53 67 (fix-str-format str (+ pos 1) end)))
          (else (cons (string-ref str pos) 
                      (fix-str-format str (+ pos 1) end)))))
  (list->string (fix-str-format str 0 (string-length str))))

(define llvm-string-list '())
(define (add-llvm-string target str)
  (let ((str-type (c "[" (number->string (+ (string-length str) 1)) " x sbyte]")))
    (set! llvm-string-list 
          (cons (c (llvm-repr target) " = internal constant " str-type 
                      " c\"" (fix-string-format str) "\\00\"")
                llvm-string-list))
    (c str-type "*")))

;; Lexical addressing.

(define (extend-c-t-env params c-t-env) (cons params c-t-env))
(define (current-c-t-env c-t-env) (car c-t-env))
(define (outer-c-t-env c-t-env) (cdr c-t-env))

(define (find-var var c-t-env scope)
  (define (find-index var env index)
    (cond ((null? env) '())
	  ((eq? (car env) var) index)
	  (else (find-index var (cdr env) (+ 1 index)))))
  (if (null? c-t-env) '()
      (let ((index (find-index var (current-c-t-env c-t-env) 1)))
	(if (null? index)
	    (find-var var (outer-c-t-env c-t-env) (+ 1 scope))
	    (cons scope index)))))
 
;; LLVM primitives.

(define llvm-instructions
  '(;; binary operations.
    (add . "add") (sub . "sub") (mul . "mul") (div . "div") (rem . "rem")    
    ;; binary bit operations.
    (bit-and . "and") (bit-or . "or") (bit-xor . "xor") 
    (bit-shl . "shl") (bit-shr . "shr")
    ;; boolean operations.
    (seteq . "seteq") (setne . "setne") (setlt . "setlt") (setgt . "setgt")
    (setle . "setle") (setge . "setge")))

(define llvm-boolean-instructions '(seteq setne setlt setgt setle setge))
(define llvm-shift-instructions '(bit-shl bit-shr))
(define (llvm-instr-name op) (cdr (assoc op llvm-instructions)))

(define (llvm-repr exp)
  (cond ((number? exp) (number->string (* 4 exp)))
        ((symbol? exp) (c "\"%" (symbol->string exp) "\""))
        ((list? exp) (cadr exp))
        (else exp)))

(define (llvm-load target var) (lc target " = load uint* " (llvm-repr var)))
(define (llvm-store target value) (lc "store uint " value ", uint* " (llvm-repr target)))
(define (llvm-alloca-var target) (c (llvm-repr target) " = alloca uint"))

(define (llvm-init-stack-reg reg)
  (let ((t1 (make-reg)))
    (set-stack-reg-used! reg)
    (append-code
     (llvm-alloca-var reg)
     (llvm-cast t1 "uint*" (llvm-repr reg) "sbyte**")
     (c "call void %llvm.gcroot(sbyte** " (llvm-repr t1) ", sbyte* null)"))))

(define (lc2 instruction)  
 (define (llvm-loader code load-code res-instruction)
    (cond ((null? code) (append-code load-code res-instruction))
          ((stack-reg? (car code)) ;; register to be loaded
           (let ((t1 (make-reg)))
             (llvm-loader (cdr code)
                          (append-code load-code (llvm-load t1 (car code)))
                          (c res-instruction (llvm-repr t1)))))
          ((convert-to-stack-reg? (car code)) ;; arguments to procedure application
           (let ((t1 (make-stack-reg)))
             (llvm-loader (cdr code)
                          (append-code 
                           load-code 
                           (llvm-id t1 (convert-to-stack-reg-body (car code))))
                           (c res-instruction (llvm-repr t1)))))
          (else (llvm-loader (cdr code)
                             load-code
                             (c (llvm-repr res-instruction) (llvm-repr (car code)))))))
  (let ((target (car instruction)))
    (if (stack-reg? target)
        (let ((res (make-reg)))
          (append-code
           (if (stack-reg-used? target) '()
               (llvm-init-stack-reg target))
           (llvm-loader (cdr instruction) '() res)
           (llvm-store target res)))
        (llvm-loader instruction '() ""))))
(define (lc . instruction) (lc2 instruction))

(define (llvm-instruction target op x y)
  (lc target " = " (llvm-instr-name op) " uint " (llvm-repr x) ", " (llvm-repr y)))

(define (llvm-id target exp) ; Identity function
  (lc target " = add uint 0, " exp))

(define (llvm-call2 target function args)
  (define (arg-repr exp)
    (cond ((stack-reg? exp) (llvm-repr exp))
          (else (convert-to-stack-reg exp))))
  
  (define (build-arg-list arg-list fi)
    (cond ((null? arg-list) '())
          (else
           (cons (if (= fi 1) "" ", ")
                 (cons "uint*" 
                       (cons (arg-repr (car arg-list))
                             (build-arg-list (cdr arg-list) 0)))))))
  (lc2 
   (append (list target " = call uint " (llvm-repr function) "(") (build-arg-list args 1) (list ")"))))
(define (llvm-call target function . args) (llvm-call2 target function args))

(define (llvm-ret value) (lc "ret uint " value))
(define (llvm-cast target type1 x type2) (lc target " = cast " type1 " " x " to " type2))

(define (llvm-label label) (c label ":"))
(define (llvm-br label) (c "br label %" label))
(define (llvm-bool-br pred c-label a-label)
  (let ((t1 (make-reg))
        (t2 (make-reg)))
    (append-code
     (llvm-call t1 'raw-number pred) ; false iff pred = 0 or '()
     (llvm-cast t2 "uint" t1 "bool")
     (lc "br bool " t2 ", label %" c-label ", label %" a-label))))

(define (llvm-shift-op target op value sh)
  (let ((t1 (make-reg)))
    (append-code 
     (llvm-cast t1 "uint" sh "ubyte")
     (lc target " = " (llvm-instr-name op) " uint " value ", ubyte " t1))))

;; Compiler

(define (compile exp target c-t-env)
  (cond ((self-evaluating? exp)  (compile-self-evaluating exp target c-t-env))
        ((variable? exp)         (compile-variable exp target c-t-env))
        ((quote? exp)            (compile-self-evaluating
                                  (text-of-quotation exp) target c-t-env))
        ((or (assignment? exp) 
             (definition? exp))  (compile-assignment exp target c-t-env))
        ((if? exp)               (compile-if exp target c-t-env))
        ((cond? exp)             (compile-if (cond->if exp) target c-t-env))
        ((lambda? exp)           (compile-lambda exp target c-t-env))
        ((let? exp)              (compile (let->lambda exp) target c-t-env))
        ((begin? exp)            (compile-sequence (begin-actions exp) target c-t-env))
        ((llvm-instruction? exp) (compile-llvm-instruction exp target c-t-env))
        ((llvm-definition? exp)  (compile-llvm-definition exp target c-t-env))
        ((application? exp)      (compile-application exp target c-t-env))
        (else                    (error 'compile "Unknown expression type"))))

(define (compile-self-evaluating exp target c-t-env)
  (cond ((number? exp) 
         (llvm-id target exp)) ;; create tagged integer
        ((or (string? exp) (symbol? exp))
         (let ((str (make-reg))
               (t1 (make-reg))
               (str-repr (if (string? exp) exp (symbol->string exp))))
           (append-code
            (llvm-cast t1 (add-llvm-string str str-repr) str "uint")
            (lc target " = call uint \"%make-string/symbol\"(uint " (llvm-repr t1) 
                ", uint* " (convert-to-stack-reg (string-length str-repr)) 
                ", uint* " (convert-to-stack-reg (if (symbol? exp) 1 0)) ")"))))
        ((null? exp) (llvm-call target 'make-null))
        ((pair? exp)
         (let ((t1 (make-stack-reg))
               (t2 (make-stack-reg)))
           (append-code
            (compile-self-evaluating (car exp) t1 c-t-env)
            (compile-self-evaluating (cdr exp) t2 c-t-env)
            (llvm-call target 'cons t1 t2))))
        (else (error 'self-eval "not implemented"))))

(define (compile-variable exp target c-t-env)
  (cond ((eq? c-t-env 'llvm-function) 
         (llvm-load target (construct-stack-reg exp)))
        (else 
         (let ((c-t-pos (find-var exp c-t-env 0)))
           (if (null? c-t-pos) (error exp " not found")
               (llvm-call target 'lookup-variable (construct-stack-reg 'env)
                          (car c-t-pos) (cdr c-t-pos)))))))

(define (compile-assignment exp target c-t-env)
  (let ((c-t-pos (find-var (definition-variable exp) c-t-env 0))
        (t1 (make-stack-reg)))
    (if (null? c-t-pos) (error 'compile-assignment "not found")
	(append-code
         (compile (definition-value exp) t1 c-t-env)
         (llvm-call target 'set-variable! (construct-stack-reg 'env) (car c-t-pos) (cdr c-t-pos) t1)))))
  
(define (compile-if exp target c-t-env)
  (let ((c-branch (make-label))
        (a-branch (make-label))
        (after-if (make-label))
        (pred (make-stack-reg)))
    (append-code
     (if (and (stack-reg? target) (not (stack-reg-used? target)))
         (llvm-init-stack-reg target) '())
     (compile (if-predicate exp) pred c-t-env)
     (llvm-bool-br pred c-branch a-branch)
     (llvm-label c-branch)
     (compile (if-consequent exp) target c-t-env)
     (llvm-br after-if)
     (llvm-label a-branch)
     (compile (if-alternative exp) target c-t-env)
     (llvm-br after-if)
     (llvm-label after-if))))

(define (compile-sequence seq target c-t-env)
  (define (sequence-defines seq)
    (cond ((null? seq) '())
          ((definition? (car seq))
           (cons (definition-variable (car seq)) (sequence-defines (cdr seq))))
          ((llvm-definition? (car seq))
           (add-llvm-function-name (definition-variable (car seq)))
           (sequence-defines (cdr seq)))
          (else (sequence-defines (cdr seq)))))
  
  (define (append-sequences seq target code-seq c-t-env2)
    (if (last-exp? seq)
        (append-code code-seq (compile (car seq) target c-t-env2))
        (append-sequences 
         (cdr seq) target
         (append-code code-seq (compile (car seq) (make-stack-reg) c-t-env2)) c-t-env2)))

  (let ((seq-defines (sequence-defines seq)))
    (if (null? seq-defines) 
        (append-sequences seq target '() c-t-env) ;; no local definitions
        (let ((seq-target (make-stack-reg))
              (f-name (make-function-name))
              (t1 (make-stack-reg)))
          (add-llvm-function 
           f-name '(env)
           (append-sequences seq seq-target '() (extend-c-t-env seq-defines c-t-env)) 
           seq-target)
          (append-code
           (llvm-call t1 'make-env (length seq-defines) (construct-stack-reg 'env))
           (llvm-call target f-name t1))))))

(define (compile-lambda exp target c-t-env)
  (let ((f-name (make-function-name))
        (l-target (make-stack-reg))
        (t1 (make-stack-reg)))
    (add-llvm-function 
     f-name '(env) 
     (compile-sequence (lambda-body exp) l-target
                       (extend-c-t-env (lambda-parameters exp) c-t-env)) l-target)
    (append-code
     (lc target " = call uint \"%make-procedure\"(uint (uint*)* " f-name ", uint* " 'env ", uint* "
         (convert-to-stack-reg
          (if (lambda-arbitrary-args? exp)
              (length (lambda-parameters exp))
              0)) ")"))))

(define (compile-llvm-definition exp target c-t-env)
  (let ((f-name (definition-variable exp))
	(f-lambda (definition-value exp))
        (f-target (make-stack-reg)))
    (add-llvm-function 
     f-name (lambda-parameters f-lambda)
     (compile-sequence (lambda-body f-lambda) f-target 'llvm-function)
     f-target)
    '()))

(define (compile-llvm-instruction exp target c-t-env)
  (let ((x (make-reg))
        (y (make-reg))
        (rx (make-reg))
        (ry (make-reg))
        (target2 (make-reg)))
    (append-code
     (compile (first-arg exp) x c-t-env)
     (compile (second-arg exp) y c-t-env)
     (lc rx " = shr uint " x ", ubyte 2") ; raw-number
     (lc ry " = shr uint " y ", ubyte 2")
     (cond ((member (operator exp) llvm-shift-instructions)
            (llvm-shift-op target2 (operator exp) rx ry))
           ((member (operator exp) llvm-boolean-instructions) ;; FIXME: what if we compare two pointers?
            (let ((t1 (make-reg)))
              (append-code (llvm-instruction t1 (operator exp) rx ry)
                           (llvm-cast target2 "bool" t1 "uint"))))
           (else ;; binary operation
            (llvm-instruction target2 (operator exp) rx ry)))
     (lc target " = call uint \"%make-number\"(uint " target2 ")"))))
     

(define (compile-application exp target c-t-env)
  (define (build-param-list param-list operands index)
    (if (null? operands) '()
        (let ((t1 (make-stack-reg)))
          (append-code 
           (compile (car operands) t1 c-t-env)
           (llvm-call (make-reg) 'vector-set! param-list index t1)
           (build-param-list param-list (cdr operands) (+ 1 index))))))

  (if (member (operator exp) llvm-primitive-functions)
      (compile-llvm-application exp target c-t-env)
      (let ((proc (make-stack-reg))
            (f-env (make-stack-reg))
            (f-nparams (make-stack-reg))
            (call-env (make-stack-reg))
            (f-func (make-reg))
            (func (make-reg)))
        (append-code
         (compile (operator exp) proc c-t-env)
         (llvm-call f-env 'get-procedure-env proc)
         (llvm-call call-env 'make-env (length (operands exp)) f-env)
         (build-param-list call-env (operands exp) 1)
         (llvm-call f-nparams 'get-procedure-nparams proc)
         (llvm-call (make-reg) 'fix-arbitrary-procs f-nparams call-env)
         (llvm-call target 'apply-procedure proc call-env)))))

(define (compile-llvm-application exp target c-t-env)
  (define (operands-code ops t-vars)
    (if (null? ops) '()
        (append-code
         (compile (car ops) (car t-vars) c-t-env)
         (operands-code (cdr ops) (cdr t-vars)))))
  
  (let ((t-vars (map (lambda (operand) (make-stack-reg)) (operands exp))))        
    (append-code
     (operands-code (operands exp) t-vars)
     (llvm-call2 target (operator exp) t-vars))))

(define bootstrap-llvm-code
"implementation
declare int %printf(sbyte*, ...)
declare int %exit(int)
declare int %getchar()
declare void %llvm.memcpy(sbyte*, sbyte*, uint, uint)

declare sbyte* %llvm_gc_allocate(uint)
declare void %llvm_gc_initialize(uint)

declare void %llvm.gcroot(sbyte**, sbyte*)
declare void %llvm.gcwrite(sbyte*, sbyte*, sbyte**)

;; Support functions

uint \"%allocate-bytearray\"(uint* %size) {
  %size1 = call uint \"%raw-number\"(uint* %size)
  %res = malloc sbyte, uint %size1
  %res1 = cast sbyte* %res to uint
  ret uint %res1
}

uint \"%bytearray-ref\"(sbyte* %arr, uint %index) {
  %ptr = getelementptr sbyte* %arr, uint %index
  %res = load sbyte* %ptr
  %res2 = cast sbyte %res to uint
  %res3 = call uint \"%make-number\"(uint %res2)
  ret uint %res3
}

void \"%bytearray-set!\"(sbyte* %arr, uint %index, uint %value) {
 %ptr = getelementptr sbyte* %arr, uint %index
 %value.1 = cast uint %value to sbyte
 store sbyte %value.1, sbyte* %ptr
 ret void
}

uint \"%make-procedure\"(uint (uint*)* %raw-func, uint* %env, uint* %nparams) {
 %obj = call sbyte* %llvm_gc_allocate(uint 16)
 %obj.1 = cast sbyte* %obj to uint*
 store uint 38, uint* %obj.1 ; size 4, tag 3, forward bit = 0

 %ptr.1 = getelementptr uint* %obj.1, uint 1
 %raw-func.1 = cast uint (uint*)* %raw-func to uint
 store uint %raw-func.1, uint* %ptr.1

 %ptr.2 = getelementptr uint* %obj.1, uint 2
 %env.1 = load uint* %env
 store uint %env.1, uint* %ptr.2

 %ptr.3 = getelementptr uint* %obj.1, uint 3
 %nparams.1 = load uint* %nparams
 store uint %nparams.1, uint* %ptr.3

 %res = cast sbyte* %obj to uint
 %res.1 = or uint %res, 3 ; tag 3, procedure. 
 ret uint %res.1
}

uint \"%make-string/symbol\"(uint %raw-str, uint* %size, uint* %symbolp) {
 %obj = call sbyte* %llvm_gc_allocate(uint 16)
 %obj.1 = cast sbyte* %obj to uint*
 store uint 36, uint* %obj.1 ; size 4, tag 2, forward bit = 0

 %ptr.1 = getelementptr uint* %obj.1, uint 1
 store uint %raw-str, uint* %ptr.1

 %ptr.2 = getelementptr uint* %obj.1, uint 2
 %size.1 = load uint* %size
 store uint %size.1, uint* %ptr.2

 %ptr.3 = getelementptr uint* %obj.1, uint 3
 %symbolp.1 = load uint* %symbolp
 store uint %symbolp.1, uint* %ptr.3

 %res = cast sbyte* %obj to uint
 %res.1 = or uint %res, 2 ; tag 2, string/symbol. 
 ret uint %res.1
}

sbyte* \"%string-bytes\"(uint* %str) {
 %str.1 = call uint* \"%points-to\"(uint* %str)
 %str.2 = getelementptr uint* %str.1, uint 1
 %bytes = load uint* %str.2
 %bytes.2 = cast uint %bytes to sbyte*
 ret sbyte* %bytes.2
}

uint \"%clear-tag\"(uint* %x) {
 %x.1 = load uint* %x
 %x.2 = shr uint %x.1, ubyte 2
 %x.3 = shl uint %x.2, ubyte 2
 ret uint %x.3
}
 
uint \"%make-number\"(uint %val) {
 %res = shl uint %val, ubyte 2
 ret uint %res
}

uint \"%raw-number\"(uint* %x) {
 %raw = load uint* %x
 %raw.1 = shr uint %raw, ubyte 2
 ret uint %raw.1
}

uint* \"%points-to\"(uint* %x) {
 %x.1 = load uint* %x
 %x.2 = shr uint %x.1, ubyte 2
 %x.3 = shl uint %x.2, ubyte 2
 %ptr.1 = cast uint %x.3 to uint*
 ret uint* %ptr.1
}

uint \"%tag-eq?\"(uint* %x, uint %tag) {
 %x.1 = load uint* %x
 %tag.1 = and uint %x.1, 3
 %bool = seteq uint %tag.1, %tag
 br bool %bool, label %eq, label %noteq
eq:
 ret uint 4 ; number 1
noteq:
 ret uint 0 ; number 0
}

;; Primitive scheme functions

uint \"%llvm-read-char\"() {
  %res = call int %getchar()
  %res.1 = cast int %res to uint
  %res.2 = call uint \"%make-number\"(uint %res.1)
  ret uint %res.2
}

uint \"%print-number\"(uint* %format, uint* %value) {
  %format.2 = call sbyte* \"%string-bytes\"(uint* %format)
  %number = call uint \"%raw-number\"(uint* %value) 
  call int (sbyte*, ...)* %printf(sbyte* %format.2, uint %number)
  ret uint 0
}

uint \"%print-string/symbol\"(uint* %str) {
  %str.2 = call sbyte* \"%string-bytes\"(uint* %str)
  call int (sbyte*, ...)* %printf(sbyte* %str.2)
  ret uint 0
}

uint \"%allocate-object\"(uint* %size, uint* %tag) {
 %size.1 = load uint* %size        ; loading number gives size * 4.
 %alloc_size = add uint %size.1, 4 ; header.
 %obj = call sbyte* %llvm_gc_allocate(uint %alloc_size)
 %obj.1 = cast sbyte* %obj to uint*
 %tag.1 = call uint \"%raw-number\"(uint* %tag)
 %objtag.1 = or uint %size.1, %tag.1
 %objtag.2 = shl uint %objtag.1, ubyte 1 ; forwarding/mark bit
 store uint %objtag.2, uint* %obj.1
 %res = cast sbyte* %obj to uint
 %res.1 = or uint %res, %tag.1     ; type tag.
 ret uint %res.1
}

uint \"%object-size\"(uint* %x) {
 %ptr = call uint* \"%points-to\"(uint* %x)
 %size = load uint* %ptr
 %size.1 = shr uint %size, ubyte 3
 %size.2 = shl uint %size.1, ubyte 2
 ret uint %size.2
}

uint \"%object-ref\"(uint* %obj, uint* %index) {
 %ptr = call uint* \"%points-to\"(uint* %obj)
 %index = call uint \"%raw-number\"(uint* %index)
 %index.1 = add uint %index, 1
 %ptr.1 = getelementptr uint* %ptr, uint %index.1
 %ref = load uint* %ptr.1 ;; gcread
 ret uint %ref
}

uint \"%object-set!\"(uint* %obj, uint* %index, uint* %value) {
 %ptr = call uint* \"%points-to\"(uint* %obj)
 %index = call uint \"%raw-number\"(uint* %index)
 %index.1 = add uint %index, 1
 %ptr.1 = getelementptr uint* %ptr, uint %index.1
 %val = load uint* %value ;; gcread
 store uint %val, uint* %ptr.1 ;; gcwrite
 %res = load uint* %obj
 ret uint %res
}

uint \"%allocate-string/symbol\"(uint* %size, uint* %symbolp) {
  %bytes = call uint \"%allocate-bytearray\"(uint *%size)
  %res = call uint \"%make-string/symbol\"(uint %bytes, uint* %size, uint* %symbolp)
  ret uint %res
}

uint \"%string-ref\"(uint* %str, uint* %index) {
 %bytes = call sbyte* \"%string-bytes\"(uint* %str)
 %index.1 = call uint \"%raw-number\"(uint* %index)
 %res = call uint \"%bytearray-ref\"(sbyte* %bytes, uint %index.1)
 ret uint %res
}

uint \"%string-set!\"(uint* %str, uint* %index, uint* %value) {
 %bytes = call sbyte* \"%string-bytes\"(uint* %str)
 %index.1 = call uint \"%raw-number\"(uint* %index)
 %value.1 = call uint \"%raw-number\"(uint* %value)
 call void \"%bytearray-set!\"(sbyte* %bytes, uint %index.1, uint %value.1)
 %res = load uint* %str
 ret uint %res
}

uint \"%mem-cpy\"(uint* %src, uint* %dst, uint* %size) {
 %src.1 = call uint* \"%points-to\"(uint* %src)
 %src.2 = cast uint* %src.1 to sbyte*
 %dst.1 = call uint* \"%points-to\"(uint* %dst)
 %dst.2 = cast uint* %dst.1 to sbyte*
 %size.2 = call uint \"%raw-number\"(uint* %size)
 call void %llvm.memcpy(sbyte* %dst.2, sbyte* %src.2, uint %size.2, uint 0)
 %res = load uint* %dst
 ret uint %res
}

uint \"%apply-procedure\"(uint* %proc, uint* %callenv) {
 %proc.1 = call uint* \"%points-to\"(uint* %proc)
 %ptr.1 = getelementptr uint* %proc.1, uint 1
 %raw-func = load uint* %ptr.1
 %raw-func.1 = cast uint %raw-func to uint (uint*)*
 %res = call uint %raw-func.1(uint* %callenv)
 ret uint %res
}

uint \"%number?\"(uint* %x) {
 %res = call uint \"%tag-eq?\"(uint* %x, uint 0)
 ret uint %res
}

uint \"%vector?\"(uint* %x) {
 %res = call uint \"%tag-eq?\"(uint* %x, uint 1)
 ret uint %res
}

uint \"%string/symbol?\"(uint* %x) {
 %res = call uint \"%tag-eq?\"(uint* %x, uint 2);
 ret uint %res
}

uint \"%procedure?\"(uint* %x) {
 %res = call uint \"%tag-eq?\"(uint* %x, uint 3)
 ret uint %res
}

uint \"%null?\"(uint* %x) {
 %x.1 = load uint* %x
 %bool = seteq uint %x.1, 1 ; null vector pointer?
 br bool %bool, label %eq, label %noteq
eq:
 ret uint 4 ; number 1
noteq:
 ret uint 0 ; number 0
}

uint \"%make-null\"() {
 ret uint 1
}

uint \"%make-true\"() {
 ret uint 4
}

uint \"%exit\"() {
  call int(int)* %exit(int 0)
  ret uint 0
}

uint %main(int %argc, sbyte** %argv) {
  call void %llvm_gc_initialize(uint 2000000)

  %env = alloca uint
  store uint 0, uint* %env
  %res = call uint %startup(uint* %env)
  ret uint %res
}

;; Autogenerated code
")

(define bootstrap
  '(begin 
     (llvm-define (and x y) (if x y (make-null)))
     (llvm-define (or x y)  (if x (make-true) y))
     (llvm-define (not x) (if x (make-null) (make-true)))

     (llvm-define (ensure x message) (cond ((not x) (print-string/symbol message) (exit))))
          
     (llvm-define (make-vector size) (allocate-object size 1)) 
             
     (llvm-define (vector-size vector) (object-size vector))

     (llvm-define (pair? x) (if (vector? x) (= (vector-size x) 2) (make-null)))
     
     (llvm-define (vector-ref vector index)
                  (ensure (vector? vector) "vector-ref: not a vector.")
                  (ensure (not (null? vector)) "vector-ref: null vector")
                  (ensure (< index (vector-size vector)) "vector-ref: out of range.")
                  (object-ref vector index))

     (llvm-define (vector-set! vector index value) 
                  (ensure (vector? vector) "vector-set!: not a vector.")
                  (ensure (not (null? vector)) "vector-set!: null vector")
                  (ensure (< index (vector-size vector)) "vector-set!: out of range.")
                  (object-set! vector index value))
    
     (llvm-define (set-enclosing-env! env enclosing-env) (vector-set! env 0 enclosing-env))
     (llvm-define (get-enclosing-env env) (vector-ref env 0))
      ; +2?: env + arbitrary arg for function called with nparams - 1.
     (llvm-define (make-env nparams env) (set-enclosing-env! (make-vector (+ nparams 2)) env))
                   
     (llvm-define (get-procedure-env procedure)
                  (ensure (procedure? procedure) "get-procedure-env: not a procedure.")
                  (object-ref procedure 1))

     (llvm-define (get-procedure-nparams procedure) (object-ref procedure 2))
     
     (llvm-define (fix-arb-procs n-params end call-env)
                  (cond ((= n-params end) (make-null))
                        (else (cons (vector-ref call-env n-params)
                                    (fix-arb-procs (+ n-params 1) end call-env)))))
     (llvm-define (fix-arbitrary-procs n-params call-env)
                  (if (= n-params 0) 1
                      (vector-set! 
                       call-env n-params 
                       (fix-arb-procs n-params (- (object-size call-env) 1) call-env))))
                       
     (llvm-define (string? x) (if (string/symbol? x) (= (object-ref x 2) 0) (make-null)))
     (llvm-define (symbol? x) (if (string/symbol? x) (= (object-ref x 2) 1) (make-null)))
     
     (llvm-define (string-length str) (object-ref str 1))
     
     (llvm-define (string->symbol str)
                  (ensure (string? str) "string->symbol: not a string")                  
                  (object-set! (mem-cpy str (allocate-object 3 2) 16) 2 1))

     (llvm-define (symbol->string sym)
                  (ensure (symbol? sym) "symbol->string: not a symbol")                  
                  (object-set! (mem-cpy sym (allocate-object 3 2) 16) 2 0))

     (llvm-define (list->string-helper str lst pos)
                  (cond ((null? lst) (string-set! str pos 0))
                        (else 
                         (string-set! str pos (car lst))
                         (list->string-helper str (cdr lst) (+ pos 1)))))
     
     (llvm-define (string->list-helper str pos end)
                  (cond ((= pos end) '())
                        (else (cons (string-ref str pos)
                                    (string->list-helper str (+ pos 1) end)))))
     (llvm-define (string->list str)
                  (ensure (string? str) "string->list: not a string")
                  (string->list-helper str 0 (string-length str)))
     
     (llvm-define (char->integer ch) ch)
     (llvm-define (integer->char ch) ch)
     
     
     (llvm-define (lookup-variable env scope index)
                  (if (= 0 scope)
                      (vector-ref env index)
                      (lookup-variable (vector-ref env 0)
                                       (- scope 1)
                                       index)))
     
     (llvm-define (set-variable! env scope index value)
                  (if (= 0 scope)
                      (vector-set! env index value)
                      (set-variable! (vector-ref env 0)
                                     (- scope 1)
                                     index value)))
                                    
     (llvm-define (cons x y) (object-set! (object-set! (make-vector 2) 0 x) 1 y))
     (llvm-define (car cell) (vector-ref cell 0))
     (llvm-define (cdr cell) (vector-ref cell 1))
     (llvm-define (cddr x) (cdr (cdr x)))
     (llvm-define (cdddr x) (cdr (cdr (cdr x))))
     (llvm-define (cadr x) (car (cdr x)))
     (llvm-define (cdadr x) (cdr (car (cdr x))))
     (llvm-define (caadr x) (car (car (cdr x))))
     (llvm-define (caddr x) (car (cdr (cdr x))))
     (llvm-define (cadddr x) (car (cdr (cdr (cdr x)))))
     (llvm-define (set-car! cell val) (vector-set! cell 0 val))
     (llvm-define (set-cdr! cell val) (vector-set! cell 1 val))
     
     
     (llvm-define (* x y)
                  (ensure (number? x) "*: first argument is not a number.")
                  (ensure (number? y) "*: second argument is not a number.")
                  (mul x y))

     (llvm-define (- x y)
                  (ensure (number? x) "-: first argument is not a number.")
                  (ensure (number? y) "-: second argument is not a number.")
                  (sub x y))
     
     (llvm-define (+ x y)
                  (ensure (number? x) "+: first argument is not a number.")
                  (ensure (number? y) "+: second argument is not a number.")
                  (add x y))

     (llvm-define (/ x y)
                  (ensure (number? x) "/: first argument is not a number.")
                  (ensure (number? y) "/: second argument is not a number.")
                  (div x y))
     
     (llvm-define (% x y)
                  (ensure (number? x) "%: first argument is not a number.")
                  (ensure (number? y) "%: second argument is not a number.")
                  (rem x y))

     (llvm-define (= x y)
                  (cond ((and (number? x) (number? y))
                         (seteq x y))
                        (else (ensure 0 "=: nonapplicable types."))))
     (llvm-define (> x y)
                  (cond ((and (number? x) (number? y))
                         (setgt x y))
                        (else (ensure 0 ">: nonapplicable types."))))

     (llvm-define (< x y)
                  (cond ((and (number? x) (number? y))
                         (setlt x y))
                        (else (ensure 0 "<: nonapplicable types."))))

     
     (llvm-define (display x)
                  (cond ((number? x) (print-number "%d" x))
                        ((string? x) (print-string/symbol x))
                        ((symbol? x) (print-string/symbol "'") (print-string/symbol x))
                        ((null? x) (print-string/symbol "nil"))
                        ((pair? x) (print-string/symbol "(")
                                   (display (car x))
                                   (print-string/symbol " . ")
                                   (display (cdr x))
                                   (print-string/symbol ")"))
                        (else (print-string/symbol "display: nonapplicable type.")))
                  x)

;(define bstrap2 '(     
;

     (define (list->string lst)
       (ensure (vector? lst) "list->string: not a list")
       (list->string-helper (allocate-string/symbol (+ (length lst) 1) 0) lst 0))

     (define (newline) (display (list->string (cons 10 '()))))

     (define (string/symbol-data-eq? x y pos len)
       (cond ((= pos len) 1)
             ((= (string-ref x pos) (string-ref y pos)) 
              (string/symbol-data-eq? x y (+ pos 1) len))
             (else '())))

     (define (eq? x y)
       (cond ((and (number? x) (number? y)) (= x y))
             ((and (string? x) (string? y))
              (if (= (string-length x) (string-length y))
                  (string/symbol-data-eq? x y 0 (string-length x)) '()))
             ((and (symbol? x) (symbol? y))
              (if (= (string-length x) (string-length y))
                  (string/symbol-data-eq? x y 0 (string-length x)) '()))
             (else (seteq x y))))
          
     (define (member el lst)
       (cond ((null? lst) '())
             ((eq? el (car lst)) 1)
             (else (member el (cdr lst)))))
     
     (define (length lst)
       (cond ((null? lst) 0)
             (else (+ 1 (length (cdr lst))))))
     
     (define (nth lst el)
       (cond ((null? lst) '())
             ((= el 0) (car lst))
             (else (nth (cdr lst) (- el 1)))))
     
     (define (map fn lst)
       (cond ((null? lst) '())
             (else (cons (fn (car lst)) (map fn (cdr lst))))))

     (define append 
       (lambda l
         (define (app2 l1 l2)
           (cond ((null? l1) l2)
                 (else (cons (car l1) (app2 (cdr l1) l2)))))
         (define (app3 l1 l2 l3)
           (cond ((null? l3) (app2 l1 l2))
                 (else (app3 (app2 l1 l2) (car l3) (cdr l3)))))
         
         (cond ((null? l) '())
               ((null? (cdr l)) (car l))
               (else (app3 (car l) (cadr l) (cddr l))))))
                           
     (define (reverse lst)
       (if (null? lst) lst
           (append (reverse (cdr lst)) (list (car lst)))))
     
     (define (number->string n)
       (define (nmb-str n res)
         (if (> n 9) 
             (nmb-str (/ n 10) (cons (nth number-chars (% n 10)) res))
             (cons (nth number-chars n) res)))
       (list->string (nmb-str n '())))
     
     (define (list? x)
       (cond ((null? x) 1)
             ((pair? x) (list? (cdr x)))
             (else '())))
     (define list (lambda x x))
     
     (define (assoc x lst)
       (cond ((null? lst) '())
             ((eq? x (car (car lst))) (car lst))
             (else (assoc x (cdr lst)))))
     
     (define (string-append str1 str2)
       (list->string (append (string->list str1) (string->list str2))))
     
     (define read-char-peek '())
     (define (peek-char)
       (cond ((null? read-char-peek)
              (set! read-char-peek (llvm-read-char))
              read-char-peek)
             (else read-char-peek)))
     (define (read-char)
       (define peek read-char-peek)
       (cond ((null? peek) (llvm-read-char))
             (else (set! read-char-peek '())
                   peek)))
     
     (define number-chars (quote (48 49 50 51 52 53 54 55 56 57)))
     (define (char-whitespace? ch) (or (eq? ch 32) (eq? ch 10)))
     (define (char-numeric? ch) (member ch number-chars))
     (define (char-left-paren? ch) (eq? ch 40))
     (define (char-right-paren? ch) (eq? ch 41))
     (define (char-comment? ch) (eq? ch 59))
     (define (char-string? ch) (eq? ch 34))
     (define (char-newline? ch) (eq? ch 10))
     (define (char-dot? ch) (eq? ch 46))
     (define (char-quote? ch) (eq? ch 39))
     (define (char-backquote? ch) (eq? ch 96))
     (define (char-comma? ch) (eq? ch 44))
     (define (char-backslash? ch) (eq? ch 92))
     (define (char-character? ch) (eq? ch 35)) ; #
     (define identifier-end '(40 41 32 10))
     
     (define (read)
       (define ch (read-char))
       (cond ((char-left-paren? ch) (read-list))
             ((char-whitespace? ch) (read))
             ((char-comment? ch) (read-comment) (read))
             ((char-quote? ch) (cons 'quote (cons (read) '())))
             ((char-string? ch) (read-string))
             ((char-character? ch) (read-char-quote))
             ((char-numeric? ch) (read-number ch))
             (else (read-identifier ch))))
     
     (define (read-char-quote)
       (read-char) (read-char))
     
     (define (read-comment)
       (if (not (char-newline? (read-char)))
           (read-comment)))
     
     (define (read-list)
       (define ch (read-char))
       (cond ((char-right-paren? ch) '())
             ((char-dot? ch) (car (read-list)))
             ((char-left-paren? ch) (cons (read-list)  (read-list)))
             ((char-whitespace? ch) (read-list))
             ((char-comment? ch) (read-comment) (read-list))
             ((char-quote? ch) (cons (cons 'quote (read)) (read-list)))
             ((char-string? ch) (cons (read-string) (read-list)))
             ((char-character? ch) (read-char-quote))
             ((char-numeric? ch) (cons (read-number ch) (read-list)))
             (else (cons (read-identifier ch) (read-list)))))
     
     (define (char-list->number lst res)
       (define (nmemb x lst pos)
         (if (= x (car lst)) pos (nmemb x (cdr lst) (+ pos 1))))
       (if (null? lst) res
           (char-list->number 
            (cdr lst) (+ (* res 10)
                         (nmemb (car lst) number-chars 0)))))
        
     (define (read-number ch)
       (define (read-nmb)
         (define peek (peek-char))
         (if (char-numeric? peek) 
             (cons (read-char) (read-nmb)) '()))
       (char-list->number (cons ch (read-nmb)) 0))
     
     (define (read-identifier ch)
       (define (read-id)
         (if (member (peek-char) identifier-end) '()
             (cons (read-char) (read-id))))
       (string->symbol (list->string (cons ch (read-id)))))
     
     (define (read-string)
       (define (read-str)
         (define ch (read-char))
         (cond ((char-backslash? ch) (cons (read-char) (read-str)))           
               ((char-string? ch) '())
               (else (cons ch (read-str)))))
       (list->string (read-str)))
  ))

(define (compiler exp)
  (init-generators)
  (let ((printer (lambda (line) (display line) (newline)))
        (target (make-reg)))
    (let ((res (compile (append bootstrap exp) target '())))
      (map printer llvm-string-list)
      (display bootstrap-llvm-code)
      (display "uint %startup(uint* \"%env\") {\n")
      (map printer res)
      (display (c "ret uint " (llvm-repr target)))
      (display "\n}\n")
      (display "; FUNCTIONS\n")
      (map (lambda (function) (map printer function) (newline))
           llvm-function-list))
    'ok))

(define (ccomp exp)
  (init-generators)
  (compile exp (make-stack-reg) '()))

;(compiler (list (read)))

;(compiler '((display (read))))

;(compiler '((display (string-append "" "bar"))))
;(compiler '((display (list->string (append (string->list "") (string->list "bar"))))))
;(compiler '((display (string-length ""))))

;(compiler (cons (with-input-from-file "compile.ss" read) '())) ;; debug
;; Some tests:
(compiler '((display "Hello World") (newline)
            (display 42) (newline)
            (display (cons 1 2)) (newline)
            (display 'symbol) (newline)
            (display (quote (1 2 3 4))) (newline)))

;(compiler '((display (vector-ref (vector-set! (allocate-object 2 1) 1 42) 1))))
;(compiler '((if (seteq (object-ref (object-set! (allocate-object 2 1) 1 42) 1) 42) (print-string/symbol "foo") (print-string/symbol "bar"))))
;(compiler '((if (pair? (cons 1 2)) (print-string/symbol "foo") (print-string/symbol "bar"))))
;(compiler '((display (object-size (allocate-object 10 1)))))
;(compiler '((display (make-null))))
;(compiler '((print-string/symbol "foo")))
;(compiler '((+ 1 2)))
;(compiler '((display (quote (1 2 "foo" "bar" fum)))))
;(compiler '((display (quote (1 . (2 . 3))))))
;(compiler '((display (list->string (quote (97 98 98 99))))))
;(compiler '((display (number->string 123546))))
;(compiler '((display (string? (symbol->string (quote foo))))))
;(compiler '((display (string->list "foobar"))))
;(compiler '((display (string-append "foo" "bar"))))
;(compiler '((display (append (quote (1 2 3)) (quote (4 5 6))))))
;(compiler '((display (string->symbol "foo"))))
;(compiler '((display (symbol->string (quote foo)))))

;(compiler '((display (member 5 (quote (1 2 3 4))))))
;(compiler '((display (read))))
; Function application.
;(compiler '((define y 5) 
;            (define (fac x) (if (= x 0) 1 (* x (fac (- x 1))))) 
;            (display (fac y))))
;->120
;(compiler '((define y 5)
;            (define (fac x)
;              (cond ((= x 0) 1)
;                    (else (* x (fac (- x 1))))))
;            (display (fac y))))
;
; Returning lambda with state.
;(compiler '((define (adder x) (lambda (y) (+ x y)))
;            (define a (adder 3)) 
;            (define b (adder 4))
;            (display (+ (a 1) (b 2)))))
;->10                  
;
; Mutually recursive functions.
;(compiler '((define (odd x) (if (= x 1) 1
;                                (if (even (- x 1))
;                                    1
;				    0)))
;            (define (even x) (if (= x 2)
;                                 1
;                                 (if (odd (- x 1))
;                                     1
;                                     0)))
;            (display (even 10))))
;->1

;(compiler '((define (foo x . y) (display y))
;            (foo 1 2 3 4)))


;(compiler '((* (cons 1 2) 3)))
;*: first argument is not a number.
;(compiler '((vector-ref 2 3)))
;vector-ref: not a vector.
;(compiler '((peek-char) (read-char)))

)

