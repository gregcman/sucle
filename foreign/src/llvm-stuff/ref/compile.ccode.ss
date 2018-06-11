;; a small self applicable scheme->c compiler. (Tobias Nurmiranta)
;;
;; reads scheme-code from standard input:
;; mzscheme --script compile.ccode.ss > t.c;gcc t.c;./a.out
;; self-application example:
;; cat compile.ccode.ss|mzscheme --script compile.ccode.ss > t.c;gcc t.c -o t 
;; cat compile.ccode.ss|./t > tt.c
;;
;; -- c-define --
;; c-define defines a C function with formal parameters, so it doesn't
;; use an environment for variable lookup. 
;;
;; -- Implemented Types --
;; all objects are represented with a 32 bit uint, with a typetag of 2 bits.
;;
;; - 30 bit immediate fixnums (also used for characters)
;; - symbols, strings
;; - vectors (which are also used as conscells)
;; - functions (fixed and arbitrary number of arguments)
(begin

(define (error func str)
  (display func) (display " ") (display str) (newline)
  (c-exit))

;; abstract syntax
(define (self-evaluating? exp) 
  (or (number? exp) (or (string? exp) (char? exp))))
(define (variable? exp) (symbol? exp))
(define (tagged-list? exp tag) (if (pair? exp) (eq? (car exp) tag) (= 1 0)))
(define (assignment? exp) (tagged-list? exp 'set!))
(define (definition? exp) (tagged-list? exp 'define))
(define (if? exp) (tagged-list? exp 'if))
(define (cond? exp) (tagged-list? exp 'cond))
(define (lambda? exp) (tagged-list? exp 'lambda))
(define (begin? exp) (tagged-list? exp 'begin))
(define (quote? exp) (tagged-list? exp 'quote))
(define (let? exp) (tagged-list? exp 'let))
(define (application? exp) (pair? exp))
(define (c-definition? exp) (tagged-list? exp 'c-define))
(define (c-instruction? exp) (assoc (operator exp) c-instructions))

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (first-arg exp) (cadr exp))
(define (second-arg exp) (caddr exp))
(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (text-of-quotation exp) (cadr exp))

(define (make-lambda parameters body) (cons 'lambda (cons parameters body)))
(define (lambda-parameters exp) 
  (define (fix-list lst)
    (cond ((not (pair? lst)) (cons lst '()))
          (else (cons (car lst) (fix-list (cdr lst))))))
  (if (list? (cadr exp)) (cadr exp) (fix-list (cadr exp))))
(define (lambda-arbitrary-args? exp) (not (list? (cadr exp))))
(define (lambda-body exp) (cddr exp))

(define (definition-variable exp) 
  (if (symbol? (cadr exp)) (cadr exp) (caadr exp)))
(define (definition-value exp)
  (if (symbol? (cadr exp)) (caddr exp) (make-lambda (cdadr exp) (cddr exp))))

(define (make-if pred t-branch f-branch) (list 'if pred t-branch f-branch))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp) (if (not (null? (cdddr exp))) (cadddr exp) 0))

;; code transformation
(define (make-begin seq) (cons 'begin seq))
(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (car seq))
        (else (make-begin seq))))

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
  (cons (make-lambda (let-vars exp) (let-body exp)) (let-vals exp)))

;; code constructors
(define (append-code2 instrs)
    (cond ((null? instrs) instrs)
          ((null? (car instrs)) (append-code2 (cdr instrs)))
          ((pair? (car instrs))
           (append (car instrs) (append-code2 (cdr instrs))))
          (else (cons (car instrs) (append-code2 (cdr instrs))))))
(define (append-code . instrs) (append-code2 instrs))

;; C primitives.
(define c-instructions
  '((add . "+") (sub . "-") (mul . "*") (div . "/") (rem . "%")    
    (bit-and . "&") (bit-or . "|") (bit-xor . "^") 
    (seteq . "==") (setne . "!=") (setlt . "<") (setgt . ">")
    (setle . "<=") (setge . ">=")))
(define (c-instr-name op) (cdr (assoc op c-instructions)))

;; generate locals, string constants and functions.
(define (c . strs) ;; generalized string-append
  (define (str-app str1 rest)
    (if (null? rest) (if (string? str1) str1 (c-repr str1))
	(string-append (if (string? str1) str1 (c-repr str1)) 
		       (str-app (car rest) (cdr rest)))))
  (str-app (car strs) (cdr strs)))

(define (init-generators)
  (set! local-counter 4)
  (set! function-counter 0)
  (set! c-function-list '())
  (set! c-primitive-functions ;; functions implemented in C. 
        '(c-read-char 
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
          c-exit))
  (set! c-string-count 0)
  (set! c-string-list '()))

(define function-counter 0)
(define (make-function-name)
  (set! function-counter (+ 1 function-counter))
  (c "function" (number->string function-counter)))

(define local-index-list '())
(define local-counter 4)
(define (make-local)
  (let ((res (c "var[" (number->string local-counter) "]")))
    (set! local-counter (+ 1 local-counter))
    res))
(define (enter-function-context)
  (set! local-index-list (cons local-counter local-index-list))
  (set! local-counter 4))
(define (leave-function-context)
  (set! local-counter (car local-index-list))
  (set! local-index-list (cdr local-index-list)))

(define c-primitive-functions '())
(define (add-c-function-name f-name)
  (set! c-primitive-functions (cons f-name c-primitive-functions)))

(define function-res "var[3]")
(define c-function-list '())
(define (add-c-function f-name f-params f-body)
  (define (build-params params)
    (if (null? params) ""
        (c "uint* " (car params) (if (null? (cdr params)) "" ", ")
           (build-params (cdr params)))))
  (set! c-function-list
    (append c-function-list
	    (list (append-code 
		   (c "uint " f-name "(" (build-params f-params) ") {")
		   (c "function_prolog(" (number->string local-counter)
		      ", \"" f-name "\");")
		   f-body
		   "function_epilog();"
		   "}")))))

(define (fix-string-format str) ;; C syntax strings.
  (define (str-ref-int str pos) (char->integer (string-ref str pos)))
  (define (fix-str-format str pos end)
    (if (= pos end) '()
        (let ((x (str-ref-int str pos)))
          (cond ((eq? x 10) ; \n
                 (cons #\\ (cons #\n (fix-str-format str (+ 1 pos) end))))
                ((eq? x 34) ; "
                 (cons #\\ (cons #\" (fix-str-format str (+ 1 pos) end))))
                ((eq? x 92) ; \
                 (cons #\\ (cons #\\ (fix-str-format str (+ 1 pos) end))))
                (else (cons (string-ref str pos) 
                            (fix-str-format str (+ 1 pos) end)))))))
  (list->string (fix-str-format str 0 (string-length str))))
(define c-string-count 0)
(define c-string-list '())
(define (add-c-string str)
  (let ((res (c "str" (number->string c-string-count))))
    (set! c-string-list 
          (cons (c "char* " res " = \"" (fix-string-format str) "\";")
                c-string-list))
    (set! c-string-count (+ 1 c-string-count))
    res))

;; Lexical addressing.
(define (extend-c-t-env params c-t-env) (cons params c-t-env))
(define (current-c-t-env c-t-env) (car c-t-env))
(define (outer-c-t-env c-t-env) (cdr c-t-env))
(define (find-var var c-t-env scope)
  (define (find-index var env index)
    (cond ((null? env) '())
	  ((eq? (car env) var) index)
	  (else (find-index var (cdr env) (+ 1 index)))))
  (if (null? c-t-env) (error var " not found")
      (let ((index (find-index var (current-c-t-env c-t-env) 1)))
	(if (null? index) (find-var var (outer-c-t-env c-t-env) (+ 1 scope))
	    (cons scope index)))))

(define (c-repr exp)
  (define symbol-map ;; map to create legal C names.
    '((#\- . #\_) (#\/ . #\S) (#\? . #\P) (#\> . #\G) (#\< . #\L) (#\= . #\E)
      (#\! . #\1) (#\+ . #\A) (#\* . #\C) (#\/ . #\D) (#\% . #\F)))
  (define (symbol-fix str pos)
    (if (= pos (string-length str)) '()
	(let ((a (assoc (string-ref str pos) symbol-map)))
	  (cons (if a (cdr a) (string-ref str pos))
		(symbol-fix str (+ 1 pos))))))
  (cond ((number? exp) (number->string (* 4 exp)))
        ((symbol? exp) (list->string (symbol-fix (symbol->string exp) 0)))
        ((char? exp) (number->string (* 4 (char->integer exp))))
        (else exp)))

(define (c-store target value) (c target " = " value ";"))
(define (c-instruction target op x y)
  (c target " = ((" x " >> 2) " (c-instr-name op) " (" y " >> 2));"))

(define (c-call2 target function args)
  (define (build-arg-list arg-list fi)
    (cond ((null? arg-list) "")
          (else (c (if (= fi 1) "" ", ") "&" (car arg-list)
                   (build-arg-list (cdr arg-list) 0)))))
  (c target " = " function "(" (build-arg-list args 1) ");"))
(define (c-call target function . args) (c-call2 target function args))

;; the compiler
(define (compile exp target c-t-env)
  (cond ((self-evaluating? exp) (compile-self-evaluating exp target c-t-env))
        ((variable? exp) (compile-variable exp target c-t-env))
        ((quote? exp) 
	 (compile-self-evaluating (text-of-quotation exp) target c-t-env))
        ((or (assignment? exp) (definition? exp)) 
	 (compile-assignment exp target c-t-env))
        ((if? exp) (compile-if exp target c-t-env))
        ((cond? exp) (compile-if (cond->if exp) target c-t-env))
        ((lambda? exp) (compile-lambda exp target c-t-env))
        ((let? exp) (compile (let->lambda exp) target c-t-env))
        ((begin? exp) (compile-sequence (begin-actions exp) target c-t-env))
        ((c-instruction? exp) (compile-c-instruction exp target c-t-env))
        ((c-definition? exp) (compile-c-definition exp target c-t-env))
        ((application? exp) (compile-application exp target c-t-env))
        (else (error 'compile "Unknown expression type"))))

(define (compile-self-evaluating exp target c-t-env)
  (cond ((or (number? exp) (char? exp)) (c-store target exp))
        ((or (string? exp) (symbol? exp))
         (let ((str-repr (if (string? exp) exp (symbol->string exp))))
           (c-store 
            target (c 'make-string/symbol "(" (add-c-string str-repr) ", "
                      (+ (string-length str-repr) 1) ", "
                      (if (symbol? exp) 1 0) ");"))))
        ((null? exp) (c-call target 'make-null))
        ((pair? exp)
         (let ((t1 (make-local)))
           (append-code
            (compile-self-evaluating (car exp) t1 c-t-env)
            (compile-self-evaluating (cdr exp) target c-t-env)
            (c-call target 'cons t1 target))))
        (else (error 'self-eval "not implemented"))))

(define (compile-variable exp target c-t-env)
  (if (eq? c-t-env 'c-function) (c-store target (c "*" exp))
      (let ((c-t-pos (find-var exp c-t-env 0)))
	(c-store target (c "lookup_variable(env, " 
			   (number->string (car c-t-pos))  ", " 
			   (number->string (cdr c-t-pos)) ");")))))

(define (compile-assignment exp target c-t-env)
  (if (eq? c-t-env 'c-function)
      (compile (definition-value exp)
	       (c "*" (definition-variable exp)) c-t-env)
      (let ((c-t-pos (find-var (definition-variable exp) c-t-env 0)))
	(append-code
	 (c "// assignment " (definition-variable exp))
	 (compile (definition-value exp) target c-t-env)
	 (c-store 
	  target (c "set_variable1(env, " 
		    (number->string (car c-t-pos)) ", " 
	            (number->string (cdr c-t-pos)) ", &" target ");"))))))
  
(define (compile-if exp target c-t-env)
  (append-code (compile (if-predicate exp) target c-t-env)
               (c "if(raw_number(&" target ")) {")
               (compile (if-consequent exp) target c-t-env)
               (c "} else {")
               (compile (if-alternative exp) target c-t-env)
               (c "}")))

(define (compile-sequence seq target c-t-env)
  (define (sequence-defines seq)
    (cond ((null? seq) '())
          ((definition? (car seq))
           (cons (definition-variable (car seq)) (sequence-defines (cdr seq))))
          ((c-definition? (car seq))
           (add-c-function-name (definition-variable (car seq)))
           (sequence-defines (cdr seq)))
          (else (sequence-defines (cdr seq)))))
  (define (append-sequences seq target c-t-env2)
    (if (null? seq) '()
        (append-code (compile (car seq) target c-t-env2)
                     (append-sequences (cdr seq) target c-t-env2))))

  (let ((seq-defines (sequence-defines seq)))
    (if (null? seq-defines) 
        (append-sequences seq target c-t-env) ;; no local definitions
        (let ((f-name (make-function-name))) ;; create local function context
	  (enter-function-context)
          (add-c-function 
           f-name '(env)
           (append-sequences seq function-res (extend-c-t-env seq-defines c-t-env)))
	  (leave-function-context)
          (append-code
           (c-store target (length seq-defines))
           (c-store target (c "make_env(&" target ", env);"))
           (c-call target f-name target))))))

(define (compile-lambda exp target c-t-env)
  (let ((f-name (make-function-name)))
    (enter-function-context)
    (add-c-function 
     f-name '(env) 
     (compile-sequence (lambda-body exp) function-res
                       (extend-c-t-env (lambda-parameters exp) c-t-env)))
    (leave-function-context)
    (append-code
     (c-store target (if (lambda-arbitrary-args? exp)
                         (length (lambda-parameters exp)) 0))
     (c target " = make_procedure(" f-name ", env, &" target ");"))))

(define (compile-c-definition exp target c-t-env)
  (let ((f-name (definition-variable exp))
	(f-lambda (definition-value exp)))
    (enter-function-context)
    (add-c-function 
     f-name (lambda-parameters f-lambda)
     (compile-sequence (lambda-body f-lambda) function-res 'c-function))
    (leave-function-context)
    '()))

(define (compile-c-instruction exp target c-t-env)
  (let ((t1 (make-local)))
    (append-code
     (compile (first-arg exp) target c-t-env)
     (compile (second-arg exp) t1 c-t-env)
     (c-instruction target (operator exp) target t1)
     (c target " = make_number(" target ");"))))

(define (compile-application exp target c-t-env)
  (let ((t1 (make-local)))
    (define (build-param-list param-list operands index)
      (if (null? operands) '()
          (append-code 
           (compile (car operands) target c-t-env)
           (c-store t1 index)
           (c-call target 'vector-set! param-list t1 target)
           (build-param-list param-list (cdr operands) (+ 1 index)))))

    (if (member (operator exp) c-primitive-functions)
        (compile-c-application exp target c-t-env)
        (let ((proc (make-local))
              (f-env (make-local))
              (f-nparams (make-local)))
          (append-code
           (compile (operator exp) proc c-t-env)
           (c-call f-env 'get-procedure-env proc)
           (c-store t1 (length (operands exp)))
           (c-call f-env 'make-env t1 f-env)
           (build-param-list f-env (operands exp) 1)
           (c-call f-nparams 'get-procedure-nparams proc)
           (c-call f-nparams 'fix-arbitrary-procs f-nparams f-env)
           (c-call target 'apply-procedure proc f-env))))))

(define (compile-c-application exp target c-t-env)
  (define (operands-code ops t-vars)
    (if (null? ops) '()
        (append-code
         (compile (car ops) (car t-vars) c-t-env)
         (operands-code (cdr ops) (cdr t-vars)))))
  (let ((t-vars (map (lambda (operand) (make-local)) (operands exp))))
    (append-code
     (operands-code (operands exp) t-vars)
     (c-call2 target (operator exp) t-vars))))

(define bootstrap-c-code
"
// -- Support functions --
#include <stdlib.h>
#include <alloca.h>
#include <stdio.h>
#include <string.h>

typedef unsigned int uint;
uint gc_allocate(uint size);

/* 2 low bits used as type information:
   00 - number
   01 - vector
   10 - string/symbol
   11 - procedure */

uint make_number(uint val) { return val << 2; }
uint raw_number(uint* x) { return (*x) >> 2; }
uint* points_to(uint* x) { return (uint*) ((*x >> 2) << 2); }
char* string_bytes(uint* str) { return (char*) (points_to(str)[1]); }
uint tag_eqP(uint* x, uint tag) { return (*x & 0x3) == tag ? 4 : 0; }

uint allocate_bytearray(uint* size) { return (uint) malloc(raw_number(size)); }
uint bytearray_ref(char* arr, uint index) { return make_number(arr[index]); }
void bytearray_set1(char* arr, uint index, uint value) { arr[index] = value; }

uint make_procedure(uint (*raw_func)(uint*) , uint* env, uint* nparams) {
 uint* obj = (uint*) gc_allocate(16);
 obj[0] = 30; // size 4, tag 3, forward bit 0
 obj[1] = (uint) raw_func;
 obj[2] = *env;
 obj[3] = *nparams;
 return ((uint) obj) | 0x3;
}
uint make_stringSsymbol(char* raw_str, uint size, uint symbolp) {
 uint* obj = (uint*) gc_allocate(16);
 obj[0] = 28; // size 4, tag 2, forward bit 0
 obj[1] = (uint) raw_str;
 obj[2] = size;
 obj[3] = symbolp;
 return ((uint) obj) | 0x2;
}

// garbage collection forwarding
void set_forward(uint* x, uint* to) { *points_to(x) = ((uint) to) | 0x1; }
uint forwarded(uint* x) { return (*points_to(x) & 0x1) ? 4 : 0; }

// -- Primitive scheme functions --
uint c_read_char() { return make_number(getchar()); }
void stack_trace();
uint c_exit() { stack_trace(); exit(0); return 0; }

uint print_number(uint* format, uint* value) {
 printf(string_bytes(format), raw_number(value));
 return 4;
}
uint print_stringSsymbol(uint* str) {
 printf(\"%s\", string_bytes(str));
 return 4;
}

uint allocate_object(uint* size, uint* tag) {
 uint* obj = (uint*) gc_allocate(*size + 4); // loading number gives size * 4.
 *obj = (*size | raw_number(tag)) << 1;
 return ((uint) obj) | raw_number(tag);
}
uint object_size(uint* x) { return (*points_to(x) >> 3) << 2; }
uint object_ref_raw(uint *obj, uint raw_index) {
 return points_to(obj)[raw_index + 1]; 
}
uint object_ref(uint* obj, uint* index) {
 return object_ref_raw(obj, raw_number(index));
}
uint object_set1_raw(uint* obj, uint raw_index, uint* value) {
 points_to(obj)[raw_index + 1] = *value;
 return *obj; 
}
uint object_set1(uint* obj, uint* index, uint* value) {
 return object_set1_raw(obj, raw_number(index), value);
}

uint allocate_stringSsymbol(uint* size, uint* symbolp) {
 return make_stringSsymbol((char*)allocate_bytearray(size), *size, *symbolp);
}
uint string_ref(uint* str, uint* index) {
 return bytearray_ref(string_bytes(str), raw_number(index));
}
uint string_set1(uint* str, uint* index, uint* value) {
 bytearray_set1(string_bytes(str), raw_number(index), raw_number(value));
 return *str;
}

uint mem_cpy(uint* src, uint* dst, uint* size) {
 memcpy(points_to(dst), points_to(src), raw_number(size));
 return *dst;
}

uint apply_procedure(uint* proc, uint* callenv) {
 return ((uint (*)(uint*)) points_to(proc)[1])(callenv);
}

uint numberP(uint* x) { return tag_eqP(x, 0); }
uint vectorP(uint* x) { return tag_eqP(x, 1); }
uint stringSsymbolP(uint* x) { return tag_eqP(x, 2); }
uint procedureP(uint* x) { return tag_eqP(x, 3); }
uint nullP(uint* x) { return (*x == 1) ? 4 : 0; }
uint make_null() { return 1;}
uint make_true() { return 4;}

// variable lookup/assignment
uint lookup_variable(uint *env, uint scope, uint index) {
 uint e = *env;
 for(;scope > 0; scope--, e = object_ref_raw(&e, 0));
 return object_ref_raw(&e, index);
}
uint set_variable1(uint *env, uint scope, uint index, uint* value) {
 uint e = *env;
 for(;scope > 0; scope--, e = object_ref_raw(&e, 0));
 return object_set1_raw(&e, index, value);
}

// Garbage collector
void gc_collect();
static uint gc_space_size;
static char *gc_alloc_ptr;
static char *gc_alloc_end;
static char *gc_cur_space;
static char *gc_to_space;

void gc_initialize(uint heap_size) {
 gc_space_size = heap_size;
 gc_cur_space = gc_alloc_ptr = calloc(1, gc_space_size);
 gc_to_space = calloc(1, gc_space_size);
 gc_alloc_end = gc_alloc_ptr + gc_space_size;

 if(((uint) gc_cur_space) & 0x3 != 0 || ((uint) gc_to_space) & 0x3 != 0) {
   printf(\"memory not aligned\\n\"); exit(1);
 }
}

uint gc_allocate(uint size) {
 char *old_aptr = gc_alloc_ptr;
 char *new_aptr = gc_alloc_ptr + size;
 if(new_aptr > gc_alloc_end) {
  gc_collect();
  if(gc_alloc_ptr + size > gc_alloc_end) { // resize heap
   char *old = gc_cur_space;
   free(gc_to_space);
   gc_initialize(gc_space_size * 2);
   gc_collect();
   free(old);
  }
  return gc_allocate(size);
 }
 gc_alloc_ptr = new_aptr;
 return (uint) old_aptr;
}

#define function_prolog(vnum, fname) \\
  uint var[vnum]; memset(var, 0, sizeof(uint) * vnum); \\
  var[0] = (uint) frame_stack;var[1] = vnum;var[2] = (uint) fname; \\
  frame_stack = var;
#define function_epilog() frame_stack = (uint*) var[0]; return var[3];
uint* frame_stack = NULL;

static char *gc_scan;
static char *gc_free;

void gc_walk_roots(void (*callback)(uint*)) {
 uint *fs, *v;
 for(fs = frame_stack;fs;fs = (uint*)fs[0])
  for(v = &fs[3]; v < &fs[fs[1]]; v++)
   callback(v);
}

void stack_trace() {
 uint *fs, *v, i;
 for(fs = frame_stack;fs;fs = (uint*)fs[0])
  printf(\"trace: %s\\n\", (char*)fs[2]);
}

// Cheney collector 
// (C. J. Cheney. 1970. A non-recursive list compacting algorithm.)
static void gc_copy_forward(uint* cell) {
 uint tag = *cell & 0x3;
 if(tag == 0) { return; } // number -> ignore  
 uint* obj = points_to(cell);       
 if(obj == 0) { return; } // null pointer -> ignore
 if (forwarded(cell)) { // update pointer with forwarding info
  *cell = ((uint) points_to(obj)) | tag; 
 } else { // copy and forward object
  uint size = object_size(cell) + 4;
  memcpy(gc_free, obj, size);
  set_forward(cell, (uint*) gc_free);
  *cell = ((uint) gc_free) | tag;
  gc_free += size;
 }
}

void gc_collect() {
 //  printf(\"Garbage collecting, heapsize %u..\\n\", gc_alloc_ptr - gc_cur_space);
 memset(gc_to_space, 0, gc_space_size);
 gc_scan = gc_free = gc_to_space;
 gc_walk_roots(gc_copy_forward);

 while(gc_scan < gc_free) {
  uint* obj = (uint *) gc_scan;
  uint tag = (*obj >> 1) & 0x3;
  switch(tag) {
   case 0:
        printf(\"scan incorrect, unboxed type.\\n\"); *((uint*)0) = 0;
        break;
   case 1: // vector
        {
         uint i, size = *obj >> 3; // printf(\"scan vector %u\\n\", size);
         for(i = 0; i < size; i++)
          gc_copy_forward(&obj[i+1]);
         gc_scan += (size + 1) * 4; // 32 bits.
        } break;
   case 2: // printf(\"scan string/symbol\\n\");
        gc_scan += 16;
        break;
   case 3: // printf(\"scan proc\\n\");
        gc_copy_forward(&obj[2]); // procedure closure
        gc_scan += 16;
        break;
  }
 }
 // printf(\"switch roles of gc spaces\\n\");
 char* temp = gc_to_space;
 gc_to_space = gc_cur_space;
 gc_cur_space = temp;
 gc_alloc_ptr = gc_free;
 gc_alloc_end = gc_cur_space + gc_space_size;
 // printf(\"GC complete. heapsize: %u \\n\", gc_alloc_ptr - gc_cur_space);
}

// -- Bootstrap code --
uint startup(uint* env);
int main(int argc, uint** argv) {
  gc_initialize(40000);
  uint env = 0;
  return startup(&env);
}

// -- Autogenerated code --
")

(define bootstrap
  '(begin 
     (c-define (and x y) (if x y (make-null)))
     (c-define (or x y)  (if x (make-true) y))
     (c-define (not x) (if x (make-null) (make-true)))
     (c-define (ensure x message) 
               (cond ((not x) (print-string/symbol message) (c-exit))))
          
     (c-define (make-vector size) (allocate-object size 1))     
     (c-define (vector-size vector) (object-size vector))     
     (c-define (vector-ref vector index)
               (ensure (vector? vector) "vector-ref: not a vector.")
               (ensure (not (null? vector)) "vector-ref: null vector")
               (ensure (setlt index (vector-size vector))
                       "vector-ref: out of range.")
               (object-ref vector index))

     (c-define (vector-set! vector index value) 
               (ensure (vector? vector) "vector-set!: not a vector.")
               (ensure (not (null? vector)) "vector-set!: null vector")
               (ensure (setlt index (vector-size vector)) 
                       "vector-set!: out of range.")
               (object-set! vector index value))
  
     (c-define (pair? x) (if (and (vector? x) (not (null? x))) 
                             (seteq (vector-size x) 2) (make-null)))          
     (c-define (cons x y) (object-set! (object-set! (make-vector 2) 0 x) 1 y))
     (c-define (car cell) (vector-ref cell 0))
     (c-define (cdr cell) (vector-ref cell 1))
     (c-define (cddr x) (cdr (cdr x)))
     (c-define (cdddr x) (cdr (cdr (cdr x))))
     (c-define (cadr x) (car (cdr x)))
     (c-define (cdadr x) (cdr (car (cdr x))))
     (c-define (caadr x) (car (car (cdr x))))
     (c-define (caddr x) (car (cdr (cdr x))))
     (c-define (cadddr x) (car (cdr (cdr (cdr x)))))
     (c-define (set-car! cell val) (vector-set! cell 0 val))
     (c-define (set-cdr! cell val) (vector-set! cell 1 val))
          
     (c-define (set-enclosing-env! env enclosing-env) 
               (vector-set! env 0 enclosing-env))
     (c-define (get-enclosing-env env) (vector-ref env 0))
     (c-define (make-env nparams env) 
               (set-enclosing-env! (make-vector (add nparams 2)) env))
                   
     (c-define (get-procedure-env procedure)
               (ensure (procedure? procedure)
                       "get-procedure-env: not a procedure.")
               (object-ref procedure 1))
     (c-define (get-procedure-nparams procedure) (object-ref procedure 2))
     
     (c-define (fix-arb-procs n-params end call-env)
               (cond ((seteq n-params end) (make-null))
                     (else (cons (vector-ref call-env n-params)
                                 (fix-arb-procs (add n-params 1) end 
                                                call-env)))))
     (c-define (fix-arbitrary-procs n-params call-env)
               (if (seteq n-params 0) 1
                   (vector-set! 
                    call-env n-params 
                    (fix-arb-procs n-params (sub (object-size call-env) 1) 
                                   call-env))))
     
     (c-define (string? x) 
               (if (string/symbol? x) (seteq (object-ref x 2) 0) (make-null)))
     (c-define (symbol? x) 
               (if (string/symbol? x) (seteq (object-ref x 2) 1) (make-null)))
     (c-define (string-length str) (sub (object-ref str 1) 1))
     (c-define (string->symbol str)
               (ensure (string? str) "string->symbol: not a string")
               (object-set! (mem-cpy str (allocate-object 3 2) 16) 2 1))
     (c-define (symbol->string sym)
               (ensure (symbol? sym) "symbol->string: not a symbol")
               (object-set! (mem-cpy sym (allocate-object 3 2) 16) 2 0))
     (c-define (list->string-helper str lst pos)
               (cond ((null? lst) (string-set! str pos 0))
                     (else 
                      (string-set! str pos (car lst))
                      (list->string-helper str (cdr lst) (add pos 1)))))
     (c-define (string->list-helper str pos end)
               (cond ((seteq pos end) '())
                     (else (cons (string-ref str pos)
                                 (string->list-helper str 
                                                      (add pos 1) end)))))
     (c-define (string->list str)
               (ensure (string? str) "string->list: not a string")
               (string->list-helper str 0 (string-length str)))
     
     (c-define (char? ch) (number? ch)) ;; char and integer, same datatype
     (c-define (char->integer ch) ch)
     (c-define (integer->char ch) ch)
     
     (c-define (* x y)
               (ensure (number? x) "*: first argument is not a number.")
               (ensure (number? y) "*: second argument is not a number.")
               (mul x y))
     (c-define (- x y)
               (ensure (number? x) "-: first argument is not a number.")
               (ensure (number? y) "-: second argument is not a number.")
               (sub x y))
     (c-define (+ x y)
               (ensure (number? x) "+: first argument is not a number.")
               (ensure (number? y) "+: second argument is not a number.")
               (add x y))
     (c-define (/ x y)
               (ensure (number? x) "/: first argument is not a number.")
               (ensure (number? y) "/: second argument is not a number.")
               (div x y))
     (c-define (% x y)
               (ensure (number? x) "%: first argument is not a number.")
               (ensure (number? y) "%: second argument is not a number.")
               (rem x y))
     (c-define (= x y)
               (cond ((and (number? x) (number? y)) (seteq x y))
                     (else (ensure 0 "=: nonapplicable types."))))
     (c-define (> x y)
               (cond ((and (number? x) (number? y)) (setgt x y))
                     (else (ensure 0 ">: nonapplicable types."))))
     (c-define (< x y)
               (cond ((and (number? x) (number? y)) (setlt x y))
                     (else (ensure 0 "<: nonapplicable types."))))
     
     (c-define (display x)
               (cond ((number? x) (print-number "%d" x))
                     ((string? x) (print-string/symbol x))
                     ((symbol? x) (print-string/symbol x))
                     ((null? x) (print-string/symbol "()"))
                     ((pair? x) (print-string/symbol "(")
                      (display (car x))
                      (print-string/symbol " . ")
                      (display (cdr x))
                      (print-string/symbol ")"))
                     (else 
                      (print-string/symbol "display: nonapplicable type.")))
               x)
     
     (define (list->string lst)
       (ensure (vector? lst) "list->string: not a list")
       (list->string-helper (allocate-string/symbol 
                             (add (length lst) 1) 0) lst 0))

     (define (newline) (display (list->string (cons 10 '()))))

     (define (string/symbol-data-eq? x y pos len)
       (cond ((seteq pos len) 1)
             ((seteq (string-ref x pos) (string-ref y pos)) 
              (string/symbol-data-eq? x y (add pos 1) len))
             (else '())))
     (define (eq? x y)
       (cond ((and (number? x) (number? y)) (seteq x y))
             ((and (string? x) (string? y))
              (if (seteq (string-length x) (string-length y))
                  (string/symbol-data-eq? x y 0 (string-length x)) '()))
             ((and (symbol? x) (symbol? y))
              (if (seteq (string-length x) (string-length y))
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
     (define (list? x)
       (cond ((null? x) 1)
             ((pair? x) (list? (cdr x)))
             (else '())))
     (define list (lambda x x))     
     (define (assoc x lst)
       (cond ((null? lst) '())
             ((eq? x (car (car lst))) (car lst))
             (else (assoc x (cdr lst)))))
     
     (define (number->string n)
       (define (nmb-str n res)
         (if (> n 9) 
             (nmb-str (/ n 10) (cons (nth number-chars (% n 10)) res))
             (cons (nth number-chars n) res)))
       (list->string (nmb-str n '())))
     
     (define (string-append str1 str2)
       (list->string (append (string->list str1) (string->list str2))))
     
     (define read-char-peek '())
     (define (peek-char)
       (cond ((null? read-char-peek)
              (set! read-char-peek (c-read-char))
              read-char-peek)
             (else read-char-peek)))
     (define (read-char)
       (define peek read-char-peek)
       (cond ((null? peek) (c-read-char))
             (else (set! read-char-peek '())
                   peek)))
     
     (define number-chars (quote (48 49 50 51 52 53 54 55 56 57)))
     (define (char-whitespace? ch) (member ch '(32 10 9)))
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
     (define identifier-end '(40 41 32 10 9))

     (define (read-quote) (cons 'quote (cons (read) '())))
     (define (read-char-quote) (read-char) (read-char))
     (define (read-comment) (if (not (char-newline? (read-char))) (read-comment)))
     
     (define (read)
       (define ch (read-char))
       (cond ((char-left-paren? ch) (read-list))
             ((char-whitespace? ch) (read))
             ((char-comment? ch) (read-comment) (read))
             ((char-quote? ch) (read-quote))
             ((char-string? ch) (read-string))
             ((char-character? ch) (read-char-quote)) ; #\X
             ((char-numeric? ch) (read-number ch))
             (else (read-identifier ch))))     

     (define (read-list)
       (define ch (read-char))
       (cond ((char-right-paren? ch) '())
             ((char-dot? ch) (car (read-list)))
             ((char-left-paren? ch) (cons (read-list) (read-list)))
             ((char-whitespace? ch) (read-list))
             ((char-comment? ch) (read-comment) (read-list))
             ((char-quote? ch) (cons (read-quote) (read-list)))
             ((char-string? ch) (cons (read-string) (read-list)))
             ((char-character? ch) (cons (read-char-quote) (read-list)))
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
  (define (printer line) (display line) (newline))
  (init-generators)
  (let ((res (compile (append bootstrap (list exp)) function-res '())))
    (map printer c-string-list)
    (display bootstrap-c-code)
    (add-c-function 'startup '(env) res)
    (map (lambda (function) (map printer function) (newline)) c-function-list)
    'ok))

(compiler (read)) ;; read input from standard input.
)

