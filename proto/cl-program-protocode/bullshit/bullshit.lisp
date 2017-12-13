(in-package :bullshit)

  "Function to indent the arguments of a Lisp function call.
This is suitable for use as the value of the variable
`lisp-indent-function'.  INDENT-POINT is the point at which the
indentation function is called, and STATE is the
`parse-partial-sexp' state at that position.  Browse the
`lisp-indent' customize group for options affecting the behavior
of this function.

If the indentation point is in a call to a Lisp function, that
function's common-lisp-indent-function property specifies how
this function should indent it.  Possible values for this
property are:

* defun, meaning indent according to `lisp-indent-defun-method';
  i.e., like (4 &lambda &body), as explained below.

* any other symbol, meaning a function to call.  The function should
  take the arguments: PATH STATE INDENT-POINT SEXP-COLUMN NORMAL-INDENT.
  PATH is a list of integers describing the position of point in terms of
  list-structure with respect to the containing lists.  For example, in
  ((a b c (d foo) f) g), foo has a path of (0 3 1).  In other words,
  to reach foo take the 0th element of the outermost list, then
  the 3rd element of the next list, and finally the 1st element.
  STATE and INDENT-POINT are as in the arguments to
  `common-lisp-indent-function'.  SEXP-COLUMN is the column of
  the open parenthesis of the innermost containing list.
  NORMAL-INDENT is the column the indentation point was
  originally in.  This function should behave like `lisp-indent-259'.

* an integer N, meaning indent the first N arguments like
  function arguments, and any further arguments like a body.
  This is equivalent to (4 4 ... &body).

* a list starting with `as' specifies an indirection: indentation is done as
  if the form being indented had started with the second element of the list.

* any other list.  The list element in position M specifies how to indent the
  Mth function argument.  If there are fewer elements than function arguments,
  the last list element applies to all remaining arguments.  The accepted list
  elements are:

  * nil, meaning the default indentation.

  * an integer, specifying an explicit indentation.

  * &lambda.  Indent the argument (which may be a list) by 4.

  * &rest.  When used, this must be the penultimate element.  The
    element after this one applies to all remaining arguments.

  * &body.  This is equivalent to &rest lisp-body-indent, i.e., indent
    all remaining elements by `lisp-body-indent'.

  * &whole.  This must be followed by nil, an integer, or a
    function symbol.  This indentation is applied to the
    associated argument, and as a base indent for all remaining
    arguments.  For example, an integer P means indent this
    argument by P, and all remaining arguments by P, plus the
    value specified by their associated list element.

  * a symbol.  A function to call, with the 6 arguments specified above.

  * a list, with elements as described above.  This applies when the
    associated function argument is itself a list.  Each element of the list
    specifies how to indent the associated argument.

For example, the function `case' has an indent property
\(4 &rest (&whole 2 &rest 1)), meaning:
  * indent the first argument by 4.
  * arguments after the first should be lists, and there may be any number
    of them.  The first list element has an offset of 2, all the rest
    have an offset of 2+1=3."

(defparameter *common-lisp-init-standard-indentation*
  (quote
   ((block (4 &body))
    (case        (4 &rest (&whole 2 &rest 1)))
    (ccase       (as case))
    (ecase       (as case))
    (typecase    (as case))
    (etypecase   (as case))
    (ctypecase   (as case))
    (catch (4 &body))
    (cond        (&rest (&whole 2 &rest nil)))
    ;; for DEFSTRUCT
    (:constructor (4 &lambda))
    (defvar      (4 2 2))
    (defclass    (6 (&whole 4 &rest 1)
		    (&whole 2 &rest 1)
		    (&whole 2 &rest 1)))
    (defconstant (as defvar))
    (defcustom   (4 2 2 2))
    (defparameter     (as defvar))
    (defconst         (as defcustom))
    (define-condition (as defclass))
    (define-modify-macro (4 &lambda &body))
    (defun       (4 &lambda &body))
    (defgeneric  (4 &lambda &body))
    (define-setf-method   (as defun))
    (define-setf-expander (as defun))
    (defmacro     (as defun))
    (defsubst     (as defun))
    (deftype      (as defun))
    (defpackage  (4 2))
    (defstruct   ((&whole 4 &rest (&whole 2 &rest 1))
		   &rest (&whole 2 &rest 1)))
    (destructuring-bind (&lambda 4 &body))
    (dolist      ((&whole 4 2 1) &body))
    (dotimes     (as dolist))
    (eval-when   (4 &body))
    (flet        ((&whole 4 &rest (&whole 1 4 &lambda &body)) &body))
    (labels         (as flet))
    (macrolet       (as flet))
    (generic-flet   (as flet))
    (generic-labels (as flet))
    (handler-case (4 &rest (&whole 2 &lambda &body)))
    (restart-case (as handler-case))
    (if          (&rest nil))
    (let         ((&whole 4 &rest (&whole 1 1 2)) &body))
    (let*         (as let))
    (compiler-let (as let))
    (handler-bind (as let))
    (restart-bind (as let))
    (locally (4 &body))   
    (multiple-value-bind ((&whole 6 &rest 1) 4 &body))
    (multiple-value-call (4 &body))
    (multiple-value-prog1 1)
    (multiple-value-setq (4 2))
    (multiple-value-setf (as multiple-value-setq))
    (pprint-logical-block (4 2))
    (print-unreadable-object ((&whole 4 1 &rest 1) &body)) 
    (prog1 (4 &body))
    (prog2 (4 4 &body))
    (progn (&body))
    (progv       (4 4 &body))
    (return (&body))
    (return-from (nil &body))
    (symbol-macrolet (as let))   
    (throw (4 &body))
    (unless (4 &body))
    (unwind-protect (5 &body))
    (when (4 &body))
    (with-accessors          (as multiple-value-bind))
    (with-compilation-unit   ((&whole 4 &rest 1) &body))
    (with-condition-restarts (as multiple-value-bind))
    (with-output-to-string (4 2))
    (with-slots              (as multiple-value-bind))
    (with-standard-io-syntax (2)))))

(progno
 (quote
  ((defsetf      lisp-indent-defsetf);;;;;
   (defmethod   lisp-indent-defmethod);;;;;;;;;;;;;;;;;;;;;;;;;;;
   (do          lisp-indent-do);;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   (do*         (as do));;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   (if*         common-lisp-indent-if*);;;;;;;;;;;;;;;;;;;;
   (lambda      (&lambda &rest lisp-indent-function-lambda-hack));;;;;;;;;;;;;;;;;;;
   (named-lambda (4 &lambda &rest lisp-indent-function-lambda-hack));;;;;;;;;;;;;;;
   (loop           lisp-indent-loop);;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   (:method        lisp-indent-defmethod) ; in `defgeneric';;;;;;;;;;;;;;;;;;;;;;;
   ;; Combines the worst features of BLOCK, LET and TAGBODY
   (prog        (&lambda &rest lisp-indent-tagbody));;;;;;;;;;;;
   (prog* (as prog));;;;;;;;;;;;;;;;;;;;
   (tagbody     lisp-indent-tagbody))))


 (progno
  "tagbody stuff"
  (defcustom lisp-tag-indentation 1
    "Indentation of tags relative to containing list.
This variable is used by the function `lisp-indent-tagbody'.")
  (defcustom lisp-tag-body-indentation 3
    "Indentation of non-tagged lines relative to containing list.
This variable is used by the function `lisp-indent-tagbody' to indent normal
lines (lines without tags).
The indentation is relative to the indentation of the parenthesis enclosing
the special form.  If the value is t, the body of tags will be indented
as a block at the same indentation as the first s-expression following
the tag.  In this case, any forms before the first tag are indented
by `lisp-body-indent'."))

(progno
 "backquote stuff"
 (defcustom lisp-backquote-indentation t
   "Whether or not to indent backquoted lists as code.
If nil, indent backquoted lists as data, i.e., like quoted lists."))


(progno
 "loop shit"
 (defcustom lisp-loop-indent-subclauses t
  "Whether or not to indent loop subclauses.")
 (defcustom lisp-simple-loop-indentation 2
   "Indentation of forms in simple loop forms.")
 (defcustom lisp-loop-clauses-indentation 2
   "Indentation of loop clauses if `loop' is immediately followed by a newline.")
 (defcustom lisp-loop-indent-body-forms-relative-to-loop-start nil
   "When true, indent loop body clauses relative to the open paren of the loop
form, instead of the keyword position.")
 (defcustom lisp-loop-body-forms-indentation 3
   "Indentation of loop body clauses.")
 (defcustom lisp-loop-indent-forms-like-keywords nil
   "Whether or not to indent loop subforms just like
loop keywords. Only matters when `lisp-loop-indent-subclauses'
is nil."))

(progno
 "lambda list stuff"
 (defcustom lisp-align-keywords-in-calls t
   "Whether to align keyword arguments vertically or not.
If t (the default), keywords in contexts where no other
indentation rule takes precedence are aligned like this:

\(make-instance 'foo :bar t
                    :quux 42)

If nil, they are indented like any other function
call arguments:

\(make-instance 'foo :bar t
               :quux 42)")

 (defcustom lisp-lambda-list-indentation t
   "Whether to indent lambda-lists specially. Defaults to t. Setting this to
nil makes `lisp-lambda-list-keyword-alignment',
`lisp-lambda-list-keyword-parameter-alignment', and
`lisp-lambda-list-keyword-parameter-indentation' meaningless, causing
lambda-lists to be indented as if they were data:

\(defun example (a b &optional o1 o2
                o3 o4
                &rest r
                &key k1 k2
                k3 k4)
  #|...|#)")

 (defcustom lisp-lambda-list-keyword-alignment nil
   "Whether to vertically align lambda-list keywords together.
If nil (the default), keyworded lambda-list parts are aligned
with the initial mandatory arguments, like this:

\(defun foo (arg1 arg2 &rest rest
            &key key1 key2)
  #|...|#)

If non-nil, alignment is done with the first keyword
\(or falls back to the previous case), as in:

\(defun foo (arg1 arg2 &rest rest
                      &key key1 key2)
  #|...|#)")

 (defcustom lisp-lambda-list-keyword-parameter-indentation 2
   "Indentation of lambda list keyword parameters.
See `lisp-lambda-list-keyword-parameter-alignment'
for more information.")

 (defcustom lisp-lambda-list-keyword-parameter-alignment nil
   "Whether to vertically align lambda-list keyword parameters together.
If nil (the default), the parameters are aligned
with their corresponding keyword, plus the value of
`lisp-lambda-list-keyword-parameter-indentation', like this:

\(defun foo (arg1 arg2 &key key1 key2
                        key3 key4)
  #|...|#)

If non-nil, alignment is done with the first parameter
\(or falls back to the previous case), as in:

\(defun foo (arg1 arg2 &key key1 key2
                           key3 key4)
  #|...|#)"))


(defvar lisp-indent-defun-method '(4 &lambda &body)
  "Defun-like indentation method.
This applies when the value of the `common-lisp-indent-function' property
is set to `defun'.")


(define-common-lisp-style "basic"
  "This style merely gives all identation variables their default values,
   making it easy to create new styles that are proof against user
   customizations. It also adjusts comment indentation from default.
   All other predefined modes inherit from basic."
  (:variables
   (lisp-indent-maximum-backtracking 6)
   (lisp-tag-indentation 1)
   (lisp-tag-body-indentation 3)
   (lisp-backquote-indentation t)
   (lisp-loop-indent-subclauses t)
   (lisp-loop-indent-forms-like-keywords nil)
   (lisp-simple-loop-indentation 2)
   (lisp-align-keywords-in-calls t)
   (lisp-lambda-list-indentation t)
   (lisp-lambda-list-keyword-alignment nil)
   (lisp-lambda-list-keyword-parameter-indentation 2)
   (lisp-lambda-list-keyword-parameter-alignment nil)
   (lisp-indent-defun-method (4 &lambda &body))
   ;; Without these (;;foo would get a space inserted between
   ;; ( and ; by indent-sexp.
   (comment-indent-function (lambda () nil))
   (lisp-loop-clauses-indentation 2)
   (lisp-loop-indent-body-forms-relative-to-loop-start nil)
   (lisp-loop-body-forms-indentation 3)))

(define-common-lisp-style "classic"
  "This style of indentation emulates the most striking features of 1995
   vintage cl-indent.el once included as part of Slime: IF indented by two
   spaces, and CASE clause bodies indentented more deeply than the keys."
  (:inherit "basic")
  (:variables
   (lisp-lambda-list-keyword-parameter-indentation 0))
  (:indentation
   (case (4 &rest (&whole 2 &rest 3)))
   (if   (4 2 2))))

(define-common-lisp-style "modern"
  "A good general purpose style. Turns on lambda-list keyword and keyword
   parameter alignment, and turns subclause aware loop indentation off.
   (Loop indentation so because simpler style is more prevalent in existing
   sources, not because it is necessarily preferred.)"
  (:inherit "basic")
  (:variables
   (lisp-lambda-list-keyword-alignment t)
   (lisp-lambda-list-keyword-parameter-alignment t)
   (lisp-lambda-list-keyword-parameter-indentation 0)
   (lisp-loop-indent-subclauses nil)))

(define-common-lisp-style "sbcl"
  "Style used in SBCL sources. A good if somewhat intrusive general purpose
   style based on the \"modern\" style. Adds indentation for a few SBCL
   specific constructs, sets indentation to use spaces instead of tabs,
   fill-column to 78, and activates whitespace-mode to show tabs and trailing
   whitespace."
  (:inherit "modern")
  (:eval
   (whitespace-mode 1))
  (:variables
   (whitespace-style (tabs trailing))
   (indent-tabs-mode nil)
   (comment-fill-column nil)
   (fill-column 78))
  (:indentation
   (def!constant       (as defconstant))
   (def!macro          (as defmacro))
   (def!method         (as defmethod))
   (def!struct         (as defstruct))
   (def!type           (as deftype))
   (defmacro-mundanely (as defmacro))
   (define-source-transform (as defun))
   (!def-type-translator (as defun))
   (!def-debug-command (as defun))))
