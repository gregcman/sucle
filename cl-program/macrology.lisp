(defpackage #:macrology
  (:use #:cl)
  (:nicknames #:coge)

;;;general macros
  (:export
   #:toggle
   #:dorange
   #:dobox
   #:progno
   #:ret
   #:rename
   #:null!)

;;;common rebindings
  (:export
   #:dp
   #:l
   #:mvb)

;;;code generation
  (:export
   #:gen-spec
   #:add-spec #:spec-assoc #:is-param 
   #:rp #:legalp #:get-actual-args #:defspec))

(in-package :macrology)

;;;toggle a place between true and nil
(defmacro toggle (var)
  `(setf ,var (not ,var)))

;;;comment out code the macro way!
(defmacro progno (&rest args))

;;;when the only let value is returned at the end
(defmacro ret (var bound-form &body body)
  `(let ((,var ,bound-form))
     ,@body
     ,var))

;;;used to rename something
(defmacro rename (&rest rebind-forms)
  (let (acc)
    (dolist (x rebind-forms)
      (push `(defmacro ,(second x) (&body body)
	       (cons ',(car x) body)) acc))
    (cons 'progn acc)))

;;defparamter dp is rotationally symmetric
;;lambda gave rise to the latin l
;;multiple-value-bind is really fucking long
(rename (defparameter dp)
	(lambda l)
	(multiple-value-bind mvb))

(defmacro dorange ((var times-form start-form) &rest body)
  (let ((times (gensym)))
    `(let ((,times ,times-form))
       (dobox ((,var ,times (+ ,times ,start-form))) ,@body))))

;;;iterate through a multidimensional box 
(defmacro dobox ((&rest interval-forms) &rest body)
  (let ((let-one nil)
	(let-one-declarations nil))
    (let ((body (cons 'progn body)))
      (dolist (form interval-forms)
	(multiple-value-bind (let-len temp-length let-end temp-end bod)
	    (apply #'dorange-generator body form)
	  (push let-len let-one)
	  (push temp-length let-one-declarations)
	  (push let-end let-one)
	  (push temp-end let-one-declarations)
	  (setq body bod)))
      `(progn (let ,let-one
		(declare (type fixnum ,@let-one-declarations))
		,body)))))

(defun dorange-generator (body var start-form end-form)
  (let ((temp (gensym))
	(temp2 (gensym))
	(start (gensym))
	(end (gensym)))
    (values
     `(,start ,start-form) ;;length init
     start  ;;;var names for declarations
     `(,end ,end-form)
     end   
     `(let ((,var ,start))
	(tagbody
	   (go ,temp2)
	   ,temp
	   ,body
	   (psetq ,var (1+ ,var))
	   ,temp2
	   (unless (>= ,var ,end) (go ,temp)))))))

(defmacro null! (&rest args)
  (let (acc)
    (dolist (arg args)
      (push `(setf ,arg nil) acc))
    `(progn ,@acc)))

(in-package :macrology)

;;1. start with a bunch of constant specifications
;;2. generate multiple functions according to the specifications
;;3. specifications are in a hash table which hash names bound to constants
;;4. function generating functions are toplevel so they can be tested
(defun gen-spec ()
  (make-hash-table :test 'eq))

(defun add-spec (spec body)
  (dolist (pair body)
    (setf (gethash (car pair) spec) (cdr pair)))
  spec)

(defun spec-assoc (hash)
  (let ((acc nil))
    (maphash
     (lambda (k v)
       (push (cons k v) acc))
     hash)
    acc))

;;replace all symbols which start with p!x.. with
;;the value from the hash table with keyx..
(defun is-param (symbol)
  (let ((string (symbol-name symbol)))
    (if (> (length string) 2)
	(if (string= "P!" (subseq string 0 2))
	    (intern (subseq string 2) (symbol-package symbol))))))

;;rp = replace params
(defun rp (spec code)
  (labels ((rec (piece)
	     (if (atom piece)
		 (if (symbolp piece)
		     (let ((val (is-param piece)))
		       (if val
			   (gethash val spec)
			   piece))
		     piece)
		 (cons (rec (car piece))
		       (rec (cdr piece))))))
    (rec code)))

;;below is for dealing with function headers
;;is the symbol actually a parameter or a keyword

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defun legal-p (symbol)
    (if (member symbol '(&optional &rest &body &key &optional &aux t))
	nil
	symbol))
  (defun get-actual-args (lambda-list)
    (let ((actual-args nil))
      (dolist (arg lambda-list actual-args)
	(if (atom arg)
	    (if (legal-p arg)
		(push arg actual-args))
	    (push (car arg) actual-args))))))

(defmacro defspec (name lambda-list)
  (let ((actual-args (get-actual-args lambda-list)))
    `(defun ,name ,(cons 'spec lambda-list)
       (add-spec spec ,(cons 'list (mapcar (lambda (x) (list 'cons (list 'quote x) x)) actual-args))))))
