(defpackage #:deflazy
  (:use #:cl)
  (:export
   #:getfnc
   #:deflazy
   #:refresh
   #:defdep))

(in-package :deflazy)
;;;;

;;#:*namespace*
;; #:ensure-node
;; #:redefine-node
;; #:map-dependents2
;; #:%invalidate-node
;; #:get-node
#+nil
(defvar *namespace* (make-hash-table :test 'eq))
#+nil
(progn
  (defun get-node (id &optional (namespace *namespace*))
    (gethash id namespace))
  (defun set-node (id node &optional (namespace *namespace*))
    (setf (gethash id namespace) node)))

;;;;convenience stuff below
#+nil
(defmacro with-named-node ((node-var &optional (namespace 'namespace))
				       name &optional t-form
					      (nil-form
					       `(no-named-node ,name)))
  (let ((existsp (gensym)))
    `(multiple-value-bind (,node-var ,existsp) (get-node ,name ,namespace)
       (if ,existsp
	   ,t-form
	   ,nil-form))))
#+nil
(defun no-named-node (id)
  (error "no lazy load named as ~a" id))


(defun refresh (thing &optional (same-thread nil))
  ;;refresh symbol -> look it up
  ;;refresh node -> refresh node
  (dependency-graph:%%refresh
   (etypecase thing
     (symbol
      (symbol-value thing))
     (dependency-graph:node
      thing)) :same-thread same-thread)
  (values))
(defun getfnc (thing
	       ;; &optional (namespace *namespace*)
		    )
  ;;getfnc symbol -> look it up
  ;;getfnc node -> fulfill node
  (dependency-graph:%get-value
   (etypecase thing
     (symbol
      (symbol-value thing))
     (dependency-graph:node
      thing))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;essentially deflazy 2.0?
;;No--- deflazy can be reevaluated, this 
;;its not named, it pairs with defparameter
#+nil
(defmacro defdep (name (&rest pairs) &body body)
  (let ((lambda-args (mapcar 'first pairs))
	(dependencies (mapcar 'second pairs))
	(fun-name
	 ;;FIXME::safety through obscurity.
	 ;;FIXME::pollutes the package?
	 (utility:symbolicate2 (list "----defdep---" name))))
    `(progn
       (load-time-value
	(defun ,fun-name ,lambda-args
	  ,@body))
       (%defdep ',fun-name (list ,@dependencies) ',lambda-args ',name))))
(defun %defdep (fun dependencies tags &optional name)
  (let
      ;;Make the node
      ((node (make-instance 'dependency-graph:node)))
    (dependency-graph:%redefine-node fun node dependencies name tags)
    node))

;;;;Test
#+nil
(defun defdep-test (&optional what)
  ;;Run this function in the repl, then
  ;;redefine it here. 
  (let* ((foo (defdep defdep-foo-test ((a 56)
				       (b "hello"))
		(let ((c (random 100000)))		  
		  (list a b c)))))
    (dotimes (i 10)
      (unless what
	(sleep 1))
      (refresh foo t)
      (print (getfnc foo)))
    foo))

;;(dependency-graph:dirty-p *bar*)
;;(defparameter *bar* (defdep-test t))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;

;;;tests
;;[TODO] -> move to test file
#+nil
(deflazy what ()
  45)
#+nil
(deflazy foobar ((what what))
  (print what))

#+nil
(defun get-value-no-update (id &optional (namespace *namespace*))
  (with-named-node (node) id
		   (dependency-graph:node-value node)))
#+nil
(defun invalidate-node (id &optional (namespace *namespace*))
  (with-named-node (node) id
		   (dependency-graph:%invalidate-node node)))

#+nil
(defun %map-dependents (node fun)
  (with-locked-node (node nil)
    (dolist (dependent (dependents node))
      (funcall fun dependent)
      (%map-dependents dependent fun))))

#+nil
(defun map-dependents (name fun &optional (namespace *namespace*))
  (with-named-node (node) name
		   (%map-dependents node fun)))

#+nil
(defun print-dependents (name)
  (map-dependents name #'print))

#+nil
(defun map-dependents2 (name fun test &optional (namespace *namespace*))
  (with-named-node (node) name
		   (%map-dependents2 node fun test)))

#+nil
(defmacro defnode (name deps &body body)
  (multiple-value-bind (fun node-deps) (%defnode deps body)
    `(reload-node ,fun ',node-deps ',name)))
#+nil
(progn
  (defnode b () (print 2893))
  (defnode a (b) (* b b)))

#+nil
(progn
  (remove-node 'c)
  (defnode c (c) c))
;;no longer keep track of node
#+nil
(defun remove-node (id &optional (namespace *namespace*))
  (with-named-node
      (node) id
      (progn
	(remhash id namespace)
	(map nil
	     (lambda (x) (remove-dependent node x))
	     (dependencies node)))))
;;;;;
#+nil
(defun test90 ()
  (deflazy foo () 34)
  (deflazy bar () 47)
  (deflazy test1 (foo ((bar :bar)))
    (list foo bar))
  (getfnc 'test1)
  (dependency-graph:get-mutable-cell-by-name 
   (get-node 'test1) :bar))
#+nil
(defvar *bar* (defdep bar ((hello "world") (foo 34))
		(list hello foo)))
#+nil
(defvar *yon* (defdep yon ((what *bar*) (what2 *bar*))
		(list what what2)))
#+nil
(defvar *bar* (defdep bar ((hello "wod") (foo 34))
		(list hello foo)))

(symbol-macrolet ((sym :dlaz-lambda-list))
  (defun set-lambda-list (name lambda-list)
    (setf (get name sym) lambda-list))
  (defun get-lambda-list (name)
    (get name sym)))
;;;;;;

(defmacro dlaz (form)
  ;;Wrap defun and apply so we et correct slime hints.
  ;;defun has to be a simple lambda list with no special keywords.
  ;;no &rest, &optional, &key
  (ecase (car form)
    ((defun)
     ;;define the creator function, saving its lambda-list for future reference
     ;;The lambda-list
     (let ((lambda-list (third form))
	   (name (second form)))
       `(progn
	  (set-lambda-list ',name ',lambda-list)
	  ,form)
       ))
    ((apply)
     ;;
     (let ((name (second form)))
       (utility:once-only (name)
	 `(%defdep ,name (list* ,@ (nthcdr 2 form)) (get-lambda-list ,name) ,name))))))
(defmacro lazgen (name &rest args)
  `(dlaz (apply ',name ,@args nil)))
(defmacro define-lazgen (name lambda-list &body body)
  `(dlaz (defun ,name ,lambda-list ,@body)))

;;Run test-dlaz, and redefine foo while it is running.
(dlaz ;;->saves the lambda-list for future reference and swapping.
 (defun foo (a b c)
   (list a b c (load-time-value (random 10000)))))
;;->saves the lambda-list for future reference and swapping.
(define-lazgen foo2 (a b c)
  (declare (ignorable a b c))
  (list (load-time-value (random 10000))))

(defun test-dlaz ()
  (let*
      ((foo1 (dlaz ;;creates new lazy nodes/promises
	      (apply 'foo 1 2 4 ())))
        ;;creates new lazy nodes/promises
       (foo2 (lazgen foo 1 foo1 foo1))
       (foo2-1 (lazgen foo2 foo2 foo2 foo2)))
    (dotimes (i 10)
      (sleep 1)
      ;;
      (refresh foo1 t)
      (print
       ;;fulfills the promise
       (getfnc foo2-1)))))

;;deflazy replacement?
(progn
  (define-lazgen foo3 (a b c)
    (list a b c (random 10000)))
  (defvar foo3 (lazgen foo3 0 1 2)))

(progn
  (define-lazgen foo4 (x y)
    (list x (random 10000) y))
  (defvar foo4 (lazgen foo4 foo3 foo3)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#+nil
(utility:eval-always
  (defun %defnode (specification)
    ;;Convert a deflazy specification into a
    ;;function and body.
    (let (lambda-args
	  node-deps
	  tags
	  (count 0))
      (dolist (item specification)
	(let (dep
	      tag
	      arg)
	  
	  (trivia:match
	   item
	   ((list* item adep)
	    (when adep
	      ;;(... baz)
	      (setf dep (car adep)))
	    
	    (trivia:match
	     item
	     ((list* var a-tag)
	      ;;((foo ...) ...)
	      (setf arg var)
	      (when a-tag
		;;((foo bar) ...)
		(setf tag (car a-tag))))
	     ;;(foo ...)
	     (_
	      (setf arg item))))
	   ;;foo
	   (_
	    (setf arg item
		  dep item)))

	  (push arg lambda-args)
	  (push (or dep arg) node-deps)
	  (when tag
	    (push (cons tag count) tags)))
	(incf count))
      (values (nreverse lambda-args)
	      (nreverse node-deps)
	      tags))))
#+nil
(defun test-defnode ()
  (print (multiple-value-list
	  (%defnode '(foo bar (baz) ((yon qux)) (woo foobar) ((1 2) 3))))))

;;;;;[FIXME]: clean this area up with dependency graph 
(defmacro deflazy (name (&rest deps) &body body)
  ;;Careful -> defines both a variable and a function
  ;;with the same name
  `(progn
     (define-lazgen ,name ,(mapcar (lambda (x)
				     (etypecase x
				       (symbol x)
				       (cons (car x))))
				   deps)
       ,@body)
     (defvar ,name 
       (lazgen ,name
	       ,@(mapcar (lambda (x)
			   (etypecase x
			     (symbol x)
			     (cons (cdr x))))
			 deps)))
     (dependency-graph:refresh-old-node ,name)))

#+nil
(defun %deflazy (fun dependencies tags &optional name)
  (let
      ;;Make sure the node is in the namespace
      ((node
	(or
	 ;;It either exists
	 (multiple-value-bind (node existsp) (get-node name)
	   (if existsp
	       node
	       nil))
	 ;;Otherwise make it
	 (let ((new (make-instance 'dependency-graph:node)))
	   (set-node name new)
	   new))))
    (dependency-graph:with-locked-lock (node)
      ;;Queue it for cleanup if it already exists
      (dependency-graph:refresh-old-node node)
      (dependency-graph:%redefine-node
       fun
       node
       (mapcar
	(lambda (dep-name)
	  (multiple-value-bind (node existp) (get-node dep-name)
	    (unless existp
	      (error "node node named:~a ~%while loading name" dep-name))
	    node))
	dependencies
	;;,(cons 'list node-deps)
	)
       name
       tags))
    node))
