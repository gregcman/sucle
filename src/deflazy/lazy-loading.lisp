(defpackage #:deflazy
  (:use #:cl)
  (:export
   #:getfnc
   #:deflazy
   #:refresh
   #:flush-refreshes))

(in-package :deflazy)
;;;;

;;#:*namespace*
;; #:ensure-node
;; #:redefine-node
;; #:map-dependents2
;; #:%invalidate-node
;; #:get-node

(defvar *namespace* (make-hash-table :test 'eq))

(progn
  (defun get-node (id &optional (namespace *namespace*))
    (gethash id namespace))
  (defun set-node (id node &optional (namespace *namespace*))
    (setf (gethash id namespace) node)))

;;;;convenience stuff below
(defmacro with-named-node ((node-var &optional (namespace 'namespace))
				       name &optional t-form
					      (nil-form
					       `(no-named-node ,name)))
  (let ((existsp (gensym)))
    `(multiple-value-bind (,node-var ,existsp) (get-node ,name ,namespace)
       (if ,existsp
	   ,t-form
	   ,nil-form))))
(defun no-named-node (id)
  (error "no lazy load named as ~a" id))


(defun refresh (name &optional (main-thread nil))
  (let ((node (get-node name)))
    (when node      
      (%%refresh node main-thread))))
(defun getfnc (id &optional (namespace *namespace*))
  (with-named-node (node) id
		   (dependency-graph:%get-value node)))

;;;;;
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
  (multiple-value-bind (lambda-args dependencies tags) (%defnode deps)
    `(eval-when (:load-toplevel :execute)
       (%deflazy ',name (lambda ,lambda-args ,@body) ',dependencies ',tags))))

(defun %deflazy (name fun dependencies tags)
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
    ;;Queue it for cleanup if it already exists
    (refresh-old-node node)
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
     tags)
    node))

;;;;Refreshing system
;;;;queue node to be unloaded if it already has stuff in it 
(defun refresh-old-node (node)
  (when (not (zerop (dependency-graph:node-timestamp node)))
    (%%refresh node)))

(defparameter *refresh* (make-hash-table :test 'eq))
(defparameter *refresh-lock* (bordeaux-threads:make-recursive-lock "refresh"))
(defun %%refresh (node &optional (main-thread nil))
  (if main-thread
      (dependency-graph:%refresh node)
      (bordeaux-threads:with-recursive-lock-held (*refresh-lock*)
	(setf (gethash node *refresh*) t))))
(defun flush-refreshes ()
  (bordeaux-threads:with-recursive-lock-held (*refresh-lock*)
    (let ((length (hash-table-count *refresh*)))
      (unless (zerop length)
	(utility:dohash (node value) *refresh*
			(declare (ignore value))
			(dependency-graph:%refresh node))
	(clrhash *refresh*)))))
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

(defun test90 ()
  (deflazy foo () 34)
  (deflazy bar () 47)
  (deflazy test1 (foo ((bar :bar)))
    (list foo bar))
  (getfnc 'test1)
  (dependency-graph:get-mutable-cell-by-name 
   (get-node 'test1) :bar))
