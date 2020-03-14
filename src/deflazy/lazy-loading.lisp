(defpackage #:deflazy
  (:use #:cl)
  (:export
   #:getfnc
   #:deflazy
   #:refresh
   #:flush-refreshes
   #:cleanup-node-value))

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
(defun redefine-node (fun deps name &optional (namespace *namespace*))
  (let ((dependencies (mapcar
		       (lambda (dep-name)
			 (multiple-value-bind (node existp) (get-node dep-name)
			   (unless existp
			     (error "node node named:~a ~%while loading name" dep-name))
			   node))
		       deps))
	(node (ensure-node name namespace)))
    (dependency-graph:%redefine-node fun node dependencies name)))

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

(defun no-named-node (id)
  (error "no lazy load named as ~a" id))

(defun get-value (id &optional (namespace *namespace*))
  (with-named-node (node) id
		   (dependency-graph:%get-value node)))

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

(defun %map-dependents2 (node fun test)
  (dependency-graph:with-locked-node (node nil)
    (dolist (dependent (dependency-graph:node-dependents node))
      (when (funcall test dependent)
	(funcall fun dependent)
	(%map-dependents2 dependent fun test)))))

#+nil
(defun map-dependents (name fun &optional (namespace *namespace*))
  (with-named-node (node) name
		   (%map-dependents node fun)))

#+nil
(defun print-dependents (name)
  (map-dependents name #'print))

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

;;;;;
(utility:eval-always
  (defun %defnode (deps body)
    (let ((lambda-args ())
	  (node-deps ()))
      (dolist (item deps)
	(if (symbolp item)
	    (progn (push item lambda-args)
		   (push item node-deps))
	    (destructuring-bind (var dep) item
	      (push var lambda-args)
	      (push dep node-deps))))
      (values `(lambda ,lambda-args ,@body)
	      node-deps))))
;;;;;[FIXME]: clean this area up with dependency graph 
(defmacro deflazy (name (&rest deps) &rest gen-forms)
  `(eval-when (:load-toplevel :execute)
     (refresh-new-node ',name)
     ,(multiple-value-bind
	    (fun node-deps) (%defnode deps gen-forms)
	`(redefine-node ,fun
			',node-deps
			;;,(cons 'list node-deps)
			',name))))

(defun ensure-node (name &optional (namespace *namespace*))
  (or (multiple-value-bind (node existsp)
	  (get-node name)
	(if existsp node nil))
      (let ((new (make-instance 'dependency-graph:node)))
	(set-node name new namespace)
	new)))

;;;;queue node to be unloaded if it already has stuff in it 
(defun refresh-new-node (name)
  (let ((node (ensure-node name)))
    (unless (= 0 (dependency-graph:node-timestamp node))
      (refresh name))))

(defparameter *refresh* (make-hash-table :test 'eq))
(defparameter *refresh-lock* (bordeaux-threads:make-recursive-lock "refresh"))
(defun refresh (name &optional (main-thread nil))
  (if main-thread
      (%refresh name)
      (bordeaux-threads:with-recursive-lock-held (*refresh-lock*)
	(setf (gethash name *refresh*) t))))
(defun flush-refreshes ()
  (bordeaux-threads:with-recursive-lock-held (*refresh-lock*)
    (let ((length (hash-table-count *refresh*)))
      (unless (zerop length)
	(utility:dohash (name value) *refresh*
			(declare (ignore value))
			(%refresh name))
	(clrhash *refresh*)))))

(defun %refresh (name)
  (let ((node (get-node name)))
    (when node
      (dependency-graph:touch-node node)
      (clean-and-invalidate-node node)
      (map-dependents2
       name
       #'clean-and-invalidate-node
       #'dependency-graph:dirty-p))))

(defun getfnc (name)
  (get-value name))

(defgeneric cleanup-node-value (object))
(defmethod cleanup-node-value ((object t))
  (declare (ignorable object)))
(defun cleanup-node (node)
  (let ((value (dependency-graph:node-value node)))
    (cleanup-node-value value)))

(defun clean-and-invalidate-node (node)
  (when (dependency-graph:node-state node)
    (cleanup-node node))
  (dependency-graph:%invalidate-node node))

;;;tests
;;[TODO] -> move to test file
#+nil
(deflazy what ()
  45)
#+nil
(deflazy foobar ((what what))
  (print what))
