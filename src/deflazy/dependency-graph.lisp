(defpackage :dependency-graph
  (:use #:cl)
  (:export
   #:*namespace*
   #:%defnode
   #:redefine-node
   #:ensure-node
   #:node-timestamp
   #:touch-node
   #:map-dependents2
   #:dirty-p
   #:get-value
   #:node-value
   #:node-state
   #:%invalidate-node
   #:get-node))
(in-package :dependency-graph)

(struct-to-clos:struct->class
 (defstruct node
   (state ;fulfilled or unfulfilled
    nil)
   (timestamp ;last time fulfilled
    0)
   (value nil)
   (fun nil)
   (fun-old nil)
   (arguments nil)
   #+nil
   (dependencies-symbols nil)
   (dependencies nil) 
   (dependencies-timestamps nil)  
   (lock (bordeaux-threads:make-recursive-lock))
   (lock-value nil)
   (dependents nil)
   #+nil
   (height ;height of node in dependency tree
    nil)
   (name "anon")))

(set-pprint-dispatch
 'node
 (lambda (stream node)
   (write (node-name node) :stream stream :escape nil :case :downcase)))

(defvar *namespace* (make-hash-table :test 'eq))

(progn
  (defun get-node (id &optional (namespace *namespace*))
    (gethash id namespace))
  (defun set-node (id node &optional (namespace *namespace*))
    (setf (gethash id namespace) node)))


(defun ensure-node (name &optional (namespace *namespace*))
  (or (multiple-value-bind (node existsp)
	  (get-node name)
	(if existsp node nil))
      (let ((new (make-instance 'node)))
	(set-node name new namespace)
	new)))

(defmacro with-t (place &body body)
  `(unwind-protect
	(progn
	  (setf ,place t) ;;;to catch circular dependencies
	  ,@body)
     (setf ,place nil)))
(defmacro with-locked-node ((node &optional else) &body body)
  `(with-locked-lock (node)
     (if (node-lock-value ,node)
	 ,else
	 (with-t (node-lock-value ,node)
	   ,@body))))

(defmacro with-locked-lock ((node) &body body)
  `(bordeaux-threads:with-recursive-lock-held ((node-lock ,node))
     ,@body))

(defun %get-value (node)
  (if (node-state node)
      (node-value node)
      (with-locked-node (node (error "circular dependency: ~a" node))
	(let ((dependencies (node-dependencies node)))
	  (let ((args (map-into (node-arguments node)
				#'%get-value
				dependencies)))
	    (map-into (node-dependencies-timestamps node)
		      #'node-timestamp
		      dependencies)
	    #+nil
	    (update-node-height node)
	    (let ((value (apply (node-fun node)
				args)))
	      (prog1 value
		(setf (node-value node) value)
		(touch-node node)
		(setf (node-fun-old node)
		      (node-fun node))
		(setf (node-state node) t))))))))

#+nil
(defun update-node-height (node)
  (setf (node-height node)
	(1+ (reduce #'max (node-dependencies node)
		    :key
		    #'height
		    :initial-value 0))))

(defun make-node (func deps node)
  (with-locked-lock (node)
    (with-slots (state fun dependencies) node
      (let ((deps-same? (equal deps dependencies))
	    (fun-same? (eq fun func))
	    (deps-len (list-length deps)))
	#+nil
	(when (zerop deps-len)
	  (setf (height node) 0))
	(setf (node-arguments node)
	      (make-list deps-len))
	(setf (node-dependencies-timestamps node)
	      (make-list deps-len :initial-element 0))
	(unless (and deps-same?
		     fun-same?)
	  (setf state nil))
	(unless fun-same?
	  (setf fun func))
	(unless deps-same?
	  (setf dependencies deps))))
    node))

(defun ensure-dependent (dependent dependency)
  (with-locked-lock (dependent)
    (with-locked-lock (dependency)
      (symbol-macrolet ((place (node-dependents dependency)))
	(unless (find dependent place)
	  (push dependent place))))))
(defun remove-dependent (dependent dependency)
  (with-locked-lock (dependent)
    (with-locked-lock (dependency)
      (symbol-macrolet ((place (node-dependents dependency)))
	(setf place (delete dependent place))))))

(defun redefine-node (fun deps name &optional (namespace *namespace*))
  (let ((dependencies (mapcar (lambda (x) (ensure-node x namespace)) deps))
	(node (ensure-node name namespace)))
    (with-locked-lock (node)
      (let ((old-dependencies (node-dependencies node)))
	#+nil
	(setf (dependencies-symbols node) deps)
	(setf (node-name node) name)
	(map nil (lambda (x) (remove-dependent node x)) (set-difference old-dependencies dependencies))
	(map nil (lambda (x) (ensure-dependent node x)) (set-difference dependencies old-dependencies))
	(make-node fun dependencies node)
	node))))

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
	    node-deps)))

(defun touch-node (node)
  (incf (node-timestamp node)))

(defun %invalidate-node (node)
  (with-slots ((state state)) node
    (touch-node node)
    (setf state nil)))

(defmacro any (&rest forms)
  (nth (random (length forms)) forms))

(defun dirty-p (node)
  (with-slots (dependencies-timestamps dependencies fun fun-old) node
    (or (not (eq fun fun-old))
	(block nil
	  (do ((stamp dependencies-timestamps (cdr stamp))
	       (arg dependencies (cdr arg)))
	      ((not (any arg
			 stamp))
	       nil)
	      ;;;iterate old timestamps and see if any differ now
	    (when (not (= (car stamp)
			  (node-timestamp (car arg))))
	      (return t)))))))

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
		   (%get-value node)))

(defun get-value-no-update (id &optional (namespace *namespace*))
  (with-named-node (node) id
		   (node-value node)))

(defun invalidate-node (id &optional (namespace *namespace*))
  (with-named-node (node) id
		   (%invalidate-node node)))

#+nil
(defun %map-dependents (node fun)
  (with-locked-node (node nil)
    (dolist (dependent (dependents node))
      (funcall fun dependent)
      (%map-dependents dependent fun))))

(defun %map-dependents2 (node fun test)
  (with-locked-node (node nil)
    (dolist (dependent (node-dependents node))
      (when (funcall test dependent)
	(funcall fun dependent)
	(%map-dependents2 dependent fun test)))))

#+nil
(defun map-dependents (name fun &optional (namespace *namespace*))
  (with-named-node (node) name
		   (%map-dependents node fun)))

#+nil
(defun print-dependents (name)
  (map-dependents name #'print *stuff*))

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

