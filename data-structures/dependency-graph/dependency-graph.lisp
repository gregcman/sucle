(defpackage :dependency-graph
  (:use #:cl #:utility))
(in-package :dependency-graph)

(defclass node ()
  ((state ;fulfilled or unfulfilled
    :accessor state
    :initform nil)
   (timestamp ;last time fulfilled
    :accessor timestamp
    :initform 0)
   (value
    :accessor value
    :initform nil)
   (fun 
    :accessor fun
    :initform nil)
   (fun-old
    :accessor fun-old
    :initform nil)
   (arguments
    :accessor arguments
    :initform nil)
   #+nil
   (dependencies-symbols
    :accessor dependencies-symbols
    :initform nil)
   (dependencies 
    :accessor dependencies
    :initform nil) 
   (dependencies-timestamps
    :accessor dependencies-timestamps
    :initform nil)
   (lock
    :accessor lock
    :initform (bordeaux-threads:make-recursive-lock))
   (lock-value
    :accessor lock-value
    :initform nil)
   (dependents
    :accessor dependents
    :initform nil)
   #+nil
   (height ;height of node in dependency tree
    :accessor height
    :initform nil)
   (name 
    :accessor name
    :initform "anon")))

(set-pprint-dispatch
 'node
 (lambda (stream node)
   (write (name node) :stream stream :escape nil :case :downcase)))

(defvar *namespace* (make-hash-table :test 'eq))
(etouq
 (let ((place '(gethash id namespace)))
   `(progn
      (defun get-node (id &optional (namespace *namespace*))
	,place)
      (defun set-node (id node &optional (namespace *namespace*))
	(setf ,place node)))))


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
     (if (lock-value ,node)
	 ,else
	 (with-t (lock-value ,node)
	   ,@body))))

(defmacro with-locked-lock ((node) &body body)
  `(bordeaux-threads:with-recursive-lock-held ((lock ,node))
     ,@body))

(defun %get-value (node)
  (if (state node)
      (value node)
      (with-locked-node (node (error "circular dependency: ~a" node))
	(let ((dependencies (dependencies node)))
	  (let ((args (map-into (arguments node)
				#'%get-value
				dependencies)))
	    (map-into (dependencies-timestamps node)
		      #'timestamp
		      dependencies)
	    #+nil
	    (update-node-height node)
	    (let ((value (apply (fun node)
				args)))
	      (prog1 value
		(setf (value node) value)
		(touch-node node)
		(setf (fun-old node)
		      (fun node))
		(setf (state node) t))))))))

#+nil
(defun update-node-height (node)
  (setf (height node)
	(1+ (reduce #'max (dependencies node)
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
	(setf (arguments node)
	      (make-list deps-len))
	(setf (dependencies-timestamps node)
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
      (symbol-macrolet ((place (dependents dependency)))
	(unless (find dependent place)
	  (push dependent place))))))
(defun remove-dependent (dependent dependency)
  (with-locked-lock (dependent)
    (with-locked-lock (dependency)
      (symbol-macrolet ((place (dependents dependency)))
	(setf place (delete dependent place))))))

(defun reload-node (fun deps name &optional (namespace *namespace*))
  (let ((dependencies (mapcar (lambda (x) (ensure-node x namespace)) deps))
	(node (ensure-node name namespace)))
    (with-locked-lock (node)
      (let ((old-dependencies (dependencies node)))
	#+nil
	(setf (dependencies-symbols node) deps)
	(setf (name node) name)
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

(defun %map-dependents (node fun)
  (with-locked-node (node nil)
    (dolist (dependent (dependents node))
      (funcall fun dependent)
      (%map-dependents dependent fun))))

(defun touch-node (node)
  (incf (timestamp node)))

(defun destroy-node (node)
  (with-slots ((value dependency-graph::value)
	       (state dependency-graph::state)) node
    (touch-node node)
    (setf value nil
	  state nil)))

(defmacro any (&rest forms)
  (nth (random (length forms)) forms))

(defun dirty-p (node)
  (with-slots (dependencies-timestamps dependencies fun fun-old) node
    (or (not (eq fun fun-old))
	(block nil
	  (do ((stamp dependencies-timestamps (cdr stamp))
	       (arg dependencies (cdr arg)))
	      ((not (any arg
			 stamp)) nil)
	      ;;;iterate old timestamps and see if any differ now
	    (when (not (= (car stamp)
			  (timestamp (car arg))))
	      (return t)))))))




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
		   (value node)))

(defun destroy-value (id &optional (namespace *namespace*))
  (with-named-node (node) id
		   (destroy-node node)))

(defun map-dependents (name fun &optional (namespace *namespace*))
  (with-named-node (node) name
		   (%map-dependents node fun)))

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

