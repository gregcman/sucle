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
 (let ((place '(gethash id *namespace*)))
   `(progn
      (defun get-node (id)
	,place)
      (defun set-node (id node)
	(setf ,place node)))))

;;no longer keep track of node
(defun remove-node (id)
  (let ((node (get-node id)))
    (when node
      (remhash id *namespace*)
      (map nil
	   (lambda (x) (remove-dependent node x))
	   (dependencies node)))))

(defun ensure-node (name)
  (or (get-node name)
      (let ((new (make-instance 'node)))
	(set-node name new)
	new)))

(defun get-value (id)
  (multiple-value-bind (node existsp) (get-node id)
    (if existsp
	(%get-value node)
	(error "no lazy load named as ~a" id))))

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
	    (update-node-height node)
	    (let ((value (apply (fun node)
				args)))
	      (prog1 value
		(setf (value node) value)
		(incf (timestamp node))
		(setf (fun-old node)
		      (fun node))
		(setf (state node) t))))))))

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
      (unless (find dependent (dependents dependency))
	(push dependent (dependents dependency))))))
(defun remove-dependent (dependent dependency)
  (with-locked-lock (dependent)
    (with-locked-lock (dependency)
      (symbol-macrolet ((place (dependents dependency)))
	(setf place (delete dependent place))))))

(defun reload-node (fun deps name)
  (let ((dependencies (mapcar #'ensure-node deps))
	(node (ensure-node name)))
    (with-locked-lock (node)
      (let ((old-dependencies (dependencies node)))
	(setf (dependencies-symbols node) deps)
	(setf (name node) name)
	(map nil (lambda (x) (remove-dependent node x)) (set-difference old-dependencies dependencies))
	(map nil (lambda (x) (ensure-dependent node x)) (set-difference dependencies old-dependencies))
	(make-node fun dependencies node)
	node))))

(defmacro defnode (name deps &body body)
  (let ((lambda-args ())
	(node-deps ()))
    (dolist (item deps)
      (if (symbolp item)
	  (progn (push item lambda-args)
		 (push item node-deps))
	  (destructuring-bind (var dep) item
	    (push var lambda-args)
	    (push dep node-deps))))
    `(reload-node (lambda ,lambda-args ,@body) ',node-deps ',name)))

(defun %map-dependents (node fun)
  (with-locked-node (node nil)
    (dolist (dependent (dependents node))
      (funcall fun dependent)
      (%map-dependents dependent fun))))

(defun map-dependents (name fun)
  (%map-dependents (get-node name) fun))

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

#+nil
(progn
  (defnode b () (print 2893))
  (defnode a (b) (* b b)))

#+nil
(progn
  (remove-node 'c)
  (defnode c (c) c))
