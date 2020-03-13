(defpackage :dependency-graph
  (:use #:cl)
  (:export
 
   #:%defnode
   
   #:node-timestamp
   #:node-value
   #:node-state
   #:node-dependents
   #:node-dependencies
   #:node-name
   #:touch-node

   #:remove-dependent
   #:ensure-dependent
   #:really-make-node

   #:with-locked-lock
   #:with-locked-node
   #:dirty-p
   #:%get-value

   #:%invalidate-node
   #:node
  ))
(in-package :dependency-graph)

(struct-to-clos:struct->class
 (defstruct node
   (value nil)
   (state ;fulfilled or unfulfilled
    nil)
   (timestamp ;last time fulfilled
    0)
   (lock (bordeaux-threads:make-recursive-lock))
   (lock-value nil)
   
   (fun nil)
   (fun-old nil)
   (arguments nil)
   #+nil
   (dependency nil)
   (dependencies nil) 
   (dependencies-timestamps nil)  
   
   (dependents nil)
   #+nil
   (height ;height of node in dependency tree
    nil)
   (name "anon")))

(set-pprint-dispatch
 'node
 (lambda (stream node)
   (write (node-name node) :stream stream :escape nil :case :downcase)))


(defmacro with-t (place &body body)
  `(unwind-protect
	(progn
	  (setf ,place t) ;;;to catch circular dependencies
	  ,@body)
     (setf ,place nil)))
(defmacro with-locked-node ((node &optional else) &body body)
  (utility:once-only (node)
    `(with-locked-lock (,node)
       (if (node-lock-value ,node)
	   ,else
	   (with-t (node-lock-value ,node)
	     ,@body)))))

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

(defun really-make-node (func deps node)
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
