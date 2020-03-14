(defpackage :dependency-graph
  (:use #:cl)
  (:export
   
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
   #:do-node-dependencies))
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
   
   ;;(fun nil)
   ;;(fun-old nil)
   ;;(arguments nil)
   #+nil
   (dependency nil)
   ;;(dependencies nil) 
   ;;(dependencies-timestamps nil)

   (%data (make-hash-table :test 'eql))
   
   (dependents nil)
   #+nil
   (height ;height of node in dependency tree
    nil)
   (name "anon")))

(defun node-data (node name)
  (gethash name (node-%data node)))
(defun (setf node-data) (new node name)
  (setf (gethash name (node-%data node)) new))
(defun do-node-dependencies (node fun)
  (let ((hash (node-%data node)))
    (utility:dohash (k v) hash
		    (funcall fun k v))))

(set-pprint-dispatch
 'node
 (lambda (stream node)
   (write (node-name node) :stream stream :escape nil :case :downcase)))

(struct-to-clos:struct->class
 ;;more like mutable-observation-cell
 (defstruct mutable-cell
   result
   observing
   (snapshot nil) ;;what new values are tested against to see if they times this cell was written to
   (un-changed 'equal)
   (snapshot-key 'identity)
   (observe 'identity)
   ;;(funcall changed? (funcall snapshot-key value) snapshot)
   ))
;;observing a lisp object for changes
;;'observe' extracts the important data from the lisp object into 'result'
;;'unchanged' detects the difference
;;'snapshot' stores a reduced version to test whether there is a difference 
(set-pprint-dispatch 'mutable-cell
		     (lambda (stream obj)
		       (format stream "snapshot:~s ~%value:~s"
			       (mutable-cell-snapshot obj)
			       (mutable-cell-observing obj))))

(defun mutable-cell-difference (cell &optional (new (mutable-cell-observing cell)))
  (let* ((new-snapshot (funcall (mutable-cell-snapshot-key cell) new))
	 (un-changed (funcall
		      (mutable-cell-un-changed cell)
		      (mutable-cell-snapshot cell)
		      new-snapshot)))
    (values
     (not un-changed)
     new-snapshot)))
(defun touch-mutable-cell (cell)
  (multiple-value-bind (changed new-snapshot) (mutable-cell-difference cell)
    (when changed
      (setf (mutable-cell-snapshot cell) new-snapshot)
      (setf (mutable-cell-result cell)
	    (funcall (mutable-cell-observe cell)
		     (mutable-cell-observing cell))))))

#+nil
(defun log-difference (cell)
  (print (multiple-value-list (mutable-cell-difference cell)))
  (touch-mutable-cell cell)
  (print cell)
  (print (multiple-value-list (mutable-cell-difference cell))))
#+nil
(defun test3 ()
  (let ((value (cons 1 2)))
    (let ((cell (make-mutable-cell :value value :snapshot-key 'cdr)))
      (log-difference cell)     
      (incf (cdr value))
      (log-difference cell))))

;;;;Locking
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
;;;;

(defun %get-value (node)
  "fulfill a promise of the node"
  (cond
    (;;Node is 'fulfilled already'
     (node-state node)
     (node-value node))
    (;;Node is 'unfulfilled' or dirty
     t
     (with-locked-node (node (error "circular dependency: ~a" node))
       ;;(let ((dependencies (node-dependencies node))))
       (let ((args nil
	       #+nil
	       (map 'list
		    #'%get-value
		    dependencies)))
	 ;;FIXME::arguments not saved
	 ;;(setf (node-arguments node) args)
	 ;;(print "here")
	 (do-node-dependencies node
	   (lambda (k v)
	     (declare (ignore k))
	     (touch-mutable-cell v)))
	 ;;(print "ho")
	 (let ((size (hash-table-count (node-%data node))))
	   (dotimes (i (1- size))
	     (push (mutable-cell-result (node-data node i))
		   args))
	   (setf args (nreverse args)))
	 ;;(print "hey")
	 #+nil
	 ;;This should be updated by touch-mutable-cell
	 (setf (node-dependencies-timestamps node)
	       (map 'list
		    #'node-timestamp
		    dependencies))
	 #+nil
	 (update-node-height node)
	 (let ((value (apply (mutable-cell-result (node-data node :function))
			     args)))
	   (prog1 value
	     (setf (node-value node) value)
	     (touch-node node)
	     #+nil
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

(defun ensure-func (node)
  (let ((mutable-cell (node-data node :function)))
    (unless mutable-cell
      (let ((new (make-mutable-cell)))
	(setf (node-data node :function)
	      new)
	(setf mutable-cell new)))
    mutable-cell))
(defun really-make-node (func deps node)
  (with-locked-lock (node)
    (let (;;(deps-same? (equal deps (node-dependencies node)))
	  ;;FIXME
	  ;;(fun-same? (eq (node-fun node) func))
	  ;;(deps-len (list-length deps))
	  )
      #+nil
      (when (zerop deps-len)
	(setf (height node) 0))
      #+nil ;;FIXME::arguments not saved
      (setf (node-arguments node)
	    (make-list deps-len))
      #+nil
      (setf (node-dependencies-timestamps node)
	    (make-list deps-len :initial-element 0))
      (unless nil
	#+nil
	(and deps-same?
		   ;;fun-same?
	     )
	(setf (node-state node) nil))
      
      (let ((mutable-cell (ensure-func node)))
	(setf (mutable-cell-observing mutable-cell)
	      func))
      #+nil
      (unless fun-same?
	)
      #+nil
      (unless deps-same?
	(setf (node-dependencies node) deps))
      (let ((count 0))
	(dolist (dep deps)
	  (let ((cell (make-mutable-cell :observing dep)))
	    (typecase dep
	      (node (setf (mutable-cell-snapshot-key cell)
			  'node-timestamp)
		    (setf (mutable-cell-observe cell)
			  '%get-value)))
	    (setf (node-data node count) cell)
	    (incf count)
	    ))))
    node))

(defun ensure-dependent (dependent dependency)
  (typecase dependency
    (node
     (with-locked-lock (dependent)
       (with-locked-lock (dependency)
	 (symbol-macrolet ((place (node-dependents dependency)))
	   (unless (find dependent place)
	     (push dependent place))))))))
(defun remove-dependent (dependent dependency)
  (typecase dependency
    (node
     (with-locked-lock (dependent)
       (with-locked-lock (dependency)
	 (symbol-macrolet ((place (node-dependents dependency)))
	   (setf place (delete dependent place))))))))

(defun touch-node (node)
  (incf (node-timestamp node)))

(defun %invalidate-node (node)
  (touch-node node)
  (setf (node-state node) nil))

(defmacro any (&rest forms)
  (nth (random (length forms)) forms))

(defun dirty-p (node)
  ;;FIXME::function
  ;;(or (not (eq (node-fun node) (node-fun-old node))))
  ;;Iterate through all the dependencies,
  ;;if there is a difference detected,
  ;;then it is dirty.
  (do-node-dependencies node
    (lambda (k v)
      (declare (ignorable k))
      (when (mutable-cell-difference v)
	(return-from dirty-p t))))
  #+nil
  (block nil
    (do ((stamp (node-dependencies-timestamps node) (cdr stamp))
	 (arg (node-dependencies node) (cdr arg)))
	((not (any arg
		   stamp))
	 nil)
	      ;;;iterate old timestamps and see if any differ now
      (when (not (= (car stamp)
		    (node-timestamp (car arg))))
	(return t)))))

;;;

"
cell,promise,node:
1. object
-- place can be mutable
2. dependencies
-- name [optional], previous, timestamp, new, onchange
-- 

- dependencies, with names
- snapshots of dependencies
- check whether those dependencies differ per snapshot [or not, just look at timestamp]
- 
"
#+nil
(defparameter *node-generator*
  (deflambda (a b c &key (other (get-node 'foo)) &optional (a b c))))
#+nil
(invoke-node *node-generator* 4 ..other node.. 6 :other nil );;;???
#+nil
(deplambda (&key (name default delta) (foo 4 '=) (bar "what" 'string=)))
#+nil
(make-node-from-prototype *)
;;each deplambda has a cached value?
#+nil
(invoke-node * (:function (lambda ()
			    (get :foo)
			    (get :bar)))
	     (:foo 4) (:bar 6)
	     (:changed)
	     (:snapshot-maker))
