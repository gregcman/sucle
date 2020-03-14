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
   #:do-node-dependencies

   #:%redefine-node
   #:%refresh
   #:cleanup-node-value

   #:get-mutable-cell-by-name
   #:flush-refreshes
   #:refresh-old-node
   #:%%refresh))
(in-package :dependency-graph)

(struct-to-clos:struct->class
 (defstruct node
   ;;The value
   value
   ;;Fulfilled or unfulfilled
   state
   ;;Last time fulfilled
   (timestamp 0)
   (lock (bordeaux-threads:make-recursive-lock))
   ;;T or NIL depending on whether its currently locked
   lock-value
   (%data (make-hash-table :test 'eql))
   ;;List of dependents
   dependents
   ;;For changing dependencies later
   tags
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
(set-pprint-dispatch
 'mutable-cell
 ;;Example taken from sb-cga/matrix.lisp
 (lambda (stream obj)
   (pprint-logical-block (stream nil)
     (print-unreadable-object (obj stream :type nil :identity nil)
       (format stream "snapshot:~s,"
	       (mutable-cell-snapshot obj))
       (pprint-newline :mandatory stream)
       (format stream "value:~s"
	       (mutable-cell-observing obj))))))

(defun mutable-cell-difference (cell &optional (new (mutable-cell-observing cell)))
  ;;Detect whether there is a difference being observed
  ;;and if so whether a change should occur.
  ;;new is given as a parameter for debugging
  (let* ((new-snapshot (funcall (mutable-cell-snapshot-key cell) new))
	 (un-changed (funcall
		      (mutable-cell-un-changed cell)
		      (mutable-cell-snapshot cell)
		      new-snapshot)))
    (values
     (not un-changed)
     new-snapshot)))
(defun touch-mutable-cell (cell)
  ;;Observe a change
  (multiple-value-bind (changed new-snapshot) (mutable-cell-difference cell)
    (when changed
      (setf (mutable-cell-snapshot cell) new-snapshot)
      (setf (mutable-cell-result cell)
	    (funcall (mutable-cell-observe cell)
		     (mutable-cell-observing cell))))))

;;Permanently prevent a mutable cell from updating anymore
;;Keep in mind when used in conjuction with node,
;;that the dependents have to be updated with (remove-dependent observing)
(defun sterilize-mutable-cell (cell)
  (let ((result (mutable-cell-result cell))
	(observing (mutable-cell-observing cell)))
    (setf (mutable-cell-observing cell) result
	  (mutable-cell-snapshot cell) result
	  (mutable-cell-un-changed cell) (load-time-value (constantly t))
	  (mutable-cell-snapshot-key cell) 'identity
	  (mutable-cell-observe cell) 'identity)))

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
     (;;circular node is detected by detecting repeated locking of
      ;;the same node.
      with-locked-node (node (error "circular dependency: ~a" node))
       ;;Update observables
       (do-node-dependencies node
	 (lambda (k v)
	   (declare (ignore k))
	   (touch-mutable-cell v)))
       
       (let (args)
	 ;;Obtain arguments to send to function
	 (let ((size (hash-table-count (node-%data node))))
	   (dotimes (i (1- size))
	     (push (mutable-cell-result (node-data node i))
		   args))
	   (setf args (nreverse args)))

	 ;;apply function to collected arguments and clean up
	 (let ((value (apply (mutable-cell-result (node-data node :function))
			     args)))
	   (prog1 value
	     (setf (node-value node) value)
	     (touch-node node)
	     (setf (node-state node) t))))))))

(defun get-mutable-cell-by-name (node name)
  (let ((cell (assoc name (node-tags node))))
    (node-data node (cdr cell))))

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
    (setf (node-state node) nil)
    ;;The function that is called when the cell is fulfilled
    (let ((mutable-cell (ensure-func node))
	  (witness (etypecase func
		     (function #'identity)
		     (symbol #'symbol-function))))
      (setf (mutable-cell-observing mutable-cell) func
	    (mutable-cell-observe mutable-cell) witness
	    (mutable-cell-snapshot-key mutable-cell) witness
	    ))
    ;;Convert dependencies to mutable cells and store them
    ;;in the node
    (let ((count 0))
      ;;funny thing with hash table storing numbers
      (dolist (dep deps)
	(let ((cell (make-mutable-cell :observing dep)))
	  ;;Nodes are observed with timestamps and getting their value
	  (typecase dep
	    (node (setf (mutable-cell-snapshot-key cell)
			'node-timestamp)
		  (setf (mutable-cell-observe cell)
			'%get-value)))
	  (setf (node-data node count) cell)
	  (incf count)
	  )))
    node))

(defun ensure-dependent (dependent dependency)
  ;;dependency is a node
  (with-locked-lock (dependent)
    (with-locked-lock (dependency)
      (symbol-macrolet ((place (node-dependents dependency)))
	(unless (find dependent place)
	  (push dependent place))))))
(defun remove-dependent (dependent dependency)
  ;;dependency is a node
  (with-locked-lock (dependent)
    (with-locked-lock (dependency)
      (symbol-macrolet ((place (node-dependents dependency)))
	(setf place (delete dependent place))))))

(defun touch-node (node)
  (incf (node-timestamp node)))

(defun dirty-p (node)
  ;;Iterate through all the dependencies,
  ;;if there is a difference detected,
  ;;then it is dirty.
  (do-node-dependencies node
    (lambda (k v)
      (declare (ignorable k))
      (when (mutable-cell-difference v)
	(return-from dirty-p (values t v))))))

;;;
(defun %redefine-node (fun node dependencies &optional name tags)
  (with-locked-lock (node)
    (setf (node-tags node) tags)
    #+nil
    (setf (dependencies-symbols node) deps)
    (setf (node-name node) name)
    ;;FIXME::Inelegant, way to update dependencies. Just eliminate
    ;;all the old dependencies and rebuild.
    
    ;;Remove all current dependents
    (do-node-dependencies node
      (lambda (k v)
	(declare (ignorable k))
	(let ((observing (mutable-cell-observing v)))
	  (when (typep observing 'node)
	    (remove-dependent node observing)))
	))
    ;;Rebuild dependents
    (map nil (lambda (x)
	       (when (typep x 'node)
		 (ensure-dependent node x)))
	 dependencies)
    
    #+nil
    (let ((old-dependencies (node-dependencies node)))
      #+nil
      (map nil (lambda (x) (remove-dependent node x))
	   (set-difference old-dependencies dependencies))
      #+nil
      (map nil (lambda (x) (ensure-dependent node x))
	   (set-difference dependencies old-dependencies)))
    (really-make-node fun dependencies node)
    node))
;;;

(defun %refresh (node &optional (value (node-value node)))
  ;;[FIXME] May walk a single node exponential amount of times,
  ;;because this is breadth first search!!!
  (clean-and-invalidate-node node value)
  (labels ((%map-dependents2 (node fun test)
	 ;;FIXME::first node is a special case???
	 (dependency-graph:with-locked-node (node nil)
	   (dolist (dependent (dependency-graph:node-dependents node))
	     (when (funcall test dependent)
	       (funcall fun dependent)
	       (%map-dependents2 dependent fun test))))))
    (%map-dependents2
     node
     #'clean-and-invalidate-node
     #'dirty-p)))

(defgeneric cleanup-node-value (object))
(defmethod cleanup-node-value ((object t))
  (declare (ignorable object)))

(defun clean-and-invalidate-node (node &optional (value nil value-supplied-p))
  ;;FIXME::real logging facilities?
  ;;(format t "~% old node:~s" node)
  (when (or (node-state node)
	    ;;value-supplied-p means ignore
	    ;;whatever happened to the node.
	    value-supplied-p)
    ;;(format t "~%-> cleaning")
    ;;cleanup the node 
    (let ((value (if value-supplied-p value
		     (node-value node))))
      (cleanup-node-value value)
      (setf (node-value node)
	    ;;FIXME -> is nil correct?
	    nil)))
  ;;invalidate it
  (touch-node node)
  (setf (node-state node) nil))
;;;;


;;;;Refreshing system
;;;;queue node to be unloaded if it already has stuff in it 
(defun refresh-old-node (node)
  (when (not (zerop (dependency-graph:node-timestamp node)))
    (%%refresh node)))

(defparameter *refresh* (make-hash-table :test 'eq))
(defparameter *refresh-lock* (bordeaux-threads:make-recursive-lock "refresh"))
(defun %%refresh (node &key same-thread;; wait
			 )
  (cond
    (same-thread
     (%refresh node))
    (t (bordeaux-threads:with-recursive-lock-held (*refresh-lock*)
	 (setf (gethash node *refresh*)
	       (dependency-graph:node-value node)))
       ;;Spin lock when wait is t
       ;;FIXME::node values getting overwritten before node
       ;;can be cleaned up
       #+nil
       (when wait
	 (loop :while (gethash node *refresh*))))))
;;This is a catch-all for OpenGL objects and everything. FIXME?
(defun flush-refreshes ()
  (bordeaux-threads:with-recursive-lock-held (*refresh-lock*)
    (let ((length (hash-table-count *refresh*)))
      (unless (zerop length)
	(utility:dohash (node value) *refresh*
			;;(declare (ignore value))
			(dependency-graph:%refresh node value))
	(clrhash *refresh*)))))
;;;;

;;[TODO] -> move to test file, documentation?
#+nil
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
