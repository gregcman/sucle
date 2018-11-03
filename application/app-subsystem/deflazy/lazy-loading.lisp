(defpackage #:deflazy
  (:use #:cl #:utility)
  (:export
   #:getfnc
   #:deflazy
   #:refresh
   #:flush-refreshes))

(in-package :deflazy)

;;;;;TODO: clean this area up with dependency graph 
(defvar *stuff* (make-hash-table :test 'eq))
(defmacro deflazy (name (&rest deps) &rest gen-forms)
  `(eval-when (:load-toplevel :execute)
     (let ((dependency-graph::*namespace* *stuff*))
       (refresh-new-node ',name)
       ,(multiple-value-bind
	 (fun node-deps) (dependency-graph::%defnode deps gen-forms)
	 `(dependency-graph::redefine-node ,fun ',node-deps ',name)))))

;;;;queue node to be unloaded if it already has stuff in it 
(defun refresh-new-node (name)
  (let ((node (dependency-graph::ensure-node name *stuff*)))
    (unless (= 0 (dependency-graph::timestamp node))
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
	(dohash (name value) *refresh*
		(declare (ignore value))
		(%refresh name))
	(clrhash *refresh*)))))

(defun %refresh (name)
  (let ((node (dependency-graph::get-node name *stuff*)))
    (when node
      (dependency-graph::touch-node node)
      (clean-and-invalidate-node node)
      (dependency-graph::map-dependents2
       name
       #'clean-and-invalidate-node
       #'dependency-graph::dirty-p
       *stuff*))))

(defun getfnc (name)
  (dependency-graph::get-value name *stuff*))

(defgeneric cleanup-node-value (object))
(defmethod cleanup-node-value ((object t))
  (declare (ignorable object)))
(defun cleanup-node (node)
  (let ((value (dependency-graph::value node)))
    (cleanup-node-value value)))

(defun clean-and-invalidate-node (node)
  (when (dependency-graph::state node)
    (cleanup-node node))
  (dependency-graph::%invalidate-node node))
