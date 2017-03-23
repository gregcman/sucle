(in-package :sandbox)

(defun make-attrib-table (array)
  (map-into array #'flhat:make-array-array))

(defparameter *attrib-scratch* (make-array 16))
(defparameter *attrib-scratch-fill* (make-array 16 :element-type '(unsigned-byte 32) :initial-element 0))

(defun zero-scratch-fill ()
  (fill *attrib-scratch-fill* 0))

(defun destroy-attrib-table ()
  (make-attrib-table *attrib-scratch*)
  (reset-all-mesh-scratches))

(defun reset-all-mesh-scratches ()
  (zero-scratch-fill)
  (values))

(progn
  (declaim (inline vertex-attrib))
  (locally (declare (optimize (speed 3) (safety 0))
		    (inline flhat:sets))
    (defun vertex-attrib (attrib-table table-fill attrib-location value)
      (declare (type (simple-array (unsigned-byte 32) (16)) table-fill)
	       (type simple-vector attrib-table))
      (let ((fill (aref table-fill attrib-location))
	    (single-attrib (aref attrib-table attrib-location)))
	(declare (type (unsigned-byte 32) fill))
	(flhat:sets single-attrib fill value)
	(setf (aref table-fill attrib-location) (1+ fill)))))
  (declaim (notinline vertex-attrib)))

(defmacro *vertex-attrib* (&body pairs)
  (let ((acc nil))
    (alexandria:with-gensyms (fill buffer)
      (dolist (pair pairs)
	(destructuring-bind (attrib-location-form &rest values) pair
	  (alexandria:with-gensyms (loc)
	    (let ((pair-acc (list `((,loc ,attrib-location-form))'let)))
	      (dolist (value values)
		(push `(vertex-attrib ,buffer ,fill ,loc ,value) pair-acc))
	      (push (nreverse pair-acc) acc)))))
      (list* 'let `((,buffer *attrib-scratch*)
		    (,fill *attrib-scratch-fill*))
	     (nreverse acc)))))
