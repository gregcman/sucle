(defpackage #:doubly-linked-list
  (:use #:cl #:utility)
  (:export
   #:insert-next
   #:insert-prev
   #:detach
   #:link
   #:attach-next
   #:attach-prev
   #:circular
   #:do-circular-doubly-linked-list)
  (:export
   #:node-prev
   #:node-next
   #:node-payload
   #:make-node))
(in-package :doubly-linked-list)

(defstruct node
  (prev nil)
  (next nil)
  (payload nil))

(defun insert-next (link new)
  (let ((next (node-next link)))
    (link link new)
    (when (node-p next)
      (link new next))
    new))
(defun insert-prev (new link)
  (let ((prev (node-prev link)))
    (link new link)
    (when (node-p prev)
      (link prev new))
    new))
(defun detach (link)
  (let ((prev (node-prev link))
	(next (node-next link)))
    (link prev next)
    link))

(defun link (prev next)
  (when (node-p prev)
    (setf (node-next prev) next))
  (when (node-p next)
    (setf (node-prev next) prev)))

(defun attach-next (item next)
  (make-node :payload item :next next))
(defun attach-prev (item next)
  (make-node :payload item :prev next))

(defun circular (&optional (payload nil))
  (let ((cell (make-node :payload payload)))
    (setf (node-next cell) cell
	  (node-prev cell) cell)
    cell))

(defmacro do-circular-doubly-linked-list ((object &optional (forward-p t))
						    start &body body)
  (let ((next-fun (if forward-p
		      'node-next
		      'node-prev)))
    (with-gensyms (node-var)
      (once-only (start)
	`(do ((,node-var (,next-fun ,start)
			 (,next-fun ,node-var)))
	     ((or
	       (not (node-p ,start))
	       (eq ,node-var ,start)))
	   (let ((,object (node-payload ,node-var)))
	     ,@body))))))



(set-pprint-dispatch
 'node
 (lambda (stream object)
   (do-circular-doubly-linked-list (obj) object
     (write obj :stream stream)
     (terpri stream))))
