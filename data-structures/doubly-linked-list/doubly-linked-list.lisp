(defpackage #:doubly-linked-list
  (:use #:cl)
  (:export
   #:insert-next
   #:insert-prev
   #:detach
   #:link
   #:attach-next
   #:attach-prev
   #:circular)
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
