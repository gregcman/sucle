(defpackage #:double-link
  (:use #:cl)
  (:export
   #:dlink-insert-right
   #:dlink-insert-left
   #:dlink-remove
   #:dlink-link
   #:dlink-cons
   #:circular-dlink)
  (:export
   #:dlink-left
   #:dlink-right
   #:dlink-payload
   #:make-dlink))
(in-package :double-link)

(defstruct dlink
  (left nil)
  (right nil)
  (payload nil))

(defun dlink-insert-right (link new)
  (let ((right (dlink-right link)))
    (dlink-link link new)
    (when (dlink-p right)
      (dlink-link new right))
    new))
(defun dlink-insert-left (new link)
  (let ((left (dlink-left link)))
    (dlink-link new link)
    (when (dlink-p left)
      (dlink-link left new))
    new))
(defun dlink-remove (link)
  (let ((left (dlink-left link))
	(right (dlink-right link)))
    (dlink-link left right)
    link))

(defun dlink-link (left right)
  (when (dlink-p left)
    (setf (dlink-right left) right))
  (when (dlink-p right)
    (setf (dlink-left right) left)))

(defun dlink-cons (item right)
  (make-dlink :payload item :right right))

(defun circular-dlink (&optional (payload nil))
  (let ((cell (make-dlink :payload payload)))
    (setf (dlink-right cell) cell
	  (dlink-left cell) cell)
    cell))
