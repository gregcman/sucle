(defpackage :reverse-array-iterator-user
  (:use #:cl #:reverse-array-iterator)
  (:export
   #:bind-iterator-out
   #:bind-iterator-in)
  (:export
   #:iterator-value
   #:iterator-next
   #:iterator-read
   #:iterator-emit))
(in-package :reverse-array-iterator-user)

(defparameter *iterator* (make-default-iterator))

(declaim (inline iterator-value (setf iterator-value)))
(defun iterator-value (&optional (iterator *iterator*))
  (aref (p-array iterator) (p-index iterator)))
(defun (setf iterator-value) (value &optional (iterator *iterator*))
  (setf (aref (p-array iterator) (p-index iterator)) value))
(defun iterator-next (&optional (iterator *iterator*))
  (with-bound-iterator (next place (array) (index)) iterator
    (next))
  (values))

(defun iterator-read (&optional (iterator *iterator*))
  (iterator-next iterator)
  (iterator-value iterator))
(defun iterator-emit (item &optional (iterator *iterator*))
  (iterator-next iterator)
  (setf (iterator-value iterator) item))
