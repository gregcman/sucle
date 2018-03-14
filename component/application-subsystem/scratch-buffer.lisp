(defpackage #:scratch-buffer
  (:use :cl)
  (:export
   my-iterator
   free-my-iterator-memory
   flush-my-iterator))

(in-package #:scratch-buffer)
(defparameter *scratch-space* nil)
(defparameter *lock* (bordeaux-threads:make-lock "scratch-buffer-lock"))
(defun getmem ()
  (bordeaux-threads:with-lock-held (*lock*)
    (let ((a *scratch-space*))
      (cond ((consp a)
	     (pop *scratch-space*)
	     (setf (cdr a) nil)
	     a)
	    (t (list (make-array 256 :element-type 'single-float)))))))
(defun givemem (values)
  (bordeaux-threads:with-lock-held (*lock*)
    (let ((cons *scratch-space*))
      (let ((ans (nconc cons values)))
	(when (not (eq cons ans))
	  (setf *scratch-space* ans))))))

(defun next-array (data)
  (let* ((last-cell (cdr data))
	 (cons-array (or (cdr last-cell)
			 (setf (cdr last-cell)
			       (getmem))))
	 (array (first cons-array))
	 (len (array-total-size array)))
    (when (null (car data))
      (setf (car data)
	    (or last-cell
		cons-array
		(error "how?"))))
    (setf (cdr data) cons-array)
    (values (1- len)
	    array)))

(defun reset-my-iterator (iterator)
  (let ((data (iterator:p-data iterator)))
    (setf (cdr data) (car data))
    (setf (iterator:p-index iterator) 0)))
(defun my-iterator ()
  (let ((cons (cons "my-iterator" nil)))
    (iterator:make-iterator 0 nil (cons cons cons) #'next-array)))
;;todo: make data hold the getmem function,
;;and not be a cons cell, or at least a named cons cell

(defun free-my-iterator-memory (iterator)
  (let* ((cons (iterator:p-data iterator))
	 (head (car cons)))
    (givemem (cdr (car cons)))
    
    (setf (cdr cons) head)
    (setf (cdr head) nil))
  ;;cleanup
  (setf (iterator:p-array iterator) nil
	(iterator:p-index iterator) 0))

(defmacro flush-my-iterator (a &body body)
  (let ((var (gensym)))
    `(let ((,var ,a)) 
       (reset-my-iterator ,var)
       ,@body
       (free-my-iterator-memory ,var))))
 
