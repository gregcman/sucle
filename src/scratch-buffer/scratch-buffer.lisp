(defpackage #:scratch-buffer
  (:use :cl #:reverse-array-iterator)
  (:export
   my-iterator
   free-my-iterator-memory
   flush-my-iterator
   flush-my-iterator*
   iterator-fill-pointer
   bind-in 
   bind-in*
   bind-out
   bind-out*
   flush-bind-in
   flush-bind-in*))

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
  (let ((data (p-data iterator)))
    (setf (cdr data) (car data))
    (setf (p-index iterator) 0)))
(defun my-iterator ()
  (let ((cons (cons "my-iterator" nil)))
    (make-zeroed-iterator #'next-array (cons cons cons))))
;;todo: make data hold the getmem function,
;;and not be a cons cell, or at least a named cons cell

(defun free-my-iterator-memory (iterator)
  (let* ((cons (p-data iterator))
	 (head (car cons)))
    (givemem (cdr (car cons)))
    
    (setf (cdr cons) head)
    (setf (cdr head) nil))
  ;;cleanup
  (setf (p-array iterator) nil
	(p-index iterator) 0))

(defmacro flush-my-iterator ((a) &body body)
  (let ((var (gensym)))
    `(let ((,var ,a)) 
       (reset-my-iterator ,var)
       (multiple-value-prog1 (locally ,@body)
	 (free-my-iterator-memory ,var)))))
 
(defun iterator-fill-pointer (iterator)
  (let ((len (- (p-index iterator))))
    (dolist (item (cdr (car (p-data iterator))))
      (incf len (array-total-size item)))
    len))

(defmacro bind-in ((iterator name) &body body)
  `(bind-iterator-in
    (,name single-float) ,iterator
    ,@body))

(defmacro bind-out ((iterator name) &body body)
  `(bind-iterator-out
    (,name single-float) ,iterator
    ,@body))

(defmacro start-nest (name1)
  `(defmacro ,(utility:symbolicate2 (list name1 "*")) ((&rest forms) &body body)
     `(utility:nest ,@(mapcar (lambda (x) `(,',name1 ,x)) forms)
		    (locally ,@body))))

;;FIXME::refactor out for * type macros?
(start-nest bind-in)
(start-nest bind-out)
(start-nest flush-bind-in)
(start-nest flush-my-iterator)

(defmacro flush-bind-in ((iterator name) &body body)
  (utility:once-only (iterator)
    `(flush-my-iterator (,iterator) (bind-in (,iterator ,name) ,@body))))
