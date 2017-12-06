(defpackage #:aplayground
  (:use 
   #:cl
   #:fuktard))

(in-package :aplayground)

(defmacro with-iterators ((&rest bufvars) buf func &body body)
  (let ((syms (mapcar (lambda (x) (declare (ignorable x)) (gensym)) bufvars)))
    (with-vec-params
	syms `(,buf)
	(let ((acc (cons 'progn body)))
	  (dolist (sym syms)
	    (let ((value (pop bufvars)))
	      (unless (consp value)
		(setf value (list value)))
	      (setf acc (list func value sym acc))))
	  acc))))

(in-package :sandbox)

(defun reset-attrib-buffer-iterators (fill-data)
  (dotimes (x (array-total-size fill-data))
    (reset-my-iterator (aref fill-data x))))

(defparameter *scratch-space* nil)
(defun getmem ()
  (let ((a *scratch-space*))
    (cond ((consp a)
	   (pop *scratch-space*)
	   (setf (cdr a) nil)
	   a)
	  (t (list (make-array 256 :element-type 'single-float))))))
(defun givemem (values)
  (let* ((cons *scratch-space*)
	 (ans (nconc cons values)))
    (when (not (eq cons ans))
      (setf *scratch-space* ans))))

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
  (let ((data (iter-ator:p-data iterator)))
    (setf (cdr data) (car data))
    (setf (iter-ator:p-index iterator) 0)))
(defun my-iterator ()
  (let ((cons (cons "my-iterator" nil)))
    (iter-ator:make-iterator 0 nil (cons cons cons) #'next-array)))
(defun free-my-iterator-memory (iterator)
  (let* ((cons (iter-ator:p-data iterator))
	 (head (car cons)))
    (givemem (cdr (car cons)))
    
    (setf (cdr cons) head)
    (setf (cdr head) nil))
  ;;cleanup
  (setf (iter-ator:p-array iterator) nil
	(iter-ator:p-index iterator) 0))
