(defpackage #:text-tree
  (:use #:cl))
(in-package :text-tree)

(defparameter *anarray* nil)
(defparameter *nums* (make-array (expt 10 7)))

(defun reset-test (x)
  (setf *anarray* (make-array x))
  (map-into *anarray* (function gensym))
  (map-into *nums* (lambda () (aref *anarray* (random x))))
  *anarray*)

(reset-test 21)

(defun reset-two ()
  (setf *alist* nil)
  (clrhash *ahash*))

(funland:with-unsafe-speed
  (defun test-hash ()
    (let ((hash *ahash*)
	  (nums *nums*))
      (declare (type hash-table hash)
	       (type simple-vector nums))
      (time
       (dotimes (x (expt 10 6))
	 (let ((item (aref nums x)))
	   (setf (gethash item hash) item)))))))

(funland:with-unsafe-speed
  (defun test-hash2 ()
    (let ((hash *ahash*)
	  (nums *nums*)
	  (a nil))
      (declare (type hash-table hash)
	       (type simple-vector nums))
      (time
       (dotimes (x (expt 10 6))
	 (let ((item (aref nums x)))
	   (setf a (gethash item hash)))))
      a)))

(funland:with-unsafe-speed
  (defun test-alist2 ()
    (let ((nums *nums*)
	  (alist *alist*)
	  (a nil))
      (declare (type simple-vector nums))
      (time
       (dotimes (x (expt 10 6))
	 (let ((item (aref nums x)))
	   (let ((cons (assoc item alist :test (function eq))))
	     (setf a (car cons))))))
      a)))

(funland:with-unsafe-speed
  (defun test-alist ()
    (let ((nums *nums*)
	  (alist *alist*))
      (declare (type simple-vector nums))
      (time
       (dotimes (x (expt 10 6))
	 (let ((item (aref nums x)))
	   (let ((cons (assoc item alist :test (function eq))))
	     (if cons
		 (setf (car cons) item)
		 (push (cons item item) alist))))))
      (setf *alist* alist))))

(defparameter *ahash* (make-hash-table :test (quote eq)))
(defparameter *alist* nil)


(funland:with-unsafe-speed
  (defun online-test ()
    (let* ((num-obj 22)
	   (num-iterations 1000000)
	   (syms (loop for i from 1 to num-obj collect (gensym)))
	   (alist (let (list)
		    (loop for s in syms
		       for i from 1 do (setf list (acons s i list)))
		    list))
	   (htable (let ((table (make-hash-table :test #'eq)))
		     (loop for s in syms
			for i from 1 do (setf (gethash s table) i))
		     table)))

      (time (let ((count 0))
	      (dotimes (i num-iterations)
		(dolist (j syms)
		  (let* ((exists (assoc j alist :test #'eq))
			 (value (cdr exists)))
		    (when exists
		      (setf count (+ count value))))))
	      (format t "~&Assoc list count check ~A" count)
	      ))

      (time (let ((count 0))
	      (dotimes (i num-iterations)
		(dolist (j syms)
		  (multiple-value-bind (value exists)
		      (gethash j htable)
		    (when exists
		      (setf count (+ count value))))))
	      (format t "~&Hash table count check ~A" count)
	      )))))

;;;;plist for 21 elements or less is equal or faster than hash table

(defun tree-deph (tree)
  (labels ((rec (node depth)
	      (if (atom node)
		  depth
		  (max (rec (car node) (1+ depth))
		       (rec (cdr node) (1+ depth))))))
    (rec tree 0)))
