(defpackage :matrix
  (:use :cl)
  (:export
   #:row-vector
   #:column-vector
   #:matrix-multiply))
(in-package :matrix)

(defmacro row-vector (&rest args)
  `(make-array
    '(1 ,(length args)) :element-type 'single-float :initial-contents
    (list (list ,@args))))

(defmacro column-vector (&rest args)
  `(make-array
    '(,(length args) 1) :element-type 'single-float :initial-contents
    (list ,@ (mapcar (lambda (x) (list (quote list) x)) args))))

;;;stolen from opticl
(defun matrix-multiply (matrix-a matrix-b)
  (destructuring-bind (matrix-a-rows matrix-a-columns)
      (array-dimensions matrix-a)
    (destructuring-bind (matrix-b-rows matrix-b-columns)
        (array-dimensions matrix-b)
      (if (= matrix-a-columns matrix-b-rows)
          (let* ((c (make-array (list matrix-a-rows matrix-b-columns)
                                :element-type (array-element-type matrix-a))))
            (dotimes (i matrix-a-rows)
              (dotimes (j matrix-b-columns)
                (let ((v 0))
                  (dotimes (r matrix-a-columns)
                    (incf v (* (aref matrix-a i r) (aref matrix-b r j))))
                  (setf (aref c i j) v))))
            c))))) 
