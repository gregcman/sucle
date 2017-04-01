(in-package :sandbox)

(defun make-attrib-buffer-data ()
  (make-array 16 :element-type t :initial-element nil))

(defun fill-with-flhats (array)
  (map-into array #'flhat:make-flhat))
(defun make-iterators (buffer result)
  (map-into result (lambda (x) (flhat:make-flhat-iterator x)) buffer))

(defun tally-buffer (iterator-buffer result)
  (map-into result (lambda (x) (iterator-count x)) iterator-buffer))

(defun reset-attrib-buffer-iterators (fill-data)
  (dotimes (x (array-total-size fill-data))
    (flhat:reset-iterator (aref fill-data x))))

(defun iterator-count (iterator)
  (if (iter-ator:p-array iterator)
      (1+ (flhat:iterator-position iterator))
      0))
