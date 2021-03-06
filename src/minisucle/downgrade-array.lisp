(defpackage :downgrade-array
  (:use :cl)
  (:export
   #:storage-type
   #:really-downgrade-array))
(in-package :downgrade-array)

(defun downgrade-array (&rest items)
  (block out
    (let ((min 0)
	  (max 0))
      (flet ((check-thing (item)
	       (typecase item
		 (fixnum
		  (when (< max item)
		    (setf max item))
		  (when (< item min)
		    (setf min item)))
		 (otherwise (return-from out t)))))
	(dolist (item items)
	  (typecase item
	    (array (dotimes (i (array-total-size item))
		     (let ((item (row-major-aref item i)))
		       (check-thing item))))
	    ((and (not null) sequence) (map nil #'check-thing item))
	    (t (check-thing item)))))
      (upgraded-array-element-type (num-type min max)))))


;;From the CLHS
;;(integer -(2^(s-1)) (2^(s-1))-1) -> signed byte
;;(integer 0 n) for n=(2^s)-1 -> unsigned byte
(defun num-type (min max)
  (cond ((zerop min)
	 ;;unsigned-byte
	 `(unsigned-byte ,(1+ (upgrade max))))
	(t `(signed-byte ,(max (up2 min)
			       (+ 2 (upgrade max)))))))

(defun storage-type (item)
  (typecase item
    (fixnum
     (upgraded-array-element-type
      (if (plusp item)
	  (num-type 0 item)
	  (num-type item 0))))
    (otherwise t)))

(defun upgrade (x)
  (if (zerop x)
      0
      (floor (log x 2))))
(defun up2 (x)
  (+ 2 (upgrade (1- (- x)))))

(defun really-downgrade-array (array &rest items)
  (let* ((type (apply 'downgrade-array array items))
	 (new-array
	  (make-array (array-dimensions array)
		      :element-type type)))
    (dotimes (i (array-total-size array))
      (setf (aref new-array i)
	    (aref array i)))
    (values new-array
	    type)))
