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

;;;;
;;;;Tests
;;;;

(defun typetest (item test)
  (let ((result (typep item test)))
    (if result
	(format t "~% ~a is ~a" item test)
	(format t "~% ~a is not ~a" item test))))
(defun test ()
  (typetest 1 '(signed-byte 1))
  (typetest -1 '(signed-byte 1))
  (typetest 256 '(signed-byte 8))
  (typetest -256 '(signed-byte 8))
  (typetest 127 '(signed-byte 8))
  (typetest -127 '(signed-byte 8))
  (typetest 128 '(signed-byte 8))
  (typetest -128 '(signed-byte 8)))

(defun test-at-random (&optional (min (- (random most-positive-fixnum)))
			 (max (random most-positive-fixnum)))
  (let ((type (num-type min max)))
    (typetest min type)
    (typetest max type)))

(defun test-again (&optional (s (random 64)))
  (let ((foo (expt 2 (1- s))))
    (test-at-random (- foo) (1+ foo))))

(defun test0 ()
  (print (downgrade-array #(0 1 2 3 4 5)))
  (print (downgrade-array #(0 1 2 3 4 5 34524352345)))
  (print (downgrade-array #(-345345 0 1 2 3 4 5))))

(defun test1 ()
  (let ((*print-readably* t))
    (print
     (list
      (really-downgrade-array #(1 1 0 0 0 1 0 1 0))
      (really-downgrade-array #(1 1 0 0 0 1 0 -1 0))
      (really-downgrade-array #(1 1 0 0 0 1 0 -345345 0))))))
