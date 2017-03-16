(in-package :sandbox)

(eval-when (:load-toplevel :execute :compile-toplevel)
  (defconstant six-bits-in-a-fixnum (floor (logcount most-positive-fixnum) 6)))
;;;the amount of alpha-numeric characters one can put into a single
;;;fixnum on a given machine. turns out to be 11 on 64 bit machines
;;;and 5 on 32 bit machines.
;;;passing around alphanumeric strings inside integers
;;;how convenient
(defun max-fixnum-string-length ()
  (floor (log most-positive-fixnum 36)))

(defun fresh-fixnum-string ()
  (make-array six-bits-in-a-fixnum :element-type 'character :initial-element #\ ))

(defun string-number (string)
  (let ((correct (fresh-fixnum-string)))
    (dotimes (x (min (length string) six-bits-in-a-fixnum))
      (setf (schar correct x) (char-upcase (schar string x))))
    (values
     (%string-number correct)
     correct)))

(defun %string-number (string)
  (declare (type (simple-array character *)))
  (let ((num 0)
	(thash 0))
    (declare (type fixnum num thash))
    (dotimes (x six-bits-in-a-fixnum)
      (incf num (ash (- (char-code (schar string x)) 32) thash))
      (incf thash 6))
    num))

(defun number-string (number)
  (declare (type fixnum number))
  (let ((string (fresh-fixnum-string)))
    (declare (type (simple-array character #.six-bits-in-a-fixnum)))
    (dotimes (x six-bits-in-a-fixnum)
      (if (zerop number)
	  (return))
      (setf (schar string x) (code-char (+ 32 (mod number 64))))
      (setf number (ash number -6)))
    string))

;;;;turn a string made of letters, [also a minus sign]
;;;;and numbers into a fixnum 
(defun %string-number (string)
  (logand (parse-integer string :radix 36) most-positive-fixnum))

(defparameter *number-names!* (make-hash-table :test 'equal))
(defun name-it (string)
  (setf (gethash string *number-names!*) (string-number string)))
;;;greg - 782008 lisp - 1004137 

(defun print-number-letters (number)
  (let ((*print-base* 36))
    (print number)))
