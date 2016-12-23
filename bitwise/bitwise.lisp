(in-package :bitwise)
;;print the twos complement binary representation of a number, padded to size
;;size is by default 62 because that is fixnums on 64 bit machines
(defun print-bits (n &optional (size 62))
  (let ((string (concatenate 'string "~" (write-to-string size) ",'0B")))
    (let ((num (ldb (byte size 0) n)))      
      (format t string num)
      num)))

