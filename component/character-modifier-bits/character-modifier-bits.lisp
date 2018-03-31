(defpackage #:character-modifier-bits
  (:use #:cl)
  (:export
   #:character-modifier-bits))
(in-package #:character-modifier-bits)

(defun ascii-control (char)
  (logxor (ash 1 6) char))

(defparameter *char-char*
  "`~1!2@3#4$5%6^7&8*9(0)-_=+qQwWeErRtTyYuUiIoOpP[{]}\\|aAsSdDfFgGhHjJkKlL;:'\"zZxXcCvVbBnNmM,<.>/?")
(defparameter *char-codes* (map 'vector (lambda (x) (char-code x)) *char-char*))
(defparameter *upcase-codes*
  (let ((array (make-array 128)))
    (dotimes (x 128)
      (setf (aref array x)
	    (char-code (char-upcase (code-char x)))))
    array))
(defparameter *shifted-keys* (make-array 128))
(defparameter *controlled-keys*
  (let ((array (make-array 128)))
    (dotimes (x 128)
      (setf (aref array x)
	    (ascii-control x)))
    array))

(defun set-shift-conversion (&optional (shift-keys *char-codes*))
  (loop for i from 0 below (length shift-keys) by 2 do	 
       (let ((code (aref shift-keys i)))
	 (setf (aref *shifted-keys* code)
	       (aref shift-keys (1+ i))))))

(set-shift-conversion)

(defun character-modifier-bits (byte &optional shift control meta super)
  (declare (ignorable super))
  (when shift
    (setf byte (aref *shifted-keys* byte)))
  (when (or meta control)
    (setf byte (aref *upcase-codes* byte)))
  (when control
    (setf byte (aref *controlled-keys* byte)))
  (values byte
	  meta))
