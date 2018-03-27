(defpackage #:keys-to-chars
  (:use #:cl #:utility))
(in-package #:keys-to-chars)

(defun ascii-control (char)
  (logxor (ash 1 6) char))
(progn
  (defparameter *shifted-keys* (make-array 128))
  (defparameter *controlled-keys* (make-array 128))
  (let ((shift-keys
	 "`~1!2@3#4$5%6^7&8*9(0)-_=+qQwWeErRtTyYuUiIoOpP[{]}\\|aAsSdDfFgGhHjJkKlL;:'\"zZxXcCvVbBnNmM,<.>/?"))
    (loop for i from 0 below (length shift-keys) by 2 do	 
	 (let ((code (char-code (aref shift-keys i))))
	   (setf (aref *shifted-keys* code)
		 (char-code (aref shift-keys (1+ i)))))))
  (dotimes (x 128)
    (setf (aref *controlled-keys* x)
	  (ascii-control x))))

(defun convert-char (char shift control alt super)
  (declare (ignorable super))
  (when shift
    (setf char (aref *shifted-keys* char)))
  (let ((meta alt))
    (when (or meta control)
      (setf char (char-code (char-upcase (code-char char)))))
    (when control
      (setf char (aref *controlled-keys* char)))
    (values char
	    (if meta
		(etouq (char-code #\esc))
		nil))))

(defmacro get-control-sequence ((control-state char-var shift control alt super) &body body)
  (once-only (control-state shift control alt super)
    `(let ((something-flag nil))
       (labels ((enter-string (string)
		  (let ((len (length string)))
		    (unless (zerop len)
		      (setf something-flag t)
		      (dotimes (index len)
			(enter-char (aref string index))))))
		(enter-char (,char-var)
		  ,@body)
		(enter (x)
		  (setf something-flag t)
		  (enter-char x)))
	 (macrolet ((foo (x a b)
		      `(when (window::skey-j-p (window::keyval ,a))
			 ,(list (ecase x
				  (0 'enter)
				  (1 'enter-string)) b))))
	   (foo 0 :enter #\return)
	   (foo 0 :backspace #\del)
	   (foo 0 :tab #\Tab)
	   (foo 1 :up "[A")
	   (foo 1 :down "[B")
	   (foo 1 :left "[D")
	   (foo 1 :right "[C"))      

	 (window::do-character-keys ((window::control-state-jp ,control-state) true? code)
	   (when true?
	     (multiple-value-bind (char esc)
		 (convert-char (char-code (char-downcase (code-char code)))
			       ,shift ,control ,alt ,super)
	       (when esc
		 (enter #\esc))
	       (enter (code-char char))))))
       something-flag)))
