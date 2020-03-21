(defpackage #:control
  (:use #:cl #:utility)
  (:export
   #:num-key-jp)

  (:export
   #:get-input-characters
   #:*terminal-emulator-p*))

(in-package #:control)

(defun num-key-jp (&optional (type :pressed) (control-state window:*control-state*))
  (find-if 
   (lambda (n)
     (window:button :key type n control-state))
   '(0 1 2 3 4 5 6 7 8 9)))

(defmacro get-control-sequence ((control-state char-var shift control alt super
					       &optional (terminal-sequence nil))
				&body body)
  (once-only (control-state shift control alt super)
    `(let ((something-flag nil))
       (labels ((enter-char (,char-var)
		  ,@body)
		(enter (x)
		  (setf something-flag t)
		  (enter-char x)))
	 (when ,terminal-sequence
	   (flet ((enter-string (string)
		    (let ((len (length string)))
		      (unless (zerop len)
			(setf something-flag t)
			(dotimes (index len)
			  (enter-char (aref string index)))))))
	     (macrolet ((foo (x a b)
			  `(when (window:button :key :repeat ,a)
			     ,(list (ecase x
				      (0 'enter)
				      (1 'enter-string))
				    b))))
	       (foo 0 :enter #\return)
	       (foo 0 :backspace #\del)
	       (foo 0 :tab #\Tab)
	       (foo 1 :up "[A")
	       (foo 1 :down "[B")
	       (foo 1 :left "[D")
	       (foo 1 :right "[C"))))      

	 (window:do-character-keys ((window:control-state-jp-or-repeat ,control-state) true? code)
	   (when true?
	     (multiple-value-bind (char esc)
		 (character-modifier-bits:character-modifier-bits
		  (char-code (char-downcase (code-char code)))
		  ,shift ,control ,alt ,super)
	       (when esc
		 (enter #\esc))
	       (enter (code-char char))))))
       something-flag)))
(defparameter *command-buffer* (make-array 0 :adjustable t :fill-pointer 0 :element-type 'character))
(defparameter *terminal-emulator-p* t)
(defun get-input-characters (&optional (command-buffer *command-buffer*))
  (setf (fill-pointer command-buffer) 0)
  (values
   command-buffer
   (get-control-sequence (window:*control-state*
			       char
			       window:*shift*
			       window:*control*
			       window:*alt*
			       window:*super*
			       *terminal-emulator-p*)
	  (vector-push-extend char command-buffer))))

