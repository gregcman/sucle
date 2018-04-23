(defpackage :terminal-test
  (:use #:cl #:utility))
(in-package :terminal-test)

(defun run-program (command)
  (sb-ext:run-program  (car command)
		       (cdr command)
		       :wait nil
		       :output :stream
		       :input :stream
		       :external-format :utf-8))

(defparameter *proc* nil)
(defun reset-ssh ()
  (kill-proc *proc*)
  (let ((command
	 (list "/usr/bin/ssh" "-tt" "localhost"
		       )))
    (setf *proc* (run-program command))))

(defun kill-proc (proc)
  (when proc
    (sb-ext:process-kill proc 9)))

(defun kill ()
  (kill-proc *proc*))

(defparameter *width* 80)
(defparameter *height* 25)

(defparameter *term* nil)
(defun reset-term ()
  (reset-ssh)
  (setf *term* (make-instance '3bst:term :rows *height* :columns *width*))
  (enter
   (format nil "
stty rows ~a
stty columns ~a
export TERM=~a
" *height* *width* (etouq (nth 0 '("xterm" "xterm-256color")))))
  (update-terminal-stuff))

(defun living-process-p (proc)
  (and (sb-ext:process-p proc)
       (sb-ext:process-alive-p proc)))

(defparameter *text* (make-array 0 :fill-pointer t :element-type 'character))
(defun update-terminal-stuff (&optional (term *term*) (proc *proc*))
  (let ((command *text*))
    (if (living-process-p proc)
	(collect-all-output (sb-ext:process-output proc)
			    command)
	(setf *term* nil))
    (cond ((zerop (fill-pointer command))
	   nil)
	  (t (progn (3bst:handle-input command :term term)
		    (setf (fill-pointer command) 0))
	     t))))

(defun collect-all-output (stream vector)
  (tagbody rep
     (let ((c (read-char-no-hang stream nil :eof)))
       (if (eq c :eof)
	   nil
	   (when c
	     (vector-push-extend c vector)
	     (go rep))))))

(defun enter (args &optional (proc *proc*))
  (let ((alive? (living-process-p proc)))
    (when alive?
      (let ((in (sb-ext:process-input proc)))
	(write-string args in)
	(finish-output in)))
    alive?))

(defun color-fun (color)
    (labels ((bcolor (r g b)
	       (values (/ (floatify r) 255.0)
		       (/ (floatify g) 255.0)
		       (/ (floatify b) 255.0)))
	     (c (r g b)
	       (bcolor r g b))
	     (c6 (x)
	       (let ((b (mod x 6))
		     (g (mod (floor x 6) 6))
		     (r (mod (floor x 36) 6)))
		 (bcolor (* 51 r)
			 (* 51 g)
			 (* 51 b))))
	     (g (x)
	       (c (* x 16) (* x 16) (* x 16))))
      (case color
	(0 (c 0 0 0))
	(1 (c 205 0 0))
	(2 (c 0 205 0))
	(3 (c 205 205 0))
	(4 (c 0 0 238))
	(5 (c 205 0 205))
	(6 (c 0 205 205))
	(7 (c 229 229 229))
	(8 (c 127 127 127))
	(9 (c 255 0 0))
	(10 (c 0 255 0))
	(11 (c 255 255 0))
	(12 (c 92 92 255))
	(13 (c 255 0 255))
	(14 (c 0 255 255))
	(15 (c 255 255 255))
	(t (let ((c (- color 16)))
	     (if (< c 216)
		 (c6 c)
		 (g (- c 216))))))))

(defmacro do-term-values ((glyph x y &optional (term '*term*)) &body body)
  (once-only (term)
    `(dobox ((,y 0 (3bst:rows ,term)))
	    (dobox ((,x 0 (3bst:columns ,term)))
		   (let ((,glyph (3bst:glyph-at (3bst::screen ,term) ,y ,x)))
		     ,@body)))))

(defun print-term (&optional (term *term*))
  (loop with dirty = (3bst:dirty term)
     for row below (3bst:rows term)
     do (format t "~,' 2d~a:" row (if (plusp (aref dirty row)) "*" " "))
       (loop for col below (3bst:columns term)
	  for glyph = (3bst:glyph-at (3bst::screen term) row col)
	  for char = (3bst::c glyph)
	  do (format t "~a" char))
       (format t "~%")))
