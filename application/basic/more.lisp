(defpackage :terminal-test
  (:use #:cl))
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

(defparameter *term* nil)
(defun reset-term ()
  (reset-ssh)
  (setf *term* (make-instance '3bst:term :rows 24 :columns 80))
  (update-terminal-stuff))

(defparameter *text* (make-array 0 :fill-pointer t :element-type 'character))
(defun update-terminal-stuff (&optional (term *term*) (proc *proc*))
  (let ((command *text*))
    (when (sb-ext:process-alive-p proc)
      (collect-all-output (sb-ext:process-output proc)
			  command))
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

(progn
  (defun enter (args &optional (proc *proc*))
    (progn
      (let ((in (sb-ext:process-input proc)))
	(write-string args in)
	(finish-output in))))

  (defun term-cursor-info ()
    (with-slots (3bst::x 3bst::y 3bst::state 3bst::attributes) (with-slots (3bst::cursor) *term* 3bst::cursor)
      (values 3bst::x 3bst::y 3bst::state 3bst::attributes))))

(defun print-term (&optional (term *term*))
  (loop with dirty = (3bst:dirty term)
     for row below (3bst:rows term)
     do (format t "~,' 2d~a:" row (if (plusp (aref dirty row)) "*" " "))
       (loop for col below (3bst:columns term)
	  for glyph = (3bst:glyph-at (3bst::screen term) row col)
	  for char = (3bst::c glyph)
	  do (format t "~a" char))
       (format t "~%")))
