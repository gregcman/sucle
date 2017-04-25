(in-package :sandbox)

(defparameter *proc* nil)

(defun run-program (command)
  (sb-ext:run-program  (car command)
		       (cdr command)
		       :wait nil
		       :output :stream
		       :input :stream
		       :external-format :utf-8))

(defun reset-ssh ()
  (when *proc*
    (sb-ext:process-kill *proc* 9))
  (let ((command (list "/usr/bin/ssh" "-tt" "terminal256@0.0.0.0")))
    (setf *proc* (run-program command))))

(defparameter *term* nil)

(defun reset-term ()
  (reset-ssh)
  (setf *term* (make-instance '3bst:term :rows 25 :columns 80))
  (update-terminal-stuff))

(defun update-terminal-stuff (&optional (term *term*) (proc *proc*))
  (when (sb-ext:process-alive-p proc)
    (tagbody rep
       (let ((c (read-char-no-hang (sb-ext:process-output proc)
				   nil :eof)))
	 (cond ((eq c :eof))
	       ((eq c nil))
	       (t (let ((a (load-time-value " ")))
		    (setf (aref a 0) c)
		    (3bst:handle-input a :term term))
		  (go rep)))))))

(defun char-print-term (x y &optional (term *term*))
  (let ((dirty (3bst:dirty term))
	(rowlen (3bst:rows term))
	(collen (3bst:columns term)))
    (dobox ((row 0 rowlen))
	   (if (zerop (aref dirty row))
	       (dobox ((col 0 collen))
		      (let* ((glyph (3bst:glyph-at (3bst::screen term) row col))
			     (char (3bst:c glyph))
			     (value (logior *white-black-color* (char-code char))))
			(set-char-with-update (pix:xy-index (+ x col) (- y row)) value)))
	       (setf (aref dirty row) 0)))))


(defun print-term (&optional (term *term*))
  (loop with dirty = (3bst:dirty term)
     for row below (3bst:rows term)
     do (format t "~,' 2d~a:" row (if (plusp (aref dirty row)) "*" " "))
       (loop for col below (3bst:columns term)
	  for glyph = (3bst:glyph-at (3bst::screen term) row col)
	  for char = (3bst::mode glyph)
	  do (format t "~a" char))
       (format t "~%")))

(defun enter (args &optional (proc *proc*))
  (progn
    (let ((in (sb-ext:process-input proc)))
      (format in args)
      (finish-output in))))

(defun term-cursor-info ()
 (with-slots (3bst::x 3bst::y 3bst::state 3bst::attributes) (with-slots (3bst::cursor) *term* 3bst::cursor)
   (values 3bst::x 3bst::y 3bst::state 3bst::attributes))) 
