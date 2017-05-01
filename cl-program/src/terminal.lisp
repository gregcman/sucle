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

(defun bcolor (&rest values)
  (setf values (nreverse values))
  (let ((acc 0))
    (dolist (value values)
      (setf acc (ash acc 8))
      (setf acc (logior acc value)))
    (logand acc most-positive-fixnum)))
(progn
  (defparameter *color-map* (make-array 256))
  (defun truecolorp (x)
    (logbitp 24 x))
  (defun color-rgb (color)
  ;; fixme: should this return list or (typed?) vector?
  (if (truecolorp color)
      (mod color (ash 1 24))
      (aref *color-map* color)))
  (defun fill-color-map ()
    (dotimes (color 256)
      (labels ((c (r g b)
		 (bcolor r g b))
	       (c6 (x)
		 (let ((b (mod x 6))
		       (g (mod (floor x 6) 6))
		       (r (mod (floor x 36) 6)))
		   (bcolor (floor (* 255.0 (/ r 5.0)))
			   (floor (* 255.0 (/ g 5.0)))
			   (floor (* 255.0 (/ b 5.0))))))
	       (g (x)
		 (c (* x 16) (* x 16) (* x 16))))
	(setf (aref *color-map* color)
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
			 (g (- c 216))))))))))

  (fill-color-map))

(defun char-print-term (x y &optional (term *term*))
  (let ((rowlen (3bst:rows term))
	(collen (3bst:columns term)))
    (dobox ((row 0 rowlen))
	   (dobox ((col 0 collen))
		  (let* ((glyph (3bst:glyph-at (3bst::screen term) row col))
			 (char (3bst:c glyph)))
		    (let ((value (logior (ash (color-rgb (3bst:bg glyph)) 32)
					 (ash (color-rgb (3bst:fg glyph)) 8)
					 (char-code char))))
		      (set-char-with-update (pix:xy-index (+ x col) (- y row)) value)))))))


(defun print-term (&optional (term *term*))
  (loop with dirty = (3bst:dirty term)
     for row below (3bst:rows term)
     do (format t "~,' 2d~a:" row (if (plusp (aref dirty row)) "*" " "))
       (loop for col below (3bst:columns term)
	  for glyph = (3bst:glyph-at (3bst::screen term) row col)
	  for char = (3bst::fg glyph)
	  do (format t "~a" char))
       (format t "~%"))) 
