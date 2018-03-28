(in-package :sandbox)


(progn)

(defun char-print-term (x y world term)
  (let ((rowlen (3bst:rows term))
	(collen (3bst:columns term)))
    (dobox ((row 0 rowlen))
	   (dobox ((col 0 collen))
		  (let* ((glyph (3bst:glyph-at (3bst::screen term) row col))
			 (char (3bst:c glyph)))
		    (let ((value (logior (ash (color-rgb (3bst:bg glyph)) 32)
					 (ash (color-rgb (3bst:fg glyph)) 8)
					 (char-code char))))
		      (set-char-with-update (+ x col) (- y row) value world)))))))


(defun terminal-stuf2 ()
  (progn  
    (unless (zerop (fill-pointer *command-buffer*))
      (setf (fill-pointer *command-buffer*) 0))
    (get-control-sequence *command-buffer*)

    (progn
      (terminal-stuff 0 0 *command-buffer* *chunks*))))

(defun terminal-stuff (startx starty command world)
  (progn
    (when (update-terminal-stuff)
      (char-print-term startx
		       starty world *term*)
      (multiple-value-bind (x y state other) (term-cursor-info)
	(declare (ignorable state other))
	(let ((newx (+ startx x))
	      (newy (- starty y)))
	  (let ((char (get-char newx newy world)))
	    (when char
	      (set-char
	       (logxor char (dpb -1 (byte (* 6 8) 8) 0))
	       newx newy
	       world))))))
    (enter command)))
