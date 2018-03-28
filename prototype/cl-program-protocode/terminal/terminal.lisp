(in-package :sandbox)


(progn
  (defun color-fun (color)
    (labels ((bcolor (r g b)
	       (values r g b))
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
		 (g (- c 216)))))))))

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
