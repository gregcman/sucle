(in-package :sandbox)


(defparameter *mouse-rectangle* (vector 0.0 0.0 0.0 0.0))
(defparameter *cursor-rectangle* (vector 0.0 0.0 0.0 0.0))
(progn
  (progn
    (defparameter *old-mouse-x* 0.0)
    (defparameter *old-mouse-y* 0.0))
  (progn
    (defparameter *mouse-x* 0.0)
    (defparameter *mouse-y* 0.0)))

(progn
  (defparameter *block-height* (/ 32.0 1.0))
  (defparameter *block-width* (/ 18.0 1.0)))

(progn
  (defparameter *camera-x* 0)
  (defparameter *camera-y* 0))

(defparameter *chunks* (pix:make-world))
(defparameter *chunk-call-lists* (make-eq-hash))

(defparameter *cam-rectangle* (vector 0 0 0 0))

(defparameter *ticks* 0)

(defparameter *running* nil)

(defun physics ()
  (incf *ticks*)
  (etouq
   (with-vec-params (vec-slots :rectangle
			       (quote ((x0 :x0)
				       (y1 :y1)
				       (x1 :x1)
				       (y0 :y0))))
     (quote (*mouse-rectangle*))
     (quote (declare (type single-float x0 y1 x1 y0)))
     (quote (progn
	      (setf
	       x0 x1
	       y0 y1)
	      (multiple-value-bind (x y) (window:get-mouse-position)
		(setf x1 (- (+ x x) *window-width*)
		      y1 (+ (- (+ y y)) *window-height*)))
	      (etouq
	       (with-vec-params (vec-slots :rectangle
					   (quote ((cx0 :x0)
						   (cy1 :y1)
						   (cx1 :x1)
						   (cy0 :y0))))
		 (quote (*cursor-rectangle* symbol-macrolet))
		 (quote (setf cx0 (floor x0 *block-width*)
			      cy0 (floor y0 *block-height*)
			      cx1 (floor x1 *block-width*)
			      cy1 (floor y1 *block-height*)))))
	      (etouq
	       (with-vec-params (vec-slots :rectangle
					   (quote ((rx0 :x0)
						   (ry1 :y1)
						   (rx1 :x1)
						   (ry0 :y0))))
		 (quote (*mouse-rectangle* symbol-macrolet))
		 (quote (setf rx0 x0
			      ry0 y0
			      rx1 x1
			      ry1 y1))))))))
  (when (skey-j-p :escape)
    (toggle *running*)
    (if *running*
	(copy-string-to-world 0 9 "do stuff now" *white-black-color*)
	(copy-string-to-world 0 9 "drag to move" *white-black-color*)))
  (if *running*
      (when (zerop (mod *ticks* (floor (/ 60 60))))
	(other-stuff))
      (etouq
       (with-vec-params (vec-slots :rectangle
				   (quote ((cx0 :x0)
					   (cy1 :y1)
					   (cx1 :x1)
					   (cy0 :y0))))
	 (quote (*cursor-rectangle*))
	 (quote
	  (when (smice-p :left)
	    (decf *camera-x* (- cx1 cx0))
	    (decf *camera-y* (- cy1 cy0)))))))

  (centered-rectangle *cam-rectangle* *camera-x* *camera-y*
		      (/ e:*width* *block-width*) (/ e:*height* *block-height*)))


(defun centered-rectangle (rect x y width height)
  (etouq
   (with-vec-params (vec-slots :rectangle
			       (quote ((x0 :x0)
				       (y1 :y1)
				       (x1 :x1)
				       (y0 :y0))))
     (quote (rect symbol-macrolet))
     (quote
      (setf
       x0 (- x width)
       y0 (- y height)
       x1 (+ x width)
       y1 (+ y height))))))

(defun set-char-with-update (x y value world)
  (multiple-value-bind (chunk offset) (pix::area x y world)
    (setf (aref chunk offset) value)
    (setf (aref chunk (* 16 16)) *ticks*)))

(defun chunk-update (x y world)
  (multiple-value-bind (chunk offset) (pix::area x y world)
    (setf (aref chunk (* 16 16)) *ticks*)))

(defun (setf get-char) (value x y world)
  (set-char value x y world))

(defun get-char (x y world)
  (multiple-value-bind (chunk offset) (pix::area x y world)
    (aref chunk offset)))

 (defun set-char (value x y world)
  (multiple-value-bind (chunk offset) (pix::area x y world)
    (setf (aref chunk offset) value)))


(progn
  (declaim (ftype (function (t) fixnum) get-char-num))
  (with-unsafe-speed
    (defun get-char-num (obj)
      (typecase obj
	(fixnum obj)
	(cons (get-char-num (car obj)))
	(character (logior *white-black-color* (char-code obj)))
	(t (sxhash obj))))))

(defun print-page (x y)
  (let ((array (gethash (pix:xy-index x y)
			sandbox::*chunks*)))
    (if array
	(let ((fin (make-array (+ 16 (* 16 16)) :element-type 'character)))
	  (let ((counter 0))
	    (dotimes (y 16)
	      (progn (setf (aref fin counter) #\Newline)
		     (incf counter))
	      (dotimes (x 16)
		(let ((value (aref array (+ x (ash y 4)))))
		  (setf (aref fin counter)
			(if value
			    (code-char (mod (get-char-num value) 256))
			    #\Space)))
		(incf counter))))
	  fin))))

(progn
  (declaim (ftype (function (fixnum fixnum t fixnum)
			    (values fixnum fixnum))
		  copy-string-to-world))
  (defun copy-string-to-world (x y string color)
    (let ((start x))
      (let ((len (length string)))
	(dotimes (index len)
	  (let ((char (aref string index)))
	    (cond ((char= char #\Newline)
		   (setf x start y (1- y)))
		  (t
		   (set-char-with-update x y
					 (logior (char-code char) color)
					 *chunks*)
		   (setf x (1+ x))))))
	(values x y)))))

(defun scwu (char x y)
  (set-char-with-update x
			y
			char
			*chunks*))

(defun keyword-ascii (keyword &optional (value (gethash keyword e:*keypress-hash*)))
  (when value
    (let ((code (gethash keyword *keyword-ascii*)))
      (when code
	(let ((mods (ash value (- e::+mod-key-shift+))))
	  (multiple-value-bind (char esc) (convert-char code mods)
	    (values char esc)))))))


(progn
  (defparameter wombo nil)
  (defparameter hello nil)
  (defun test ()
    (setf wombo (vector-circular-node "wombo "))
    (setf hello (vector-nodes2 "hello "))
    (node-splice
     (nthcdr 5 hello)
     wombo)
    (nodes-vector hello)))

(defparameter node nil)

(defparameter directions (alexandria:circular-list :up :left :down :right))
(defun other-stuff ()
  (let ((moved? nil)
	(last-node node))
    (flet ((turnt (a b)
	     (nthfnc (function turn-node) b
		     (cdr
		      (nthfnc (function turn-node) a node)))))
      (with-hash-table-iterator (next e:*keypress-hash*)
	(loop (multiple-value-bind (more key value) (next)
		(if more
		    (let ((ans (keyword-ascii key value)))
		      (when ans
			(when (e::r-or-p (e::get-press-value value))
			  (setf moved? t)
			  (node-splice
			   (turnt 2 2)
			    (vector-circular-node
			     (string (code-char ans)))))))
		    (return)))))
      (when (skey-r-or-p :up)
	(setf moved? t)
	(let ((ans (turnt 1 3)))
	  (when ans
	   (setf node ans))))
      (when (skey-r-or-p :left)
	(setf moved? t)
	(let ((ans (turnt 2 2)))
	  (when ans
	    (setf node ans))))
      (when (skey-r-or-p :down)
	(setf moved? t)
	(let ((ans (turnt 3 1)))
	  (when ans
	    (setf node ans))))
      (when (skey-r-or-p :right)
	(setf moved? t)
	(let ((ans (turnt 4 0)))
	  (when ans
	    (setf node ans))))

      (when (skey-r-or-p :backspace)
	(setf moved? t)
	(let ((ans (turnt 2 2)))
	  (node-disconnect ans)))
      (when (skey-r-or-p :enter)
	(setf moved? t)
	(setf (cdr (node-payload node)) -4))
      (progn
       (when (skey-r-or-p :kp-enter)
	 (setf moved? t)
	 (setf node (turn-node node))
	 (pop directions)
	 (copy-string-to-world 0 5 (symbol-name (car directions)) *white-black-color*))))
    (when moved?
      (clear-screen)
      (unless (eq node last-node)
	(setf (car (node-payload node))
	      (let ((char (car (node-payload node))))
		(if (typep char (quote character))
		    (setf char (char-code char)))
		(typecase char
		  (fixnum (let ((a (logand 255 char)))
			    (logior a (random-color)))))))
	(setf (car (node-payload last-node))
	      (let ((char (car (node-payload last-node))))
		(if (typep char (quote character))
		    (setf char char))
		(typecase char
		  (fixnum (let ((a (logand 255 char)))
			    (logior a *white-black-color*)))))))
      (draw-nodal-text node 0 0 1 -1 nil 64)
      (draw-nodal-text (reverse-node node) 0 0 1 -1 t 64))))

(defun random-color ()
  (logandc1 255 (random most-positive-fixnum)))

(defun color-invert (color)
  (logior (logand 255 color)
	  (logand (logxor most-positive-fixnum 255)
		  (lognot color))))

(defun draw-nodal-text (node x y dx dy reversep &optional (count 32))
  (block nil
    (flet ((draw-forward ()
	     (dotimes (counter count)
	       (unless node
		 (return))
	       (let ((payload (node-payload node)))
		 (let ((char (car payload)))
		   (scwu char x y))
		 (let ((linefeed (cdr payload)))
		   (when linefeed
		     (incf x linefeed)
		     (incf y dy)))
		 (pop node))
	       (incf x dx)))
	   (draw-backwards ()
	     (dotimes (counter count)
	       (unless node
		 (return))
	       (let ((payload (node-payload node)))
		 (unless (zerop counter)
		   (let ((linefeed (cdr payload)))
		     (when linefeed
		       (decf x linefeed)
		       (decf y dy))))
		 (let ((char (car payload)))
		   (scwu char x y))
		 (pop node))
	       (decf x dx))))
      (if reversep
	  (draw-backwards)
	  (draw-forward)))))

(defun clear-screen (&optional (rect *cam-rectangle*))
  (etouq
   (with-vec-params (vec-slots :rectangle
			       (quote ((x0 :x0)
				       (y1 :y1)
				       (x1 :x1)
				       (y0 :y0))))
     (quote (rect let))
     (quote
      (progn
	(dobox ((x (floor x0) (ceiling x1))
		(y (floor y0) (ceiling y1)))
	       (scwu nil x y)))))))

(setf *print-case* :downcase)

(defun print-sexp (sexp)
  (if (listp sexp)
      (if sexp
	  (progn
	    (princ "(")
	    (print-cells sexp))
	  (princ nil))
      (princ sexp)))

(defun emit-spaces (times)
  (dotimes (x times)
    (princ " ")))

(defun print-cells (sexp)
     (let ((cdr (cdr sexp))
	   (car (car sexp)))
       (if (listp car)
	   (if car
	       (progn
		 (princ "(")
		 (print-cells car))
	       (princ nil))
	   (prin1 car))
       (if (listp cdr)
	   (if cdr
	       (progn
		 (princ " ")
		 (print-cells cdr))
	       (princ ")"))
	   (progn
	     (princ " . ")
	     (prin1  cdr)
	     (princ ")")))))

(defparameter *test-tree*
  (quote 
   (defun print-cells (sexp)
     (let ((cdr (cdr sexp))
	   (car (car sexp)))
       (if (listp car)
	   (if car
	       (progn
		 (princ "(")
		 (print-cells car))
	       (princ nil))
	   (prin1 car))
       (if (listp cdr)
	   (if cdr
	       (progn
		 (princ " ")
		 (print-cells cdr))
	       (princ ")"))
	   (progn
	     (princ " . ")
	     (prin1  cdr)
	     (princ ")")))))))

(defun reset-test ()
  (test)
  (setf node hello))
