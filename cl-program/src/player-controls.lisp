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
  (when (skey-j-p :caps-lock) (toggle *running*))
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
  (declaim (ftype (function (fixnum fixnum (vector character) fixnum)
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

(defun other-stuff ()
  (let ((mousex (+ *camera-x* (aref *cursor-rectangle*
				    (etouq (caar (vec-slots :rectangle (quote ((nil :x1)))))))))
	(mousey (+ *camera-y* (aref *cursor-rectangle*
				    (etouq (caar (vec-slots :rectangle (quote ((nil :y1))))))))))
    (let ((a (load-time-value (cons 0 0)))
	  (b (load-time-value (cons 0 0)))
	  (c (load-time-value (cons 0 0)))
	  (d (load-time-value (make-array 0 :fill-pointer 0 :adjustable t :element-type 'character))))
      (with-let-mapped-places ((x (car a))
			       (y (cdr a))
			       (val1 (car b))
			       (val2 (cdr b))
			       (val3 (car c))
			       (val4 (cdr c)))
	(when (smice-p :right)
	  (let ((char (get-char mousex mousey *chunks*)))
	    (setf (fill-pointer d) 0)
	    (let ((*print-base* 36))
	      (with-output-to-string (str d)
		(print char str)))
	    (copy-string-to-world mousex mousey d (if char (logandc2
							    (GET-char-num char) 255)
						      (logandc2 (random most-positive-fixnum) 255)))))

	(when (skey-j-p :w)
	  (let ((char (get-char mousex mousey *chunks*)))
	    (set-char-with-update mousex mousey (cons char char) *chunks*)))

	(progn
	 (when (skey-p :a)
	   (delete-fill mousex mousey))
	 (when (smice-p :left)
	   (let ((char (get-char mousex mousey *chunks*)))
	    ;; (print char)
	     (cond ((pointer-p char)
		    (pull-pointer mousex mousey char))))))
	(when (skey-j-p :e)
	  (setf (fill-pointer *qux*) 0)
	  (klear)
	  (test3))
	(when (skey-p :space)
	  (wow))))))

(defun make-pointer (x y)
  (cons :pointer (cons x y)))
(defun pointer-p (pointer)
  (when (listp pointer)
    (eq :pointer (car pointer))))
(defun deref-pointer (pointer)
  (let ((coord (cdr pointer)))
    (values (car coord) (cdr coord))))

(defun pointer-alive-p (xp yp data)
  (multiple-value-bind (xdelta ydelta) (deref-pointer data)
    (let ((absx (+ xp xdelta))
	  (absy (+ yp ydelta)))
      (let ((num (get-char absx absy *chunks*)))
	(let ((in-use-p 0))
	  (when num
	    (when (typep num (quote fixnum))
	      (flet ((neighbor (dx dy)
		       (let ((neighborx (+ dx absx))
			     (neighbory (+ dy absy)))
			 (let ((char (get-char neighborx neighbory *chunks*)))
			   (cond ((pointer-p char)
				  (multiple-value-bind (x y) (deref-pointer char)
				    (if (and (zerop (+ xdelta x))
					     (zerop (+ ydelta y)))
					(incf in-use-p)))))))))
		(etouq
		 (let ((start 56))
		   (let ((left (ash 1 (+ 0 start)))
			 (down (ash 1 (+ 1 start)))
			 (right (ash 1 (+ 2 start)))
			 (up (ash 1 (+ 3 start))))
		     `(progn
			(when (logtest num ,left)
			  (neighbor -1 0))		
			(when (logtest num ,down)
			  (neighbor 0 -1))		
			(when (logtest num ,right)
			  (neighbor 1 0))		
			(when (logtest num ,up)
			  (neighbor 0 1)))))))))
	  in-use-p)))))

(defun pull-pointer (xp yp data)
  (let ((pointers-to-update
	 (let ((buf (load-time-value (make-array 0 :adjustable t :fill-pointer 0))))
	   (setf (fill-pointer buf) 0)
	   buf))
	(new-pointers
	 (let ((buf (load-time-value (make-array 0 :adjustable t :fill-pointer 0))))
	   (setf (fill-pointer buf) 0)
	   buf)))
    (multiple-value-bind (xdelta ydelta) (deref-pointer data)
      (let ((absx (+ xp xdelta))
	    (absy (+ yp ydelta)))
	(let ((num (get-char absx absy *chunks*)))
	  (when num
	    (when (typep num (quote fixnum))
	      (let ((movable-p t)
		    (last-one t))
		(flet ((neighbor (dx dy)
			 (let ((neighborx (+ dx absx))
			       (neighbory (+ dy absy)))
			   (unless (and (= neighborx xp)
					(= neighbory yp))
			     (let ((char (get-char neighborx neighbory *chunks*)))
			       (cond ((pointer-p char)
				      (multiple-value-bind (x y) (deref-pointer char)
					(if (and (zerop (+ xdelta x))
						 (zerop (+ ydelta y)))
					    (progn
					    ;;;pointing the correct direction, update on move
					      (flet ((add-item (x)
						       (vector-push-extend x pointers-to-update)))
						(add-item neighborx) ;;where it is
						(add-item neighbory)
						(add-item char)))
					    (progn
					      (print "lol")
					     ;;;not pointing in same direction
					      (setf movable-p nil)))))
				     (t
			      ;;;; its data
				      (setf last-one nil)
				      (let ((offsetx (+ xp dx))
					    (offsety (+ yp dy)))
					(let ((new-pos (get-char offsetx offsety *chunks*)))
					  (if new-pos
					      (progn
						(cond ((pointer-p new-pos)
						       (multiple-value-bind (xold yold)
							   (deref-pointer new-pos)
							 (if (and (= xdelta xold)
								  (= ydelta yold))
							     (progn)
							     (setf movable-p nil))))
						      (t 
						       (setf movable-p nil)))) ;;cannot transfer, data in the way
					      (progn
				      ;;;;space for new triangles, remember
				      ;;;the new space and the place it points to
				      ;;;so a new triangle can be created
						(flet ((add-item (x)
							 (vector-push-extend x new-pointers)))
						  (add-item offsetx) ;;where it will be
						  (add-item offsety)))))))))))))
		  (etouq
		   (let ((start 56))
		     (let ((left (ash 1 (+ 0 start)))
			   (down (ash 1 (+ 1 start)))
			   (right (ash 1 (+ 2 start)))
			   (up (ash 1 (+ 3 start))))
		       `(progn
			  (when (logtest num ,left)
			    (neighbor -1 0))		
			  (when (logtest num ,down)
			    (neighbor 0 -1))		
			  (when (logtest num ,right)
			    (neighbor 1 0))		
			  (when (logtest num ,up)
			    (neighbor 0 1)))))))
		(when movable-p
		  (progn (set-char-with-update xp yp num *chunks*))
		  (let ((new-pointer-count (fill-pointer new-pointers)))
		   (dobox ((offset 0 new-pointer-count :inc 2))
			  (etouq
			   (with-vec-params '((offset nx ny))
			     '(new-pointers)
			     '(set-char-with-update nx ny
			       (make-pointer xdelta ydelta) *chunks*)))))
		  (if last-one
		    ;;;dont leave a pointer
		       (progn
			 (set-char-with-update absx absy nil *chunks*)
			 )

		    ;;;;leave a pointer
		       (progn
			 (set-char-with-update absx absy (make-pointer (- xdelta)
								       (- ydelta)) *chunks*)
			 )
		       )
		  (dobox ((offset 0 (fill-pointer pointers-to-update) :inc 3))
			 (etouq
			  (with-vec-params '((offset sx sy data))
			    '(pointers-to-update)
			    '(if (zerop (pointer-alive-p sx sy data))
			      (set-char-with-update sx sy nil *chunks*))))))))))))))

(progn
  (eval-always
   (defun damn-test (l)
      (let ((num 0))
	(etouq
	 (let ((start 56))
	   (let ((left (ash 1 (+ 0 start)))
		 (down (ash 1 (+ 1 start)))
		 (right (ash 1 (+ 2 start)))
		 (up (ash 1 (+ 3 start))))
	     `(progn
		(when (member :left l) (setf num (logior num ,left)))
		(when (member :down l) (setf num (logior num ,down)))
		(when (member :right l) (setf num (logior num ,right)))
		(when (member :up l) (setf num (logior num ,up)))))))
	num)))

   (defun damn-test2 (x y)
     (flet ((uh (dx dy value)
	      (set-char-with-update (+ dx x) (+ dy y) (logior
						       (damn-test value)
						       (random (aref pix::+n-bits+ 56))) *chunks*)))
       (uh -1 -1 '(:up :right))
       (uh 0 -1 '(:up :left :right))
       (uh 1 -1 '(:up :left))
       (uh -1 0 '(:up :down :right))
       (uh 0 0 '(:up :down :left :right))
       (uh 1 0 '(:up :down :left))
       (uh -1 1 '(:down :right))
       (uh 0 1 '(:down :left :right))
       (uh 1 1 '(:down :left))))

   (defun damn-test3 (x0 y0 x1 y1)
     ;;(let ((char (random (aref pix::+n-bits+ 56)))))
     (dobox ((x x0 x1)
	     (y y0 y1))
	    (set-char-with-update x y (if nil
					  (random (aref pix::+n-bits+ 56))
					  (Logior *white-black-color* (random 256))) *chunks*))
     (dobox ((x (1+ x0) x1)
	     (y y0 y1))
	    (let ((char (get-char x y *chunks*)))
	      (set-char-with-update x y (logior (etouq (damn-test (quote (:left))))
						char)
				    *chunks*)))
     (dobox ((x x0 x1)
	     (y (1+ y0) y1))
	    (let ((char (get-char x y *chunks*)))
	      (set-char-with-update x y (logior (etouq (damn-test (quote (:down))))
						char)
				    *chunks*)))
     (dobox ((x x0 (1- x1))
	     (y y0 y1))
	    (let ((char (get-char x y *chunks*)))
	      (set-char-with-update x y (logior (etouq (damn-test (quote (:right))))
						char)
				    *chunks*)))
     (dobox ((x x0 x1)
	     (y y0 (1- y1)))
	    (let ((char (get-char x y *chunks*)))
	      (set-char-with-update x y (logior (etouq (damn-test (quote (:up))))
						char)
				    *chunks*)))))

(defun klear ()
  (map-box (lambda (x y)
	     (set-char-with-update x y nil *chunks*))))

(progn
   (progn
     (declaim (ftype (function (fixnum fixnum fixnum (function (fixnum fixnum))))
		     map-neighbors))
     (etouq
      (let ((start 56))
	`(defun map-neighbors (x y num func)
	   (when (logtest num ,(ash 1 (+ 0 start)))
	     (funcall func (1- x) y))			   ;;left
	   (when (logtest num ,(ash 1 (+ 1 start)))
	     (funcall func x (1- y)))			   ;;down
	   (when (logtest num ,(ash 1 (+ 2 start)))
	     (funcall func (1+ x) y))			   ;;right
	   (when (logtest num ,(ash 1 (+ 3 start)))
	     (funcall func x (1+ y))))))) ;;up

   (defun delete-fill (x y)
     (let ((char (get-char x y *chunks*)))
       (when (typep char (quote fixnum))
	 (set-char-with-update x y nil *chunks*)
	 (map-neighbors x y char (function delete-fill)))))

   (defun map-box (func)
     (dobox ((y 0 32)
	     (x 0 32))
	    (funcall func x y)))

   (defun huh ()
     (map-box
      (lambda (x y)
	(let ((char (get-char x y *chunks*)))
	  (when (typep char (quote fixnum))
	    (set-char-with-update x y (logior char (ash (random 16) 56)) *CHUnks*)))))))

(progn
   (defparameter *baz* (make-array 0 :adjustable t :fill-pointer 0))
   (defun test3 ()
     (let ((vertical nil)
	   (horiz t))
       (let ((w 32)
	     (h 5))
	 (damn-test3 0 0 w h)
	 (setf (fill-pointer *baz*) 0)
	 (flet ((add (x)
		  (vector-push-extend x *baz*)))
	   (let ((woffset (random w))
		 (hoffset (random h)))
	     (let ((x (+ woffset (if horiz 0 (* 1 w))))
		   (y (+ hoffset (if vertical 0 (* 1 h)))))
	       (progn
		 (add x)
		 (add y)
		 (add woffset)
		 (add hoffset))
	       (set-char-with-update x y
				     (make-pointer (- woffset x)
						   (- hoffset y)) *chunks*)))
	   (when t
	     (let ((woffset (random w))
		   (hoffset (random h)))
	       (let ((x (+ woffset (if horiz 0 (* 2 w))))
		     (y (+ hoffset (if vertical 0 (* 2 h)))))
		 (progn
		   (add x)
		   (add y)
		   (add woffset)
		   (add hoffset))
		 (set-char-with-update x y
				       
				       (make-pointer (- woffset x)
						     (- hoffset y)) *chunks*)))

	     (let ((woffset (random w))
		   (hoffset (random h)))
	       (let ((x (+ woffset (if horiz 0 (* 3 w))))
		     (y (+ hoffset (if vertical 0 (* 3 h)))))
		 (progn
		   (add x)
		   (add y)
		   (add woffset)
		   (add hoffset))
		 (set-char-with-update 
		  x y
		  (make-pointer (- woffset x)
				(- hoffset y)) *chunks*)))))))))

(defun wow ()
     (map-box (lambda (x y)
		(let ((char (get-char x y *chunks*)))
		  (cond ((pointer-p char)		   
			 (when (zerop (random 10))
			   (pull-pointer x y char
			    ))))))))
(defun test23 ()
  (set-char-with-update 6 8 (make-pointer 1 0) *chunks*)
  (set-char-with-update 8 8 (make-pointer -1 0) *chunks*)
  (set-char-with-update 7 8(logior (damn-test '(:right :left))
				   *white-black-color* (char-code #\G)) *chunks*))

(defun test24 ()
  (set-char-with-update 6 8 (make-pointer 4 0) *chunks*)
  (set-char-with-update 11 8 (make-pointer -4 0) *chunks*)
  (set-char-with-update 7 8(logior (damn-test '(:right :left))
				   *white-black-color* (char-code #\G)) *chunks*)
  (set-char-with-update 8 8(logior (damn-test '(:right :left))
				   *white-black-color* (char-code #\R)) *chunks*)
  (set-char-with-update 9 8(logior (damn-test '(:right :left))
				   *white-black-color* (char-code #\E)) *chunks*)
  (set-char-with-update 10 8(logior (damn-test '(:right :left))
				    *white-black-color* (char-code #\G)) *chunks*))

(defun test25 ()
  (let ((connect (damn-test '(:up :down))))
    (set-char-with-update 8 6 (make-pointer 0 4) *chunks*)
    (set-char-with-update 8 11 (make-pointer 0 -4) *chunks*)
    (set-char-with-update 8 7(logior connect
			      *white-black-color* (char-code #\G)) *chunks*)
    (set-char-with-update 8 8(logior connect
				     *white-black-color* (char-code #\R)) *chunks*)
    (set-char-with-update 8 9(logior connect
				     *white-black-color* (char-code #\E)) *chunks*)
    (set-char-with-update 8 10(logior connect
				      *white-black-color* (char-code #\G)) *chunks*)))

(defun test26 ()
  (let ((connect1 (damn-test '(:right :down)))
	(connect2 (damn-test '(:left :up))))
    (set-char-with-update 11 10 (make-pointer -2 -2) *chunks*)
    (set-char-with-update 8 8 (make-pointer 2 2) *chunks*)
    (set-char-with-update 9 8(logior connect2
				     *white-black-color* (char-code #\G)) *chunks*)
    (set-char-with-update 9 9(logior connect1
				     *white-black-color* (char-code #\R)) *chunks*)
    (set-char-with-update 10 9(logior connect2
				      *white-black-color* (char-code #\E)) *chunks*)
    (set-char-with-update 10 10(logior connect1
				       *white-black-color* (char-code #\G)) *chunks*)))
