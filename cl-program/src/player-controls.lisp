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
	(t (etouq (sxhash nil)))))))

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
	(when (smice-p :left)
	  (delete-fill mousex mousey))))))

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
  (dobox ((x 0 128)
	  (y 0 128))
	 (funcall func x y)))

(defun huh ()
  (map-box
   (lambda (x y)
     (let ((char (get-char x y *chunks*)))
       (when (typep char (quote fixnum))
	 (set-char-with-update x y (logior char (ash (random 16) 56)) *CHUnks*))))))

(defun update-elastic (xtri ytri xloc yloc)
  (let ((squares (let ((buf (load-time-value (make-array 0 :adjustable t :fill-pointer 0))))
		   (setf (fill-pointer buf) 0)
		   buf))
	(new-triangles (let ((buf (load-time-value (make-array 0 :adjustable t :fill-pointer 0))))
			 (setf (fill-pointer buf) 0)
			 buf)))
    (let ((num (get-char xloc yloc *chunks*)))
      (when num
	(when (typep num (quote fixnum))
	  (let ((move-p t))
	    (flet ((neighbor (dx dy)
		     (let ((neighborx (+ dx xloc))
			   (neighbory (+ dy yloc))
			   (trioffsetx (+ dx xtri))
			   (trioffsety (+ dy ytri)))
		       (let ((char (get-char neighborx neighbory *chunks*)))
			 (cond ((square-p char)
			      ;;;;reverse square unless transferring to the correct location
				(multiple-value-bind (squarexto squareyto) (deref-square char)
				  (if (and (= squarexto trioffsetx)
					   (= squareyto trioffsety))
				      ;;its transferring the correct way, make sure to notify on move
				      (progn
					(flet ((add-item (x)
						 (vector-push-extend x squares)))
					  (add-item squarexto)
					  (add-item squareyto)
					  (add-item neighborx)
					  (add-item neighbory)))
				      ;;else reverse it
				      (progn
					(setf move-p nil)))))
			       ((triangle-p char)
			      ;;;;wait for it to get out of the way, or add it to the active cell states
				(setf move-p nil))
			       (t
			      ;;;; its data
				(let ((new-pos (get-char trioffsetx trioffsety *chunks*)))
				  (if new-pos
				      (progn
					(setf move-p nil)) ;;cannot transfer, data in the way
				      (progn
				      ;;;;space for new triangles, remember
				      ;;;the new space and the place it points to
				      ;;;so a new triangle can be created
					(flet ((add-item (x)
						 (vector-push-extend x new-triangles)))
					  (add-item trioffsetx) 
					  (add-item trioffsety) 
					  (add-item neighborx)
					  (add-item neighbory)))))
				))))))
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
	    (when move-p
	      ;;;;move the item
	      (progn (set-char-with-update xtri ytri num *chunks*))
	      (let ((new-tri-count (fill-pointer new-triangles)))
		(if (zerop new-tri-count)
		    ;;;dont leave a square, because theres no one left,
		    ;;;and update the sqaures left behind so they can be cleaned up 
		    (progn
		      (set-char-with-update xloc yloc nil *chunks*) ;;;don't leave anything
		      )

		    ;;;;leave a square, and create the new triangles
		    (progn
		      (set-char-with-update xloc yloc (make-square xloc yloc) *chunks*);;;deposit square
		      (dobox ((offset 0 new-tri-count :inc 4))
			     (etouq
			      (with-vec-params '((offset ny nx ty tx))
				'(new-triangles)
				'(set-char-with-update tx ty
				  (make-triangle nx ny) *chunks*)))))
		    )))))))))

(defun make-square (x y)
  (cons :square (pix::xy-index x y)))

(defun make-triangle (x y)
  (cons :triangle (pix::xy-index x y)))

(defun square-p (x)
  (when (listp x)
    (eq :square (car x))))

(defun deref-square (x)
  (pix::index-xy (cdr x)))

(defun triangle-p (x)
  (when (listp x)
    (eq :triangle (car x))))

(defun update-square (xsquare ysquare xlocation ylocation)
  )
