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
  (defparameter *camera-x* 0)
  (defparameter *camera-y* 0))

(progn
  (defparameter *point-x* 0)
  (defparameter *point-y* 0))

(defparameter *point-rectangle* (vector 0 0 0 0))

(defparameter *chunks* (pix:make-world))
(defparameter *chunk-call-lists* (make-eq-hash))

(defparameter *cam-rectangle* (vector 0 0 0 0))

(defparameter *ticks* 0)

(defparameter *running* nil)
(defparameter foobar (make-array 0 :adjustable t :element-type 'character :fill-pointer 0))


(defun physics ()
  (incf *ticks*)
  (etouq
   (with-vec-params
       (vec-slots :rectangle (quote ((x0 :x0) (y1 :y1)
				     (x1 :x1) (y0 :y0))))
     (quote (*mouse-rectangle* with-let-mapped-places))
     (quote
      (progn
	(setf x0 x1 y0 y1)
	(multiple-value-bind (x y) (window:get-mouse-position)
	  (setf x1 (+ x x)
		y1 (+ (- (+ y y)) (* 2 *window-height*))))
	(etouq
	 (with-vec-params
	     (vec-slots :rectangle (quote ((cx0 :x0) (cy1 :y1)
					   (cx1 :x1) (cy0 :y0))))
	   (quote (*cursor-rectangle* symbol-macrolet))
	   (quote
	    (progn (setf cx0 (floor x0 *block-width*)
			 cy0 (floor y0 *block-height*)
			 cx1 (floor x1 *block-width*)
			 cy1 (floor y1 *block-height*))))))))))
  (when (skey-j-p :escape)
    (setf e:*status* t))
  (progn *running*
      (when (zerop (mod *ticks* (floor (/ 60 60))))
	(other-stuff))
      (etouq
       (with-vec-params
	   (vec-slots :rectangle (quote ((cx0 :x0) (cy1 :y1)
					 (cx1 :x1) (cy0 :y0))))
	 (quote (*cursor-rectangle*))
	 (quote
	  (when (smice-p :right)
	    (decf *camera-x* (- cx1 cx0))
	    (decf *camera-y* (- cy1 cy0)))))))
  (etouq
   (with-vec-params
       (vec-slots :rectangle (quote ((px0 :x0) (py1 :y1)
				     (px1 :x1) (py0 :y0))))
     (quote (*point-rectangle* with-let-mapped-places))
     (quote
      (etouq
       (with-vec-params
	   (vec-slots :rectangle (quote ((cx1 :x1) (cy1 :y1))))
	 (quote (*cursor-rectangle* with-let-mapped-places))
	 (quote
	  (progn (setf px0 px1
		       py0 py1
		       px1 (+ cx1 *camera-x*)
		       py1 (+ cy1 *camera-y*)))))))))

  (uncentered-rectangle *cam-rectangle* *camera-x* *camera-y*
		      (/ e:*width* *block-width*) (/ e:*height* *block-height*)))


(defun uncentered-rectangle (rect x y width height)
  (etouq
   (with-vec-params
       (vec-slots :rectangle (quote ((x0 :x0) (y1 :y1)
				     (x1 :x1) (y0 :y0))))
     (quote (rect symbol-macrolet))
     (quote
      (setf
       x0 x 
       y0 y 
       x1 (+ x width width)
       y1 (+ y height height))))))

(defun set-char-with-update (x y value world)
  (multiple-value-bind (chunk offset) (pix::area x y world)
    (setf (aref chunk offset) value)
    (setf (aref chunk (* 16 16)) *ticks*)))
(defun (setf get-char) (value x y world)
  (set-char value x y world))
(defun get-char (x y world)
  (multiple-value-bind (chunk offset) (pix::area x y world)
    (aref chunk offset)))
(defun set-char (value x y world)
  (multiple-value-bind (chunk offset) (pix::area x y world)
    (setf (aref chunk offset) value)))
(defun scwu (char x y)
  (set-char-with-update x
			y
			char
			*chunks*))
(progn
  (declaim (ftype (function (vector (function (fixnum fixnum)))) map-box))
  (with-unsafe-speed
    (defun map-box (box function)
      (etouq
       (with-vec-params
	   (vec-slots :rectangle (quote ((x0 :x0) (y0 :y0) (x1 :x1) (y1 :y1))))
	 (quote (box))
	 (quote
	  (dobox ((x (floor x0) (floor x1))
		  (y (floor y0) (floor y1)))
		 (funcall function x y))))))))

(progn
  (declaim (ftype (function (fixnum fixnum t &optional fixnum)
			    (values fixnum fixnum))
		  copy-string-to-world))
  (defun copy-string-to-world (x y string &optional (color *color-white-black*))
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
  (when (smice-p :left)
    (etouq
       (with-vec-params
	   (vec-slots :rectangle (quote ((cx1 :x1) (cy1 :y1) (cx0 :x0) (cy0 :y0))))
	 (quote (*point-rectangle*))
	 (quote
	  (let ((xstart cx0)
		(ystart cy0))
	    (let ((value (list (random most-positive-fixnum))))
	      (scwu value xstart ystart)
	      (if (> xstart cx1)
		  (rotatef xstart cx1))
	      (if (> ystart cy1)
		  (rotatef ystart cy1))
	      (dobox ((x xstart (1+ cx1))
		      (y ystart (1+ cy1)))
		     (scwu value x y))))))))
  (when (skey-p :t)
    (map-box
     *cam-rectangle*
     (lambda (x y)
       (let ((value (get-char x y *chunks*)))
	 (if (and (listp value) value)
	     (setf (car value) (random most-positive-fixnum)))))))
  (when (skey-p :q)
    (map-box
     *cam-rectangle*
     (lambda (x y)
       (let ((value (get-char x y *chunks*))
	     (value2 (get-char x (1- y) *chunks*)))
	 (when (> (get-char-num value2) (get-char-num value))
	   (scwu value x (1- y))
	   (scwu value2 x y)))))
    (map-box
     *cam-rectangle*
     (lambda (x y)
       (let ((value (get-char x y *chunks*))
	     (value2 (get-char (1- x) y *chunks*)))
	 (when (> (get-char-num value2) (get-char-num value))
	   (scwu value (1- x) y)
	   (scwu value2 x y))))))
  (when (skey-p :r)
    (etouq
     (with-vec-params
	 (vec-slots :rectangle (quote ((x :x1) (y :y1))))
       '(*point-rectangle*)
       (quote
	(let ((xstart (- x 0))
	      (ystart (- y 0))
	      (counter 0))
	  (dobox ((x xstart (+ xstart 16))
		  (y ystart (+ ystart 16)))
		 (scwu (logior counter
			       (ash (logior counter
					    (ash (- 256 counter) 8)) 8)) x y)
		 (incf counter)))))))
  (when (skey-p :l)
    (dobox ((x 0 228)
	    (y 0 70))
	   (scwu (random most-positive-fixnum) x y)))
  
  (incf *camera-y* (* 5 e:*scroll-y*))
  (progno
   (progn (setf (fill-pointer foobar) 0)
	  (with-output-to-string (out foobar)
	    (print *mouse-rectangle* out))
	  (copy-string-to-world 0 10 foobar))
   (progn (setf (fill-pointer foobar) 0)
	  (with-output-to-string (out foobar)
	    (print *cursor-rectangle* out))
	  (copy-string-to-world 0 11 foobar))
   (progn (setf (fill-pointer foobar) 0)
	  (with-output-to-string (out foobar)
	    (print (list *point-x* *point-y*) out))
	  (copy-string-to-world 0 12 foobar)))
  (progn
    (let ((width *window-block-width*)
	  (height *window-block-height*))
      (let ((xstart *camera-x*)
	    (ystart *camera-y*))
	(progno (etouq)
		(with-vec-params
		    (vec-slots :rectangle (quote ((cx0 :x0) (cy1 :y1)
						  (cx1 :x1) (cy0 :y0))))
		  (quote (*cam-rectangle*))
		  (quote)))
	(let ((b (get-stuff :glyph-screen *other-stuff* *backup*)))
	  (dobox ((xpos 0 *window-block-width*)
		  (ypos 0 *window-block-height*))		   
		 (let ((offset (* 4 (+ xpos (* ypos width))))
		       (value (get-char (+ xpos xstart) (+ ypos ystart) *chunks*)))
		   (let ((num (get-char-num value)))
		     (setf (cffi:mem-aref b :uint8 (+ offset 0)) (logand 255 num))
		     (setf (cffi:mem-aref b :uint8 (+ offset 1)) (ldb (byte 8 8) num))
		     (setf (cffi:mem-aref b :uint8 (+ offset 2)) (ldb (byte 8 16) num))
		     )))
	  (progn
	    (gl:bind-texture :texture-2d (get-stuff :text-scratch *stuff* *backup*))
	    (gl:tex-sub-image-2d :texture-2d 0 0 0 width height :rgba :unsigned-byte b)))))))

(defun color-rgb (color)
  (labels ((c (r g b)
	     (values (/ r 255.0) (/ g 255.0) (/ b 255.0)))
	   (c6 (x)
	     (let ((b (mod x 6))
		   (g (mod (floor x 6) 6))
		   (r (mod (floor x 36) 6)))
	       (values (/ r 5.0) (/ g 5.0) (/ b 5.0))))
	   (g (x)
	     (let ((gray (/ x 23.0)))
	       (values gray gray gray))))
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

(progn
  (defparameter *color-white-black* (ash 255 8))
  (progn
    (declaim (ftype (function (t &optional fixnum) fixnum) get-char-num))
    (with-unsafe-speed
      (defun get-char-num (obj &optional (depth 8))
	(flet ((recurse (obj)
		 (let ((new-num (1- depth)))
		   (if (zerop new-num)
		       (sxhash nil)
		       (get-char-num obj new-num)))))
	  (typecase obj
	    (fixnum obj)
	    (cons (recurse (car obj)))
	    (character (logior *color-white-black* (char-code obj)))
	    (t (sxhash obj))))))))
