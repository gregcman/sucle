(in-package :aplayground)


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
  (etouq
   (with-vec-params
       (vec-slots :rectangle (quote ((cx0 :x0) (cy1 :y1)
				     (cx1 :x1) (cy0 :y0))))
     (quote (*cursor-rectangle*))
     (quote
      (when (smice-p :right)
	(decf *camera-x* (- cx1 cx0))
	(decf *camera-y* (- cy1 cy0))))))
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
;;  (other-stuff)
  (uncentered-rectangle *cam-rectangle* *camera-x* *camera-y*
			(/ e:*width* *block-width*) (/ e:*height* *block-height*))
  (progn
   (let ((width *window-block-width*)
	 (height *window-block-height*))
     (let ((xstart *camera-x*)
	   (ystart *camera-y*))
       (let ((b (get-stuff :glyph-screen *other-stuff* *backup*)))
	 (dobox ((xpos 0 *window-block-width*)
		 (ypos 0 *window-block-height*))		   
		(let ((offset (* 4 (+ xpos (* ypos width))))
		      (value (get-char (+ xpos xstart)
				       (+ ypos ystart))))
		  (let ((num (get-char-num value)))
		    (setf (cffi:mem-aref b :uint8 (+ offset 0)) (logand 255 num))
		    (setf (cffi:mem-aref b :uint8 (+ offset 1)) (ldb (byte 8 8) num))
		    (setf (cffi:mem-aref b :uint8 (+ offset 2)) (ldb (byte 8 16) num))
		    (setf (cffi:mem-aref b :uint8 (+ offset 3)) (ldb (byte 8 24) num))
		    )))
	 (progn
	   (gl:bind-texture :texture-2d (get-stuff :text-scratch *stuff* *backup*))
	   (gl:tex-sub-image-2d :texture-2d 0 0 0 width height :rgba :unsigned-byte b)))))))


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
		   (scwu (logior (char-code char) color) x y)
		   (setf x (1+ x))))))
	(values x y)))))


(defun clear-cam ()
  (map-box *cam-rectangle*
	     (lambda (x y)
	       (scwu nil x y))))
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

(defun nthfnc (function data &optional (times 0))
  (dotimes (amount times)
    (setf data (funcall function data)))
  data)

(defun print-to-buf (object &optional (print-func #'prin1))
  (let ((buffer (load-time-value
		 (make-array 0 :fill-pointer 0 :adjustable t :element-type (quote character)))))
    (setf (fill-pointer buffer) 0)
    (with-output-to-string (var buffer)
      (funcall print-func object var))
    buffer))

(defparameter *firing-time* (make-array 128 :initial-element nil))
(defparameter *pivot* nil)
(defparameter *cursor-x* 0)
(defparameter *cursor-y* 0)

(defun other-stuff ()
  (let ((hash e:*keypress-hash*)
	(firing-time *firing-time*))
    (let ((pivot nil)
	  (timing -1)
	  (news nil))
      (with-hash-table-iterator (gen-fn hash)
	(loop
	   (multiple-value-bind (more? key value) (gen-fn)
	     (if more?
		 (let ((value (e::get-press-value value))
		       (code (keyword-code key)))
		   (when code
		     (let ((new-timing
			    (case value
			      (1 nil) ;;;release
			      (2 (1+ (aref firing-time code))) ;;;;true
			      (3 (push code news) 0) ;;;;press
			      (4 (1+ (aref firing-time code)))))) ;;;;repeat
		       (setf (aref firing-time code) new-timing)
		       (unless (or (eql 3 value) (eql 1 value))
			 (when (> new-timing timing)
			   (setf pivot code)
			   (setf timing new-timing))))))
		 (return)))))
      (when pivot
	(let ((x 0)
	      (y 0)
	      (changed? nil))
	  (let ((pivot-pos (physical-keyboard::code-position pivot)))
	    (dolist (key news)
	      (unless (= key pivot)
		(setf changed? t)
		(let ((pos (physical-keyboard::code-position key)))
		  (incf x (car pos))
		  (incf y (cdr pos))
		  (decf x (car pivot-pos))
		  (decf y (cdr pivot-pos))))))
	  (when changed?
	    (etouq
	     (with-vec-params
		 (vec-slots :rectangle (quote ((px0 :x0) (py1 :y1)
					       (px1 :x1) (py0 :y0))))
	       (quote (*cam-rectangle* with-let-mapped-places))
	       (quote
		(let ((a0 (floor px0))
		      (a1 (floor px1))
		      (b0 (floor py0))
		      (b1  (floor py1)))
		  (let ((xnew (+ a0 (mod (- (+ *cursor-x* x) a0) (- a1 a0))))
			(ynew (+ b0 (mod (- (+ *cursor-y* y) b0) (- b1 b0)))))
		    (progn (foobar (sxhash (sxhash (sxhash (ash (get-internal-run-time) -6))))
				   *cursor-x* *cursor-y* xnew ynew))
		    (setf *cursor-x* xnew)
		    (setf *cursor-y* ynew)))))))))))
  (set-char 0 *cursor-x* *cursor-y*))


(defun foobar (a x y x0 y0)
  (dobox ((c (min x x0) (1+ (max x x0)))
	  (d (min y y0) (1+ (max y y0))))
	 (set-char a c d)))

(defun rectangle-size (rectangle)
  (etouq
   (with-vec-params
       (vec-slots :rectangle (quote ((px0 :x0) (py1 :y1)
				     (px1 :x1) (py0 :y0))))
     (quote (rectangle let))
     (quote
      (* (- px1 px0)
	 (- py1 py0))))))
