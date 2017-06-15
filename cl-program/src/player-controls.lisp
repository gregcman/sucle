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

(progn
 (defun (setf get-char) (value x y)
   (set-char value x y))
 (defun get-char (x y)
   (multiple-value-bind (chunk offset) (pix::area2 x y pix::*world*)
     (aref chunk offset)))
 (defun set-char (value x y)
   (multiple-value-bind (chunk offset) (pix::area2 x y pix::*world*)
     (setf (aref chunk offset) value)))
 (defun scwu (char x y)
   (set-char char x y)))


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

(defun atest ()
  (let ((circle (emit-cons (cons *test-tree* nil))))
    (let ((last (node-prev circle)))
      (setf (node-payload last) #\Space))
    (let ((barfoo (disconnect-node-prev circle)))
      (let ((start-hole (make-hole)))
	(set-hole-type start-hole (quote nope))
	(let ((start (make-node :payload start-hole)))
	  (link-nodes start barfoo)
	  start)))))

(progn
  (setf *print-case* :downcase)
  (defparameter *test-tree*
    (copy-tree
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
		(princ ")")))))))))

(defparameter pos (load-time-value (cons 0 0)))
(defparameter barfoo (atest))
(defun reset-barfoo-indentation ()
  (let ((first-node (first-node barfoo)))
    (map-nodes first-node (lambda (payload) 
				  (when (typep payload (quote hole))
				    (zerofy-hole payload))))
    (set-hole-type (node-payload first-node) (quote form))))
(defparameter *sleepy-time* 0.1)
(defun other-stuff ()
  (etouq
   (with-vec-params
       (vec-slots :rectangle (quote ((cx1 :x1) (cy1 :y1))))
     (quote (*point-rectangle*))
     (quote
      (progn
	(let ((value (get-char cx1 cy1)))
	  (when (skey-r-or-p :Z)
	    (when (typep value (quote node))
	      (let ((payload (find-enclosing-block-left
			      (node-prev
			       (bracket-left (find-enclosing-block-left value))))))
		(print payload))))
	  (when (smice-p :left)
	    (when (typep value (quote node))
	      (setf (car pos) cx1
		    (cdr pos) cy1)
	      (setf barfoo value))))))))

  (block nil
    (when (skey-r-or-p :f)
      (let ((next (node-next barfoo)))
	(multiple-value-bind (it )
	    (find-node-forward
	     (or next (return))
	     (function characterp))
	  (when it
	    (setf barfoo it))))))
  (block nil
    (when (skey-r-or-p :s)
      (let ((prev (node-prev barfoo)))
	(multiple-value-bind (it)
	    (find-node-backward (or prev (return))
				(function characterp))
	  (when it
	    (setf barfoo it))))))
  (when (skey-r-or-p :d)
    (multiple-value-bind (it) (char-search-down barfoo)
      (when it
	(setf barfoo it))))
  (when (skey-r-or-p :e)
    (multiple-value-bind (it) (char-search-up barfoo)
      (when it
	(setf barfoo it))))
  (clear-cam)
  (progn
   (multiple-value-bind (node hole)
       (find-node-forward barfoo (function hole-p))
     (declare (ignorable node))
     (when node
       (when (skey-j-p :kp-3)
	 (format t "~a" hole)
	 (terpri))
       (when (skey-j-p :kp-1)
	 (reset-barfoo-indentation))
       (when (skey-p :kp-8)
	 (when (hole-active hole)
	   (print hole)
	   (deactivate-hole hole)
	   (width-prop node)))
       (when (skey-p :kp-5)
	 (unless (hole-active hole)
	   (print hole)
	   (activate-hole hole)
	   (width-prop node)))))
   (multiple-value-bind (node hole)
       (find-node-forward barfoo (lambda (x) (typecase x
					       (hole (hole-active x)))))
     (declare (ignorable node))
     (when node
       (when (skey-r-or-p :kp-6)
	 (incf (hole-width hole))
	 (decf (hole-motion hole))
	 (width-prop node))
       (when (skey-r-or-p :kp-4)
	 (decf (hole-width hole))
	 (incf (hole-motion hole))
	 (width-prop node)))))
  (progn
    (when (skey-j-p :a)
      (let ((barfoo (find-enclosing-block-left (node-prev (bracket-left (find-enclosing-block-left barfoo))))))
	(print barfoo)))
    (let ((x (car pos))
	  (y (cdr pos)))
      (scwu t x y)
      (draw-nodes2 (1+ x) y (node-next barfoo))
      (draw-nodes2-reverse (1- x) y (node-prev barfoo)))
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
		     )))
	  (progn
	    (gl:bind-texture :texture-2d (get-stuff :text-scratch *stuff* *backup*))
	    (gl:tex-sub-image-2d :texture-2d 0 0 0 width height :rgba :unsigned-byte b)))))))

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
	    (node (recurse (node-payload obj)))
	    (t (sxhash obj))))))))

(defun nthfnc (function data &optional (times 0))
  (dotimes (amount times)
    (setf data (funcall function data)))
  data)

(defun char-search-down (node)
  (labels ((rec (node offset height)
	     (when node
	       (let ((data (node-payload node)))
		 (typecase data
		   (character
		    (if (zerop offset)
			(values node height)
			(rec (node-next node)
			     (+ offset 1)
			     height)))
		   (hole (if (hole-active data)
			     (rec (node-next node)
				  (+ offset (hole-width data))
				  (1- height))
			     (rec (node-next node) offset height)))
		   (t (rec (node-next node) offset height)))))))
    (rec (node-next node) 1 0)))

(defun char-search-up (node)
  (labels ((rec (node offset height)
	     (when node
	       (let ((data (node-payload node)))
		 (typecase data
		   (character
		    (if (zerop offset)
			(values node height)
			(rec (node-prev node)
			     (- offset 1)
			     height)))
		   (hole (if (hole-active data)
			     (rec (node-prev node)
				  (- offset (hole-width data))
				  (1+ height))
			     (rec (node-prev node) offset height)))
		   (t (rec (node-prev node) offset height)))))))
    (rec (node-prev node) -1 0)))

(progn
  (declaim (ftype (function (node (function (t) t)) (or null node))
		  find-node-forward find-node-backward))
  (defun find-node-forward (nodes test)
    (labels ((rec (node)
	       (if node
		   (let ((payload (node-payload node)))
		     (if (funcall test payload)
			 (values node payload)
			 (rec (node-next node))))
		   nil)))
      (rec nodes)))
  (defun find-node-backward (nodes test)
    (labels ((rec (node)
	       (if node
		   (let ((payload (node-payload node)))
		     (if (funcall test payload)
			 (values node payload)
			 (rec (node-prev node))))
		   nil)))
      (rec nodes))))

(defun first-node (node)
  (block nil
    (first-node (or (node-prev node)
		    (return node)))))

(defun map-nodes (nodes function)
  (labels ((rec (node)
	     (when node
	       (funcall function (node-payload node))
	       (rec (node-next node)))))
    (rec nodes)))
