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
	(decf *camera-y* (- cy1 cy0))))))

  (centered-rectangle *cam-rectangle* *camera-x* *camera-y*
		      (/ e:*width* *block-width*) (/ e:*height* *block-height*)))

(defun other-stuff ()
  (let ((a (load-time-value (cons 0 0)))
	(b (load-time-value (cons 0 0)))
	(c (load-time-value (cons 0 0))))
    (let ((mousex (+ *camera-x* (aref *cursor-rectangle*
				      (etouq (caar (vec-slots :rectangle (quote ((nil :x1)))))))))
	  (mousey (+ *camera-y* (aref *cursor-rectangle*
				      (etouq (caar (vec-slots :rectangle (quote ((nil :y1))))))))))
      (with-let-mapped-places ((x (car a))
			       (y (cdr a))
			       (val1 (car b))
			       (val2 (cdr b))
			       (val3 (car c))
			       (val4 (cdr c)))
	(dotimes (foobar 10)
	  (set-char-with-update x y (logior (acolor val4 val2 val1 val3 val1 val2)
					    (if nil (char-code #\space) (random 256))) *chunks*)
	  (incf val1 1)
	  (incf val2 2)
	  (incf val3 3)
	  (incf val4 4)
	  (if (zerop (random 2))
	      (incf x (if (< mousex x)
			  -1
			  1))
	      (incf y (if (< mousey y)
			  -1
			  1)))
	  (progno (setf x (mod x 128)
			y (mod y 128))))))))

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
	(cons (car obj))
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
