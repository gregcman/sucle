(in-package :sandbox)


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

(defparameter *command-buffer* (make-array 0 :adjustable t :fill-pointer 0 :element-type 'character))

(defparameter *ticks* 0)

(defun physics ()
  (incf *ticks*)

  (progn
    (unless (zerop (fill-pointer *command-buffer*))
      (setf (fill-pointer *command-buffer*) 0))
    (get-control-sequence *command-buffer*))

  (terminal-stuff 0 0 *command-buffer* *chunks*)
  (setf *old-mouse-x* *mouse-x*
	*old-mouse-y* *mouse-y*)
  (multiple-value-bind (x y) (window:get-mouse-position)
    (setf *mouse-x* (+ x x)
	  *mouse-y* (- (+ y y))))

  (when (smice-p :left)
    (decf *camera-x* (- (floor *mouse-x* *block-width*)
			(floor *old-mouse-x* *block-width*)))
    (decf *camera-y* (- (floor *mouse-y* *block-height*)
			(floor *old-mouse-y* *block-height*))))

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

(defun get-control-sequence (buffer)
  (with-output-to-string (command buffer)
    (flet ((enter (x)
	     (princ x command)))
      (etouq
       (ngorp
	(mapcar (lambda (x)
		  `(when (skey-r-or-p ,(pop x))
		     (enter ,(pop x))))
		'((:enter (etouq (string #\return)))
		  (:backspace (string #\del))
		  (:tab (etouq (string #\Tab)))
		  (:up "[A")
		  (:down "[B")
		  (:left "[D")
		  (:right "[C")))))      

      (with-hash-table-iterator (next e:*keypress-hash*)
	(loop (multiple-value-bind (more key value) (next)
		(if more
		    (let ((code (gethash key *keyword-ascii*)))
		      (when code
			(when (e::r-or-p (e::get-press-value value))
			  (let ((mods (ash value (- e::+mod-key-shift+))))
			    (multiple-value-bind (char esc) (convert-char code mods)
			      (when esc
				(enter (etouq (string #\esc))))
			      (enter (string (code-char char))))))))
		    (return))))))))

(defun set-char-with-update (x y value)
  (multiple-value-bind (chunk offset) (pix::area (pix:xy-index x y) *chunks*)
    (setf (aref chunk offset) value)
    (setf (aref chunk (* 16 16)) *ticks*)))

(defun get-char (x y world)
  (pix:get-obj (pix:xy-index x y) world))
(defun set-char (value x y world)
  (setf (pix:get-obj (pix:xy-index x y) world) value))


(progn
  (declaim (ftype (function (t) fixnum) get-char-num))
  (with-unsafe-speed
    (defun get-char-num (obj)
      (typecase obj
	(cons (car obj))
	(fixnum obj)
	(t (etouq (sxhash nil)))))))
