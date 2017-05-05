(in-package :sandbox)


(progn
  (defparameter *old-mouse-x* 0.0)
  (defparameter *old-mouse-y* 0.0)
  (defparameter *mouse-x* 0.0)
  (defparameter *mouse-y* 0.0))
(progn
  (defparameter *mouse-sensitivity* (coerce 2.0 'single-float)))

(progn
  (defparameter *block-height* (/ 32.0 1.0))
  (defparameter *block-width* (/ 18.0 1.0)))

(progn
  (defparameter *cursor-x* 0)
  (defparameter *cursor-y* 0)
  (defparameter *old-cursor-x* 0)
  (defparameter *old-cursor-y* 0))

(progn
  (defparameter *old-hud-cursor-x* 0)
  (defparameter *old-hud-cursor-y* 0)
  (defparameter *hud-cursor-x* 0)
  (defparameter *hud-cursor-y* 0))

(progn
  (defparameter *hud-x* 1999)
  (defparameter *hud-y* 1999))

(progn
  (defparameter *camera-x* 0)
  (defparameter *camera-y* 0))

(defparameter *chunks* (pix:make-world))
(defparameter *chunk-call-lists* (make-eq-hash))
(progn
  (defparameter *chunk-width* 16)
  (defparameter *chunk-height* 16))

(progn
  (defparameter *window-block-height* 0.0)
  (defparameter *window-block-width* 0.0))


(defparameter *show-cursor* t)
(defparameter *cursor-moved* 0)

(progn
  (defparameter *terminal-start-x* 0)
  (defparameter *terminal-start-y* 0))

(defparameter *command-buffer* (make-array 0 :adjustable t :fill-pointer 0 :element-type 'character))

(defparameter *ticks* 0)

(defun physics ()
  (incf *ticks*)
  (when (skey-j-p :escape) (window:toggle-mouse-capture))

  (progn
    (setf *old-hud-cursor-x* *hud-cursor-x*
	  *old-hud-cursor-y* *hud-cursor-y*)
    (setf *old-cursor-x* *cursor-x*
	  *old-cursor-y* *cursor-y*))

  (progn
    (setf (fill-pointer *command-buffer*) 0)
    (get-control-sequence *command-buffer*))
  
  (multiple-value-bind (newx newy)
      (terminal-stuff *terminal-start-x* *terminal-start-y* *command-buffer*)
    (setf *cursor-x* newx
	  *cursor-y* newy))

  (when (e:mice-locked-p)
    (setf *old-mouse-x* *mouse-x*
	  *old-mouse-y* *mouse-y*)
    (setf (values *mouse-x* *mouse-y*)
	  (next-mouse-state *mouse-x* *mouse-y* *mouse-sensitivity*))

    (when (smice-p :left)
      (decf *camera-x* (- (floor *mouse-x* *block-width*)
			  (floor *old-mouse-x* *block-width*)))
      (decf *camera-y* (- (floor *mouse-y* *block-height*)
			  (floor *old-mouse-y* *block-height*)))))
  

  (setf *window-block-width* (/ e:*width* *block-width*)
	*window-block-height* (/ e:*height* *block-height*))
  (setf *hud-cursor-x* (floor (clamp (- *cursor-x* *camera-x*)
				     (- *window-block-width*)
				     *window-block-width*))
	*hud-cursor-y* (floor (clamp (- *cursor-y* *camera-y*)
				     (- *window-block-height*)
				     *window-block-width*)))
  (unless (and (= *old-hud-cursor-x* *hud-cursor-x*)
	       (= *old-hud-cursor-y* *hud-cursor-y*))
    (set-char-with-update (+ *hud-x* *old-hud-cursor-x*)
			  (+ *hud-y* *old-hud-cursor-y*)
			  nil))
  (unless (and (= *old-cursor-x* *cursor-x*)
	       (= *old-cursor-y* *cursor-y*))
    (setf *cursor-moved* *ticks*))
  (progn
    (let ((diff (- *ticks* *cursor-moved*)))
      (labels ((set-cursor (x)
		 (set-char-with-update (+ *hud-x* *hud-cursor-x*)
				       (+ *hud-y* *hud-cursor-y*) 
				       x))
	       (set-hightlight ()
		 (let ((char (get-char-num (pix:get-obj (pix:xy-index *cursor-x* *cursor-y*) *chunks*))))
		   (unless char
		     (setf char 0))
		   (set-cursor (logior (strip-char (lognot char))
				       (mod char 256))))))
	(cond ((zerop diff)
	       (set-hightlight)
	       (setf *show-cursor* t))
	      ((< 615 diff))
	      ((= 0 (mod diff 30))
	       (if *show-cursor*
		   (progn
		     (set-cursor nil)
		     (setf *show-cursor* nil))
		   (progn
		     (set-hightlight)
		    (setf *show-cursor* t))))))))

  (centered-rectangle *cam-rectangle* *camera-x* *camera-y*
		      *window-block-width* *window-block-height*)
  (centered-rectangle *hud-rectangle* *hud-x* *hud-y*
		      *window-block-width* *window-block-height*))

(defparameter *hud-rectangle* (vector 0 0 0 0))
(defparameter *cam-rectangle* (vector 0 0 0 0))

(defun centered-rectangle (rect x y width height)
  (etouq
   (with-vec-params (vec-slots :rectangle
			       (quote ((x0 :x0) (y1 :y1) (x1 :x1) (y0 :y0))))
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
  (let ((chunk-id
	 (setf (pix:get-obj (pix:xy-index x y) *chunks*) value)))
    (let ((chunk (gethash chunk-id *chunk-call-lists*)))
      (when chunk
	(gl:delete-lists chunk 1)
	(remhash chunk-id *chunk-call-lists*)))))


(progn
  (declaim (ftype (function (t) fixnum) get-char-num))
  (with-unsafe-speed
    (defun get-char-num (obj)
      (typecase obj
	(cons (car obj))
	(fixnum obj)
	(t (etouq (sxhash nil)))))))
