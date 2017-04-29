(in-package :sandbox)


(progn
  (defun skey-p (enum)
    (e:key-p enum;(cffi:convert-to-foreign enum (quote %cl-glfw3::key))
	     ))
  (defun skey-j-r (enum)
    (e:key-j-r enum;(cffi:convert-to-foreign enum (quote %cl-glfw3::key))
	       ))
  (defun skey-j-p (enum)
    (e:key-j-p enum;(cffi:convert-to-foreign enum (quote %cl-glfw3::key))
	       ))
  (defun smice-p (enum)
    (e:mice-p enum;(cffi:convert-to-foreign enum (quote %cl-glfw3::mouse))
	      ))
  (defun smice-j-p (enum)
    (e:mice-j-p enum;(cffi:convert-to-foreign enum (quote %cl-glfw3::mouse))
		))
  (defun skey-r-or-p (enum)
    (e:key-r-or-p enum;(cffi:convert-to-foreign enum (quote %cl-glfw3::key))
		  ))
  (defun smice-r-or-p (enum)
    (e:mice-r-or-p enum;(cffi:convert-to-foreign enum (quote %cl-glfw3::mouse))
		   )))

(defparameter old-mouse-x 0)
(defparameter old-mouse-y 0)
(defun delta ()
  (multiple-value-bind (newx newy) (window:get-mouse-position)
    (multiple-value-prog1 (values
			   (- newx old-mouse-x)
			   (- newy old-mouse-y))
      (setf old-mouse-x newx
	    old-mouse-y newy))))
(defparameter mousecapturestate nil)
(defun remove-spurious-mouse-input ()
  (if (window:mice-locked-p)
      (cond ((eq nil mousecapturestate)
	     (delta) ;;toss spurious mouse movement
	     (setf mousecapturestate :justcaptured))
	    ((eq mousecapturestate :justcaptured)
	     (setq mousecapturestate t)))
      (setq mousecapturestate nil)))

(defparameter *mouse-x* 0.0)
(defparameter *mouse-y* 0.0)
(progn
  (defparameter *mouse-sensitivity* (coerce 2.0 'single-float)))

(defparameter *block-height* (/ 32.0 1.0))
(defparameter *block-width* (/ 18.0 1.0))

(defparameter *cursor-x* 0)
(defparameter *cursor-y* 0)
(defparameter *old-cursor-x* 0)
(defparameter *old-cursor-y* 0)

(defparameter *old-hud-cursor-x* 0)
(defparameter *old-hud-cursor-y* 0)

(defparameter *hud-x* 1999)
(defparameter *hud-y* 1999)

(defparameter *hud-cursor-x* 0)
(defparameter *hud-cursor-y* 0)

(defparameter *camera-x* 0)
(defparameter *camera-y* 0)

(defparameter *chunks* (pix:make-world))
(defparameter *chunk-call-lists* (make-eq-hash))
(defparameter *chunk-width* 16)
(defparameter *chunk-height* 16)

(defparameter *window-block-height* 0.0)
(defparameter *window-block-width* 0.0)

(defparameter *achar* 0)

(defparameter *ticks* 0)
(defparameter foo
  (make-array 0 :adjustable t :fill-pointer 0
	      :element-type (quote character)))

(defun floor-chunk (x y)
  (* y (floor x y)))

(defun acolor (&rest values)
  (setf values (nreverse values))
  (let ((acc 0))
    (dolist (value values)
      (setf acc (ash (logior acc value) 8)))
    (logand acc most-positive-fixnum)))

(defparameter *white-black-color* (acolor 255 255 255 0 0 0))
(defparameter *color-nil* (logandc1 255 (sxhash nil)))
(defparameter *show-cursor* t)
(defparameter *cursor-moved* 0)
(defparameter *scroll-sideways* nil)

(defparameter *print-head-x* 0)
(defparameter *print-head-y* 127)

(defparameter *terminal-start-x* 0)
(defparameter *terminal-start-y* 0)

(defun strip-char (color)
  (logandc1 255 color))

(defun physics ()
  (remove-spurious-mouse-input)
  (incf *ticks*)
  
  (update-terminal-stuff)
  (char-print-term *terminal-start-x*
		   *terminal-start-y*)
  (when (skey-j-p :enter)
    (enter (etouq (string #\Newline))))
  
  (setf *old-hud-cursor-x* *hud-cursor-x*
	*old-hud-cursor-y* *hud-cursor-y*)
  (setf *old-cursor-x* *cursor-x*
	*old-cursor-y* *cursor-y*)
 
  (when (skey-j-p :escape) (window:toggle-mouse-capture))
  (when (e:mice-locked-p)
    (multiple-value-bind (dx dy) (delta)
      (let ((width e:*width*)
	    (height e:*height*))
	(let ((deltax (* *mouse-sensitivity* dx))
	      (deltay (* *mouse-sensitivity* dy)))
	  (let ((old-mouse-x *mouse-x*)
		(old-mouse-y *mouse-y*))
	    (setf *mouse-x* (clamp (+ *mouse-x* deltax) (- width) (- width 2.0)))
	    (setf *mouse-y* (clamp (- *mouse-y* deltay) (+  2.0 (- height)) height))
	    (when (smice-p :left)
	      (decf *camera-x* (- (floor *mouse-x* *block-width*)
				  (floor old-mouse-x *block-width*)))
	      (decf *camera-y* (- (floor *mouse-y* *block-height*)
				  (floor old-mouse-y *block-height*)))))))))

  (when (skey-r-or-p :backspace)
    (enter (string #\del)))
  (let ((len (length e:*chars*)))
    (unless (zerop len) (setf *cursor-moved* *ticks*))
    (dotimes (x len)
      (let ((char (vector-pop e:*chars*)))
	(enter (string char)))))

  (multiple-value-bind (x y state other) (term-cursor-info)
    (declare (ignorable state other))
    (let ((newx (+ *terminal-start-x* x))
	  (newy (- *terminal-start-y* y)))
      (unless (= *cursor-x* newx)	
	(setf *cursor-x* newx))
      (unless (= *cursor-y* newy)
	(setf *cursor-y* newy)))

    (setf *window-block-width* (/ e:*width* *block-width*)
	  *window-block-height* (/ e:*height* *block-height*))
    (setf *hud-cursor-x* (floor (clamp (- *cursor-x* *camera-x*)
				       (- *window-block-width*)
				       *window-block-width*))
	  *hud-cursor-y* (floor (clamp (- *cursor-y* *camera-y*)
				       (- *window-block-height*)
				       *window-block-width*)))
    (when (not (and (= *old-hud-cursor-x* *hud-cursor-x*)
		    (= *old-hud-cursor-y* *hud-cursor-y*)))
      (set-char-with-update (pix:xy-index (+ *hud-x* *old-hud-cursor-x*)
					  (+ *hud-y* *old-hud-cursor-y*))
			    nil))
    (when (not (and (= *old-cursor-x* *cursor-x*)
		    (= *old-cursor-y* *cursor-y*)))
      (setf *cursor-moved* *ticks*))
    (let ((diff (- *ticks* *cursor-moved*)))
      (labels ((set-cursor (x)
		 (set-char-with-update (pix:xy-index (+ *hud-x* *hud-cursor-x*)
						     (+ *hud-y* *hud-cursor-y*)) 
				       x))
	       (set-hightlight ()
		 (let ((char (get-char-num (pix:get-obj (pix:xy-index *cursor-x* *cursor-y*) *chunks*))))
		   (unless char
		     (setf char 0))
		   (set-cursor (logior (strip-char (lognot char))
				       (char-code (3bst:c other)))))))
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
  
  (let ((rectangle *cam-rectangle*))
    (setf (aref rectangle 0) (- *camera-x* *window-block-width*)
	  (aref rectangle 1) (- *camera-y* *window-block-height*)
	  (aref rectangle 2) (+ *camera-x* *window-block-width*)
	  (aref rectangle 3) (+ *camera-y* *window-block-height*)))

  (let ((rectangle *hud-rectangle*))
    (setf (aref rectangle 0) (- *hud-x* *window-block-width*)
	  (aref rectangle 1) (- *hud-y* *window-block-height*)
	  (aref rectangle 2) (+ *hud-x* *window-block-width*)
	  (aref rectangle 3) (+ *hud-y* *window-block-height*))))

(defparameter *hud-rectangle* (vector 0 0 0 0))
(defparameter *cam-rectangle* (vector 0 0 0 0))

(defun quit ()
  (setf e:*status* t))

(defun set-char-with-update (place value)
  (let ((chunk-id
	 (setf (pix:get-obj place *chunks*) value)))
    (let ((chunk (gethash chunk-id *chunk-call-lists*)))
      (when chunk
	(gl:delete-lists chunk 1)
	(remhash chunk-id *chunk-call-lists*)))))
(defun typing-insert (value x y)
  (let ((start (pix:xy-index x y)))
    (let ((old-value (pix:get-obj start *chunks*)))
      (set-char-with-update start value)
      (if old-value
	  (typing-insert old-value (1+ x) y)))))

(defun typing-delete (x y)
  (let ((start (pix:xy-index x y)))
    (let ((old-value (pix:get-obj start *chunks*))
	  (prev (pix:xy-index (1- x) y)))
      (cond (old-value 
	     (set-char-with-update prev old-value)
	     (typing-delete (1+ x) y))
	    (t (set-char-with-update prev nil))))))

(progn
  (declaim (ftype (function (t) fixnum) get-char-num))
  (with-unsafe-speed
    (defun get-char-num (obj)
      (typecase obj
	(cons (car obj))
	(fixnum obj)
	(t (etouq (sxhash nil)))))))

(progn
  (declaim (ftype (function (fixnum fixnum fixnum (vector character) fixnum
				    (function (fixnum) fixnum)
				    (function (fixnum) fixnum))
			    (values fixnum fixnum))
		  copy-string-to-world))
  (defun copy-string-to-world (x y newline-start string color next-x-func next-y-func)
    (let ((len (length string)))
      (dotimes (index len)
	(let ((char (aref string index)))
	  (cond ((char= char #\Newline)
		 (setf x newline-start y (funcall next-y-func y)))
		(t		     
		 (set-char-with-update (pix:xy-index x y)
				       (logior (char-code char) color))
		 (setf x (funcall next-x-func x))))))
      (values x y))))


