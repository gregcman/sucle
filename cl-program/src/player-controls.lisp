(in-package :sandbox)


(defun skey-p (enum)
  (e:key-p (cffi:convert-to-foreign enum (quote %cl-glfw3::key))))
(defun skey-j-r (enum)
  (e:key-j-r (cffi:convert-to-foreign enum (quote %cl-glfw3::key))))
(defun skey-j-p (enum)
  (e:key-j-p (cffi:convert-to-foreign enum (quote %cl-glfw3::key))))
(defun smice-j-p (enum)
  (e:mice-j-p (cffi:convert-to-foreign enum (quote %cl-glfw3::mouse))))
(defun skey-r-or-p (enum)
  (e:key-r-or-p (cffi:convert-to-foreign enum (quote %cl-glfw3::key))))
(defun smice-r-or-p (enum)
  (e:mice-r-or-p (cffi:convert-to-foreign enum (quote %cl-glfw3::mouse))))

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

(defparameter *block-height* (/ 32.0 1.0))
(defparameter *block-width* (/ 18.0 1.0))

(defparameter *cursor-x* 0)
(defparameter *cursor-y* 0)

(defparameter *camera-x* 0.0)
(defparameter *camera-y* 0.0)

(defparameter *chunks* (pix:make-world))
(defparameter *chunks2* (pix:make-world))
(defparameter *chunk-call-lists* (make-eq-hash))
(defparameter *chunk-call-lists2* (make-eq-hash))
(defparameter *chunk-width* 16)
(defparameter *chunk-height* 16)



(defparameter *window-min-x* 0.0)
(defparameter *window-max-x* 0.0)
(defparameter *window-min-y* 0.0)
(defparameter *window-max-y* 0.0)

(defparameter *window-min-x-block* 0)
(defparameter *window-max-x-block* 0)
(defparameter *window-min-y-block* 0)
(defparameter *window-max-y-block* 0)

(defparameter *window-min-x2* 0.0)
(defparameter *window-max-x2* 0.0)
(defparameter *window-min-y2* 0.0)
(defparameter *window-max-y2* 0.0)

(defparameter *window-min-x-block2* 0)
(defparameter *window-max-x-block2* 0)
(defparameter *window-min-y-block2* 0)
(defparameter *window-max-y-block2* 0)

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
(defparameter *show-cursor* t)
(defparameter *cursor-moved* 0)
(defparameter *scroll-sideways* nil)

(defun physics ()

  (incf *ticks*)
  (when (skey-j-p :escape) (window:toggle-mouse-capture))
  (progn
    (when (skey-r-or-p :up) (incf *cursor-y*) (setf *cursor-moved* *ticks*))
    (when (skey-r-or-p :down) (decf *cursor-y*) (setf *cursor-moved* *ticks*))
    (when (skey-r-or-p :left) (decf *cursor-x*) (setf *cursor-moved* *ticks*))
    (when (skey-r-or-p :right) (incf *cursor-x*) (setf *cursor-moved* *ticks*)))
  (dotimes (x (length e:*chars*))
    (let ((char (vector-pop e:*chars*)))
      (set-char-with-update (pix:xy-index *cursor-x* *cursor-y*)
			    (logior
			     (char-code char)
			     *white-black-color*))
      (incf *cursor-x*)
      (setf *cursor-moved* *ticks*)))
  (when (skey-r-or-p :backspace)
    (set-char-with-update (pix:xy-index (1- *cursor-x*) *cursor-y*)
			  (logior (char-code #\Space) *white-black-color*))
    (decf *cursor-x*)
    (setf *cursor-moved* *ticks*))

  (when (skey-r-or-p :u)
    (toggle *scroll-sideways*))
  (let ((scroll (ceiling e:*scroll-y*)))
    (unless (zerop scroll)
      (if *scroll-sideways*
	  (incf *camera-x* (* 5 *block-width* scroll))
	  (incf *camera-y* (* 5 *block-height* scroll)))
      (setf *cursor-moved* *ticks*)))
  
  (let ((diff (- *ticks* *cursor-moved*)))
    (flet ((set-hightlight ()
	     (let ((char (pix:get-obj (pix:xy-index *cursor-x* *cursor-y*) *chunks*)))
	       (unless char
		 (setf char 0))
	       (set-char-with-update2 (pix:xy-index 0 0) 
				      (logior (logandc2 (lognot char) 255) (mod char 256))))))
      (cond ((zerop diff)
	     (set-hightlight))
	    ((< 615 diff))
	    ((= 15 (mod diff 30))
	     (cond (*show-cursor*
		    (set-hightlight)
		    (setf *show-cursor* nil))
		   (t (set-char-with-update2 (pix:xy-index 0 0) nil)
		      (setf *show-cursor* t)))

	     ))))
  (remove-spurious-mouse-input)
  (progno (when (skey-p :space)
	    (dotimes (x 16)
	      (let ((x (random 128))
		    (y (random 128)))
		(set-char-with-update (pix:xy-index x y) (random most-positive-fixnum))))))

  (let ((half-height (/ e:*height* 1.0))
	(half-width (/ e:*width* 1.0)))
    (setf *window-min-x* (- *camera-x* half-width)
	  *window-max-x* (+ *camera-x* half-width)
	  *window-min-y* (- *camera-y* half-height)
	  *window-max-y* (+ *camera-y* half-height))
    (setf *window-min-x-block* (/ *window-min-x* *block-width*)
	  *window-max-x-block* (/ *window-max-x* *block-width*)
	  *window-min-y-block* (/ *window-min-y* *block-height*)
	  *window-max-y-block* (/ *window-max-y* *block-height*)))

  (let ((half-height (/ e:*height* 1.0))
	(half-width (/ e:*width* 1.0)))
    (setf *window-min-x2* (- *camera-x* half-width)
	  *window-max-x2* (+ *camera-x* half-width)
	  *window-min-y2* (- *camera-y* half-height)
	  *window-max-y2* (+ *camera-y* half-height))
    (setf *window-min-x-block2* (- (/ *window-min-x2* *block-width*) *cursor-x*)
	  *window-max-x-block2* (- (/ *window-max-x2* *block-width*) *cursor-x*)
	  *window-min-y-block2* (- (/ *window-min-y2* *block-height*) *cursor-y*)
	  *window-max-y-block2* (- (/ *window-max-y2* *block-height*) *cursor-y*))))

(defun quit ()
  (setf e:*status* t))

(defun set-char-with-update (place value)
  (let ((chunk-id
	 (setf (pix:get-obj place *chunks*) value)))
    (let ((chunk (gethash chunk-id *chunk-call-lists*)))
      (when chunk
	(gl:delete-lists chunk 1)
	(remhash chunk-id *chunk-call-lists*)))))

(defun set-char-with-update2 (place value)
  (let ((chunk-id
	 (setf (pix:get-obj place *chunks2*) value)))
    (let ((chunk (gethash chunk-id *chunk-call-lists2*)))
      (when chunk
	(gl:delete-lists chunk 1)
	(remhash chunk-id *chunk-call-lists2*)))))

(defparameter *saves-dir* (merge-pathnames #P"save/" ourdir))
(defparameter *save-file* (merge-pathnames #P"file" *saves-dir*))

(defun asave (thing)
  (save *save-file* thing))

(defun aload ()
  (myload *save-file*))

(defun save (filename thing)
   (let ((path (merge-pathnames filename *saves-dir*)))
     (with-open-file (stream path
			     :direction :output
			     :if-does-not-exist :create
			     :if-exists :supersede
			     :element-type '(unsigned-byte 8))
       (conspack:encode thing :stream stream))))

(defun myload (filename)
   (let ((path (merge-pathnames filename *saves-dir*)))
     (conspack:decode (byte-read path))))
