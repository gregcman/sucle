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

(defparameter *old-cursor-x* 0)
(defparameter *old-cursor-y* 0)

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
(defparameter *show-cursor* t)
(defparameter *cursor-moved* 0)
(defparameter *scroll-sideways* nil)

(defun physics ()

  (setf *old-cursor-x* *hud-cursor-x*
	*old-cursor-y* *hud-cursor-y*)
  
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
	  (incf *camera-x* (* 1 scroll))
	  (incf *camera-y* (* 1 scroll)))
      (setf *cursor-moved* *ticks*)))

  (setf *window-block-width* (/ e:*width* *block-width*)
	*window-block-height* (/ e:*height* *block-height*))
  (setf *hud-cursor-x* (floor (clamp (- *cursor-x* *camera-x*)
				     (- *window-block-width*)
				     *window-block-width*))
	*hud-cursor-y* (floor (clamp (- *cursor-y* *camera-y*)
				     (- *window-block-height*)
				     *window-block-width*)))
  (when (not (and (= *old-cursor-x* *hud-cursor-x*)
		  (= *old-cursor-y* *hud-cursor-y*)))
    (set-char-with-update (pix:xy-index (+ *hud-x* *old-cursor-x*)
					(+ *hud-y* *old-cursor-y*))
			  nil))
  (let ((diff (- *ticks* *cursor-moved*)))
    (labels ((set-cursor (x)
	       (set-char-with-update (pix:xy-index (+ *hud-x* *hud-cursor-x*)
						   (+ *hud-y* *hud-cursor-y*)) 
				     x))
	     (set-hightlight ()
	       (let ((char (pix:get-obj (pix:xy-index *cursor-x* *cursor-y*) *chunks*)))
		 (unless char
		   (setf char 0))
		 (set-cursor (logior (logandc2 (lognot char) 255) (mod char 256))))))
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
		   (setf *show-cursor* t)))))))
  (remove-spurious-mouse-input)

  (progno
   (setf (fill-pointer foo) 0)
   (with-output-to-string (var foo)
     (print (list (get-internal-real-time)
		  (get-internal-run-time)) var))
   (copy-string-to-world 0 128 foo (logandc2 (random most-positive-fixnum) 255)))
  
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


(defun copy-string-to-world (x y string color)
  (let ((len (length string))
	(xoffset 0)
	(yoffset 0))
    (dotimes (index len)
      (let ((char (aref string index)))
	(cond ((char= char #\Newline)
	       (setf xoffset 0 yoffset (1- yoffset)))
	      (t (set-char-with-update (pix:xy-index (+ x xoffset) (+ y yoffset))
				       (logior (char-code char) color))
		 (incf xoffset)))))))
