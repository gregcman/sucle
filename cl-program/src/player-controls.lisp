(in-package :sandbox)


(defun skey-p (enum)
  (e:key-p (cffi:convert-to-foreign enum (quote %cl-glfw3::key))))
(defun skey-j-r (enum)
  (e:key-j-r (cffi:convert-to-foreign enum (quote %cl-glfw3::key))))
(defun skey-j-p (enum)
  (e:key-j-p (cffi:convert-to-foreign enum (quote %cl-glfw3::key))))
(defun smice-j-p (enum)
  (e:mice-j-p (cffi:convert-to-foreign enum (quote %cl-glfw3::mouse))))

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

(defparameter cursor-x 0.0)
(defparameter cursor-y 0.0)

(defparameter *camera-x* 0.0)
(defparameter *camera-y* 0.0)

(defparameter *chunks* (pix:make-world))
(defparameter *chunk-call-lists* (make-eq-hash))
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

(defparameter *achar* 0)

(defparameter foo
  (make-array 0 :adjustable t :fill-pointer 0
	      :element-type (quote character)))

(defun floor-chunk (x y)
  (* y (floor x y)))

(defun physics ()
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
  
  (when (skey-j-p :escape) (window:toggle-mouse-capture))
  (progn
    (when (skey-p :up) (incf *camera-y* 32.0))
    (when (skey-p :down) (incf *camera-y* -32.0))
    (when (skey-p :left) (incf *camera-x* -18.0))
    (when (skey-p :right) (incf *camera-x* 18.0)))
  (remove-spurious-mouse-input)
  (when (window:mice-locked-p)
    (multiple-value-bind (delx dely) (delta)
      (setf cursor-x (+ cursor-x delx))
      (setf cursor-y (- cursor-y dely))))
  

  ;(progno)
  (setf *achar* (mod  (+ *achar* (ceiling e:*scroll-y*)) 256))
  (when (skey-p :space)
    (dotimes (x 16)
      (let ((x (random 128))
	    (y (random 128)))
	(set-char-with-update (pix:xy-index x y) (random most-positive-fixnum)))))
  (progn
    (let ((list (get-stuff :chunks *stuff* *backup*)))
      (when list
	(gl:delete-lists list 1))
     (remhash :chunks *stuff*))))

(defun quit ()
  (setf e:*status* t))

(defun set-char-with-update (place value)
  (let ((chunk-id
	 (setf (pix:get-obj place *chunks*) value)))
    (let ((chunk (gethash chunk-id *chunk-call-lists*)))
      (when chunk
	(gl:delete-lists chunk 1)
	(remhash chunk-id *chunk-call-lists*)))))
