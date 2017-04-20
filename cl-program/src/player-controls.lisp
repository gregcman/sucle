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


(defparameter cursor-x 0.0)
(defparameter cursor-y 0.0)

(defparameter *chunks* (pix:make-world))

(defparameter *achar* 0)

(defparameter foo
  (make-array 0 :adjustable t :fill-pointer 0
	      :element-type (quote character)))

(defun physics ()
  (when (skey-j-p :escape) (window:toggle-mouse-capture))
  (when (skey-p :up) (incf cursor-y 32.0))
  (when (skey-p :down) (incf cursor-y -32.0))
  (when (skey-p :left) (incf cursor-x -18.0))
  (when (skey-p :right) (incf cursor-x 18.0))
  (remove-spurious-mouse-input)
  (when (window:mice-locked-p)
    (multiple-value-bind (delx dely) (delta)
      (setf cursor-x (+ cursor-x delx))
      (setf cursor-y (- cursor-y dely))))
  

  (setf *achar* (mod  (+ *achar* (ceiling e:*scroll-y*)) 256))
  (dobox ((x 0 16)
	  (y 0 16))
	 (setf (pix:get-obj (pix:xy-index x y) *chunks*) (code-char *achar*)))
  (progn
    (let ((list (get-stuff :chunks *stuff* *backup*)))
      (when list
	(gl:delete-lists list 1))
     (remhash :chunks *stuff*))))

(defun quit ()
  (setf e:*status* t))
