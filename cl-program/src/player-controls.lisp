(in-package :sandbox)

(defun physics ()
  (progno
   (when (e:key-j-p (char-code #\E)) (window:toggle-mouse-capture))
   (remove-spurious-mouse-input)
   (when (window:mice-locked-p)
     (multiple-value-bind (delx dely) (delta)
       (let ((xmax (float e:*width*))
	     (ymax (float e:*height*)))
	 (setf cursor-x (min (- xmax 4) (max (- xmax) (+ cursor-x delx))))
	 (setf cursor-y (min (+ ymax 2) (max (- ymax) (- cursor-y dely)))))))))

(defparameter cursor-x 0.0)
(defparameter cursor-y 0.0)

(defparameter mouse-sensitivity (coerce (* 60.0 pi 1/180) 'single-float))
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
