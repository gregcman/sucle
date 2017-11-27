(in-package :fuck)

(progn
  (declaim (ftype (function (single-float) single-float)
		  translator))
  (fuktard::with-unsafe-speed
    (defun translator (x)
      (let* ((a (* x 0.6))
	     (b (+ 0.2 a))
	     (c (* b b b))
	     (d (* 8.0 0.15 (/ (coerce pi 'single-float) 180.0)))
	     (e (* d c)))
	(declare (type single-float a b c d e))
	e))))

(defparameter *mouse-multiplier* (translator 0.5))

(defun delta2 ()
  (let ((mult *mouse-multiplier*))
    (multiple-value-bind (dx dy) (delta)
      (let ((dyaw (- (* dx mult)))
	    (dpitch (* dy mult)))
	(values dyaw dpitch)))))
(defun delta ()
  (let ((mouse-data (load-time-value (cons 0 0))))
    (multiple-value-bind (newx newy) (window:get-mouse-position)
      (multiple-value-prog1 (values
			     (- newx (car mouse-data))
			     (- newy (cdr mouse-data)))
	(setf (car mouse-data) newx
	      (cdr mouse-data) newy)))))

(defparameter mousecapturestate nil)
(defun remove-spurious-mouse-input ()
  (if (window:mice-locked-p)
      (case mousecapturestate
	((nil)
	 (delta) ;;toss spurious mouse movement 
	 (setf mousecapturestate :justcaptured))
	(:justcaptured (setq mousecapturestate t))
	((t)))
      (when mousecapturestate
	(setq mousecapturestate nil))))

#+nil
(defparameter mouse-sensitivity (coerce (* 60.0 pi 1/180) 'single-float))
#+nil
(defun look-around (yaw pitch dx dy)
  (let ((dyaw (- (* dx (translator 0.5))))
	(dpitch (* dy (translator 0.5))))
    (let ((yaw0? (zerop dyaw))
	  (pitch0? (zerop dpitch)))
      (values
       (if yaw0? nil
	   (mod (+ yaw dyaw) two-pi))
       (if pitch0? nil
	   (alexandria:clamp
	    (+ pitch dpitch)
	    (* -0.99 half-pi)
	    (* 0.99 half-pi)))))))
