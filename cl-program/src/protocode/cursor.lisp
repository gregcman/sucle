(in-package :sandbox)

(progno
 (progno
  (progn
    (defparameter *old-hud-cursor-x* 0)
    (defparameter *old-hud-cursor-y* 0)
    (defparameter *hud-cursor-x* 0)
    (defparameter *hud-cursor-y* 0))
  (progn
    (defparameter *hud-x* 1999)
    (defparameter *hud-y* 1999))
  (defparameter *hud-rectangle* (vector 0 0 0 0)))

 (progno (setf *old-hud-cursor-x* *hud-cursor-x*
	       *old-hud-cursor-y* *hud-cursor-y*))

 (progno (centered-rectangle *hud-rectangle* *hud-x* *hud-y*
			     *window-block-width* *window-block-height*))

 (progno
  (setf *hud-cursor-x* (floor (clamp (- *cursor-x* *camera-x*)
				     (- *window-block-width*)
				     *window-block-width*))
	*hud-cursor-y* (floor (clamp (- *cursor-y* *camera-y*)
				     (- *window-block-height*)
				     *window-block-width*))))

 (progno
  (unless (and (= *old-hud-cursor-x* *hud-cursor-x*)
	       (= *old-hud-cursor-y* *hud-cursor-y*))
    (set-char-with-update (+ *hud-x* *old-hud-cursor-x*)
			  (+ *hud-y* *old-hud-cursor-y*)
			  nil)))

 (progno
  (unless (and (= *old-cursor-x* *cursor-x*)
	       (= *old-cursor-y* *cursor-y*))
    (setf *cursor-moved* *ticks*))
  (progno
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
		    (setf *show-cursor* t)))))))))

 (progno
  (setf *old-cursor-x* *cursor-x*
	*old-cursor-y* *cursor-y*))

 (progno (setf *cursor-x* newx
	       *cursor-y* newy))

 (progno
  (progn
    (progn
      (defparameter *cursor-x* 0)
      (defparameter *cursor-y* 0))
    (progn
      (defparameter *old-cursor-x* 0)
      (defparameter *old-cursor-y* 0)))
  (progn
    (defparameter *show-cursor* t)
    (defparameter *cursor-moved* 0))))
