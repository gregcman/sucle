#+nil
(progn
  (defparameter *textx* 0.0)
  (defparameter *texty* 0.0))
#+nil
(progn
  (progn
    (defparameter *mouse-x* 0.0)
    (defparameter *mouse-y* 0.0))
  (progn
    (defparameter *old-mouse-x* 0.0)
    (defparameter *old-mouse-y* 0.0)))

#+nil
(progn
  (defparameter *selection* nil)
  (defparameter *lastpos* nil)) 


#+nil
(when (window::mice-free-p)
  (let ((newmousex (floatify (/ window::*mouse-x* text-sub::*block-width*)))
	(newmousey (floatify (/ (- window::*height* window::*mouse-y*)
				text-sub::*block-height*))))
    (setf *old-mouse-x* *mouse-x*
	  *old-mouse-y* *mouse-y*)
    (setf *mouse-x* newmousex
	  *mouse-y* newmousey)))
#+nil
(when (and (window::mice-free-p)
	   (window::skey-p (window::mouseval :left)))
  (let ((dx (- *mouse-x* *old-mouse-x*))
	(dy (- *mouse-y* *old-mouse-y*)))
    (unless (= dx dy 0)
      (incf *textx* dx)
      (incf *texty* dy)
      (text-sub::flag-text-dirty))))

#+nil
(defun more-controls ()
  (progn
    (when (window::skey-j-p (window::keyval :n))
      (copy-array-buf))
    (when (window::skey-j-p (window::keyval :6))
      (setf *blockid* (multiple-value-call #'world::getblock
			(vec-values (pop *selection*)))))
    (when (window::skey-j-p (window::keyval :y))
      (toggle sandbox-sub::*depth-buffer?*))
    (when (window::skey-j-p (window::keyval :r))
      (setf *selection* nil))
    (when (window::skey-j-p (window::keyval :q))
      (pop *selection*))
    (when (window::skey-j-p (window::keyval :2))
      (map-box (hollowbox (lambda (x y z)
			    (sandbox::plain-setblock x y z *blockid* 0))
			  2)))
    (when (window::skey-j-p (window::keyval :5))
      (map-box (hollowbox (lambda (x y z)
			    (sandbox::plain-setblock x y z *blockid* 0))
			  0)))
    (when (window::skey-j-p (window::keyval :4))
      (map-box (sphere (lambda (x y z)
			 (sandbox::plain-setblock x y z *blockid* 0)))))
    (with-vec (a b c) ((sandbox-sub::farticle-position
			(sandbox-sub::entity-particle *ent*)))
      (when (window::skey-j-p (window::keyval :b))
	(setf *box* (make-box (pop *selection*)
			      (pop *selection*))))
      (when (window::skey-j-p (window::keyval :x))      
	(push (vector (floor a) (floor b) (floor c)) *selection*))
      (when (window::skey-p (window::keyval :z))      
	(setfoo (setf *lastpos* (vector (floor a) (floor b) (floor c))))))
    
    (when (window::skey-j-p (window::keyval :1))
      (multiple-value-call #'line2
	(vec-values (pop *selection*))
	(vec-values (pop *selection*))
	*blockid*))
    (when (window::skey-j-p (window::keyval :c))
      (music::cleanup-poller))))

#+nil
(cond
  #+nil
  ((window::skey-p (window::keyval :left-control))
   (push (vector a b c) *selection*))
  #+nil
  ((window::skey-p (window::keyval :left-alt))
   (push (world::getobj a b c) *selection*))
  (t))

#+nil
(cond
  #+nil
  ((window::skey-p (window::keyval :left-control))
   (push (vector a b c) *selection*))
  #+nil
  ((window::skey-p (window::keyval :left-alt))
   (push (world::getobj a b c) *selection*))
  (t))
