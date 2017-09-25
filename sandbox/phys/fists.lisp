(in-package :sandbox)

(defparameter fist-side-x nil)
(defparameter fist-side-y nil)
(defparameter fist-side-z nil)

(defparameter fist? nil)
(defparameter fist-side nil)
(defparameter fistx 0.0)
(defparameter fisty 0.0)
(defparameter fistz 0.0)

(defparameter reach 4.0)

(defparameter *block-value* 1)
(defparameter *right-fist-fnc*
  (lambda (x y z)
    (let ((blockval *block-value*))
      (setblock-with-update
       x
       y
       z
       blockval
       (aref mc-blocks::lightvalue blockval)))))

(defparameter *left-fist-fnc*
  (lambda (x y z)
    (setblock-with-update x y z 0 0)))

(defun compute-fist (control-state)
  (when fist?
    (when (window::skey-j-p (window::mouseval :left) control-state)
      (funcall *left-fist-fnc*
	       fist-side-x
	       fist-side-y
	       fist-side-z))
    (when (window::skey-j-p (window::mouseval :right) control-state)
      (funcall *right-fist-fnc*
	       (floor fistx)
	       (floor fisty)
	       (floor fistz))))
  (let ((look-vec (load-time-value (cg-matrix:vec 0.0 0.0 0.0))))
    (unit-pitch-yaw look-vec
		    (coerce *pitch* 'single-float)
		    (coerce *yaw* 'single-float))
    (let ((avx (aref look-vec 0))
	  (avy (aref look-vec 1))
	  (avz (aref look-vec 2)))
      (let ((vx (- (* reach avx)))
	    (vy (- (* reach avy)))
	    (vz (- (* reach avz))))
	(when (and (window:mice-locked-p)
		   (window::skey-p (window::keyval :q) control-state))
	  (big-swing-fist vx vy vz))
 	(standard-fist vx vy vz)))))

(defparameter *fist-function* (constantly nil))
(defun big-swing-fist (vx vy vz)
  (let ((u 3))
    (aabb-collect-blocks
     *xpos* *ypos* *zpos* (* u vx) (* u vy) (* u vz)
     fist-aabb
     
     (lambda (x y z)
       (when (and (<= 0 x 127)
		  (<= 0 y 127)
		  (<= -128 z -1))
	 (let ((blockid 0))
	   (setblock-with-update x y z blockid  (aref mc-blocks::lightvalue blockid))))))))

(defun standard-fist (vx vy vz)
  (multiple-value-bind (frac type blockx blocky blockz)
       (punch-func (+ *xpos* -0.0) (+ *ypos* 0.0) (+ *zpos* -0.0) vx vy vz)
       (if frac
	   (setf fist? t
		 fist-side type
		 fist-side-x blockx
		 fist-side-y blocky
		 fist-side-z blockz
		 fistx (+ *xpos* (* frac vx))
		 fisty (+ *ypos* (* frac vy))
		 fistz (+ *zpos* (* frac vz)))
	   (setf fist? nil))))

(defun punch-func (px py pz vx vy vz)
  (let ((tot-min 2)
	(type :nothing)
	(ansx nil)
	(ansy nil)
	(ansz nil))
     (aabb-collect-blocks px py pz vx vy vz fist-aabb
			  (lambda (x y z)
			    (unless (zerop (world:getblock x y z))
			      (multiple-value-bind (minimum contact-type)
				  (aabbcc::aabb-collide
				   fist-aabb
				   px py pz
				   block-aabb
				   x y z
				   vx vy vz)
				(when (< minimum tot-min)
				  (setq tot-min minimum
					type contact-type
					ansx x
					ansy y
					ansz z))))))
     (values (if (= 2 tot-min) nil tot-min) type ansx ansy ansz)))
