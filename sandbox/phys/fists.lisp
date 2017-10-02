(in-package :sandbox)


(defstruct fister
  (selected-block (vector 0 0 0))
  (normal-block (vector 0 0 0))
  (exists nil)
  (position (vector 0 0 0))
  fun)

(defmacro setvec3d (vec x y z)
  (let ((a (gensym)))
    `(let ((,a ,vec))
       (setf (aref ,a 0) ,x
	     (aref ,a 1) ,y
	     (aref ,a 2) ,z))))

(defun standard-fist (fist px py pz vx vy vz)
  (multiple-value-bind (frac xclamp yclamp zclamp)
      (funcall (fister-fun fist) px py pz vx vy vz)
    (if (or xclamp yclamp zclamp)
	(progn
	  (let ((a (+ px (* frac vx)))
		(b (+ py (* frac vy)))
		(c (+ pz (* frac vz))))
	    (let ((dx (if xclamp (if (plusp vx) 1 -1) 0))
		  (dy (if yclamp (if (plusp vy) 1 -1) 0))
		  (dz (if zclamp (if (plusp vz) 1 -1) 0)))
	      (setvec3d (fister-selected-block fist)
			(floor (+ dx a))
			(floor (+ dy b))
			(floor (+ dz c))))
	    (setvec3d (fister-position fist)
		      a 
		      b
		      c)
	    (setvec3d (fister-normal-block fist)
		      (floor a) 
		      (floor b)
		      (floor c)))
	  (setf (fister-exists fist) t))
	(setf (fister-exists fist) nil))))

(defun use-fist (fist left-p right-p left-fun right-fun)
  (let ((fist? (fister-exists fist))
	(selected-block (fister-selected-block fist))
	(normal-block (fister-normal-block fist)))
    (when fist?
      (when left-p
	(funcall left-fun
		 (aref selected-block 0)
		 (aref selected-block 1)
		 (aref selected-block 2)))
      (when right-p
	(funcall right-fun
		 (aref normal-block 0)
		 (aref normal-block 1)
		 (aref normal-block 2))))))

#+nil
(defun generate-fist-suite (fist-aabb fun)
  (let ((ansx nil)
	(ansy nil)
	(ansz nil)
	(exists? nil)
	(collect-fun nil)
	(exit (gensym)))
    (labels ((collect (x y z aabb)
	       (multiple-value-bind (first? is-minimum?)
		   (funcall collect-fun x y z aabb)
		 (declare (ignorable first? is-minimum?))       
		 (when (and is-minimum? first?)
		   (setf exists? t)
		   (setq ansx x
			 ansy y
			 ansz z)
		   (throw exit nil)))))
      (let ((derived-fun (funcall fun #'collect)))
	(values
	 (lambda ()
	   (values exists? ansx ansy ansz))
	 (lambda ()
	   (setf exists? nil))
	 (configure-collision-handler
	  (lambda (&key collect set-aabb set-exit)
	    (setf collect-fun collect)
	    (funcall set-aabb fist-aabb)
	    (funcall set-exit exit)
	    derived-fun)))))))


#+nil
(lambda (collect)
  (lambda (x y z)
    (unless (zerop (world:getblock x y z))
      (funcall collect x y z *block-aabb*))))

#+nil
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

#+nil
(defparameter fist-side nil)

#+nil

(defparameter *fist-function* (constantly nil))

#+nil
(progno
 (defparameter *world-fist-collision-fun* nil)
 (defparameter *world-fist-collision-fun-reset* nil)
 (defparameter *world-fist-collision-fun-flush* nil)
 (setf (values
	*world-fist-collision-fun-flush*
	*world-fist-collision-fun-reset*
	*world-fist-collision-fun*)
       )

 (defparameter *selected-block* (vector 0 0 0))
 (defparameter *normal-block* (vector 0 0 0))
 (defparameter fist? nil)
 (defparameter *fist-position* (vector 0 0 0)))
