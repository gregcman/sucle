(in-package :sandbox)

(defun collide-world2 (aabb-gen-fnc x y z dx dy dz)
  (multiple-value-bind (new-x new-y new-z xclamp yclamp zclamp)
      (aabbcc::step-motion aabb-gen-fnc
			   x y z dx dy dz)
    (multiple-value-bind (new-dx new-dy new-dz) (aabbcc::clamp-vec
						 dx dy dz
						 xclamp yclamp zclamp)
      (values new-x new-y new-z new-dx new-dy new-dz))))

(defstruct touch-collector
  (acc #b0000000)
  (min-ratio 1.0))
(defun reset-touch-collector (taco)
  (setf (touch-collector-acc taco) #b0000000)
  (setf (touch-collector-min-ratio taco) 1.0))
(define-modify-macro logiorf (&rest args)
  logior)
(defun collect-touch (minimum type touch-collector)
  (let ((tot-min (touch-collector-min-ratio touch-collector)))
    (unless (> minimum tot-min)
      (with-let-mapped-places ((acc (touch-collector-acc touch-collector)))
	(when (< minimum tot-min)
	  (setf (touch-collector-min-ratio touch-collector) minimum)
	  (setf acc #b0000000))
	(case type
	  (:xyz (logiorf acc #b1000000))
	  (:xy  (logiorf acc #b0100000))
	  (:xz  (logiorf acc #b0010000))
	  (:yz  (logiorf acc #b0001000))
	  (:x   (logiorf acc #b0000100))
	  (:y   (logiorf acc #b0000010))
	  (:z   (logiorf acc #b0000001)))))))

(defun collapse-touch (dx dy dz touch-collector)
  (let ((acc (touch-collector-acc touch-collector)))
    (aabbcc::type-collapser
     dx dy dz 
     (logtest acc #b1000000)
     (logtest acc #b0100000)
     (logtest acc #b0010000)
     (logtest acc #b0001000)
     (logtest acc #b0000100)
     (logtest acc #b0000010)
     (logtest acc #b0000001))))

(defun make-collision-suite (&key
			       (aabb player-aabb)
			       (test (lambda (x y z)
				       (declare (ignore x y z)))))
  (declare (type (function (fixnum fixnum fixnum)) test))
  (let ((px 0.0)
	(py 0.0)
	(pz 0.0)
	(vx 0.0)
	(vy 0.0)
	(vz 0.0))
    (let ((taco (make-touch-collector)))
      (labels
	  ((set-aabb (new-aabb);;;
	     (setf aabb new-aabb))
	   (set-test (new-test);;;
	     (setf test new-test))
	   (head (dpx dpy dpz dvx dvy dvz)
	     (setf (values px py pz vx vy vz)
		   (values dpx dpy dpz dvx dvy dvz)))
	   (reset ()
	     (reset-touch-collector taco))
	   (add (foox fooy fooz fooaabb);;;
	     (multiple-value-bind (minimum type)
		 (aabbcc::aabb-collide
		  aabb
		  px py pz
		  fooaabb
		  foox fooy fooz
		  vx vy vz)
	       (collect-touch minimum type taco)))
	   (tail ()
	     (aabb-collect-blocks
	      px py pz vx vy vz aabb
	      test)
	     (multiple-value-bind (xclamp yclamp zclamp)
		 (collapse-touch vx vy vz taco)
	       (values
		(touch-collector-min-ratio taco)
		xclamp yclamp zclamp)))
	   (full (px py pz vx vy vz);;;
	     (head px py pz vx vy vz)
	     (reset)
	     (tail)))
	(list 'set-aabb #'set-aabb
	      'set-test #'set-test
	      'add #'add
	      'full #'full)))))

(defun configure-collision-handler
    (fun &optional (data (make-collision-suite)))
  (let ((set-test (getf data 'set-test))
	(full (getf data 'full))
	(add (getf data 'add))
	(set-aabb (getf data 'set-aabb)))
    (declare (type (function (number number number aabbcc::aabb)
			     (values single-float symbol))
		   add))
    (funcall
     set-test
     (funcall fun add set-aabb))
    full))

(defun make-contact-suite ()
  (let ((px 0.0)
	(py 0.0)
	(pz 0.0)
	(aabb player-aabb))
    (let ((fun (constantly nil)))
      (let ((acc 0))
	(labels
	    ((run (npx npy npz naabb)
	       (setf px npx
		     py npy
		     pz npz
		     aabb naabb)
	       (setf acc 0)
	       (get-blocks-around
		px py pz aabb
		fun)
	       acc)
	     (add (mx my mz maabb)
	       (logiorf
		acc
		(aabbcc::aabb-contact px py pz aabb mx my mz maabb)))
	     (set-fun (newfun)
	       (setf fun newfun)))
	  (list 'full #'run
		'add #'add
		'set-fun #'set-fun))))))

(defun configure-contact-handler
    (fun &optional (data (make-contact-suite)))
  (let ((full (getf data 'full))
	(set-fun (getf data 'set-fun))
	(add (getf data 'add)))
    (funcall
     set-fun
     (funcall fun add))
    full))
