(in-package :sandbox)

(defun collide-world2 (aabb-gen-fnc x y z dx dy dz)
  (multiple-value-bind (new-x new-y new-z xclamp yclamp zclamp)
      (aabbcc::step-motion aabb-gen-fnc
			   x y z dx dy dz)
    (multiple-value-bind (new-dx new-dy new-dz) (aabbcc::clamp-vec
						 dx dy dz
						 xclamp yclamp zclamp)
      (values new-x new-y new-z new-dx new-dy new-dz))))

(defun untouched (ratio)
  (= ratio 69.0))

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
    (if (> minimum tot-min)
	(values nil nil)
	(with-let-mapped-places ((acc (touch-collector-acc touch-collector)))
	  (let ((is-minimum? (< minimum tot-min)))
	    (when is-minimum?
	      (setf (touch-collector-min-ratio touch-collector) minimum)
	      (setf acc #b0000000))
	    (case type
	      (:xyz (logiorf acc #b1000000))
	      (:xy  (logiorf acc #b0100000))
	      (:xz  (logiorf acc #b0010000))
	      (:yz  (logiorf acc #b0001000))
	      (:x   (logiorf acc #b0000100))
	      (:y   (logiorf acc #b0000010))
	      (:z   (logiorf acc #b0000001)))
	    (values is-minimum? t))))))

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

(defun make-contact-suite ()
  (let ((px 0.0)
	(py 0.0)
	(pz 0.0)
	(aabb nil))
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


(defun collide-fucks (aabb some-hooks)
  (let ((taco (make-touch-collector))
	(blockvec (make-array 0 :adjustable t :fill-pointer 0)))
    (flet ((bladd-x-y-z (x y z aabb)
	     (vector-push-extend x blockvec)
	     (vector-push-extend y blockvec)
	     (vector-push-extend z blockvec)
	     (vector-push-extend aabb blockvec)))
      (let ((hooks (mapcar (lambda (func)
			    (funcall func #'bladd-x-y-z))
			   some-hooks)))
	(lambda (px py pz vx vy vz)
	  (reset-touch-collector taco)
	  (setf (fill-pointer blockvec) 0)
	  (dolist (fun hooks)
	    (funcall fun px py pz vx vy vz aabb))
	  (dobox
	   ((index 0 (fill-pointer blockvec) :inc 4))
	   (let ((foox (aref blockvec (+ 0 index)))
		 (fooy (aref blockvec (+ 1 index)))
		 (fooz (aref blockvec (+ 2 index)))
		 (fooaabb (aref blockvec (+ 3 index))))
	     (multiple-value-bind (minimum type)
		 (aabbcc::aabb-collide
		  aabb
		  px py pz
		  fooaabb
		  foox fooy fooz
		  vx vy vz)
	       (collect-touch minimum type taco))))
	  (multiple-value-bind (xclamp yclamp zclamp)
	      (collapse-touch vx vy vz taco)
	    (values
	     (touch-collector-min-ratio taco)
	     xclamp yclamp zclamp)))))))

#+nil
(configure-collision-handler
   (lambda (&key collect set-aabb &allow-other-keys)
     (funcall set-aabb *player-aabb*)
     (lambda (x y z)
       (when (aref mc-blocks::iscollidable (world:getblock x y z))
	 (funcall collect x y z *block-aabb*)))))

#+nil
(progno
 (defun make-collision-suite (&key
				(aabb nil)
				(test (lambda (x y z)
					(declare (ignore x y z)))))
   (declare (type (function (fixnum fixnum fixnum)) test))
   (let ((px 0.0)
	 (py 0.0)
	 (pz 0.0)
	 (vx 0.0)
	 (vy 0.0)
	 (vz 0.0)
	 (early-exit nil))
     (let ((taco (make-touch-collector)))
       (labels
	   ((set-aabb (new-aabb);;;
	      (setf aabb new-aabb))
	    (set-exit (exit);;;
	      (setf early-exit exit))
	    (set-test (new-test);;;
	      (setf test new-test))
	    (head (dpx dpy dpz dvx dvy dvz)
	      (setf (values px py pz vx vy vz)
		    (values dpx dpy dpz dvx dvy dvz)))
	    (reset ()
	      (reset-touch-collector taco))
	    (collect (foox fooy fooz fooaabb);;;
	      (multiple-value-bind (minimum type)
		  (aabbcc::aabb-collide
		   aabb
		   px py pz
		   fooaabb
		   foox fooy fooz
		   vx vy vz)
		(collect-touch minimum type taco)))
	    (tail ()
	      (catch early-exit
		(aabb-collect-blocks
		 px py pz vx vy vz aabb
		 test))
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
	       'set-exit #'set-exit
	       'set-test #'set-test
	       'collect #'collect
	       'full #'full)))))
 (defun configure-collision-handler
     (fun &optional (data (make-collision-suite)))
   (let ((set-test (getf data 'set-test))
	 (full (getf data 'full))
	 (collect (getf data 'collect))
	 (set-aabb (getf data 'set-aabb))
	 (set-exit (getf data 'set-exit)))
     (declare (type (function (number number number aabbcc::aabb)
			      (values single-float symbol))
		    collect))
     (funcall
      set-test
      (funcall fun
	       :collect collect
	       :set-aabb set-aabb
	       :set-exit set-exit))
     full)))
