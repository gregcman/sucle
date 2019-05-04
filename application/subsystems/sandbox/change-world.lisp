(in-package :sandbox)

(defun draw-world ()
  (declare (optimize (speed 3) (safety 0)))
  ;(let ((a (get-internal-real-time))))
  (dohash (key value) *g/chunk-call-list*
	  (declare (ignore key))
	  (gl:call-list value))
  ;(print (- (get-internal-real-time) a))
  )

(progn
  (defparameter *g/chunk-call-list* (make-hash-table :test 'equal))
  (defun get-chunk-display-list (name)
    (gethash name *g/chunk-call-list*))
  (defun set-chunk-display-list (name list-num)
    (setf (gethash name *g/chunk-call-list*) list-num))
  (defun remove-chunk-display-list (name)
    (remhash name *g/chunk-call-list*)))
(defun remove-chunk-model (name)
  ;;FIXME::this calls opengl. Use a queue instead?
  (multiple-value-bind (value existsp) (get-chunk-display-list name)
    (when existsp
      (gl:delete-lists value 1)
      (remove-chunk-display-list name))))

(defvar *world-mesh-lparallel-kernel* nil)
(defmacro with-world-mesh-lparallel-kernel (&body body)
  `(let ((lparallel:*kernel* *world-mesh-lparallel-kernel*)) ,@body))
(defparameter *achannel* nil)

(defmacro with-world-meshing-lparallel (&body body)
  `(let ((*world-mesh-lparallel-kernel* nil))
     (unwind-protect (progn (setf *world-mesh-lparallel-kernel*
				  (lparallel:make-kernel 2))
			    (with-world-mesh-lparallel-kernel
			      (let ((*achannel*
				     (lparallel:make-channel)))
				,@body)))
       (when *world-mesh-lparallel-kernel*
	 (lparallel:end-kernel)))))

(defun call-with-world-meshing-lparallel (fun)
  (with-world-meshing-lparallel
    (funcall fun)))

(defun update-world-vao (x y z)
  (clean-dirty)
  (With-world-mesh-lparallel-kernel
    (lparallel:kill-tasks 'mesh-chunk))
  (loop :for key :being :the :hash-keys :of sandbox::*g/chunk-call-list* :do
       (remove-chunk-model key))
  (map nil #'dirty-push
       (sort (alexandria:hash-table-keys world::*chunks*) #'< :key
	     (lambda (position)
	       (multiple-value-bind (i j k) (world:unhashfunc position)
		 ((lambda (x0 y0 z0 x1 y1 z1)
		    (let ((dx (- x1 x0))
			  (dy (- y1 y0))
			  (dz (- z1 z0)))
		      (sqrt (+ (* dx dx) (* dy dy) (* dz dz)))))
		  x y z
		  (- i 8)
		  (- j 8)
		  (- k 8)))))))

(defun update-chunk-mesh (coords iter)
  (when coords
    (remove-chunk-model coords)
    (with-vec (a b c) (iter)
      (let ((len (floor (scratch-buffer:iterator-fill-pointer a) 3)))
	(unless (zerop len)
	  (set-chunk-display-list
	   coords
	   (glhelp:with-gl-list
	     (gl:with-primitives :quads	     
	       (scratch-buffer:flush-my-iterator a
		 (scratch-buffer:flush-my-iterator b
		   (scratch-buffer:flush-my-iterator c
		     (mesh-chunk len a b c))))))))))))

(defun mesh-chunk (times a b c)
  (declare (type fixnum times))
  (declare (optimize (speed 3) (safety 0)))
  (bind-iterator-in
   (xyz single-float) a
   (bind-iterator-in
    (uv single-float) b
    (bind-iterator-in
     (dark single-float) c
     (dotimes (x times)
       (%gl:vertex-attrib-3f 2 (xyz) (xyz) (xyz))
       (%gl:vertex-attrib-2f 8 (uv) (uv))
       (%gl:vertex-attrib-4f 1 (dark) (dark) (dark) (dark))
       (%gl:vertex-attrib-4f 0 (dark) (dark) (dark) (dark));;;zero always comes last?
       )))))

(defun attrib-buffer-iterators ()
  (map-into (make-array 3 :element-type t :initial-element nil)
	    (function scratch-buffer:my-iterator)))
(defun designatemeshing ()
  (loop
     (multiple-value-bind (value success-p) (lparallel:try-receive-result *achannel*)
       (if success-p
	   (apply (car value) (cdr value))
	   (return))))
  (loop
     (let ((thechunk (dirty-pop)))
       (if thechunk
	   (let ((lparallel:*task-category* 'mesh-chunk))
	     (lparallel:submit-task
	      *achannel*
	      (lambda (iter space chunk-pos)
		(map nil (lambda (x) (scratch-buffer:free-my-iterator-memory x)) iter)
		(multiple-value-bind (io jo ko) (world:unhashfunc chunk-pos)
		  (chunk-shape iter io jo ko)
		  (%list space #'update-chunk-mesh chunk-pos iter)))
	      (attrib-buffer-iterators)
	      (make-list 3)
	      thechunk))
	   (return)))))


(defun setblock-with-update (i j k blockid &optional
					     (new-light-value (aref block-data:*lightvalue* blockid)))
 (let ((old-light-value (world:getlight i j k)))
   (when (setf (world:getblock i j k) blockid)
     #+nil
      (when (< new-light-value old-light-value)
	(de-light-node i j k))
      (unless (= old-light-value new-light-value)
	(setf (world:getlight i j k) new-light-value))
      #+nil
      (sky-de-light-node i j k)
      #+nil
      (unless (zerop new-light-value)
	(light-node i j k))
      #+nil
      (flet ((check (a b c)
	       (light-node (+ i a) (+ j b) (+ k c))
	       (sky-light-node (+ i a) (+ j b) (+ k c))))
	(check -1 0 0)
	(check 1 0 0)
	(check 0 -1 0)
	(check 0 1 0)
	(check 0 0 -1)
	(check 0 0 1))
      (block-dirtify i j k))))

(defun plain-setblock (i j k blockid &optional
				       (new-light-value (aref block-data:*lightvalue* blockid))
				       (new-sky-light-value 0))
  (when (setf (world:getblock i j k) blockid)
    (setf (world:getlight i j k) new-light-value)
    (setf (world:skygetlight i j k) new-sky-light-value)
    (block-dirtify i j k)))

;;;;chunk loading

;;FIXME::architecture::one center, the player, and the chunk array centers around it
(defparameter *chunk-coordinate-center-x* 0)
(defparameter *chunk-coordinate-center-y* 0)
(defparameter *chunk-coordinate-center-z* 0)
(defun set-chunk-coordinate-center (player-x player-y player-z)
  (multiple-value-bind (chunk-x chunk-y chunk-z)
      (world::chunk-coordinates-from-block-coordinates
       (floor player-x)
       (floor player-y)
       (floor player-z))
    (setf *chunk-coordinate-center-x* chunk-x)
    (setf *chunk-coordinate-center-y* chunk-y)
    (setf *chunk-coordinate-center-z* chunk-z)))

(defun maybe-move-chunk-array ()
  ;;center the chunk array around the player, but don't always, only if above a certain
  ;;threshold
  ;;FIXME::is this expensive to recompute every frame or does it matter?
  ;;maybe put it in the chunk array object?
  (let ((half-x-size (utility:etouq (floor world::*chunk-array-default-size-x* 2)))
	(half-y-size (utility:etouq (floor world::*chunk-array-default-size-y* 2)))
	(half-z-size (utility:etouq (floor world::*chunk-array-default-size-z* 2))))
    (let ((center-x (+ 
		     (world::chunk-array-x-min world::*chunk-array*)
		     half-x-size))
	  (center-y (+ 
		     (world::chunk-array-y-min world::*chunk-array*)
		     half-y-size))
	  (center-z (+ 
		     (world::chunk-array-z-min world::*chunk-array*)
		     half-z-size)))
      ;;FIXME::hard-coded threshold for repositioning the chunk array? 4 chunks?
      #+nil
      (print (list (- chunk-x center-x)
		   (- chunk-y center-y)
		   (- chunk-z center-z)))
      (let ((chunk-x *chunk-coordinate-center-x*)
	    (chunk-y *chunk-coordinate-center-y*)
	    (chunk-z *chunk-coordinate-center-z*))
	(when (or (<= 4 (abs (- chunk-x center-x)))
		  (<= 4 (abs (- chunk-y center-y)))
		  (<= 4 (abs (- chunk-z center-z))))
	  ;;(format t "moving chunk array")
	  (world::reposition-chunk-array (- chunk-x half-x-size)
					 (- chunk-y half-y-size)
					 (- chunk-z half-z-size)))))))

(defun safe-subseq (seq end)
  (subseq seq 0 (min (length seq) end)))

(defparameter *chunk-radius* 5)
(defparameter *maximum-allowed-chunks* (* (expt (* *chunk-radius* 2) 3)))
(defun get-unloadable-chunks (&optional
				(x0 *chunk-coordinate-center-x*)
				(y0 *chunk-coordinate-center-y*)
				(z0 *chunk-coordinate-center-z*))
  (let ((difference (- (world::total-loaded-chunks) *maximum-allowed-chunks*)))
    (when (plusp difference)
      (let ((distance-sorted-chunks
	     (sort (alexandria:hash-table-keys world::*chunks*) #'< :key
		   (lambda (position)
		     ;;FIXME::destructuring bind of chunk-key happens in multiple places.
		     ;;fix?
		     (world::with-chunk-key-coordinates (x1 y1 z1) position
		       (let ((dx (- x1 x0))
			     (dy (- y1 y0))
			     (dz (- z1 z0)))
			 (sqrt (+ (* dx dx) (* dy dy) (* dz dz)))))))))
	(safe-subseq distance-sorted-chunks difference)))))

(defun load-chunks-around (&optional
				(x0 *chunk-coordinate-center-x*)
				(y0 *chunk-coordinate-center-y*)
				(z0 *chunk-coordinate-center-z*))
  (block out
    (let ((chunk-count 0))
      (flet ((add-chunk (x y z)
	       (incf chunk-count)
	       ;;do something
	       (when (world::empty-chunk-p (world::get-chunk x y z nil))
		 ;;The chunk does not exist, therefore the *empty-chunk* was returned
		 (sandbox::chunk-load (world::create-chunk-key x y z))
		 ;;(print (list x y z))
		 )
	       (when (> chunk-count *maximum-allowed-chunks*)
		 ;;exceeded the allowed chunks to load
		 (return-from out))
	       ))
	(let ((size 6))
	  (utility::dobox ((chunk-x (- x0 size) (+ x0 size))
			   (chunk-y (- y0 size) (+ y0 size))
			   (chunk-z (- z0 size) (+ z0 size)))
			  (add-chunk chunk-x chunk-y chunk-z)))))))

(defun chunk-unload (key &optional (path (world-path)))
  (let ((chunk (world::obtain-chunk-from-chunk-key key nil)))
    (unless (world::empty-chunk-p chunk)
      (let ((worth-saving (world::chunk-worth-saving chunk)))
	;;save the chunk first?
	(if worth-saving
	    ;;write the chunk to disk if its worth saving
	    (savechunk key path)
	    ;;otherwise, if there is a preexisting file, destroy it
	    (let ((chunk-save-file (chunk-coordinate-to-filename key)))
	      (let ((file-exists-p chunk-save-file))
		(when file-exists-p
		  (delete-file chunk-save-file))))))
      
      ;;remove the opengl object
      (remove-chunk-model key)
      ;;remove from the chunk-array
      (world::with-chunk-key-coordinates (x y z) key
	(world::remove-chunk-from-chunk-array x y z))
      ;;remove from the global table
      (world::remove-chunk-at key))))

(defun chunk-load (key &optional (path (world-path)))
  (loadchunk path key))

(defun unload-extra-chunks ()
  (dolist (chunk (get-unloadable-chunks))
    (chunk-unload chunk)))
