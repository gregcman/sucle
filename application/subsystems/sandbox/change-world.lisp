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

(defun update-world-vao ()
  (clean-dirty)
  (reset-meshers)
  (loop :for key :being :the :hash-keys :of sandbox::*g/chunk-call-list* :do
     (remove-chunk-model key))
  (mapc #'dirty-push
	(sort (alexandria:hash-table-keys world::*chunks*) #'< :key
	      'unsquared-chunk-distance)))

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
(defparameter *chunk-render-radius* 6)
(defparameter *total-background-chunk-mesh-jobs* 0)
(defparameter *max-total-background-chunk-mesh-jobs* 5)
(defun reset-meshers ()
  (With-world-mesh-lparallel-kernel
    (lparallel:kill-tasks 'mesh-chunk)
    (setf *total-background-chunk-mesh-jobs* 0)))
;;We limit the amount of chunks that can be sent to the mesh queue
(defun designatemeshing ()
  (loop
     (multiple-value-bind (value success-p) (lparallel:try-receive-result *achannel*)
       (cond (success-p
	      (apply (car value) (cdr value))
	      (decf *total-background-chunk-mesh-jobs*))
	     (t (return)))))
  ;;remove chunks from the queue that are too far away, don't try to mesh them
  #+nil ;;WRONG!!FIXME::separate world loading code from opengl
  (queue::sort-queue
   *dirty-chunks*
   (lambda (list)
     (sort (delete-if (lambda (x)
			(>= (blocky-chunk-distance x) *chunk-render-radius*))
		      list)
	   '< :key 'unsquared-chunk-distance)))
  (when (> *max-total-background-chunk-mesh-jobs* *total-background-chunk-mesh-jobs*)
    (loop
       (let ((thechunk (dirty-pop)))
	 (if thechunk
	     (when (world::chunk-exists-p thechunk)
	       (incf *total-background-chunk-mesh-jobs*)
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
		  thechunk)))
	     (return))))))


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

(defparameter *reposition-chunk-array-threshold* 1)
(defun maybe-move-chunk-array ()
  ;;center the chunk array around the player, but don't always, only if above a certain
  ;;threshold
  ;;FIXME::is this expensive to recompute every frame or does it matter?
  ;;maybe put it in the chunk array object?
  ;;return t if it was moved, nil otherwise
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
	    (chunk-z *chunk-coordinate-center-z*)
	    (threshold *reposition-chunk-array-threshold*))
	(when (or (<= threshold (abs (- chunk-x center-x)))
		  (<= threshold (abs (- chunk-y center-y)))
		  (<= threshold (abs (- chunk-z center-z))))
	  ;;(format t "moving chunk array")
	  (world::reposition-chunk-array (- chunk-x half-x-size)
					 (- chunk-y half-y-size)
					 (- chunk-z half-z-size))
	  (values t))))))

(defun safe-subseq (seq end)
  (subseq seq 0 (min (length seq) end)))

(defparameter *chunk-radius* 6)
;;FIXME::how to determine the maximum allowed chunks? leave some leeway for loading?
(defparameter *maximum-allowed-chunks* (* (expt (* (+ 1 *chunk-radius*) 2) 3)))
(defun chunk-memory-usage (&optional (chunks *maximum-allowed-chunks*))
  ;;in megabytes
  (/ (* 8 #|8 bytes per word in 64bit sbcl|# chunks
	(* world::*chunk-size-x* world::*chunk-size-y* world::*chunk-size-z*))
     1024 1024 1.0 #|1.0 for converting to float|#))
(defparameter *threshold* (* 8 8))
;;threshold so that when too many chunks exist, over compensate, so not unloading every time
(defun unsquared-chunk-distance (position-key)
  (let ((x0 *chunk-coordinate-center-x*)
	(y0 *chunk-coordinate-center-y*)
	(z0 *chunk-coordinate-center-z*))
    (world::with-chunk-key-coordinates
     (x1 y1 z1) position-key
     (let ((dx (- x1 x0))
	   (dy (- y1 y0))
	   (dz (- z1 z0)))
       ;;FIXME::we don't need the sqrt for sorting
       (+ (* dx dx) (* dy dy) (* dz dz))))))
(defun blocky-chunk-distance (position-key)
  (let ((x0 *chunk-coordinate-center-x*)
	(y0 *chunk-coordinate-center-y*)
	(z0 *chunk-coordinate-center-z*))
    (world::with-chunk-key-coordinates
     (x1 y1 z1) position-key
     (let ((dx (- x1 x0))
	   (dy (- y1 y0))
	   (dz (- z1 z0)))
       (max (abs dx)
	    (abs dy)
	    (abs dz))))))
(defun get-unloadable-chunks ()
  ;;FIXME::optimize?
  (let ((difference (- (- (world::total-loaded-chunks) *maximum-allowed-chunks*)
		       *threshold*)))
    (when (plusp difference)
      (let ((distance-sorted-chunks	     
	     (sort (alexandria:hash-table-keys world::*chunks*) #'> :key
		   'unsquared-chunk-distance)))
	(safe-subseq distance-sorted-chunks difference)))))

(defun load-chunks-around ()
  (let ((x0 *chunk-coordinate-center-x*)
	(y0 *chunk-coordinate-center-y*)
	(z0 *chunk-coordinate-center-z*))
    (declare (optimize (speed 3) (safety 0))
	     (type world::chunk-coord x0 y0 z0))
    (block out
      (let ((chunk-count 0))
	(declare (type fixnum chunk-count))
	(flet ((add-chunk (x y z)
		 (incf chunk-count)
		 ;;do something
		 (let ((key (world::create-chunk-key x y z)))
		   (unless (world::chunk-exists-p key)
		     ;;The chunk does not exist, therefore the *empty-chunk* was returned
		     (sandbox::chunk-load key)
		     ;;(print (list x y z))
		     ))
		 (when (>
			;;FIXME::nonportably assume chunk-count and maxium allowed chunks are fixnums
			(the fixnum chunk-count)
			(the fixnum *maximum-allowed-chunks*))
		   ;;exceeded the allowed chunks to load
		   (return-from out))
		 ))
	  (let ((size *chunk-radius*))
	    (declare (type world::chunk-coord size))
	    (utility::dobox ((chunk-x (the world::chunk-coord (- x0 size))
				      (the world::chunk-coord (+ x0 size)))
			     (chunk-y (the world::chunk-coord (- y0 size))
				      (the world::chunk-coord (+ y0 size)))
			     (chunk-z (the world::chunk-coord (- z0 size))
				      (the world::chunk-coord (+ z0 size))))
			    (add-chunk chunk-x chunk-y chunk-z))))))))

(defun chunk-save (chunk &key (path (world-path)))
  (cond
    ((world::empty-chunk-p chunk)
     ;;when the chunk is obviously empty
     )
    (t
     ;;when the chunk is not obviously empty
     (when (world::chunk-modified chunk) ;;if it wasn't modified, no point in saving
       (let ((worth-saving (world::chunk-worth-saving chunk))
	     (key (world::chunk-key chunk)))
	 ;;save the chunk first?
	 (cond
	   (worth-saving
	    ;;write the chunk to disk if its worth saving
	    (savechunk key path))
	   (t
	    ;;otherwise, if there is a preexisting file, destroy it
	    (let ((chunk-save-file
		   ;;FIXME::bad api?
		   (merge-pathnames (convert-object-to-filename (chunk-coordinate-to-filename key))
				    (world-path))))
	      (let ((file-exists-p (probe-file chunk-save-file)))
		(when file-exists-p
		  (delete-file chunk-save-file)))))))))))

(defun chunk-unload (key &key (path (world-path)))
  (let ((chunk (world::obtain-chunk-from-chunk-key key nil)))
    (chunk-save chunk :path path)
    ;;remove the opengl object
    ;;empty chunks have no opengl counterpart, FIXME::this is implicitly assumed
    ;;FIXME::remove anyway?
    (remove-chunk-model key)
    ;;remove from the chunk-array
    (world::with-chunk-key-coordinates (x y z) key
				       (world::remove-chunk-from-chunk-array x y z))
    ;;remove from the global table
    (world::remove-chunk-at key)))

(defun dirty-push-around (key)
  ;;FIXME::although this is correct, it
  ;;lags behind player movement?
  (world::with-chunk-key-coordinates
   (x y z) key
   (dobox ((x0 (1- x) (+ x 2))
	   (y0 (1- y) (+ y 2))
	   (z0 (1- z) (+ z 2)))
	  (let ((new-key (world::create-chunk-key x0 y0 z0)))
	    (when (world::chunk-exists-p new-key)
	      (dirty-push new-key))))))


(defun chunk-load (key &optional (path (world-path)))
  ;;FIXME::using chunk-coordinate-to-filename before
  ;;running loadchunk is a bad api?
  (let ((load-type (loadchunk path (chunk-coordinate-to-filename key))))
    (unless (eq load-type :empty) ;;FIXME::better api?
      (dirty-push-around key))))

(defun unload-extra-chunks ()
  (let ((to-unload
	 ;;FIXME::get a timer library? metering?
	 (;;time
	  progn
	  (progn ;;(print "getting unloadable chunks")
		 (get-unloadable-chunks)))))
    ;;(print (length to-unload))
    (;;time
     progn
     (progn ;;(print "unloading the chunks")
	    (dolist (chunk to-unload)
	      (chunk-unload chunk))))))

(defun test34 ()
  ;;(with-output-to-string (print ))
  (let* ((data (world::chunk-data (world::get-chunk 1 1 1 nil)))
	 (str (with-output-to-string (str)
		(with-standard-io-syntax
		  (print data str))))
	 (conspack (conspack::tracking-refs () (conspack::encode data)))
	 (times (expt 10 3)))
    (time
     (dotimes (x times)
       (with-standard-io-syntax
	 (read-from-string str))))
    (time
     (dotimes (x times)
       (conspack::decode conspack)))
    (values)))
;;conspack is roughly 4 times faster than plain PRINT and READ?
