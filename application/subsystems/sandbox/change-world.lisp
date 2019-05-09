(in-package :sandbox)
(defparameter *call-lists* (make-array 0 :fill-pointer 0 :adjustable t))
(defun draw-world ()
  (declare (optimize (speed 3) (safety 0)))
  ;;(let ((a (get-internal-real-time))))
  (let ((vec *call-lists*))
    (setf (fill-pointer vec) 0)
    (let* ((foo (+ 1 *chunk-radius*)))
      (dohash (key value) *g/chunk-call-list*
	      ;;(declare (ignore key))
	      (when (> (the fixnum foo) (the fixnum (blocky-chunk-distance key)))
		(vector-push-extend value vec))))
    (gl:call-lists vec))
  ;(print (- (get-internal-real-time) a))
  )

(progn
  (defparameter *g/chunk-call-list* (make-hash-table :test 'equal))
  (defun get-chunk-display-list (name)
    (gethash name *g/chunk-call-list*))
  (defun set-chunk-display-list (name list-num)
    (setf (gethash name *g/chunk-call-list*) list-num))
  (defun remove-chunk-display-list (name)
    (remhash name *g/chunk-call-list*))
  (defun reset-chunk-display-list ()
    (clrhash *g/chunk-call-list*)))
(defun remove-chunk-model (name)
  ;;FIXME::this calls opengl. Use a queue instead?
  (multiple-value-bind (value existsp) (get-chunk-display-list name)
    (when existsp
      (gl:delete-lists value 1)
      (remove-chunk-display-list name))))

(defparameter *finished-mesh-tasks* (lparallel.queue:make-queue))

(defun call-with-world-meshing-lparallel (fun)
  (sandbox.multiprocessing::with-initialize-multiprocessing
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
(defparameter *max-total-background-chunk-mesh-jobs* 1)
(defun reset-meshers ()
  (sandbox.multiprocessing::with-kernel
    (lparallel:kill-tasks 'mesh-chunk)
    #+nil
    (progn
      (lparallel:kill-tasks :chunk-load)
      (lparallel:kill-tasks :chunk-save))
    (setf *total-background-chunk-mesh-jobs* 0)))
;;We limit the amount of chunks that can be sent to the mesh queue
(defun designatemeshing ()
  (sandbox.multiprocessing::do-queue (job-task *finished-mesh-tasks*)
    (let ((value (car (sandbox.multiprocessing::job-task-return-values job-task))))
      (cond (value
	     (destructuring-bind (type function . args) value
	       (apply function args)
	       (when (eq :mesh-chunk type)
		 ;;FIXME::document this somewhere?
		 ;;*achannel* becoming a generic command buffer?
		 (decf *total-background-chunk-mesh-jobs*))))
	    (t (print value)))))
  (when (> *max-total-background-chunk-mesh-jobs* *total-background-chunk-mesh-jobs*)
    (queue::sort-queue
     *dirty-chunks*
     (lambda (list)
       (sort list
	     ;;remove chunks from the queue that are too far away, don't try to mesh them
	     #+nil ;;WRONG!!FIXME::separate world loading code from opengl
	     (delete-if (lambda (x)
			  (>= (blocky-chunk-distance x) *chunk-render-radius*))
			list)
	     '< :key 'unsquared-chunk-distance)))
    (loop
       (let ((thechunk (dirty-pop)))
	 (if thechunk
	     (when (and (world::chunk-exists-p thechunk)
			;;(not (world::empty-chunk-p (world::get-chunk-at thechunk)))
			)
	       (incf *total-background-chunk-mesh-jobs*)
	       (let ((lparallel:*task-category* 'mesh-chunk))
		 (sandbox.multiprocessing::submit 
		  (lambda (iter space chunk-pos)
		    (map nil (lambda (x) (scratch-buffer:free-my-iterator-memory x)) iter)
		    (multiple-value-bind (io jo ko) (world:unhashfunc chunk-pos)
		      (chunk-shape iter io jo ko)
		      (%list space :mesh-chunk 'update-chunk-mesh chunk-pos iter)))
		  :args (list
			 (attrib-buffer-iterators)
			 (make-list 4)
			 thechunk)
		  :callback (lambda (job-task)
			      (lparallel.queue:push-queue job-task *finished-mesh-tasks*)))))
	     (return))))))

#+nil
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
  (mapc (lambda (key)
	  (sandbox::chunk-load key))
	(get-chunks-to-load)))
(defun get-chunks-to-load ()
  (let ((x0 *chunk-coordinate-center-x*)
	(y0 *chunk-coordinate-center-y*)
	(z0 *chunk-coordinate-center-z*))
    (declare (optimize (speed 3) (safety 0))
	     (type world::chunk-coord x0 y0 z0))
    (let ((acc nil))
      (block out
	(let ((chunk-count 0))
	  (declare (type fixnum chunk-count))
	  (flet ((add-chunk (x y z)
		   (incf chunk-count)
		   ;;do something
		   (let ((key (world::create-chunk-key x y z)))
		     (when (not (chunk-exists-at-and-not-empty key))
		       ;;The chunk does not exist, therefore the *empty-chunk* was returned
		       (push key acc)
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
			      (add-chunk chunk-x chunk-y chunk-z))))))
      acc)))

(defparameter *persist* t)
(defun chunk-save (chunk &key (path (world-path)))
  (when (not *persist*)
    (return-from chunk-save))
  (cond
    ((world::empty-chunk-p chunk)
     ;;when the chunk is obviously empty
     )
    (t
     ;;when the chunk is not obviously empty
     (when (world::chunk-modified chunk) ;;if it wasn't modified, no point in saving
       (let* ((worth-saving (world::chunk-worth-saving chunk))
	      (key (world::chunk-key chunk))
	      ;;FIXME::have multiple unique-task hashes?
	      (job-key (cons :save-chunk key)))
	 ;;save the chunk first?
	 (sandbox.multiprocessing::submit-unique-task
	  job-key
	  ((lambda ()
	     (cond
	       (worth-saving
		;;write the chunk to disk if its worth saving
		(savechunk chunk key path)
		;;(format t "~%saved chunk ~s" key)
		)
	       (t
		;;otherwise, if there is a preexisting file, destroy it
		(let ((chunk-save-file
		       ;;FIXME::bad api?
		       (merge-pathnames
			(convert-object-to-filename (chunk-coordinate-to-filename key))
			(world-path))))
		  (let ((file-exists-p (probe-file chunk-save-file)))
		    (when file-exists-p
		      (delete-file chunk-save-file)))))))
	   :data job-key
	   :callback (lambda (job-task)
		       (declare (ignorable job-task))
		       (sandbox.multiprocessing::remove-unique-task-key
			(sandbox.multiprocessing::job-task-data job-task)))
	   ;;this task, saving and loading, must not be interrupted
	   :unkillable t)))))))

(defun chunk-unload (key &key (path (world-path)))
  (let ((chunk (world::obtain-chunk-from-chunk-key key nil)))
    (chunk-save chunk :path path)
    ;;remove the opengl object
    ;;empty chunks have no opengl counterpart, FIXME::this is implicitly assumed
    ;;FIXME::remove anyway?
    (remove-chunk-model key)
    ;;remove from the chunk-array
    (world::with-chunk-key-coordinates (x y z)
	key
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

(defparameter *load-jobs* 0)

(defun chunk-exists-at-and-not-empty (key)
  (and (world::chunk-exists-p key)
       (not (world::empty-chunk-p (world::get-chunk-at key)))))

(defun chunk-load (key &optional (path (world-path)))
  ;;FIXME::using chunk-coordinate-to-filename before
  ;;running loadchunk is a bad api?
  #+nil
  (let ((load-type (loadchunk path (chunk-coordinate-to-filename key))))
    (unless (eq load-type :empty) ;;FIXME::better api?
      (dirty-push-around key)))
  ;;(print 34243)
  (let ((job-key (cons :chunk-load key)))
    (sandbox.multiprocessing::submit-unique-task
     job-key
     ((lambda ()
	(setf (cdr (sandbox.multiprocessing::job-task-data
		    sandbox.multiprocessing::*current-job-task*))
	      (cond ((chunk-exists-at-and-not-empty key)
		     :skipping)
		    (t (loadchunk key path)))))
      :data (cons job-key "")
      :callback (lambda (job-task)
		  (declare (ignorable job-task))
		  (cond
		    ((chunk-exists-at-and-not-empty key)
		     (format t "~%WTF? ~a chunk already exists" key))
		    (t (let* ((job-key (car (sandbox.multiprocessing::job-task-data job-task)))
			      (key (cdr job-key))
			      (chunk
			       (cdr (sandbox.multiprocessing::job-task-data job-task))))
			 ;;FIXME? locking is not necessary if the callback runs in the
			 ;;same thread as the code which changes the chunk-array and *chunks* ?
			 (cond
			   ((eq chunk :skipping)
			    (format t "~%chunk skipped loading, "))
			   (t
			    (progn
			      (apply #'world::remove-chunk-from-chunk-array key)
			      (world::set-chunk-at key chunk))
			    ;;(format t "~%making chunk ~a" key)

			    ;;(world::set-chunk-at key new-chunk)

			    (cond
			      ((world::empty-chunk-p chunk)
			       ;;(background-generation key)
			       )
			      (t (dirty-push-around key)))))
			 )))
		  (sandbox.multiprocessing::remove-unique-task-key job-key)
		  (decf *load-jobs*)))
     (incf *load-jobs*))))

(defun background-generation (key)
  (let ((job-key (cons :world-gen key)))
    (sandbox.multiprocessing::submit-unique-task
     job-key
     ((lambda ()
	(generate-for-new-chunk key))
      :callback (lambda (job-task)
		  (declare (ignore job-task))
		  (dirty-push-around key)
		  (sandbox.multiprocessing::remove-unique-task-key job-key))))))

(utility:with-unsafe-speed
  (defun generate-for-new-chunk (key)
    (multiple-value-bind (x y z) (world::unhashfunc key)
      (declare (type fixnum x y z))
      ;;(print (list x y z))
      (when (>= y -1)
	(dobox ((x0 x (the fixnum (+ x 16)))
		(y0 y (the fixnum (+ y 16)))
		(z0 z (the fixnum (+ z 16))))
	       (let ((block (let ((threshold 0.3))
			      (if (> threshold (black-tie::perlin-noise-single-float
						(* x0 0.05)
						(* y0 0.05)
						(* z0 0.05)))
				  0
				  1))))
		 (setf (world::getobj x0 y0 z0)
		       (world::blockify block
					(case block
					  (0 15)
					  (1 0))
					0))))))))


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

;;FIXME::thread-safety for:
;;world::*chunks*
;;world::*chunk-array*
;;sandbox::*dirty-chunks*
;;sandbox::*achannel*
