;;;;Currently this file contains:
;;;;- block types data 
;;;;- world format
;;;;- keeping track of changes to the world
;;;;- accessors

(in-package #:world)

(defgeneric lispobj-dispatch (obj))

(defun value-dispatch (value)
  (typecase value
    (fixnum value)
    (otherwise (lispobj-dispatch value))))

(defun (setf num-getobj) (new i j k)
  (voxel-chunks:setobj i j k new))
(defun num-getobj (i j k)
  (let ((value (voxel-chunks:getobj i j k)))
    (value-dispatch value)))

(defmacro suite (bits position get get-ldb)
  (let* ((bytespec `(byte ,bits ,position))
	 (access `(ldb ,bytespec
		       (num-getobj i j k))))
    `(progn
       (defun ,get-ldb (value)
	 (ldb ,bytespec value))
       (defun (setf ,get) (new i j k)
	 (setf ,access new))
       (defun ,get (i j k)
	 ,access))))

(progn
  (suite 8 0 getblock getblock-extract)
  (suite 4 8 getlight getlight-extract)
  (suite 4 12 skygetlight skygetlight-extract))

;;[FIXME]move this to a better place?
(defun blockify (blockid light sky)
  (dpb sky (byte 4 12)
       (dpb light (byte 4 8) blockid)))

(eval-when (:load-toplevel :execute)
  (voxel-chunks::reset-empty-chunk-value (blockify 0 0 15)))

(defmethod lispobj-dispatch ((obj character))
  (blockify (char-code obj) 0 0))

(defmethod lispobj-dispatch ((obj t))
  (blockify (logcount (sxhash obj)) 0 0))

(defmethod lispobj-dispatch ((obj symbol))
  *empty-space*)


#+nil
(defun test ()
  (let ((times (expt 10 6)))
    (time (dotimes (x times) (setf (getobj 0 0 0) 0)))
    (time (dotimes (x times) (setf (world:getobj 0 0 0) 0))))
  (let ((times (expt 10 6)))
    (time (dotimes (x times) (getobj 0 0 0)))
    (time (dotimes (x times) (world:getobj 0 0 0)))))
;;;;************************************************************************;;;;
;;;;<PERSIST-WORLD>
(in-package #:world)

;;[FIXME]move generic loading and saving with printer and conspack to a separate file?
;;And have chunk loading in another file?

;;world loading code below?
(defun convert-object-to-filename (obj)
  (format nil "~s" obj))

(defparameter *some-saves* nil)
(defparameter *world-directory* nil)
(defun world-path (&optional (path *world-directory*) (base-dir *some-saves*))
  (utility:rebase-path path base-dir))

(defun savechunk (chunk position &optional (path (world-path)))
  ;;[FIXME]undocumented swizzling and multiplication by 16, as well as loadchunk
  (let ((filename (convert-object-to-filename (chunk-coordinate-to-filename position))))
    ;;(format t "~%Saving chunk ~a" filename)
    (sucle-serialize::store-lisp-objects-to-file
     (merge-pathnames
      filename
      path)
     (list
      (voxel-chunks::chunk-data chunk)))))

(defun loadchunk (chunk-coordinates &optional (path (world-path)))
  (let ((data
	 (sucle-serialize::retrieve-lisp-objects-from-file
	  (merge-pathnames (convert-object-to-filename
			    (chunk-coordinate-to-filename chunk-coordinates))
			   path))))
    (case (length data)
      (0
       ;;if data is nil, just load an empty chunk
       (voxel-chunks::with-chunk-key-coordinates (x y z) chunk-coordinates
	 (voxel-chunks::create-chunk x y z :type :empty)))

      (3 ;;[FIXME]does this even work?
       (destructuring-bind (blocks light sky) data
	 (let ((len (length blocks)))
	   (let ((new (make-array len)))
	     (dotimes (i len)
	       (setf (aref new i)
		     (world:blockify (aref blocks i)  (aref light i) (aref sky i))))
	     (voxel-chunks::make-chunk-from-key-and-data chunk-coordinates new)))))
      (1
       (destructuring-bind (objdata) data
	 (voxel-chunks::make-chunk-from-key-and-data
	  chunk-coordinates
	  (coerce objdata '(simple-array t (*)))))))))

;;The world is saved as a directory full of files named (x y z) in block coordinates, with
;;x and y swizzled

(defun filename-to-chunk-coordinate (filename-position-list)
  (let ((position
	 (mapcar
	  ;;[FIXME]assumes chunks are 16 by 16 by 16
	  (lambda (n) (floor n 16))
	  filename-position-list)))
    (rotatef (third position)
	     (second position))
    position))

(defun chunk-coordinate-to-filename (chunk-coordinate)
  (let ((position-list (multiple-value-list (voxel-chunks:unhashfunc chunk-coordinate))))
    (rotatef (second position-list)
	     (third position-list))
    position-list))

#+nil
(defun load-world (path)
  ;;[FIXME]don't load the entire world
  (let ((files (uiop:directory-files path)))
    (dolist (file files)
      (loadchunk path (read-from-string (pathname-name file))))))

#+nil
(defun delete-garbage (&optional (path (world-path)))
  (let ((files (uiop:directory-files path)))
    (dolist (file files)
      (let ((data
	     (retrieve-lisp-objects-from-file file)))
	(when (typep data '(cons array null))
	  (delete-file file))))))

;;;;</PERSIST-WORLD>
;;;;************************************************************************;;;;
;;;;<CHANGE-WORLD?>
;;;;keeping track of the changes to the world
;;;; *DIRTY-CHUNKS* contains chunks that need to be remeshed or have their
;;;; meshes removed.
(progn
  (defparameter *dirty-chunks* (queue:make-uniq-q))
  (defun clean-dirty ()
    (setf *dirty-chunks* (queue:make-uniq-q)))
  (defun dirty-pop ()
    (queue:uniq-pop *dirty-chunks*))
  (defun dirty-push (item)
    (queue:uniq-push item *dirty-chunks*))
  (defun block-dirtify (i j k)
    (let ((imask (mod i 16))
	  (jmask (mod j 16))
	  (kmask (mod k 16)))
      (labels ((add (x y z)
		 (let ((chunk (voxel-chunks::obtain-chunk-from-block-coordinates x y z)))
		   (unless nil;;(voxel-chunks::empty-chunk-p chunk)
		     (dirty-push (voxel-chunks::chunk-key chunk)))))
	       (i-permute ()
		 (case imask
		   (0 (j-permute (1- i)))
		   (15 (j-permute (1+ i))))
		 (j-permute i))
	       (j-permute (ix)
		 (case jmask
		   (0 (k-permute ix (1- j)))
		   (15 (k-permute ix (1+ j))))
		 (k-permute ix j))
	       (k-permute (ix jx)
		 (case kmask
		   (0 (add ix jx (1- k)))
		   (15 (add ix jx (1+ k))))
		 (add ix jx k)))
	(i-permute)))))

#+nil
(defun block-dirtify-hashed (xyz)
  (multiple-value-bind (x y z) (world:unhashfunc xyz)
    (block-dirtify x y z)))


#+nil
(defun setblock-with-update (i j k blockid &optional
					     (new-light-value (block-data:data blockid :light)))
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
				       (new-light-value (block-data:data blockid :light))
				       (new-sky-light-value
					(if (eq blockid (block-data:lookup :void))
					    15
					    0)))
  (when (setf (world:getblock i j k) blockid)
    (setf (world:getlight i j k) new-light-value)
    (setf (world:skygetlight i j k) new-sky-light-value)
    (block-dirtify i j k)))

;;;;chunk loading

;;[FIXME]architecture::one center, the player, and the chunk array centers around it
(defparameter *chunk-coordinate-center-x* 0)
(defparameter *chunk-coordinate-center-y* 0)
(defparameter *chunk-coordinate-center-z* 0)
(defun set-chunk-coordinate-center (player-x player-y player-z)
  (multiple-value-bind (chunk-x chunk-y chunk-z)
      (voxel-chunks::chunk-coordinates-from-block-coordinates
       (floor player-x)
       (floor player-y)
       (floor player-z))
    (setf *chunk-coordinate-center-x* chunk-x)
    (setf *chunk-coordinate-center-y* chunk-y)
    (setf *chunk-coordinate-center-z* chunk-z)))

(defparameter *reposition-chunk-array-threshold* 2)
(defun maybe-move-chunk-array ()
  ;;center the chunk array around the player, but don't always, only if above a certain
  ;;threshold
  ;;[FIXME]is this expensive to recompute every frame or does it matter?
  ;;maybe put it in the chunk array object?
  ;;return t if it was moved, nil otherwise
  (let ((half-x-size (utility:etouq (floor voxel-chunks::*chunk-array-default-size-x* 2)))
	(half-y-size (utility:etouq (floor voxel-chunks::*chunk-array-default-size-y* 2)))
	(half-z-size (utility:etouq (floor voxel-chunks::*chunk-array-default-size-z* 2))))
    (let ((center-x (+ 
		     (voxel-chunks::chunk-array-x-min voxel-chunks::*chunk-array*)
		     half-x-size))
	  (center-y (+ 
		     (voxel-chunks::chunk-array-y-min voxel-chunks::*chunk-array*)
		     half-y-size))
	  (center-z (+ 
		     (voxel-chunks::chunk-array-z-min voxel-chunks::*chunk-array*)
		     half-z-size)))
      ;;[FIXME]hard-coded threshold for repositioning the chunk array? 4 chunks?
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
	  (voxel-chunks::reposition-chunk-array (- chunk-x half-x-size)
					 (- chunk-y half-y-size)
					 (- chunk-z half-z-size))
	  (values t))))))

(defun safe-subseq (seq end)
  (subseq seq 0 (min (length seq) end)))

(defparameter *chunk-radius* 6)
;;[FIXME]how to determine the maximum allowed chunks? leave some leeway for loading?
(defparameter *maximum-allowed-chunks* (* (expt (* (+ 1 *chunk-radius*) 2) 3)))
(defun chunk-memory-usage (&optional (chunks *maximum-allowed-chunks*))
  ;;in megabytes
  (/ (* 8 #|8 bytes per word in 64bit sbcl|# chunks
	(* voxel-chunks::*chunk-size-x* voxel-chunks::*chunk-size-y* voxel-chunks::*chunk-size-z*))
     1024 1024 1.0 #|1.0 for converting to float|#))
(defparameter *threshold* (* 8 8))
;;threshold so that when too many chunks exist, over compensate, so not unloading every time
(defun unsquared-chunk-distance (position-key)
  (let ((x0 *chunk-coordinate-center-x*)
	(y0 *chunk-coordinate-center-y*)
	(z0 *chunk-coordinate-center-z*))
    (voxel-chunks::with-chunk-key-coordinates
     (x1 y1 z1) position-key
     (let ((dx (- x1 x0))
	   (dy (- y1 y0))
	   (dz (- z1 z0)))
       ;;[FIXME]we don't need the sqrt for sorting
       (+ (* dx dx) (* dy dy) (* dz dz))))))
(defun blocky-chunk-distance (position-key)
  (let ((x0 *chunk-coordinate-center-x*)
	(y0 *chunk-coordinate-center-y*)
	(z0 *chunk-coordinate-center-z*))
    (voxel-chunks::with-chunk-key-coordinates
     (x1 y1 z1) position-key
     (let ((dx (- x1 x0))
	   (dy (- y1 y0))
	   (dz (- z1 z0)))
       (max (abs dx)
	    (abs dy)
	    (abs dz))))))
(defun get-unloadable-chunks ()
  ;;[FIXME]optimize?
  (let ((difference (- (- (voxel-chunks::total-loaded-chunks) *maximum-allowed-chunks*)
		       *threshold*)))
    (when (plusp difference)
      (let ((distance-sorted-chunks	     
	     (sort (alexandria:hash-table-keys voxel-chunks::*chunks*) #'> :key
		   'unsquared-chunk-distance)))
	(safe-subseq distance-sorted-chunks difference)))))

(defun load-chunks-around ()
  (mapc (lambda (key)
	  (world::chunk-load key))
	(get-chunks-to-load)))
(defun get-chunks-to-load ()
  (let ((x0 *chunk-coordinate-center-x*)
	(y0 *chunk-coordinate-center-y*)
	(z0 *chunk-coordinate-center-z*))
    (declare (optimize (speed 3) (safety 0))
	     (type voxel-chunks::chunk-coord x0 y0 z0))
    (let ((acc nil))
      (block out
	(let ((chunk-count 0))
	  (declare (type fixnum chunk-count))
	  (flet ((add-chunk (x y z)
		   (incf chunk-count)
		   ;;do something
		   (let ((key (voxel-chunks::create-chunk-key x y z)))
		     (when (space-for-new-chunk-p key)
		       ;;The chunk does not exist, therefore the *empty-chunk* was returned
		       (push key acc)
		       ;;(print (list x y z))
		       ))
		   (when (>
			  ;;[FIXME]nonportably assume chunk-count and maxium allowed chunks are fixnums
			  (the fixnum chunk-count)
			  (the fixnum *maximum-allowed-chunks*))
		     ;;exceeded the allowed chunks to load
		     (return-from out))
		   ))
	    (let ((size *chunk-radius*))
	      (declare (type voxel-chunks::chunk-coord size))
	      (utility:dobox ((chunk-x (the voxel-chunks::chunk-coord (- x0 size))
					(the voxel-chunks::chunk-coord (+ x0 size)))
			       (chunk-y (the voxel-chunks::chunk-coord (- y0 size))
					(the voxel-chunks::chunk-coord (+ y0 size)))
			       (chunk-z (the voxel-chunks::chunk-coord (- z0 size))
					(the voxel-chunks::chunk-coord (+ z0 size))))
			      (add-chunk chunk-x chunk-y chunk-z))))))
      acc)))

(defun load-world (&optional (force nil))
  (let ((maybe-moved (maybe-move-chunk-array)))
    (when (or force
	      maybe-moved)
      (load-chunks-around)
      (unload-extra-chunks))))

(defun unload-extra-chunks ()
  (let ((to-unload
	 ;;[FIXME]get a timer library? metering?
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

(defun chunk-unload (key &key (path (world:world-path)))
  (let ((chunk (voxel-chunks::obtain-chunk-from-chunk-key key nil)))
    (cond
      (chunk
       (world::chunk-save chunk :path path)
       (dirty-push key)
       ;;remove from the chunk-array
       (voxel-chunks::with-chunk-key-coordinates (x y z)
	   key
	 (voxel-chunks::remove-chunk-from-chunk-array x y z))
       ;;remove from the global table
       (voxel-chunks::remove-chunk-at key)
       t)
      (t nil))))

(defparameter *persist* t)
(defun chunk-save (chunk &key (path (world:world-path)))
  (when (not *persist*)
    (return-from chunk-save))
  (cond
    ((voxel-chunks::empty-chunk-p chunk)
     ;;when the chunk is obviously empty
     )
    (t
     ;;when the chunk is not obviously empty
     (when (voxel-chunks::chunk-modified chunk) ;;if it wasn't modified, no point in saving
       (let* ((worth-saving (voxel-chunks::chunk-worth-saving chunk))
	      (key (voxel-chunks::chunk-key chunk))
	      ;;[FIXME]have multiple unique-task hashes?
	      (job-key (cons :save-chunk key)))
	 ;;save the chunk first?
	 (sucle-mp:submit-unique-task
	  job-key
	  ((lambda ()
	     (cond
	       (worth-saving
		;;write the chunk to disk if its worth saving
		(world:savechunk chunk key path)
		;;(format t "~%saved chunk ~s" key)
		)
	       (t
		;;otherwise, if there is a preexisting file, destroy it
		(let ((chunk-save-file
		       ;;[FIXME]bad api?
		       (merge-pathnames
			(world:convert-object-to-filename (world:chunk-coordinate-to-filename key))
			(world:world-path))))
		  (let ((file-exists-p (probe-file chunk-save-file)))
		    (when file-exists-p
		      (delete-file chunk-save-file)))))))
	   :data job-key
	   :callback (lambda (job-task)
		       (declare (ignorable job-task))
		       (sucle-mp:remove-unique-task-key
			(sucle-mp:job-task-data job-task)))
	   ;;this task, saving and loading, must not be interrupted
	   :unkillable t)))))))

(defun dirty-push-around (key)
  ;;[FIXME]although this is correct, it
  ;;lags behind player movement?
  (voxel-chunks::with-chunk-key-coordinates
   (x y z) key
   (dobox ((x0 (1- x) (+ x 2))
	   (y0 (1- y) (+ y 2))
	   (z0 (1- z) (+ z 2)))
	  (let ((new-key (voxel-chunks::create-chunk-key x0 y0 z0)))
	    (when (voxel-chunks::chunk-exists-p new-key)
	      (dirty-push new-key))))))

(defparameter *load-jobs* 0)

(defun space-for-new-chunk-p (key)
  (voxel-chunks::empty-chunk-p (voxel-chunks::get-chunk-at key)))

(defun chunk-load (key &optional (path (world:world-path)))
  ;;[FIXME]using chunk-coordinate-to-filename before
  ;;running loadchunk is a bad api?
  #+nil
  (let ((load-type (loadchunk path (chunk-coordinate-to-filename key))))
    (unless (eq load-type :empty) ;;[FIXME]better api?
      (dirty-push-around key)))
  ;;(print 34243)
  (let ((job-key (cons :chunk-load key)))
    (sucle-mp:submit-unique-task
     job-key
     ((lambda ()
	(setf (cdr (sucle-mp:job-task-data
		    sucle-mp:*current-job-task*))
	      (cond ((not (space-for-new-chunk-p key))
		     ;;(format t "~%WTF? ~a chunk already exists" key)
		     :skipping)
		    (t (world:loadchunk key path)))))
      :data (cons job-key "")
      :callback (lambda (job-task)
		  (declare (ignorable job-task))
		  (let* ((job-key (car (sucle-mp:job-task-data job-task)))
			 (key (cdr job-key))
			 (chunk
			  (cdr (sucle-mp:job-task-data job-task))))
		    ;;[FIXME]? locking is not necessary if the callback runs in the
		    ;;same thread as the code which changes the chunk-array and *chunks* ?
		    (cond
		      ((eq chunk :skipping)
		       ;;(format t "~%chunk skipped loading, ")
		       )
		      (t
		       (cond
			 ((and (not (voxel-chunks::empty-chunk-p chunk)) (not (space-for-new-chunk-p key)))
			  (format t "~%OMG? ~a chunk already exists" key))
			 (t 
			  (progn
			    (apply #'voxel-chunks::remove-chunk-from-chunk-array key)
			    (voxel-chunks::set-chunk-at key chunk))
			  ;;(format t "~%making chunk ~a" key)

			  ;;(voxel-chunks::set-chunk-at key new-chunk)

			  (cond
			    ((voxel-chunks::empty-chunk-p chunk)
			     ;;(background-generation key)
			     )
			    (t (dirty-push-around key))))))))
		  (sucle-mp:remove-unique-task-key job-key)
		  (decf *load-jobs*)))
     (incf *load-jobs*))))

;;[FIXME]thread-safety for:
;;voxel-chunks::*chunks*
;;voxel-chunks::*chunk-array*
;;world::*dirty-chunks*
;;world::*achannel*

   ;;#:msave
   ;;#:save-world

(defun msave (&optional (path world:*world-directory*))
  (let ((newpath (world:world-path path)))
    (ensure-directories-exist newpath)
    (save-world newpath)))
(defun save-world (&optional (path (world:world-path)))
  (loop :for chunk :being :the :hash-values :of  voxel-chunks::*chunks* :do
     (chunk-save chunk :path path)))

#+nil
(defun mload (&optional (path *world-directory*))
  (let ((newpath (world-path path)))
    (load-world newpath)))
;;;;<CHANGE-WORLD?>
;;;;************************************************************************;;;;
