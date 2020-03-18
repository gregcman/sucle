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
  (voxel-chunks:reset-empty-chunk-value (blockify 0 0 15)))

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
    (time (dotimes (x times) (setf (getobj 0 0 0) 0))))
  (let ((times (expt 10 6)))
    (time (dotimes (x times) (getobj 0 0 0)))
    (time (dotimes (x times) (getobj 0 0 0)))))
;;;;************************************************************************;;;;
;;;;<PERSIST-WORLD>
(in-package #:world)


;;CRUD implementation for map from lisp_obj -> lisp_obj
;;create, read, update, delete
;;- slqlite database
;;- pile of files
(defclass crud () ((path :initarg :path :accessor path)))
(defparameter *implementation* nil)
(defgeneric crud_create (lisp-object data crud))
(defgeneric crud_read (lisp-object crud))
(defgeneric crud_update (lisp-object data crud))
(defgeneric crud_delete (lisp-object crud))
(defun crud-create (lisp-object data)
  (crud_create lisp-object data *implementation*))
(defun crud-read (lisp-object)
  (crud_read lisp-object *implementation*))
(defun crud-update (lisp-object data)
  (crud_update lisp-object data *implementation*))
(defun crud-delete (lisp-object)
  (crud_delete lisp-object *implementation*))

;;world loading code below?
(defun convert-object-to-filename (&optional (obj '(0 1 2 'obj :cl)))
  (with-output-to-string (str)
    ;;FIXME::what about circular data structures?
    (sucle-serialize::safer-write obj str)))

(defclass crud-sqlite (crud) ())
(defmethod crud_create (name data (impl crud-sqlite))
  (let ((database::*database* (path impl)))
    (crud_create_sqlite name data)))
(defmethod crud_read (name (impl crud-sqlite))
  (let ((database::*database* (path impl)))
    (crud_read_sqlite name)))
(defmethod crud_update (name data (impl crud-sqlite))
  (let ((database::*database* (path impl)))
    (crud_update_sqlite name data)))
(defmethod crud_delete (name (impl crud-sqlite))
  (let ((database::*database* (path impl)))
    (crud_delete_sqlite name)))
;;;;SQLITE + sucle-serialize
(defun crud_create_sqlite (lisp-object data)
  ;;FIXME:update creates a row regardless, so update
  ;;is the real create.
  (crud_update_sqlite lisp-object data))
(defun crud_read_sqlite (lisp-object)
  (let* ((file-name (convert-object-to-filename lisp-object))
	 (stuff
	  (database::with-open-database2
	    (database::retreive file-name))))
    (when stuff
      (sucle-serialize::decode-zlib-conspack-payload stuff))))
(defun crud_update_sqlite (lisp-object data)
  (database::with-open-database2
    (database::add
     (convert-object-to-filename lisp-object)
     (sucle-serialize::encode-zlib-conspack-payload data))))
(defun crud_delete_sqlite (lisp-object)
  (database::with-open-database2
    (database::delete-entry (convert-object-to-filename lisp-object))))
;;;;sucle-serialize
(defclass crud-file-pile (crud) ())
(defparameter *path* nil)
(defmethod crud_create (name data (impl crud-file-pile))
  (let ((*path* (path impl)))
    (crud_create_file-pile name data)))
(defmethod crud_read (name (impl crud-file-pile))
  (let ((*path* (path impl)))
    (crud_read_file-pile name)))
(defmethod crud_update (name data (impl crud-file-pile))
  (let ((*path* (path impl)))
    (crud_update_file-pile name data)))
(defmethod crud_delete (name (impl crud-file-pile))
  (let ((*path* (path impl)))
    (crud_delete_file-pile name)))
;;[FIXME]:can possibly create filenames that are illegal.
;;use base64 instead? how to mix the two?
;;
(defun string-base64 (string)
  (cl-base64:string-to-base64-string string :uri t))
(defun base64-string (string)
  (cl-base64:base64-string-to-string string :uri t))
(defun commentify (&optional (string "test"))
  (concatenate 'string ";" string))
(defun un-commentify (&optional (string ";test"))
  (subseq string 1 (length string)))
(defparameter *junk* '(4/5 "foo/bar.../baz" 3))
;;FIXME: 'check-' functions get highlighted red in emacs
(defun %check-safe-string
    (lispobj
     &optional
       (directory "")
       &aux
       (pathname (convert-object-to-filename lispobj))
       (path (merge-pathnames pathname directory))
       (base64-path (merge-pathnames (commentify (string-base64 pathname))
				     directory)))
  ;;FIXME::This probably won't work on windows.
  ;;https://stackoverflow.com/questions/4814040/allowed-characters-in-filename
  
  ;;Check that they both point to files in same directory
  ;;If they are, then it is safe
  (if (equal (pathname-directory base64-path)
	     (pathname-directory path))
      (values path base64-path)
      (values nil base64-path)))

(defun crud_create_file-pile (lisp-object data)
  (crud_update_file-pile lisp-object data))
(defun crud_read_file-pile (lisp-object &aux (path *path*))
  (multiple-value-bind (file-path base64) (%check-safe-string lisp-object path)
    ;;if both are valid, prefer the base64 one.
    ;;if only base64 is valid, just use that.  
    (sucle-serialize:load
     (cond (file-path
	    (if (probe-file base64)
		base64
		file-path))
	   (t base64)))))
(defun crud_update_file-pile (lisp-object data &aux (path *path*))
  (ensure-directories-exist
   (uiop:pathname-directory-pathname path))
  ;;Don't use the old format, just the base64 one.
  (multiple-value-bind (file-path base64) (%check-safe-string lisp-object path)
    (declare (ignore file-path))
    (sucle-serialize:save base64 data)))
(defun crud_delete_file-pile (lisp-object &aux (path *path*))
  ;;Delete the base64 version, and the regular version, if they exist.
  ;;FIXME:reads from the disk, slow? FIXME: use OPTIMIZE rather than FIXME?
  (multiple-value-bind (file-path base64) (%check-safe-string lisp-object path)
    (when (and file-path (probe-file file-path))
      (delete-file file-path))
    (when (probe-file base64)
      (delete-file base64))))

#+nil
"U28/PHA+VGhpcyA0LCA1LCA2LCA3LCA4LCA5LCB6LCB7LCB8LCB9IHRlc3RzIEJhc2U2NCBlbmNv
ZGVyLiBTaG93IG1lOiBALCBBLCBCLCBDLCBELCBFLCBGLCBHLCBILCBJLCBKLCBLLCBMLCBNLCBO
LCBPLCBQLCBRLCBSLCBTLCBULCBVLCBWLCBXLCBYLCBZLCBaLCBbLCBcLCBdLCBeLCBfLCBgLCBh
LCBiLCBjLCBkLCBlLCBmLCBnLCBoLCBpLCBqLCBrLCBsLCBtLCBuLCBvLCBwLCBxLCByLCBzLg=="

;;;
(defun detect-crud-path (path)
  (if (uiop:file-pathname-p path)
      ;;A sqlite database is a singular file.
      'crud-sqlite
      'crud-file-pile))
(defun make-crud-from-path (path)
  (make-instance (detect-crud-path path) :path path))
(defun use-crud-from-path (path)
  (setf *implementation* (make-crud-from-path path)))
;;;;************************************************************************;;;;

;;[FIXME]move generic loading and saving with printer and conspack to a separate file?
;;And have chunk loading in another file?

(defun savechunk (chunk position)
  (crud-update (chunk-coordinate-to-filename position)
	       (voxel-chunks:chunk-data chunk)))

(defun loadchunk (chunk-coordinates)
  (let* ((data (crud-read (chunk-coordinate-to-filename chunk-coordinates))))
    (flet ((make-data (data)
	     (let ((chunk-data (coerce data '(simple-array t (*)))))
	       (assert (= 4096 (length chunk-data))
		       nil
		       "offending chunk ~a"
		       chunk-coordinates)
	       (voxel-chunks:make-chunk-from-key-and-data chunk-coordinates data))))
      (typecase data
	(list
	 (ecase (length data)
	   (0
	    ;;if data is nil, just load an empty chunk
	    (voxel-chunks:with-chunk-key-coordinates (x y z) chunk-coordinates
						     (voxel-chunks:create-chunk x y z :type :empty)))

	   (3 ;;[FIXME]does this even work?
	    (destructuring-bind (blocks light sky) data
	      (let ((len (length blocks)))
		(let ((new (make-array len)))
		  (dotimes (i len)
		    (setf (aref new i)
			  (blockify (aref blocks i)  (aref light i) (aref sky i))))
		  (voxel-chunks:make-chunk-from-key-and-data chunk-coordinates new)))))
	   (1 (make-data (car data)))))
	(otherwise (make-data data))))))

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
		 (let ((chunk (voxel-chunks:obtain-chunk-from-block-coordinates x y z)))
		   (unless nil;;(voxel-chunks:empty-chunk-p chunk)
		     (dirty-push (voxel-chunks:chunk-key chunk)))))
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
  (multiple-value-bind (x y z) (unhashfunc xyz)
    (block-dirtify x y z)))


#+nil
(defun setblock-with-update (i j k blockid &optional
					     (new-light-value (block-data:data blockid :light)))
 (let ((old-light-value (getlight i j k)))
   (when (setf (getblock i j k) blockid)
     #+nil
      (when (< new-light-value old-light-value)
	(de-light-node i j k))
      (unless (= old-light-value new-light-value)
	(setf (getlight i j k) new-light-value))
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
  (when (setf (getblock i j k) blockid)
    (setf (getlight i j k) new-light-value)
    (setf (skygetlight i j k) new-sky-light-value)
    (block-dirtify i j k)))

;;;;chunk loading

;;[FIXME]architecture:one center, the player, and the chunk array centers around it
(defparameter *chunk-coordinate-center-x* 0)
(defparameter *chunk-coordinate-center-y* 0)
(defparameter *chunk-coordinate-center-z* 0)
(defun set-chunk-coordinate-center (player-x player-y player-z)
  (multiple-value-bind (chunk-x chunk-y chunk-z)
      (voxel-chunks:chunk-coordinates-from-block-coordinates
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
  (let ((half-x-size (utility:etouq (floor voxel-chunks:*chunk-array-default-size-x* 2)))
	(half-y-size (utility:etouq (floor voxel-chunks:*chunk-array-default-size-y* 2)))
	(half-z-size (utility:etouq (floor voxel-chunks:*chunk-array-default-size-z* 2))))
    (let ((center-x (+ 
		     (voxel-chunks:chunk-array-x-min voxel-chunks:*chunk-array*)
		     half-x-size))
	  (center-y (+ 
		     (voxel-chunks:chunk-array-y-min voxel-chunks:*chunk-array*)
		     half-y-size))
	  (center-z (+ 
		     (voxel-chunks:chunk-array-z-min voxel-chunks:*chunk-array*)
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
	  (voxel-chunks:reposition-chunk-array (- chunk-x half-x-size)
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
	(* voxel-chunks:*chunk-size-x* voxel-chunks:*chunk-size-y* voxel-chunks:*chunk-size-z*))
     1024 1024 1.0 #|1.0 for converting to float|#))
(defparameter *threshold* (* 8 8))
;;threshold so that when too many chunks exist, over compensate, so not unloading every time
(defun unsquared-chunk-distance (position-key)
  (let ((x0 *chunk-coordinate-center-x*)
	(y0 *chunk-coordinate-center-y*)
	(z0 *chunk-coordinate-center-z*))
    (voxel-chunks:with-chunk-key-coordinates
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
    (voxel-chunks:with-chunk-key-coordinates
     (x1 y1 z1) position-key
     (let ((dx (- x1 x0))
	   (dy (- y1 y0))
	   (dz (- z1 z0)))
       (max (abs dx)
	    (abs dy)
	    (abs dz))))))
(defun get-unloadable-chunks ()
  ;;[FIXME]optimize?
  (let ((difference (- (- (voxel-chunks:total-loaded-chunks) *maximum-allowed-chunks*)
		       *threshold*)))
    (when (plusp difference)
      (let ((distance-sorted-chunks	     
	     (sort (alexandria:hash-table-keys voxel-chunks:*chunks*) #'> :key
		   'unsquared-chunk-distance)))
	(safe-subseq distance-sorted-chunks difference)))))

(defun load-chunks-around ()
  (mapc (lambda (key)
	  (chunk-load key))
	(get-chunks-to-load)))
(defun get-chunks-to-load ()
  (let ((x0 *chunk-coordinate-center-x*)
	(y0 *chunk-coordinate-center-y*)
	(z0 *chunk-coordinate-center-z*))
    (declare (optimize (speed 3) (safety 0))
	     (type voxel-chunks:chunk-coord x0 y0 z0))
    (let ((acc nil))
      (block out
	(let ((chunk-count 0))
	  (declare (type fixnum chunk-count))
	  (flet ((add-chunk (x y z)
		   (incf chunk-count)
		   ;;do something
		   (let ((key (voxel-chunks:create-chunk-key x y z)))
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
	      (declare (type voxel-chunks:chunk-coord size))
	      (utility:dobox ((chunk-x (the voxel-chunks:chunk-coord (- x0 size))
					(the voxel-chunks:chunk-coord (+ x0 size)))
			       (chunk-y (the voxel-chunks:chunk-coord (- y0 size))
					(the voxel-chunks:chunk-coord (+ y0 size)))
			       (chunk-z (the voxel-chunks:chunk-coord (- z0 size))
					(the voxel-chunks:chunk-coord (+ z0 size))))
			      (add-chunk chunk-x chunk-y chunk-z))))))
      acc)))

(defun load-world (&optional (force nil))
  (let ((maybe-moved (maybe-move-chunk-array)))
    (when (or force
	      maybe-moved)
      (load-chunks-around)
      (unload-extra-chunks))))

(defun unload-extra-chunks ()
  (let (to-unload)
    ;;[FIXME]get a timer library? metering?
    (print "getting unloadable chunks")
    (setf to-unload (get-unloadable-chunks))
    ;;(print (length to-unload))
    (print "unloading the chunks")
    (dolist (chunk to-unload)
      (chunk-unload chunk))))

(defun chunk-unload (key)
  (let ((chunk (voxel-chunks:obtain-chunk-from-chunk-key key nil)))
    (cond
      (chunk
       (chunk-save chunk)
       (dirty-push key)
       ;;remove from the chunk-array
       (voxel-chunks:with-chunk-key-coordinates (x y z)
	   key
	 (voxel-chunks:remove-chunk-from-chunk-array x y z))
       ;;remove from the global table
       (voxel-chunks:remove-chunk-at key)
       t)
      (t nil))))

(defparameter *persist* t)
(defun chunk-save (chunk)
  (when (not *persist*)
    (return-from chunk-save))
  (cond
    ((voxel-chunks:empty-chunk-p chunk)
     ;;when the chunk is obviously empty
     )
    (t
     ;;when the chunk is not obviously empty
     (when (voxel-chunks:chunk-modified chunk) ;;if it wasn't modified, no point in saving
       (let* ((worth-saving (voxel-chunks:chunk-worth-saving chunk))
	      (key (voxel-chunks:chunk-key chunk))
	      ;;[FIXME]have multiple unique-task hashes?
	      (job-key (cons :save-chunk key)))
	 ;;save the chunk first?
	 (sucle-mp:submit-unique-task
	  job-key
	  ((lambda ()
	     (cond
	       (worth-saving
		;;write the chunk to disk if its worth saving
		(savechunk chunk key)
		;;(format t "~%saved chunk ~s" key)
		)
	       (t
		;;otherwise, if there is a preexisting file, destroy it
		(crud-delete (chunk-coordinate-to-filename key))
		)))
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
  (voxel-chunks:with-chunk-key-coordinates
   (x y z) key
   (dobox ((x0 (1- x) (+ x 2))
	   (y0 (1- y) (+ y 2))
	   (z0 (1- z) (+ z 2)))
	  (let ((new-key (voxel-chunks:create-chunk-key x0 y0 z0)))
	    (when (voxel-chunks:chunk-exists-p new-key)
	      (dirty-push new-key))))))

(defparameter *load-jobs* 0)

(defun space-for-new-chunk-p (key)
  (voxel-chunks:empty-chunk-p (voxel-chunks:get-chunk-at key)))

(defun chunk-load (key)
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
		    (t (loadchunk key)))))
      :data (cons job-key "")
      :callback (lambda (job-task)
		  (declare (ignorable job-task))
		  (cond
		    ((eq :complete (sucle-mp::job-task-status job-task))
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
			    ((and (not (voxel-chunks:empty-chunk-p chunk))
				  (not (space-for-new-chunk-p key)))
			     (format t "~%OMG? ~a chunk already exists" key))
			    (t 
			     (progn
			       (apply #'voxel-chunks:remove-chunk-from-chunk-array key)
			       (voxel-chunks:set-chunk-at key chunk))
			     ;;(format t "~%making chunk ~a" key)

			     ;;(voxel-chunks:set-chunk-at key new-chunk)

			     (cond
			       ((voxel-chunks:empty-chunk-p chunk)
				;;(background-generation key)
				)
			       (t (dirty-push-around key)))))))))
		    (t (print "job task bad")))
		  (sucle-mp:remove-unique-task-key job-key)
		  (decf *load-jobs*)))
     (incf *load-jobs*))))

;;[FIXME]thread-safety for:
;;voxel-chunks:*chunks*
;;voxel-chunks:*chunk-array*
;;*dirty-chunks*
;;*achannel*

;;#:msave
;;#:save-world

(defun msave ()
  (loop :for chunk :being :the :hash-values :of  voxel-chunks:*chunks* :do
     (chunk-save chunk)))

#+nil
(defun mload (&optional (path *world-directory*))
  (let ((newpath (world-path path)))
    (load-world newpath)))
;;;;<CHANGE-WORLD?>
;;;;************************************************************************;;;;
