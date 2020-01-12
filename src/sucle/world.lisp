(defpackage #:world
  (:use :cl #:utility)
  (:export
   ;;;block accessors
   #:getblock #:setblock
   #:getlight #:setlight
   #:skygetlight #:skysetlight
   #:getblock-extract
   #:getlight-extract
   #:skygetlight-extract
   #:num-getobj

   #:blockify)
  (:export
   #:world-path
   #:savechunk
   #:loadchunk
   
   #:filename-to-chunk-coordinate
   #:chunk-coordinate-to-filename
   #:*some-saves*
   #:*world-directory*
   #:convert-object-to-filename))
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

;;FIXME::move this to a better place?
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
;;;;<BLOCK-DATA>
(defpackage #:block-data
  (:use #:cl
	#:utility))

(in-package :block-data)

(utility::eval-always
  (macrolet
      ((defblockprop (name type)
	 `(progn
	    (defparameter ,name
	      (make-array 256 ;;block types limit
			  ;;the number of different types of blocks in the world. its 8 bit
			  :element-type (quote ,type)
			  :initial-element (coerce 0 (quote ,type))))
	    (export ',name))))
    (defblockprop *names* t)
    (defblockprop *opaquecubelooukup* t) ;;needed
    (defblockprop *lightOpacity* fixnum) ;;needed
    (defblockprop *lightvalue* (unsigned-byte 4)) ;;needed
    (defblockprop *blockIndexInTexture* (unsigned-byte 8)) ;;needed
    (defblockprop *isCollidable* t))

  (defparameter *names-to-blocks* (make-hash-table :test 'eq))

  (map
   nil
   (lambda (x)
     (apply
      (lambda (id name texture-index light solid opaque?)
	(macrolet ((put (attribute-name array)
		     `(setf (aref ,array id) ,attribute-name)))
	  (let ((name (keywordify (string-upcase name))))
	    (put name *names*)
	    (setf (gethash name *names-to-blocks*) id))
	  (put texture-index *blockindexintexture*)
	  (put opaque? *opaquecubelooukup*)
	  (put light *lightvalue*)
	  (put solid *iscollidable*)))
      x))
   '(   
     ;;earth
     (3 "dirt" 2 0 T T)

     (12 "sand" 18 0 T T)
     (13 "gravel" 19 0 T T)
     (24 "sandstone" 208 0 T T)  
     (1 "stone" 1 0 T T)

     ;;air
     (0 "air" 0 0 NIL NIL)
     ;;wind??
     
     ;;life
     (2 "grass" 0 0 T T)
     (18 "leaves" 53 0 T t) 
     (17 "log" 20 0 T T) 
     
     ;;artificial
     ;;make glass 7 light?
     (20 "glass" 49 7 T NIL)
     (5 "planks" 4 0 T T) 
     
     (89 "lamp" 105 15 T T)))

  (defun blockid (&optional (block-name :void))
    (gethash block-name *names-to-blocks* 0))
  (define-compiler-macro blockid (&whole form &optional (block-name :void))
    (multiple-value-bind (value existsp)
	(blockid block-name)
      (if existsp
	  value
	  form))))
;;;;</BLOCK-DATA>
;;;;************************************************************************;;;;
;;;;<PERSIST-WORLD>
(in-package #:world)

;;FIXME::move generic loading and saving with printer and conspack to a separate file?
;;And have chunk loading in another file?

;;world loading code below?
(defun convert-object-to-filename (obj)
  (format nil "~s" obj))

(defparameter *some-saves* nil)
(defparameter *world-directory* nil)
(defun world-path (&optional (path *world-directory*) (base-dir *some-saves*))
  (utility:rebase-path path base-dir))

(defun savechunk (chunk position &optional (path (world-path)))
  ;;FIXME::undocumented swizzling and multiplication by 16, as well as loadchunk
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

      (3 ;;FIXME::does this even work?
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
	  ;;FIXME::assumes chunks are 16 by 16 by 16
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
  ;;FIXME::don't load the entire world
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
