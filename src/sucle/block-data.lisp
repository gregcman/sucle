;;;;<BLOCK-DATA>

(in-package :block-data)

(utility:eval-always
  (macrolet
      ((defblockprop (name type)
	 `(progn
	    (defparameter ,name
	      (make-array 256 ;;block types limit
			  ;;the number of different types of blocks in the world. its 8 bit
			  :element-type (quote ,type)
			  :initial-element (coerce 0 (quote ,type)))))))
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

  (defun lookup (&optional (block-name :void))
    (gethash block-name *names-to-blocks* 0))
  (define-compiler-macro lookup (&whole form &optional (block-name :void))
    (multiple-value-bind (value existsp)
	(lookup block-name)
      (if existsp
	  value
	  form))))

(defmacro data (id value)
  (let ((var '((:name *names*)
	       (:opaque *opaquecubelooukup*)
	       (:opacity *lightOpacity*)
	       (:light *lightvalue*)
	       (:texture *blockindexintexture*)
	       (:hard *isCollidable*))))
    (mapc
     (lambda (x)
       (destructuring-bind (value-name array-variable) x
	 (when (eq value value-name)
	   (return-from data `(aref ,array-variable ,id)))))
     var)
    (error "Wanted:~s ~%Got:~s" (mapcar 'first var) value)))
;;;;

;;[FIXME]is using CLOS to dispatch on the block a good way to organize?
;; is it efficient?
(defmethod mesher:draw-dispatch ((bits-block-data fixnum) i j k)
  (blockshape (world:getblock-extract bits-block-data) i j k))

;;;;block data below

#+nil
(with-declaim-inline (block-hash)
  (defun block-hash (i j k)
    (locally (declare (optimize (speed 3) (safety 0))
		      (type voxel-chunks::block-coord i j k))
      (let ((hash (mod (the voxel-chunks::block-coord (* 2654435761 (the voxel-chunks::block-coord (+ i j k))))
		       (ash 1 32))))
	(values (logtest hash #b0100)
		(logtest hash #b1000))))))
(defmacro flipuv (&optional (i 'i) (j 'j) (k 'k) (u1 'u1) (u0 'u0) (v1 'v1) (v0 'v0))
  (with-gensyms (u v)
    `(locally ;;(declare (inline block-hash))
       (multiple-value-bind (,u ,v) (values t nil) ;;(block-hash ,i ,j ,k)
	 (when ,u
	   (rotatef ,u1 ,u0))
	 (when ,v
	   (rotatef ,v1 ,v0))))))

;;;if the block is air, the side gets rendered. if the block is transparent
;;;and the same type ex: texture between glass - there is no texture - if they are
;;;different - water and glass -it shows
(defun show-sidep (blockid other-blockid)
  (or (zerop other-blockid)
      (and (/= blockid other-blockid)
	   (not (data other-blockid :opaque)))))

(defgeneric blockshape (blockid i j k))
(defmethod blockshape ((blockid (eql 0)) i j k)
  ;;if its air, don't render anything
  )
(defmethod blockshape ((blockid (eql 24)) i j k)
  (rendersandstone blockid i j k))
(defmethod blockshape ((blockid (eql 2)) i j k)
  (rendergrass blockid i j k))
(defmethod blockshape ((blockid (eql 17)) i j k)
  (renderstandardblock blockid i j k)
  ;;(renderlog blockid i j k)
  )
(defmethod blockshape ((blockid t) i j k)
  (renderstandardblock blockid i j k))
(defun renderstandardblock (id i j k)
  (let ((texid (data id :texture)))
    (mesher:with-texture-translator2 (u0 u1 v0 v1) texid
      (flipuv)
      (let ((adj-id (world:getblock i (1- j) k)))
	(when (show-sidep id adj-id)
	  (mesher:side-j i j k u0 v0 u1 v1)))
      (let ((adj-id (world:getblock i (1+ j) k)))
	(when (show-sidep id adj-id)
	  (mesher:side+j i j k u0 v0 u1 v1)))
      (let ((adj-id (world:getblock (1- i) j k)))
	(when (show-sidep id adj-id)
	  (mesher:side-i i j k u0 v0 u1 v1)))
      (let ((adj-id (world:getblock (1+ i) j k)))
	(when (show-sidep id adj-id)
	  (mesher:side+i i j k u0 v0 u1 v1)))    
      (let ((adj-id (world:getblock i j (1- k))))
	(when (show-sidep id adj-id)
	  (mesher:side-k i j k u0 v0 u1 v1)))
      (let ((adj-id (world:getblock i j (1+ k))))
	(when (show-sidep id adj-id)
	  (mesher:side+k i j k u0 v0 u1 v1))))))

(defun rendergrass (id i j k)
  (let ((texid (data id :texture)))
    (mesher:with-texture-translator2 (u0 u1 v0 v1) 2
      (flipuv)
      (let ((adj-id (world:getblock i (1- j) k)))
	(when (show-sidep id adj-id)
	  (mesher:side-j i j k u0 v0 u1 v1))))
    (mesher:with-texture-translator2 (u0 u1 v0 v1) texid
      (flipuv)
      (let ((adj-id (world:getblock i (1+ j) k)))
	(when (show-sidep id adj-id)
	  (mesher:side+j i j k u0 v0 u1 v1))))
    (mesher:with-texture-translator2 (u0 u1 v0 v1) 3
      (let ((adj-id (world:getblock (1- i) j k)))
	(when (show-sidep id adj-id)
	  (mesher:side-i i j k u0 v0 u1 v1)))
      (let ((adj-id (world:getblock (1+ i) j k)))
	(when (show-sidep id adj-id)
	  (mesher:side+i i j k u0 v0 u1 v1)))    
      (let ((adj-id (world:getblock i j (1- k))))
	(when (show-sidep id adj-id)
	  (mesher:side-k i j k u0 v0 u1 v1)))
      (let ((adj-id (world:getblock i j (1+ k))))
	(when (show-sidep id adj-id)
	  (mesher:side+k i j k u0 v0 u1 v1))))))

(defun rendersandstone (id i j k)
  (let ((texid (data id :texture)))
    (mesher:with-texture-translator2 (u0 u1 v0 v1) texid
      (let ((adj-id (world:getblock i (1- j) k)))
	(when (show-sidep id adj-id)
	  (mesher:side-j i j k u0 v0 u1 v1))))
    (mesher:with-texture-translator2 (u0 u1 v0 v1) (- 208 32)
      (let ((adj-id (world:getblock i (1+ j) k)))
	(when (show-sidep id adj-id)
	  (mesher:side+j i j k u0 v0 u1 v1))))
    (mesher:with-texture-translator2 (u0 u1 v0 v1) (- 208 16)
      (let ((adj-id (world:getblock (1- i) j k)))
	(when (show-sidep id adj-id)
	  (mesher:side-i i j k u0 v0 u1 v1)))
      (let ((adj-id (world:getblock (1+ i) j k)))
	(when (show-sidep id adj-id)
	  (mesher:side+i i j k u0 v0 u1 v1)))    
      (let ((adj-id (world:getblock i j (1- k))))
	(when (show-sidep id adj-id)
	  (mesher:side-k i j k u0 v0 u1 v1)))
      (let ((adj-id (world:getblock i j (1+ k))))
	(when (show-sidep id adj-id)
	  (mesher:side+k i j k u0 v0 u1 v1))))))
(defun renderlog (id i j k)
  (let ((texid (data id :texture)))
    (mesher:with-texture-translator2 (u0 u1 v0 v1) 21
      (let ((adj-id (world:getblock i (1- j) k)))
	(when (show-sidep id adj-id)
	  (mesher:side-j i j k u0 v0 u1 v1)))
      (let ((adj-id (world:getblock i (1+ j) k)))
	(when (show-sidep id adj-id)
	  (mesher:side+j i j k u0 v0 u1 v1))))
    (mesher:with-texture-translator2 (u0 u1 v0 v1) texid
      (let ((adj-id (world:getblock (1- i) j k)))
	(when (show-sidep id adj-id)
	  (mesher:side-i i j k u0 v0 u1 v1)))
      (let ((adj-id (world:getblock (1+ i) j k)))
	(when (show-sidep id adj-id)
	  (mesher:side+i i j k u0 v0 u1 v1)))    
      (let ((adj-id (world:getblock i j (1- k))))
	(when (show-sidep id adj-id)
	  (mesher:side-k i j k u0 v0 u1 v1)))
      (let ((adj-id (world:getblock i j (1+ k))))
	(when (show-sidep id adj-id)
	  (mesher:side+k i j k u0 v0 u1 v1))))))

;;;;</BLOCK-DATA>
;;;;************************************************************************;;;;
