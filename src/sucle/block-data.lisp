;;;;<BLOCK-DATA>

(in-package :block-data)

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
  (defblockprop *blockIndexInTexture* t) ;;needed
  (defblockprop *isCollidable* t))
(defparameter *names-to-blocks* (make-hash-table :test 'eq))
(defun define-block (id name texture-data light solid opaque?)
  (macrolet ((put (attribute-name array)
	       `(setf (aref ,array id) ,attribute-name)))
    (let ((name (keywordify (string-upcase name))))
      (put name *names*)
      (setf (gethash name *names-to-blocks*) id))
    (put (convert-to-packed texture-data) *blockindexintexture*)
    (put opaque? *opaquecubelooukup*)
    (put light *lightvalue*)
    (put solid *iscollidable*)))

(defun define-blocks (data)
  (map
   nil
   (lambda (args)
     (apply 'define-block args))
   data
   ))

(defun lookup (&optional (block-name :void))
  (gethash block-name *names-to-blocks* 0))
#+nil
(define-compiler-macro lookup (&whole form &optional (block-name :void))
  (multiple-value-bind (value existsp)
      (lookup block-name)
    (if existsp
	value
	form)))

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

(defmethod mesher:draw-dispatch ((bits-block-data fixnum) i j k)
  (let ((blockid (world:getblock-extract bits-block-data)))
    (case blockid
      (0
       ;;if its air, don't render anything
       )
      (otherwise
       (renderpacked blockid i j k)))))

;;;;block data below

;;;if the block is air, the side gets rendered. if the block is transparent
;;;and the same type ex: texture between glass - there is no texture - if they are
;;;different - water and glass -it shows
(defun show-sidep (blockid other-blockid)
  (or (zerop other-blockid)
      (and (/= blockid other-blockid)
	   (not (data other-blockid :opaque)))))

(defun grass-packed ()
  (let ((side `("side_grass" :flip t)))
    `(("dirt" :flip t)
      ("grass" :flip t)
      ,side ,side ,side ,side)))

;;An array of 6 values representing the texture and whether or not
;;the texture should be flipped.
;;[TODO] optimization
;;lowest bit = flip or not.
;;other bits = index into spritesheet.
(defun pack (n &key (flip nil))
  (logior (ash n 1) (if flip 1 0)))
(defun unpack (packed)
  (values (ash packed -1)
	  (oddp packed)))

(defparameter *pack-test*
  '(("grass" :flip t)
    ("grass")
    ("stone")
    ("grass")
    ("sand")
    ("grass" :flip t)))

(defun convert-to-packed (&optional (spec *pack-test*))
  (map 'vector
       (lambda (data)
	 (pack
	  (spritepacker::num (car data))
	  :flip (getf (cdr data) :flip)))
       spec))

(defun standard-packed (&optional (name "grass"))
  (let ((face `(,name :flip t)))
    (make-list 6 :initial-element face)))

(defun texture-for-particle (id)
  (let ((num (load-time-value (list 0))))
    (unpack (aref (data id :texture) (mod (incf (car num)) 6)))))
(defmacro flip-uv ()
  `(rotatef v0 v1))
(defun renderpacked (id i j k)
  (let* ((packed (data id :texture)))
    (macrolet ((side (index get fun)
		 `(let ((adj-id ,get))
		    (when (show-sidep id adj-id)
		      (multiple-value-bind (num flip-p) (unpack (aref packed ,index))
			(multiple-value-bind (u0 v0 u1 v1) (spritepacker::uvs-for-id num)
			  (when flip-p
			    (flip-uv))
			  (,fun i j k u0 v0 u1 v1)))))))
      (side 0 (world:getblock i      (1- j) k)      mesher:side-j)
      (side 1 (world:getblock i      (1+ j) k)      mesher:side+j)
      (side 2 (world:getblock (1- i) j      k)      mesher:side-i)
      (side 3 (world:getblock (1+ i) j      k)      mesher:side+i)
      (side 4 (world:getblock i      j      (1- k)) mesher:side-k)
      (side 5 (world:getblock i      j      (1+ k)) mesher:side+k))))

;;;;</BLOCK-DATA>
;;;;************************************************************************;;;;


(defun fit-to-texture (id &optional (x (random 1.0)) (y (random 1.0)) (w 0.25) (h 0.25))
  (multiple-value-bind (u0 v0 u1 v1) (spritepacker::uvs-for-id id) 
    (values (alexandria:lerp x u0 u1)
	    (alexandria:lerp y v0 v1)
	    (alexandria:lerp (min 1.0 (+ w x)) u0 u1)
	    (alexandria:lerp (min 1.0 (+ h y)) v0 v1))))

;;[FIXME]: Called at load time.
(define-blocks
    `(   
      ;;earth
      (3 "dirt" ,(standard-packed "dirt") 0 T T)

      (12 "sand" ,(standard-packed "sand") 0 T T)
      (13 "gravel" ,(standard-packed "gravel") 0 T T)
      ;;(24 "sandstone" 208 0 T T)  
      (1 "stone" ,(standard-packed "stone") 0 T T)

      ;;air
      (0 "air" nil 0 NIL NIL)
      ;;wind??
      
      ;;life
      (2 "grass" ,(grass-packed) 0 T T)
      (18 "leaves" ,(standard-packed "leaves") 0 T t) 
      (17 "log" ,(standard-packed "log") 0 T T) 
      
      ;;artificial
      ;;make glass 7 light?
      ;;(20 "glass" 49 7 T NIL)
      (5 "planks" ,(standard-packed "planks") 0 T T) 
      
      ;;(89 "lamp" 105 15 T T)
      ))
