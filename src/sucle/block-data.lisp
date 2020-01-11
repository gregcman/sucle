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
