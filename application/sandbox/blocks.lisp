(defpackage #:mc-blocks
  (:use #:cl
	#:utility))

(in-package :mc-blocks)

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

(map
 nil
 (lambda (x)
   (apply
    (lambda (id name texture-index light solid opaque?)
      (macrolet ((put (attribute-name array)
		   `(setf (aref ,array id) ,attribute-name)))
	(put name *names*)
	(put texture-index *blockindexintexture*)
	(put opaque? *opaquecubelooukup*)
	(put light *lightvalue*)
	(put solid *iscollidable*)))
    x))
 '(   
   ;;earth
   (3 "soil" 2 0 T T)

   (12 "sand" 18 0 T T)
   (13 "gravel" 19 0 T T)
   (24 "cobble" 208 0 T T)  
   (1 "boulder" 1 0 T T)

   ;;air
   (0 "void" 0 0 NIL NIL)
   ;;wind??
   
   ;;life
   (2 "grass" 0 0 T T)
   (18 "foliage" 53 0 T t) 
   (17 "log" 20 0 T T) 
   
   ;;artificial
   (20 "glass" 49 0 T NIL)
   (5 "lumber" 4 0 T T) 
   
   (89 "lamp" 105 15 T T) 

   ))
