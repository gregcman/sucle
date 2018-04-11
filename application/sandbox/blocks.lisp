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
   (3 "dirt" 2 0 T T)
   (13 "gravel" 19 0 T T)
   (82 "clay" 72 0 T T)

   (12 "sand" 18 0 T T)
   (24 "sandstone" 208 0 T T)

   (4 "cobblestone" 16 0 T T) 
   (1 "stone" 1 0 T T)
   
   (7 "bedrock" 17 0 T T)

   (10 "lava" 237 15 NIL NIL)

   ;;water   
   (8 "water" 205 0 NIL NIL)
   (78 "snow" 66 0 T NIL) 
   (79 "ice" 67 0 T NIL)

   ;;air
   (0 "air" 0 0 NIL NIL)
   ;;wind??
   
   ;;life
   (2 "grass" 0 0 T T)
   (18 "foliage" 53 0 T nil) 
   (17 "log" 20 0 T T) 
   
   ;;artificial
   (20 "glass" 49 0 T NIL)
   (45 "brick" 7 0 T T)
   (5 "plank" 4 0 T T) 
   
   (89 "lightbulb" 105 15 T T) 

   ))
