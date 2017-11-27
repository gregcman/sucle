(defpackage #:mc-blocks
  (:use #:cl
	#:fuktard))

(in-package :mc-blocks)

(etouq
 (let (a)
   (flet ((defblockprop (name type)
	    (push
	     `(,name
	       (make-array 256 ;;block types limit
			   ;;the number of different types of blocks in the world. its 8 bit
			   :element-type (quote ,type)
			   :initial-element (coerce 0 (quote ,type))))
	     a)))
     (defblockprop 'names t)
     (defblockprop 'opaquecubelooukup t) ;;needed
     (defblockprop 'lightOpacity 'fixnum) ;;needed
     (defblockprop 'canblockgrass t) ;;what is this?
     (defblockprop 'lightvalue '(unsigned-byte 4)) ;;needed
     (defblockprop 'blockIndexInTexture '(unsigned-byte 8)) ;;needed
     (defblockprop 'blockhardness 'float) ;;?
     (defblockprop 'blockResistance 'float);;?
     (defblockprop 'blockParticleGravity 'float) ;;what?
     (defblockprop 'slipperiness 'float) ;;needed
     (defblockprop 'getrendertype 'fixnum) ;;???
     (defblockprop 'renderasnormalblock t) ;;??
     (defblockprop 'isCollidable t))
   `
   (etouq
    (let ,a
      (flet ((defblock (name id texture-index opaque?
			     r-regular rendertype light opacity
			     ?what? solid resistance hardness
			     friction particle-gravity
			     maxz maxy maxx minz miny minx)
	       (declare (ignorable maxz maxy maxx minz miny minx))
	       (macrolet ((put (attribute-name array)
			    `(setf (aref ,array id) ,attribute-name)))
		 (put name names)
		 (put texture-index blockindexintexture)
		 (put opaque? opaquecubelooukup)
		 (put r-regular renderasnormalblock)
		 (put rendertype getrendertype)
		 (put light lightvalue)
		 (put opacity lightopacity)
		 (put ?what? canblockgrass)
		 (put solid iscollidable)
		 (put resistance blockresistance)
		 (put hardness blockhardness)
		 (put friction slipperiness)
		 (put particle-gravity blockparticlegravity)))) ;;needed
	
	(DEFBLOCK "air"
	    0 0 nil nil 69 0 0 NIL nil 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0)
	(DEFBLOCK "stone"
	    1 1 T T 0 0 255 NIL T 30.0 1.5 0.6 1.0 1.0 1.0 1.0 0.0 0.0 0.0)
	(DEFBLOCK "grass"
	    2 3 T T 0 0 255 NIL T 3.0 0.6 0.6 1.0 1.0 1.0 1.0 0.0 0.0 0.0)
	(DEFBLOCK "dirt"
	    3 2 T T 0 0 255 NIL T 2.5 0.5 0.6 1.0 1.0 1.0 1.0 0.0 0.0 0.0)
	(DEFBLOCK "stonebrick"
	    4 16 T T 0 0 255 NIL T 30.0 2.0 0.6 1.0 1.0 1.0 1.0 0.0 0.0 0.0)
	(DEFBLOCK "wood"
	    5 4 T T 0 0 255 NIL T 15.0 2.0 0.6 1.0 1.0 1.0 1.0 0.0 0.0 0.0)
	(DEFBLOCK "sapling"
	    6 15 NIL NIL 1 0 0 T NIL 0.0 0.0 0.6 1.0 0.9 0.8 0.9 0.01 0.0 0.01)
	(DEFBLOCK "bedrock"
	    7 17 T T 0 0 255 NIL T 1.8e7 -1.0 0.6 1.0 1.0 1.0 1.0 0.0 0.0 0.0)
	(DEFBLOCK "water"
	    8 205 NIL NIL 4 0 3 NIL NIL 500.0 100.0 0.6 1.0 1.0 1.0 1.0 0.0 0.0 0.0)
	(DEFBLOCK "water"
	    9 205 NIL NIL 4 0 3 NIL NIL 500.0 100.0 0.6 1.0 1.0 1.0 1.0 0.0 0.0 0.0)
	(DEFBLOCK "lava"
	    10 237 NIL NIL 4 15 255 NIL NIL 0.0 0.0 0.6 1.0 1.0 1.0 1.0 0.0 0.0 0.0)
	(DEFBLOCK "lava"
	    11 237 NIL NIL 4 15 255 NIL NIL 500.0 100.0 0.6 1.0 1.0 1.0 1.0 0.0 0.0 0.0)
	(DEFBLOCK "sand"
	    12 18 T T 0 0 255 NIL T 2.5 0.5 0.6 1.0 1.0 1.0 1.0 0.0 0.0 0.0)
	(DEFBLOCK "gravel"
	    13 19 T T 0 0 255 NIL T 3.0 0.6 0.6 1.0 1.0 1.0 1.0 0.0 0.0 0.0)
	(DEFBLOCK "oreGold"
	    14 32 T T 0 0 255 NIL T 15.0 3.0 0.6 1.0 1.0 1.0 1.0 0.0 0.0 0.0)
	(DEFBLOCK "oreIron"
	    15 33 T T 0 0 255 NIL T 15.0 3.0 0.6 1.0 1.0 1.0 1.0 0.0 0.0 0.0)
	(DEFBLOCK "oreCoal"
	    16 34 T T 0 0 255 NIL T 15.0 3.0 0.6 1.0 1.0 1.0 1.0 0.0 0.0 0.0)
	(DEFBLOCK "log"
	    17 20 T T 0 0 255 NIL T 10.0 2.0 0.6 1.0 1.0 1.0 1.0 0.0 0.0 0.0)
	(DEFBLOCK "leaves"
	    18 53 T T 0 0 1 NIL T 1.0 0.2 0.6 1.0 1.0 1.0 1.0 0.0 0.0 0.0)
	(DEFBLOCK "sponge"
	    19 48 T T 0 0 255 NIL T 3.0 0.6 0.6 1.0 1.0 1.0 1.0 0.0 0.0 0.0)
	(DEFBLOCK "glass"
	    20 49 NIL T 0 0 0 NIL T 1.5 0.3 0.6 1.0 1.0 1.0 1.0 0.0 0.0 0.0)
	(DEFBLOCK "oreLapis"
	    21 160 T T 0 0 255 NIL T 15.0 3.0 0.6 1.0 1.0 1.0 1.0 0.0 0.0 0.0)
	(DEFBLOCK "blockLapis"
	    22 144 T T 0 0 255 NIL T 15.0 3.0 0.6 1.0 1.0 1.0 1.0 0.0 0.0 0.0)
	(DEFBLOCK "dispenser"
	    23 45 T T 0 0 255 NIL T 17.5 3.5 0.6 1.0 1.0 1.0 1.0 0.0 0.0 0.0)
	(DEFBLOCK "sandStone"
	    24 192 T T 0 0 255 NIL T 4.0 0.8 0.6 1.0 1.0 1.0 1.0 0.0 0.0 0.0)
	(DEFBLOCK "musicBlock"
	    25 74 T T 0 0 255 NIL T 4.0 0.8 0.6 1.0 1.0 1.0 1.0 0.0 0.0 0.0)
	(DEFBLOCK "bed"
	    26 134 NIL NIL 14 0 0 NIL T 1.0 0.2 0.6 1.0 1.0 0.5625 1.0 0.0 0.0 0.0)
	(DEFBLOCK "goldenRail"
	    27 179 NIL NIL 9 0 0 T NIL 3.5 0.7 0.6 1.0 1.0 0.125 1.0 0.0 0.0 0.0)
	(DEFBLOCK "detectorRail"
	    28 195 NIL NIL 9 0 0 T NIL 3.5 0.7 0.6 1.0 1.0 0.125 1.0 0.0 0.0 0.0)
	(DEFBLOCK "pistonStickyBase"
	    29 106 NIL NIL 16 0 0 NIL T 2.5 0.5 0.6 1.0 1.0 1.0 1.0 0.0 0.0 0.0)
	(DEFBLOCK "web"
	    30 11 NIL NIL 1 0 1 NIL NIL 20.0 4.0 0.6 1.0 1.0 1.0 1.0 0.0 0.0 0.0)
	(DEFBLOCK "tallgrass"
	    31 39 NIL NIL 1 0 0 T NIL 0.0 0.0 0.6 1.0 0.9 0.8 0.9 0.01 0.0 0.01)
	(DEFBLOCK "deadbush"
	    32 55 NIL NIL 1 0 0 T NIL 0.0 0.0 0.6 1.0 0.9 0.8 0.9 0.01 0.0 0.01)
	(DEFBLOCK "pistonBase"
	    33 107 NIL NIL 16 0 0 NIL T 2.5 0.5 0.6 1.0 1.0 1.0 1.0 0.0 0.0 0.0)
	(DEFBLOCK "null"
	    34 107 NIL NIL 17 0 0 NIL T 2.5 0.5 0.6 1.0 1.0 1.0 1.0 0.0 0.0 0.0)
	(DEFBLOCK "cloth"
	    35 64 T T 0 0 255 NIL T 4.0 0.8 0.6 1.0 1.0 1.0 1.0 0.0 0.0 0.0)
	(DEFBLOCK "null"
	    36 0 NIL NIL -1 0 0 NIL T 0.0 -1.0 0.6 1.0 1.0 1.0 1.0 0.0 0.0 0.0)
	(DEFBLOCK "flower"
	    37 13 NIL NIL 1 0 0 T NIL 0.0 0.0 0.6 1.0 0.7 0.6 0.7 0.3 0.0 0.3)
	(DEFBLOCK "rose"
	    38 12 NIL NIL 1 0 0 T NIL 0.0 0.0 0.6 1.0 0.7 0.6 0.7 0.3 0.0 0.3)
	(DEFBLOCK "mushroomBrown"
	    39 29 NIL NIL 1 1 0 T NIL 0.0 0.0 0.6 1.0 0.7 0.4 0.7 0.3 0.0 0.3)
	(DEFBLOCK "mushroomRed"
	    40 28 NIL NIL 1 0 0 T NIL 0.0 0.0 0.6 1.0 0.7 0.4 0.7 0.3 0.0 0.3)
	(DEFBLOCK "blockGold"
	    41 23 T T 0 0 255 NIL T 30.0 3.0 0.6 1.0 1.0 1.0 1.0 0.0 0.0 0.0)
	(DEFBLOCK "blockIron"
	    42 22 T T 0 0 255 NIL T 30.0 5.0 0.6 1.0 1.0 1.0 1.0 0.0 0.0 0.0)
	(DEFBLOCK "stoneSlab"
	    43 6 T T 0 0 255 NIL T 30.0 2.0 0.6 1.0 1.0 1.0 1.0 0.0 0.0 0.0)
	(DEFBLOCK "stoneSlab"
	    44 6 NIL NIL 0 0 255 NIL T 30.0 2.0 0.6 1.0 1.0 0.5 1.0 0.0 0.0 0.0)
	(DEFBLOCK "brick"
	    45 7 T T 0 0 255 NIL T 30.0 2.0 0.6 1.0 1.0 1.0 1.0 0.0 0.0 0.0)
	(DEFBLOCK "tnt"
	    46 8 T T 0 0 255 NIL T 0.0 0.0 0.6 1.0 1.0 1.0 1.0 0.0 0.0 0.0)
	(DEFBLOCK "bookshelf"
	    47 35 T T 0 0 255 NIL T 7.5 1.5 0.6 1.0 1.0 1.0 1.0 0.0 0.0 0.0)
	(DEFBLOCK "stoneMoss"
	    48 36 T T 0 0 255 NIL T 30.0 2.0 0.6 1.0 1.0 1.0 1.0 0.0 0.0 0.0)
	(DEFBLOCK "obsidian"
	    49 37 T T 0 0 255 NIL T 6000.0 10.0 0.6 1.0 1.0 1.0 1.0 0.0 0.0 0.0)
	(DEFBLOCK "torch"
	    50 80 NIL NIL 2 14 0 T NIL 0.0 0.0 0.6 1.0 1.0 1.0 1.0 0.0 0.0 0.0)
	(DEFBLOCK "fire"
	    51 31 NIL NIL 3 15 0 T NIL 0.0 0.0 0.6 1.0 1.0 1.0 1.0 0.0 0.0 0.0)
	(DEFBLOCK "mobSpawner"
	    52 65 NIL T 0 0 0 NIL T 25.0 5.0 0.6 1.0 1.0 1.0 1.0 0.0 0.0 0.0)
	(DEFBLOCK "stairsWood"
	    53 4 NIL NIL 10 0 255 NIL T 15.0 2.0 0.6 1.0 1.0 1.0 1.0 0.0 0.0 0.0)
	(DEFBLOCK "chest"
	    54 26 T T 0 0 255 NIL T 12.5 2.5 0.6 1.0 1.0 1.0 1.0 0.0 0.0 0.0)
	(DEFBLOCK "redstoneDust"
	    55 164 NIL NIL 5 0 0 T NIL 0.0 0.0 0.6 1.0 1.0 0.0625 1.0 0.0 0.0 0.0)
	(DEFBLOCK "oreDiamond"
	    56 50 T T 0 0 255 NIL T 15.0 3.0 0.6 1.0 1.0 1.0 1.0 0.0 0.0 0.0)
	(DEFBLOCK "blockDiamond"
	    57 24 T T 0 0 255 NIL T 30.0 5.0 0.6 1.0 1.0 1.0 1.0 0.0 0.0 0.0)
	(DEFBLOCK "workbench"
	    58 59 T T 0 0 255 NIL T 12.5 2.5 0.6 1.0 1.0 1.0 1.0 0.0 0.0 0.0)
	(DEFBLOCK "crops"
	    59 88 NIL NIL 6 0 0 T NIL 0.0 0.0 0.6 1.0 1.0 0.25 1.0 0.0 0.0 0.0)
	(DEFBLOCK "farmland"
	    60 87 NIL NIL 0 0 255 NIL T 3.0 0.6 0.6 1.0 1.0 0.9375 1.0 0.0 0.0 0.0)
	(DEFBLOCK "furnace"
	    61 45 T T 0 0 255 NIL T 17.5 3.5 0.6 1.0 1.0 1.0 1.0 0.0 0.0 0.0)
	(DEFBLOCK "furnace"
	    62 45 T T 0 13 255 NIL T 17.5 3.5 0.6 1.0 1.0 1.0 1.0 0.0 0.0 0.0)
	(DEFBLOCK "sign"
	    63 4 NIL NIL -1 0 0 NIL NIL 5.0 1.0 0.6 1.0 0.75 1.0 0.75 0.25 0.0 0.25)
	(DEFBLOCK "doorWood"
	    64 97 NIL NIL 7 0 0 NIL T 15.0 3.0 0.6 1.0 1.0 1.0 1.0 0.0 0.0 0.0)
	(DEFBLOCK "ladder"
	    65 83 NIL NIL 8 0 0 T NIL 2.0 0.4 0.6 1.0 1.0 1.0 1.0 0.0 0.0 0.0)
	(DEFBLOCK "rail"
	    66 128 NIL NIL 9 0 0 T NIL 3.5 0.7 0.6 1.0 1.0 0.125 1.0 0.0 0.0 0.0)
	(DEFBLOCK "stairsStone"
	    67 16 NIL NIL 10 0 255 NIL T 30.0 2.0 0.6 1.0 1.0 1.0 1.0 0.0 0.0 0.0)
	(DEFBLOCK "sign"
	    68 4 NIL NIL -1 0 0 NIL NIL 5.0 1.0 0.6 1.0 0.75 1.0 0.75 0.25 0.0 0.25)
	(DEFBLOCK "lever"
	    69 96 NIL NIL 12 0 0 T NIL 2.5 0.5 0.6 1.0 1.0 1.0 1.0 0.0 0.0 0.0)
	(DEFBLOCK "pressurePlate"
	    70 1 NIL NIL 0 0 0 NIL NIL 2.5 0.5 0.6 1.0 0.9375 0.03125 0.9375 0.0625 0.0 0.0625)
	(DEFBLOCK "doorIron"
	    71 98 NIL NIL 7 0 0 NIL T 25.0 5.0 0.6 1.0 1.0 1.0 1.0 0.0 0.0 0.0)
	(DEFBLOCK "pressurePlate"
	    72 4 NIL NIL 0 0 0 NIL NIL 2.5 0.5 0.6 1.0 0.9375 0.03125 0.9375 0.0625 0.0 0.0625)
	(DEFBLOCK "oreRedstone"
	    73 51 T T 0 0 255 NIL T 15.0 3.0 0.6 1.0 1.0 1.0 1.0 0.0 0.0 0.0)
	(DEFBLOCK "oreRedstone"
	    74 51 T T 0 9 255 NIL T 15.0 3.0 0.6 1.0 1.0 1.0 1.0 0.0 0.0 0.0)
	(DEFBLOCK "notGate"
	    75 115 NIL NIL 2 0 0 T T 0.0 0.0 0.6 1.0 1.0 1.0 1.0 0.0 0.0 0.0)
	(DEFBLOCK "notGate"
	    76 99 NIL NIL 2 7 0 T T 0.0 0.0 0.6 1.0 1.0 1.0 1.0 0.0 0.0 0.0)
	(DEFBLOCK "button"
	    77 1 NIL NIL 0 0 0 T NIL 2.5 0.5 0.6 1.0 1.0 1.0 1.0 0.0 0.0 0.0)
	(DEFBLOCK "snow"
	    78 66 NIL NIL 0 0 0 T T 0.5 0.1 0.6 1.0 1.0 0.125 1.0 0.0 0.0 0.0)
	(DEFBLOCK "ice"
	    79 67 NIL T 0 0 3 NIL T 2.5 0.5 0.98 1.0 1.0 1.0 1.0 0.0 0.0 0.0)
	(DEFBLOCK "snow"
	    80 66 T T 0 0 255 NIL T 1.0 0.2 0.6 1.0 1.0 1.0 1.0 0.0 0.0 0.0)
	(DEFBLOCK "cactus"
	    81 70 NIL NIL 13 0 0 NIL T 2.0 0.4 0.6 1.0 1.0 1.0 1.0 0.0 0.0 0.0)
	(DEFBLOCK "clay"
	    82 72 T T 0 0 255 NIL T 3.0 0.6 0.6 1.0 1.0 1.0 1.0 0.0 0.0 0.0)
	(DEFBLOCK "reeds"
	    83 73 NIL NIL 1 0 0 T NIL 0.0 0.0 0.6 1.0 0.875 1.0 0.875 0.125 0.0 0.125)
	(DEFBLOCK "jukebox"
	    84 74 T T 0 0 255 NIL T 30.0 2.0 0.6 1.0 1.0 1.0 1.0 0.0 0.0 0.0)
	(DEFBLOCK "fence"
	    85 4 NIL NIL 11 0 0 NIL T 15.0 2.0 0.6 1.0 1.0 1.0 1.0 0.0 0.0 0.0)
	(DEFBLOCK "pumpkin"
	    86 102 T T 0 0 255 NIL T 5.0 1.0 0.6 1.0 1.0 1.0 1.0 0.0 0.0 0.0)
	(DEFBLOCK "hellrock"
	    87 103 T T 0 0 255 NIL T 2.0 0.4 0.6 1.0 1.0 1.0 1.0 0.0 0.0 0.0)
	(DEFBLOCK "hellsand"
	    88 104 T T 0 0 255 NIL T 2.5 0.5 0.6 1.0 1.0 1.0 1.0 0.0 0.0 0.0)
	(DEFBLOCK "lightgem"
	    89 105 T T 0 15 255 NIL T 1.5 0.3 0.6 1.0 1.0 1.0 1.0 0.0 0.0 0.0)
	(DEFBLOCK "portal"
	    90 14 NIL NIL 0 11 0 T NIL 0.0 -1.0 0.6 1.0 1.0 1.0 1.0 0.0 0.0 0.0)
	(DEFBLOCK "litpumpkin"
	    91 102 T T 0 15 255 NIL T 5.0 1.0 0.6 1.0 1.0 1.0 1.0 0.0 0.0 0.0)
	(DEFBLOCK "cake"    
	    92 121 NIL NIL 0 0 0 NIL T 2.5 0.5 0.6 1.0 1.0 1.0 1.0 0.0 0.0 0.0)
	(DEFBLOCK "diode"
	    93 6 NIL NIL 15 0 0 T T 0.0 0.0 0.6 1.0 1.0 0.125 1.0 0.0 0.0 0.0)
	(DEFBLOCK "diode"    
	    94 6 NIL NIL 15 9 0 T T 0.0 0.0 0.6 1.0 1.0 0.125 1.0 0.0 0.0 0.0)
	(DEFBLOCK "lockedchest"
	    95 26 T T 0 15 255 NIL T 0.0 0.0 0.6 1.0 1.0 1.0 1.0 0.0 0.0 0.0))

      ,(let (nombres
	     exports)
	    (map nil
		 (lambda (x)
		   (let* ((name (first x))
			  (newname ((lambda (x)
				      (let ((str (symbol-name x)))
					(intern (concatenate 'string "*" str "*")
						(symbol-package x))))
				    name)))
		     (push `(list (quote ,newname) ,name) nombres)
		     (push newname exports)))
		 a)
	    `(let ((values (list ,@nombres))
		   (exports (quote ,exports)))
	       `(progn ,@(mapcar (lambda (x) (cons 'defparameter x)) values)
		       (export (quote ,exports)))))))))

#|
'(DEFBLOCK STONE
"blockName""stone"
"blockID" 1
"blockIndexInTexture" 1
"isOpaqueCube" T

  "renderAsNormalBlock" T
  "renderType" 0
  "lightValue" 0

  "lightOpacity" 255
  "canBlockGrass" NIL
  "isCollidable" T 
  "blockResistance" 30.0
  "blockHardness" 1.5
  "slipperiness" 0.6
  "blockParticleGravity" 1.0
  "maxZ" 1.0
  "maxY" 1.0
  "maxX" 1.0
  "minZ" 0.0
  "minY" 0.0
  "minX" 0.0)
|#
