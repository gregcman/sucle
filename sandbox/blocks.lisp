(in-package :mc-blocks)

(eval-when (:compile-toplevel :load-toplevel :execute)

(defparameter allblocks (make-hash-table :test (function equal)))
(defun aconverter (meh)
  (let ((wow (copy-tree meh)))
    (setf (first wow) (intern (string-upcase (third meh))))
    (push 'defblock wow)
    wow)))
(defmacro defblockthemall ()
  (list* 'progn (mapcar #'aconverter raw)))

(defmacro defblock (name &rest args)
  `(setf (gethash (quote ,name) allblocks)
	 (let ((ourhash (make-hash-table :test (function equal))))
	   (labels ((defval (nombre val)
		      (setf (gethash nombre ourhash) val))
		    (putablock (dalist)
		      (if dalist
			  (progn
			    (defval (first dalist) (second dalist))
			    (putablock (cddr dalist))))))
	     (putablock (list ,@args)))
	   ourhash)))

;;(defblockthemall)
(DEFBLOCK STONE "blockName" "stone" "blockID" 1 "blockIndexInTexture" 1
	  "isOpaqueCube" T "renderAsNormalBlock" T "renderType" 0 "lightValue"
	  0 "lightOpacity" 255 "canBlockGrass" NIL "tickOnLoad" NIL
	  "nometanotify" NIL "isCollidable" T "blockResistance" 30.0
	  "blockHardness" 1.5 "slipperiness" 0.6 "blockMaterial"
	  "net.minecraft.src.Material@53e25b76" "blockParticleGravity" 1.0
	  "stepSound " "net.minecraft.src.StepSound@73a8dfcc" "maxZ" 1.0
	  "maxY" 1.0 "maxX" 1.0 "minZ" 0.0 "minY" 0.0 "minX" 0.0)
(DEFBLOCK GRASS "blockName" "grass" "blockID" 2 "blockIndexInTexture" 3
	  "isOpaqueCube" T "renderAsNormalBlock" T "renderType" 0 "lightValue"
	  0 "lightOpacity" 255 "canBlockGrass" NIL "tickOnLoad" T
	  "nometanotify" NIL "isCollidable" T "blockResistance" 3.0
	  "blockHardness" 0.6 "slipperiness" 0.6 "blockMaterial"
	  "net.minecraft.src.Material@ea30797" "blockParticleGravity" 1.0
	  "stepSound " "net.minecraft.src.StepSound@7e774085" "maxZ" 1.0
	  "maxY" 1.0 "maxX" 1.0 "minZ" 0.0 "minY" 0.0 "minX" 0.0)
(DEFBLOCK DIRT "blockName" "dirt" "blockID" 3 "blockIndexInTexture" 2
	  "isOpaqueCube" T "renderAsNormalBlock" T "renderType" 0 "lightValue"
	  0 "lightOpacity" 255 "canBlockGrass" NIL "tickOnLoad" NIL
	  "nometanotify" NIL "isCollidable" T "blockResistance" 2.5
	  "blockHardness" 0.5 "slipperiness" 0.6 "blockMaterial"
	  "net.minecraft.src.Material@3f8f9dd6" "blockParticleGravity" 1.0
	  "stepSound " "net.minecraft.src.StepSound@aec6354" "maxZ" 1.0 "maxY"
	  1.0 "maxX" 1.0 "minZ" 0.0 "minY" 0.0 "minX" 0.0)
(DEFBLOCK STONEBRICK "blockName" "stonebrick" "blockID" 4
	  "blockIndexInTexture" 16 "isOpaqueCube" T "renderAsNormalBlock" T
	  "renderType" 0 "lightValue" 0 "lightOpacity" 255 "canBlockGrass" NIL
	  "tickOnLoad" NIL "nometanotify" NIL "isCollidable" T
	  "blockResistance" 30.0 "blockHardness" 2.0 "slipperiness" 0.6
	  "blockMaterial" "net.minecraft.src.Material@53e25b76"
	  "blockParticleGravity" 1.0 "stepSound "
	  "net.minecraft.src.StepSound@73a8dfcc" "maxZ" 1.0 "maxY" 1.0 "maxX"
	  1.0 "minZ" 0.0 "minY" 0.0 "minX" 0.0)
(DEFBLOCK WOOD "blockName" "wood" "blockID" 5 "blockIndexInTexture" 4
	  "isOpaqueCube" T "renderAsNormalBlock" T "renderType" 0 "lightValue"
	  0 "lightOpacity" 255 "canBlockGrass" NIL "tickOnLoad" NIL
	  "nometanotify" T "isCollidable" T "blockResistance" 15.0
	  "blockHardness" 2.0 "slipperiness" 0.6 "blockMaterial"
	  "net.minecraft.src.Material@1c655221" "blockParticleGravity" 1.0
	  "stepSound " "net.minecraft.src.StepSound@58d25a40" "maxZ" 1.0
	  "maxY" 1.0 "maxX" 1.0 "minZ" 0.0 "minY" 0.0 "minX" 0.0)
(DEFBLOCK SAPLING "blockName" "sapling" "blockID" 6 "blockIndexInTexture" 15
	  "isOpaqueCube" NIL "renderAsNormalBlock" NIL "renderType" 1
	  "lightValue" 0 "lightOpacity" 0 "canBlockGrass" T "tickOnLoad" T
	  "nometanotify" T "isCollidable" NIL "blockResistance" 0.0
	  "blockHardness" 0.0 "slipperiness" 0.6 "blockMaterial"
	  "net.minecraft.src.MaterialLogic@1b701da1" "blockParticleGravity"
	  1.0 "stepSound " "net.minecraft.src.StepSound@7e774085" "maxZ" 0.9
	  "maxY" 0.8 "maxX" 0.9 "minZ" 0.099999994 "minY" 0.0 "minX"
	  0.099999994)
(DEFBLOCK BEDROCK "blockName" "bedrock" "blockID" 7 "blockIndexInTexture" 17
	  "isOpaqueCube" T "renderAsNormalBlock" T "renderType" 0 "lightValue"
	  0 "lightOpacity" 255 "canBlockGrass" NIL "tickOnLoad" NIL
	  "nometanotify" NIL "isCollidable" T "blockResistance" 1.8e7
	  "blockHardness" -1.0 "slipperiness" 0.6 "blockMaterial"
	  "net.minecraft.src.Material@53e25b76" "blockParticleGravity" 1.0
	  "stepSound " "net.minecraft.src.StepSound@73a8dfcc" "maxZ" 1.0
	  "maxY" 1.0 "maxX" 1.0 "minZ" 0.0 "minY" 0.0 "minX" 0.0)
(DEFBLOCK WATER "blockName" "water" "blockID" 8 "blockIndexInTexture" 205
	  "isOpaqueCube" NIL "renderAsNormalBlock" NIL "renderType" 4
	  "lightValue" 0 "lightOpacity" 3 "canBlockGrass" NIL "tickOnLoad" T
	  "nometanotify" T "isCollidable" NIL "blockResistance" 500.0
	  "blockHardness" 100.0 "slipperiness" 0.6 "blockMaterial"
	  "net.minecraft.src.MaterialLiquid@726f3b58" "blockParticleGravity"
	  1.0 "stepSound " "net.minecraft.src.StepSound@442d9b6e" "maxZ" 1.0
	  "maxY" 1.0 "maxX" 1.0 "minZ" 0.0 "minY" 0.0 "minX" 0.0)
(DEFBLOCK WATER "blockName" "water" "blockID" 9 "blockIndexInTexture" 205
	  "isOpaqueCube" NIL "renderAsNormalBlock" NIL "renderType" 4
	  "lightValue" 0 "lightOpacity" 3 "canBlockGrass" NIL "tickOnLoad" NIL
	  "nometanotify" T "isCollidable" NIL "blockResistance" 500.0
	  "blockHardness" 100.0 "slipperiness" 0.6 "blockMaterial"
	  "net.minecraft.src.MaterialLiquid@726f3b58" "blockParticleGravity"
	  1.0 "stepSound " "net.minecraft.src.StepSound@442d9b6e" "maxZ" 1.0
	  "maxY" 1.0 "maxX" 1.0 "minZ" 0.0 "minY" 0.0 "minX" 0.0)
(DEFBLOCK LAVA "blockName" "lava" "blockID" 10 "blockIndexInTexture" 237
	  "isOpaqueCube" NIL "renderAsNormalBlock" NIL "renderType" 4
	  "lightValue" 15 "lightOpacity" 255 "canBlockGrass" NIL "tickOnLoad"
	  T "nometanotify" T "isCollidable" NIL "blockResistance" 0.0
	  "blockHardness" 0.0 "slipperiness" 0.6 "blockMaterial"
	  "net.minecraft.src.MaterialLiquid@ee7d9f1" "blockParticleGravity"
	  1.0 "stepSound " "net.minecraft.src.StepSound@442d9b6e" "maxZ" 1.0
	  "maxY" 1.0 "maxX" 1.0 "minZ" 0.0 "minY" 0.0 "minX" 0.0)
(DEFBLOCK LAVA "blockName" "lava" "blockID" 11 "blockIndexInTexture" 237
	  "isOpaqueCube" NIL "renderAsNormalBlock" NIL "renderType" 4
	  "lightValue" 15 "lightOpacity" 255 "canBlockGrass" NIL "tickOnLoad"
	  T "nometanotify" T "isCollidable" NIL "blockResistance" 500.0
	  "blockHardness" 100.0 "slipperiness" 0.6 "blockMaterial"
	  "net.minecraft.src.MaterialLiquid@ee7d9f1" "blockParticleGravity"
	  1.0 "stepSound " "net.minecraft.src.StepSound@442d9b6e" "maxZ" 1.0
	  "maxY" 1.0 "maxX" 1.0 "minZ" 0.0 "minY" 0.0 "minX" 0.0)
(DEFBLOCK SAND "blockName" "sand" "blockID" 12 "blockIndexInTexture" 18
	  "isOpaqueCube" T "renderAsNormalBlock" T "renderType" 0 "lightValue"
	  0 "lightOpacity" 255 "canBlockGrass" NIL "tickOnLoad" NIL
	  "nometanotify" NIL "isCollidable" T "blockResistance" 2.5
	  "blockHardness" 0.5 "slipperiness" 0.6 "blockMaterial"
	  "net.minecraft.src.Material@15615099" "blockParticleGravity" 1.0
	  "stepSound " "net.minecraft.src.StepSoundSand@1edf1c96" "maxZ" 1.0
	  "maxY" 1.0 "maxX" 1.0 "minZ" 0.0 "minY" 0.0 "minX" 0.0)
(DEFBLOCK GRAVEL "blockName" "gravel" "blockID" 13 "blockIndexInTexture" 19
	  "isOpaqueCube" T "renderAsNormalBlock" T "renderType" 0 "lightValue"
	  0 "lightOpacity" 255 "canBlockGrass" NIL "tickOnLoad" NIL
	  "nometanotify" NIL "isCollidable" T "blockResistance" 3.0
	  "blockHardness" 0.6 "slipperiness" 0.6 "blockMaterial"
	  "net.minecraft.src.Material@15615099" "blockParticleGravity" 1.0
	  "stepSound " "net.minecraft.src.StepSound@aec6354" "maxZ" 1.0 "maxY"
	  1.0 "maxX" 1.0 "minZ" 0.0 "minY" 0.0 "minX" 0.0)
(DEFBLOCK OREGOLD "blockName" "oreGold" "blockID" 14 "blockIndexInTexture" 32
	  "isOpaqueCube" T "renderAsNormalBlock" T "renderType" 0 "lightValue"
	  0 "lightOpacity" 255 "canBlockGrass" NIL "tickOnLoad" NIL
	  "nometanotify" NIL "isCollidable" T "blockResistance" 15.0
	  "blockHardness" 3.0 "slipperiness" 0.6 "blockMaterial"
	  "net.minecraft.src.Material@53e25b76" "blockParticleGravity" 1.0
	  "stepSound " "net.minecraft.src.StepSound@73a8dfcc" "maxZ" 1.0
	  "maxY" 1.0 "maxX" 1.0 "minZ" 0.0 "minY" 0.0 "minX" 0.0)
(DEFBLOCK OREIRON "blockName" "oreIron" "blockID" 15 "blockIndexInTexture" 33
	  "isOpaqueCube" T "renderAsNormalBlock" T "renderType" 0 "lightValue"
	  0 "lightOpacity" 255 "canBlockGrass" NIL "tickOnLoad" NIL
	  "nometanotify" NIL "isCollidable" T "blockResistance" 15.0
	  "blockHardness" 3.0 "slipperiness" 0.6 "blockMaterial"
	  "net.minecraft.src.Material@53e25b76" "blockParticleGravity" 1.0
	  "stepSound " "net.minecraft.src.StepSound@73a8dfcc" "maxZ" 1.0
	  "maxY" 1.0 "maxX" 1.0 "minZ" 0.0 "minY" 0.0 "minX" 0.0)
(DEFBLOCK ORECOAL "blockName" "oreCoal" "blockID" 16 "blockIndexInTexture" 34
	  "isOpaqueCube" T "renderAsNormalBlock" T "renderType" 0 "lightValue"
	  0 "lightOpacity" 255 "canBlockGrass" NIL "tickOnLoad" NIL
	  "nometanotify" NIL "isCollidable" T "blockResistance" 15.0
	  "blockHardness" 3.0 "slipperiness" 0.6 "blockMaterial"
	  "net.minecraft.src.Material@53e25b76" "blockParticleGravity" 1.0
	  "stepSound " "net.minecraft.src.StepSound@73a8dfcc" "maxZ" 1.0
	  "maxY" 1.0 "maxX" 1.0 "minZ" 0.0 "minY" 0.0 "minX" 0.0)
(DEFBLOCK LOG "blockName" "log" "blockID" 17 "blockIndexInTexture" 20
	  "isOpaqueCube" T "renderAsNormalBlock" T "renderType" 0 "lightValue"
	  0 "lightOpacity" 255 "canBlockGrass" NIL "tickOnLoad" NIL
	  "nometanotify" T "isCollidable" T "blockResistance" 10.0
	  "blockHardness" 2.0 "slipperiness" 0.6 "blockMaterial"
	  "net.minecraft.src.Material@1c655221" "blockParticleGravity" 1.0
	  "stepSound " "net.minecraft.src.StepSound@58d25a40" "maxZ" 1.0
	  "maxY" 1.0 "maxX" 1.0 "minZ" 0.0 "minY" 0.0 "minX" 0.0)
(DEFBLOCK LEAVES "blockName" "leaves" "blockID" 18 "blockIndexInTexture" 53
	  "isOpaqueCube" T "renderAsNormalBlock" T "renderType" 0 "lightValue"
	  0 "lightOpacity" 1 "canBlockGrass" NIL "tickOnLoad" T "nometanotify"
	  T "isCollidable" T "blockResistance" 1.0 "blockHardness" 0.2
	  "slipperiness" 0.6 "blockMaterial"
	  "net.minecraft.src.Material@368102c8" "blockParticleGravity" 1.0
	  "stepSound " "net.minecraft.src.StepSound@7e774085" "maxZ" 1.0
	  "maxY" 1.0 "maxX" 1.0 "minZ" 0.0 "minY" 0.0 "minX" 0.0)
(DEFBLOCK SPONGE "blockName" "sponge" "blockID" 19 "blockIndexInTexture" 48
	  "isOpaqueCube" T "renderAsNormalBlock" T "renderType" 0 "lightValue"
	  0 "lightOpacity" 255 "canBlockGrass" NIL "tickOnLoad" NIL
	  "nometanotify" NIL "isCollidable" T "blockResistance" 3.0
	  "blockHardness" 0.6 "slipperiness" 0.6 "blockMaterial"
	  "net.minecraft.src.Material@6996db8" "blockParticleGravity" 1.0
	  "stepSound " "net.minecraft.src.StepSound@7e774085" "maxZ" 1.0
	  "maxY" 1.0 "maxX" 1.0 "minZ" 0.0 "minY" 0.0 "minX" 0.0)
(DEFBLOCK GLASS "blockName" "glass" "blockID" 20 "blockIndexInTexture" 49
	  "isOpaqueCube" NIL "renderAsNormalBlock" T "renderType" 0
	  "lightValue" 0 "lightOpacity" 0 "canBlockGrass" NIL "tickOnLoad" NIL
	  "nometanotify" NIL "isCollidable" T "blockResistance" 1.5
	  "blockHardness" 0.3 "slipperiness" 0.6 "blockMaterial"
	  "net.minecraft.src.Material@1963006a" "blockParticleGravity" 1.0
	  "stepSound " "net.minecraft.src.StepSoundStone@7fbe847c" "maxZ" 1.0
	  "maxY" 1.0 "maxX" 1.0 "minZ" 0.0 "minY" 0.0 "minX" 0.0)
(DEFBLOCK ORELAPIS "blockName" "oreLapis" "blockID" 21 "blockIndexInTexture"
	  160 "isOpaqueCube" T "renderAsNormalBlock" T "renderType" 0
	  "lightValue" 0 "lightOpacity" 255 "canBlockGrass" NIL "tickOnLoad"
	  NIL "nometanotify" NIL "isCollidable" T "blockResistance" 15.0
	  "blockHardness" 3.0 "slipperiness" 0.6 "blockMaterial"
	  "net.minecraft.src.Material@53e25b76" "blockParticleGravity" 1.0
	  "stepSound " "net.minecraft.src.StepSound@73a8dfcc" "maxZ" 1.0
	  "maxY" 1.0 "maxX" 1.0 "minZ" 0.0 "minY" 0.0 "minX" 0.0)
(DEFBLOCK BLOCKLAPIS "blockName" "blockLapis" "blockID" 22
	  "blockIndexInTexture" 144 "isOpaqueCube" T "renderAsNormalBlock" T
	  "renderType" 0 "lightValue" 0 "lightOpacity" 255 "canBlockGrass" NIL
	  "tickOnLoad" NIL "nometanotify" NIL "isCollidable" T
	  "blockResistance" 15.0 "blockHardness" 3.0 "slipperiness" 0.6
	  "blockMaterial" "net.minecraft.src.Material@53e25b76"
	  "blockParticleGravity" 1.0 "stepSound "
	  "net.minecraft.src.StepSound@73a8dfcc" "maxZ" 1.0 "maxY" 1.0 "maxX"
	  1.0 "minZ" 0.0 "minY" 0.0 "minX" 0.0)
(DEFBLOCK DISPENSER "blockName" "dispenser" "blockID" 23 "blockIndexInTexture"
	  45 "isOpaqueCube" T "renderAsNormalBlock" T "renderType" 0
	  "lightValue" 0 "lightOpacity" 255 "canBlockGrass" NIL "tickOnLoad"
	  NIL "nometanotify" T "isCollidable" T "blockResistance" 17.5
	  "blockHardness" 3.5 "slipperiness" 0.6 "blockMaterial"
	  "net.minecraft.src.Material@53e25b76" "blockParticleGravity" 1.0
	  "stepSound " "net.minecraft.src.StepSound@73a8dfcc" "maxZ" 1.0
	  "maxY" 1.0 "maxX" 1.0 "minZ" 0.0 "minY" 0.0 "minX" 0.0)
(DEFBLOCK SANDSTONE "blockName" "sandStone" "blockID" 24 "blockIndexInTexture"
	  192 "isOpaqueCube" T "renderAsNormalBlock" T "renderType" 0
	  "lightValue" 0 "lightOpacity" 255 "canBlockGrass" NIL "tickOnLoad"
	  NIL "nometanotify" NIL "isCollidable" T "blockResistance" 4.0
	  "blockHardness" 0.8 "slipperiness" 0.6 "blockMaterial"
	  "net.minecraft.src.Material@53e25b76" "blockParticleGravity" 1.0
	  "stepSound " "net.minecraft.src.StepSound@73a8dfcc" "maxZ" 1.0
	  "maxY" 1.0 "maxX" 1.0 "minZ" 0.0 "minY" 0.0 "minX" 0.0)
(DEFBLOCK MUSICBLOCK "blockName" "musicBlock" "blockID" 25
	  "blockIndexInTexture" 74 "isOpaqueCube" T "renderAsNormalBlock" T
	  "renderType" 0 "lightValue" 0 "lightOpacity" 255 "canBlockGrass" NIL
	  "tickOnLoad" NIL "nometanotify" T "isCollidable" T "blockResistance"
	  4.0 "blockHardness" 0.8 "slipperiness" 0.6 "blockMaterial"
	  "net.minecraft.src.Material@1c655221" "blockParticleGravity" 1.0
	  "stepSound " "net.minecraft.src.StepSound@442d9b6e" "maxZ" 1.0
	  "maxY" 1.0 "maxX" 1.0 "minZ" 0.0 "minY" 0.0 "minX" 0.0)
(DEFBLOCK BED "blockName" "bed" "blockID" 26 "blockIndexInTexture" 134
	  "isOpaqueCube" NIL "renderAsNormalBlock" NIL "renderType" 14
	  "lightValue" 0 "lightOpacity" 0 "canBlockGrass" NIL "tickOnLoad" NIL
	  "nometanotify" T "isCollidable" T "blockResistance" 1.0
	  "blockHardness" 0.2 "slipperiness" 0.6 "blockMaterial"
	  "net.minecraft.src.Material@41975e01" "blockParticleGravity" 1.0
	  "stepSound " "net.minecraft.src.StepSound@442d9b6e" "maxZ" 1.0
	  "maxY" 0.5625 "maxX" 1.0 "minZ" 0.0 "minY" 0.0 "minX" 0.0)
(DEFBLOCK GOLDENRAIL "blockName" "goldenRail" "blockID" 27
	  "blockIndexInTexture" 179 "isOpaqueCube" NIL "renderAsNormalBlock"
	  NIL "renderType" 9 "lightValue" 0 "lightOpacity" 0 "canBlockGrass" T
	  "tickOnLoad" NIL "nometanotify" T "isCollidable" NIL
	  "blockResistance" 3.5 "blockHardness" 0.7 "slipperiness" 0.6
	  "blockMaterial" "net.minecraft.src.MaterialLogic@c2e1f26"
	  "blockParticleGravity" 1.0 "stepSound "
	  "net.minecraft.src.StepSound@dcf3e99" "maxZ" 1.0 "maxY" 0.125 "maxX"
	  1.0 "minZ" 0.0 "minY" 0.0 "minX" 0.0)
(DEFBLOCK DETECTORRAIL "blockName" "detectorRail" "blockID" 28
	  "blockIndexInTexture" 195 "isOpaqueCube" NIL "renderAsNormalBlock"
	  NIL "renderType" 9 "lightValue" 0 "lightOpacity" 0 "canBlockGrass" T
	  "tickOnLoad" T "nometanotify" T "isCollidable" NIL "blockResistance"
	  3.5 "blockHardness" 0.7 "slipperiness" 0.6 "blockMaterial"
	  "net.minecraft.src.MaterialLogic@c2e1f26" "blockParticleGravity" 1.0
	  "stepSound " "net.minecraft.src.StepSound@dcf3e99" "maxZ" 1.0 "maxY"
	  0.125 "maxX" 1.0 "minZ" 0.0 "minY" 0.0 "minX" 0.0)
(DEFBLOCK PISTONSTICKYBASE "blockName" "pistonStickyBase" "blockID" 29
	  "blockIndexInTexture" 106 "isOpaqueCube" NIL "renderAsNormalBlock"
	  NIL "renderType" 16 "lightValue" 0 "lightOpacity" 0 "canBlockGrass"
	  NIL "tickOnLoad" NIL "nometanotify" T "isCollidable" T
	  "blockResistance" 2.5 "blockHardness" 0.5 "slipperiness" 0.6
	  "blockMaterial" "net.minecraft.src.Material@6d9c638"
	  "blockParticleGravity" 1.0 "stepSound "
	  "net.minecraft.src.StepSound@73a8dfcc" "maxZ" 1.0 "maxY" 1.0 "maxX"
	  1.0 "minZ" 0.0 "minY" 0.0 "minX" 0.0)
(DEFBLOCK WEB "blockName" "web" "blockID" 30 "blockIndexInTexture" 11
	  "isOpaqueCube" NIL "renderAsNormalBlock" NIL "renderType" 1
	  "lightValue" 0 "lightOpacity" 1 "canBlockGrass" NIL "tickOnLoad" NIL
	  "nometanotify" NIL "isCollidable" NIL "blockResistance" 20.0
	  "blockHardness" 4.0 "slipperiness" 0.6 "blockMaterial"
	  "net.minecraft.src.Material@7dc5e7b4" "blockParticleGravity" 1.0
	  "stepSound " "net.minecraft.src.StepSound@442d9b6e" "maxZ" 1.0
	  "maxY" 1.0 "maxX" 1.0 "minZ" 0.0 "minY" 0.0 "minX" 0.0)
(DEFBLOCK TALLGRASS "blockName" "tallgrass" "blockID" 31 "blockIndexInTexture"
	  39 "isOpaqueCube" NIL "renderAsNormalBlock" NIL "renderType" 1
	  "lightValue" 0 "lightOpacity" 0 "canBlockGrass" T "tickOnLoad" T
	  "nometanotify" NIL "isCollidable" NIL "blockResistance" 0.0
	  "blockHardness" 0.0 "slipperiness" 0.6 "blockMaterial"
	  "net.minecraft.src.MaterialLogic@1b701da1" "blockParticleGravity"
	  1.0 "stepSound " "net.minecraft.src.StepSound@7e774085" "maxZ" 0.9
	  "maxY" 0.8 "maxX" 0.9 "minZ" 0.099999994 "minY" 0.0 "minX"
	  0.099999994)
(DEFBLOCK DEADBUSH "blockName" "deadbush" "blockID" 32 "blockIndexInTexture"
	  55 "isOpaqueCube" NIL "renderAsNormalBlock" NIL "renderType" 1
	  "lightValue" 0 "lightOpacity" 0 "canBlockGrass" T "tickOnLoad" T
	  "nometanotify" NIL "isCollidable" NIL "blockResistance" 0.0
	  "blockHardness" 0.0 "slipperiness" 0.6 "blockMaterial"
	  "net.minecraft.src.MaterialLogic@1b701da1" "blockParticleGravity"
	  1.0 "stepSound " "net.minecraft.src.StepSound@7e774085" "maxZ" 0.9
	  "maxY" 0.8 "maxX" 0.9 "minZ" 0.099999994 "minY" 0.0 "minX"
	  0.099999994)
(DEFBLOCK PISTONBASE "blockName" "pistonBase" "blockID" 33
	  "blockIndexInTexture" 107 "isOpaqueCube" NIL "renderAsNormalBlock"
	  NIL "renderType" 16 "lightValue" 0 "lightOpacity" 0 "canBlockGrass"
	  NIL "tickOnLoad" NIL "nometanotify" T "isCollidable" T
	  "blockResistance" 2.5 "blockHardness" 0.5 "slipperiness" 0.6
	  "blockMaterial" "net.minecraft.src.Material@6d9c638"
	  "blockParticleGravity" 1.0 "stepSound "
	  "net.minecraft.src.StepSound@73a8dfcc" "maxZ" 1.0 "maxY" 1.0 "maxX"
	  1.0 "minZ" 0.0 "minY" 0.0 "minX" 0.0)
(DEFBLOCK NULL "blockName" "null" "blockID" 34 "blockIndexInTexture" 107
	  "isOpaqueCube" NIL "renderAsNormalBlock" NIL "renderType" 17
	  "lightValue" 0 "lightOpacity" 0 "canBlockGrass" NIL "tickOnLoad" NIL
	  "nometanotify" T "isCollidable" T "blockResistance" 2.5
	  "blockHardness" 0.5 "slipperiness" 0.6 "blockMaterial"
	  "net.minecraft.src.Material@6d9c638" "blockParticleGravity" 1.0
	  "stepSound " "net.minecraft.src.StepSound@73a8dfcc" "maxZ" 1.0
	  "maxY" 1.0 "maxX" 1.0 "minZ" 0.0 "minY" 0.0 "minX" 0.0)
(DEFBLOCK CLOTH "blockName" "cloth" "blockID" 35 "blockIndexInTexture" 64
	  "isOpaqueCube" T "renderAsNormalBlock" T "renderType" 0 "lightValue"
	  0 "lightOpacity" 255 "canBlockGrass" NIL "tickOnLoad" NIL
	  "nometanotify" T "isCollidable" T "blockResistance" 4.0
	  "blockHardness" 0.8 "slipperiness" 0.6 "blockMaterial"
	  "net.minecraft.src.Material@41975e01" "blockParticleGravity" 1.0
	  "stepSound " "net.minecraft.src.StepSound@1ee0005" "maxZ" 1.0 "maxY"
	  1.0 "maxX" 1.0 "minZ" 0.0 "minY" 0.0 "minX" 0.0)
(DEFBLOCK NULL "blockName" "null" "blockID" 36 "blockIndexInTexture" 0
	  "isOpaqueCube" NIL "renderAsNormalBlock" NIL "renderType" -1
	  "lightValue" 0 "lightOpacity" 0 "canBlockGrass" NIL "tickOnLoad" NIL
	  "nometanotify" NIL "isCollidable" T "blockResistance" 0.0
	  "blockHardness" -1.0 "slipperiness" 0.6 "blockMaterial"
	  "net.minecraft.src.Material@6d9c638" "blockParticleGravity" 1.0
	  "stepSound " "net.minecraft.src.StepSound@442d9b6e" "maxZ" 1.0
	  "maxY" 1.0 "maxX" 1.0 "minZ" 0.0 "minY" 0.0 "minX" 0.0)
(DEFBLOCK FLOWER "blockName" "flower" "blockID" 37 "blockIndexInTexture" 13
	  "isOpaqueCube" NIL "renderAsNormalBlock" NIL "renderType" 1
	  "lightValue" 0 "lightOpacity" 0 "canBlockGrass" T "tickOnLoad" T
	  "nometanotify" NIL "isCollidable" NIL "blockResistance" 0.0
	  "blockHardness" 0.0 "slipperiness" 0.6 "blockMaterial"
	  "net.minecraft.src.MaterialLogic@1b701da1" "blockParticleGravity"
	  1.0 "stepSound " "net.minecraft.src.StepSound@7e774085" "maxZ" 0.7
	  "maxY" 0.6 "maxX" 0.7 "minZ" 0.3 "minY" 0.0 "minX" 0.3)
(DEFBLOCK ROSE "blockName" "rose" "blockID" 38 "blockIndexInTexture" 12
	  "isOpaqueCube" NIL "renderAsNormalBlock" NIL "renderType" 1
	  "lightValue" 0 "lightOpacity" 0 "canBlockGrass" T "tickOnLoad" T
	  "nometanotify" NIL "isCollidable" NIL "blockResistance" 0.0
	  "blockHardness" 0.0 "slipperiness" 0.6 "blockMaterial"
	  "net.minecraft.src.MaterialLogic@1b701da1" "blockParticleGravity"
	  1.0 "stepSound " "net.minecraft.src.StepSound@7e774085" "maxZ" 0.7
	  "maxY" 0.6 "maxX" 0.7 "minZ" 0.3 "minY" 0.0 "minX" 0.3)
(DEFBLOCK MUSHROOMBROWN "blockName" "mushroomBrown" "blockID" 39
	  "blockIndexInTexture" 29 "isOpaqueCube" NIL "renderAsNormalBlock"
	  NIL "renderType" 1 "lightValue" 1 "lightOpacity" 0 "canBlockGrass" T
	  "tickOnLoad" T "nometanotify" NIL "isCollidable" NIL
	  "blockResistance" 0.0 "blockHardness" 0.0 "slipperiness" 0.6
	  "blockMaterial" "net.minecraft.src.MaterialLogic@1b701da1"
	  "blockParticleGravity" 1.0 "stepSound "
	  "net.minecraft.src.StepSound@7e774085" "maxZ" 0.7 "maxY" 0.4 "maxX"
	  0.7 "minZ" 0.3 "minY" 0.0 "minX" 0.3)
(DEFBLOCK MUSHROOMRED "blockName" "mushroomRed" "blockID" 40
	  "blockIndexInTexture" 28 "isOpaqueCube" NIL "renderAsNormalBlock"
	  NIL "renderType" 1 "lightValue" 0 "lightOpacity" 0 "canBlockGrass" T
	  "tickOnLoad" T "nometanotify" NIL "isCollidable" NIL
	  "blockResistance" 0.0 "blockHardness" 0.0 "slipperiness" 0.6
	  "blockMaterial" "net.minecraft.src.MaterialLogic@1b701da1"
	  "blockParticleGravity" 1.0 "stepSound "
	  "net.minecraft.src.StepSound@7e774085" "maxZ" 0.7 "maxY" 0.4 "maxX"
	  0.7 "minZ" 0.3 "minY" 0.0 "minX" 0.3)
(DEFBLOCK BLOCKGOLD "blockName" "blockGold" "blockID" 41 "blockIndexInTexture"
	  23 "isOpaqueCube" T "renderAsNormalBlock" T "renderType" 0
	  "lightValue" 0 "lightOpacity" 255 "canBlockGrass" NIL "tickOnLoad"
	  NIL "nometanotify" NIL "isCollidable" T "blockResistance" 30.0
	  "blockHardness" 3.0 "slipperiness" 0.6 "blockMaterial"
	  "net.minecraft.src.Material@75a1cd57" "blockParticleGravity" 1.0
	  "stepSound " "net.minecraft.src.StepSound@dcf3e99" "maxZ" 1.0 "maxY"
	  1.0 "maxX" 1.0 "minZ" 0.0 "minY" 0.0 "minX" 0.0)
(DEFBLOCK BLOCKIRON "blockName" "blockIron" "blockID" 42 "blockIndexInTexture"
	  22 "isOpaqueCube" T "renderAsNormalBlock" T "renderType" 0
	  "lightValue" 0 "lightOpacity" 255 "canBlockGrass" NIL "tickOnLoad"
	  NIL "nometanotify" NIL "isCollidable" T "blockResistance" 30.0
	  "blockHardness" 5.0 "slipperiness" 0.6 "blockMaterial"
	  "net.minecraft.src.Material@75a1cd57" "blockParticleGravity" 1.0
	  "stepSound " "net.minecraft.src.StepSound@dcf3e99" "maxZ" 1.0 "maxY"
	  1.0 "maxX" 1.0 "minZ" 0.0 "minY" 0.0 "minX" 0.0)
(DEFBLOCK STONESLAB "blockName" "stoneSlab" "blockID" 43 "blockIndexInTexture"
	  6 "isOpaqueCube" T "renderAsNormalBlock" T "renderType" 0
	  "lightValue" 0 "lightOpacity" 255 "canBlockGrass" NIL "tickOnLoad"
	  NIL "nometanotify" NIL "isCollidable" T "blockResistance" 30.0
	  "blockHardness" 2.0 "slipperiness" 0.6 "blockMaterial"
	  "net.minecraft.src.Material@53e25b76" "blockParticleGravity" 1.0
	  "stepSound " "net.minecraft.src.StepSound@73a8dfcc" "maxZ" 1.0
	  "maxY" 1.0 "maxX" 1.0 "minZ" 0.0 "minY" 0.0 "minX" 0.0)
(DEFBLOCK STONESLAB "blockName" "stoneSlab" "blockID" 44 "blockIndexInTexture"
	  6 "isOpaqueCube" NIL "renderAsNormalBlock" NIL "renderType" 0
	  "lightValue" 0 "lightOpacity" 255 "canBlockGrass" NIL "tickOnLoad"
	  NIL "nometanotify" NIL "isCollidable" T "blockResistance" 30.0
	  "blockHardness" 2.0 "slipperiness" 0.6 "blockMaterial"
	  "net.minecraft.src.Material@53e25b76" "blockParticleGravity" 1.0
	  "stepSound " "net.minecraft.src.StepSound@73a8dfcc" "maxZ" 1.0
	  "maxY" 0.5 "maxX" 1.0 "minZ" 0.0 "minY" 0.0 "minX" 0.0)
(DEFBLOCK BRICK "blockName" "brick" "blockID" 45 "blockIndexInTexture" 7
	  "isOpaqueCube" T "renderAsNormalBlock" T "renderType" 0 "lightValue"
	  0 "lightOpacity" 255 "canBlockGrass" NIL "tickOnLoad" NIL
	  "nometanotify" NIL "isCollidable" T "blockResistance" 30.0
	  "blockHardness" 2.0 "slipperiness" 0.6 "blockMaterial"
	  "net.minecraft.src.Material@53e25b76" "blockParticleGravity" 1.0
	  "stepSound " "net.minecraft.src.StepSound@73a8dfcc" "maxZ" 1.0
	  "maxY" 1.0 "maxX" 1.0 "minZ" 0.0 "minY" 0.0 "minX" 0.0)
(DEFBLOCK TNT "blockName" "tnt" "blockID" 46 "blockIndexInTexture" 8
	  "isOpaqueCube" T "renderAsNormalBlock" T "renderType" 0 "lightValue"
	  0 "lightOpacity" 255 "canBlockGrass" NIL "tickOnLoad" NIL
	  "nometanotify" NIL "isCollidable" T "blockResistance" 0.0
	  "blockHardness" 0.0 "slipperiness" 0.6 "blockMaterial"
	  "net.minecraft.src.Material@3d012ddd" "blockParticleGravity" 1.0
	  "stepSound " "net.minecraft.src.StepSound@7e774085" "maxZ" 1.0
	  "maxY" 1.0 "maxX" 1.0 "minZ" 0.0 "minY" 0.0 "minX" 0.0)
(DEFBLOCK BOOKSHELF "blockName" "bookshelf" "blockID" 47 "blockIndexInTexture"
	  35 "isOpaqueCube" T "renderAsNormalBlock" T "renderType" 0
	  "lightValue" 0 "lightOpacity" 255 "canBlockGrass" NIL "tickOnLoad"
	  NIL "nometanotify" NIL "isCollidable" T "blockResistance" 7.5
	  "blockHardness" 1.5 "slipperiness" 0.6 "blockMaterial"
	  "net.minecraft.src.Material@1c655221" "blockParticleGravity" 1.0
	  "stepSound " "net.minecraft.src.StepSound@58d25a40" "maxZ" 1.0
	  "maxY" 1.0 "maxX" 1.0 "minZ" 0.0 "minY" 0.0 "minX" 0.0)
(DEFBLOCK STONEMOSS "blockName" "stoneMoss" "blockID" 48 "blockIndexInTexture"
	  36 "isOpaqueCube" T "renderAsNormalBlock" T "renderType" 0
	  "lightValue" 0 "lightOpacity" 255 "canBlockGrass" NIL "tickOnLoad"
	  NIL "nometanotify" NIL "isCollidable" T "blockResistance" 30.0
	  "blockHardness" 2.0 "slipperiness" 0.6 "blockMaterial"
	  "net.minecraft.src.Material@53e25b76" "blockParticleGravity" 1.0
	  "stepSound " "net.minecraft.src.StepSound@73a8dfcc" "maxZ" 1.0
	  "maxY" 1.0 "maxX" 1.0 "minZ" 0.0 "minY" 0.0 "minX" 0.0)
(DEFBLOCK OBSIDIAN "blockName" "obsidian" "blockID" 49 "blockIndexInTexture"
	  37 "isOpaqueCube" T "renderAsNormalBlock" T "renderType" 0
	  "lightValue" 0 "lightOpacity" 255 "canBlockGrass" NIL "tickOnLoad"
	  NIL "nometanotify" NIL "isCollidable" T "blockResistance" 6000.0
	  "blockHardness" 10.0 "slipperiness" 0.6 "blockMaterial"
	  "net.minecraft.src.Material@53e25b76" "blockParticleGravity" 1.0
	  "stepSound " "net.minecraft.src.StepSound@73a8dfcc" "maxZ" 1.0
	  "maxY" 1.0 "maxX" 1.0 "minZ" 0.0 "minY" 0.0 "minX" 0.0)
(DEFBLOCK TORCH "blockName" "torch" "blockID" 50 "blockIndexInTexture" 80
	  "isOpaqueCube" NIL "renderAsNormalBlock" NIL "renderType" 2
	  "lightValue" 14 "lightOpacity" 0 "canBlockGrass" T "tickOnLoad" T
	  "nometanotify" T "isCollidable" NIL "blockResistance" 0.0
	  "blockHardness" 0.0 "slipperiness" 0.6 "blockMaterial"
	  "net.minecraft.src.MaterialLogic@c2e1f26" "blockParticleGravity" 1.0
	  "stepSound " "net.minecraft.src.StepSound@58d25a40" "maxZ" 1.0
	  "maxY" 1.0 "maxX" 1.0 "minZ" 0.0 "minY" 0.0 "minX" 0.0)
(DEFBLOCK FIRE "blockName" "fire" "blockID" 51 "blockIndexInTexture" 31
	  "isOpaqueCube" NIL "renderAsNormalBlock" NIL "renderType" 3
	  "lightValue" 15 "lightOpacity" 0 "canBlockGrass" T "tickOnLoad" T
	  "nometanotify" T "isCollidable" NIL "blockResistance" 0.0
	  "blockHardness" 0.0 "slipperiness" 0.6 "blockMaterial"
	  "net.minecraft.src.MaterialTransparent@6f2b958e"
	  "blockParticleGravity" 1.0 "stepSound "
	  "net.minecraft.src.StepSound@58d25a40" "maxZ" 1.0 "maxY" 1.0 "maxX"
	  1.0 "minZ" 0.0 "minY" 0.0 "minX" 0.0)
(DEFBLOCK MOBSPAWNER "blockName" "mobSpawner" "blockID" 52
	  "blockIndexInTexture" 65 "isOpaqueCube" NIL "renderAsNormalBlock" T
	  "renderType" 0 "lightValue" 0 "lightOpacity" 0 "canBlockGrass" NIL
	  "tickOnLoad" NIL "nometanotify" NIL "isCollidable" T
	  "blockResistance" 25.0 "blockHardness" 5.0 "slipperiness" 0.6
	  "blockMaterial" "net.minecraft.src.Material@53e25b76"
	  "blockParticleGravity" 1.0 "stepSound "
	  "net.minecraft.src.StepSound@dcf3e99" "maxZ" 1.0 "maxY" 1.0 "maxX"
	  1.0 "minZ" 0.0 "minY" 0.0 "minX" 0.0)
(DEFBLOCK STAIRSWOOD "blockName" "stairsWood" "blockID" 53
	  "blockIndexInTexture" 4 "isOpaqueCube" NIL "renderAsNormalBlock" NIL
	  "renderType" 10 "lightValue" 0 "lightOpacity" 255 "canBlockGrass"
	  NIL "tickOnLoad" NIL "nometanotify" T "isCollidable" T
	  "blockResistance" 15.0 "blockHardness" 2.0 "slipperiness" 0.6
	  "blockMaterial" "net.minecraft.src.Material@1c655221"
	  "blockParticleGravity" 1.0 "stepSound "
	  "net.minecraft.src.StepSound@58d25a40" "maxZ" 1.0 "maxY" 1.0 "maxX"
	  1.0 "minZ" 0.0 "minY" 0.0 "minX" 0.0)
(DEFBLOCK CHEST "blockName" "chest" "blockID" 54 "blockIndexInTexture" 26
	  "isOpaqueCube" T "renderAsNormalBlock" T "renderType" 0 "lightValue"
	  0 "lightOpacity" 255 "canBlockGrass" NIL "tickOnLoad" NIL
	  "nometanotify" T "isCollidable" T "blockResistance" 12.5
	  "blockHardness" 2.5 "slipperiness" 0.6 "blockMaterial"
	  "net.minecraft.src.Material@1c655221" "blockParticleGravity" 1.0
	  "stepSound " "net.minecraft.src.StepSound@58d25a40" "maxZ" 1.0
	  "maxY" 1.0 "maxX" 1.0 "minZ" 0.0 "minY" 0.0 "minX" 0.0)
(DEFBLOCK REDSTONEDUST "blockName" "redstoneDust" "blockID" 55
	  "blockIndexInTexture" 164 "isOpaqueCube" NIL "renderAsNormalBlock"
	  NIL "renderType" 5 "lightValue" 0 "lightOpacity" 0 "canBlockGrass" T
	  "tickOnLoad" NIL "nometanotify" T "isCollidable" NIL
	  "blockResistance" 0.0 "blockHardness" 0.0 "slipperiness" 0.6
	  "blockMaterial" "net.minecraft.src.MaterialLogic@c2e1f26"
	  "blockParticleGravity" 1.0 "stepSound "
	  "net.minecraft.src.StepSound@442d9b6e" "maxZ" 1.0 "maxY" 0.0625
	  "maxX" 1.0 "minZ" 0.0 "minY" 0.0 "minX" 0.0)
(DEFBLOCK OREDIAMOND "blockName" "oreDiamond" "blockID" 56
	  "blockIndexInTexture" 50 "isOpaqueCube" T "renderAsNormalBlock" T
	  "renderType" 0 "lightValue" 0 "lightOpacity" 255 "canBlockGrass" NIL
	  "tickOnLoad" NIL "nometanotify" NIL "isCollidable" T
	  "blockResistance" 15.0 "blockHardness" 3.0 "slipperiness" 0.6
	  "blockMaterial" "net.minecraft.src.Material@53e25b76"
	  "blockParticleGravity" 1.0 "stepSound "
	  "net.minecraft.src.StepSound@73a8dfcc" "maxZ" 1.0 "maxY" 1.0 "maxX"
	  1.0 "minZ" 0.0 "minY" 0.0 "minX" 0.0)
(DEFBLOCK BLOCKDIAMOND "blockName" "blockDiamond" "blockID" 57
	  "blockIndexInTexture" 24 "isOpaqueCube" T "renderAsNormalBlock" T
	  "renderType" 0 "lightValue" 0 "lightOpacity" 255 "canBlockGrass" NIL
	  "tickOnLoad" NIL "nometanotify" NIL "isCollidable" T
	  "blockResistance" 30.0 "blockHardness" 5.0 "slipperiness" 0.6
	  "blockMaterial" "net.minecraft.src.Material@75a1cd57"
	  "blockParticleGravity" 1.0 "stepSound "
	  "net.minecraft.src.StepSound@dcf3e99" "maxZ" 1.0 "maxY" 1.0 "maxX"
	  1.0 "minZ" 0.0 "minY" 0.0 "minX" 0.0)
(DEFBLOCK WORKBENCH "blockName" "workbench" "blockID" 58 "blockIndexInTexture"
	  59 "isOpaqueCube" T "renderAsNormalBlock" T "renderType" 0
	  "lightValue" 0 "lightOpacity" 255 "canBlockGrass" NIL "tickOnLoad"
	  NIL "nometanotify" NIL "isCollidable" T "blockResistance" 12.5
	  "blockHardness" 2.5 "slipperiness" 0.6 "blockMaterial"
	  "net.minecraft.src.Material@1c655221" "blockParticleGravity" 1.0
	  "stepSound " "net.minecraft.src.StepSound@58d25a40" "maxZ" 1.0
	  "maxY" 1.0 "maxX" 1.0 "minZ" 0.0 "minY" 0.0 "minX" 0.0)
(DEFBLOCK CROPS "blockName" "crops" "blockID" 59 "blockIndexInTexture" 88
	  "isOpaqueCube" NIL "renderAsNormalBlock" NIL "renderType" 6
	  "lightValue" 0 "lightOpacity" 0 "canBlockGrass" T "tickOnLoad" T
	  "nometanotify" T "isCollidable" NIL "blockResistance" 0.0
	  "blockHardness" 0.0 "slipperiness" 0.6 "blockMaterial"
	  "net.minecraft.src.MaterialLogic@1b701da1" "blockParticleGravity"
	  1.0 "stepSound " "net.minecraft.src.StepSound@7e774085" "maxZ" 1.0
	  "maxY" 0.25 "maxX" 1.0 "minZ" 0.0 "minY" 0.0 "minX" 0.0)
(DEFBLOCK FARMLAND "blockName" "farmland" "blockID" 60 "blockIndexInTexture"
	  87 "isOpaqueCube" NIL "renderAsNormalBlock" NIL "renderType" 0
	  "lightValue" 0 "lightOpacity" 255 "canBlockGrass" NIL "tickOnLoad" T
	  "nometanotify" NIL "isCollidable" T "blockResistance" 3.0
	  "blockHardness" 0.6 "slipperiness" 0.6 "blockMaterial"
	  "net.minecraft.src.Material@3f8f9dd6" "blockParticleGravity" 1.0
	  "stepSound " "net.minecraft.src.StepSound@aec6354" "maxZ" 1.0 "maxY"
	  0.9375 "maxX" 1.0 "minZ" 0.0 "minY" 0.0 "minX" 0.0)
(DEFBLOCK FURNACE "blockName" "furnace" "blockID" 61 "blockIndexInTexture" 45
	  "isOpaqueCube" T "renderAsNormalBlock" T "renderType" 0 "lightValue"
	  0 "lightOpacity" 255 "canBlockGrass" NIL "tickOnLoad" NIL
	  "nometanotify" T "isCollidable" T "blockResistance" 17.5
	  "blockHardness" 3.5 "slipperiness" 0.6 "blockMaterial"
	  "net.minecraft.src.Material@53e25b76" "blockParticleGravity" 1.0
	  "stepSound " "net.minecraft.src.StepSound@73a8dfcc" "maxZ" 1.0
	  "maxY" 1.0 "maxX" 1.0 "minZ" 0.0 "minY" 0.0 "minX" 0.0)
(DEFBLOCK FURNACE "blockName" "furnace" "blockID" 62 "blockIndexInTexture" 45
	  "isOpaqueCube" T "renderAsNormalBlock" T "renderType" 0 "lightValue"
	  13 "lightOpacity" 255 "canBlockGrass" NIL "tickOnLoad" NIL
	  "nometanotify" T "isCollidable" T "blockResistance" 17.5
	  "blockHardness" 3.5 "slipperiness" 0.6 "blockMaterial"
	  "net.minecraft.src.Material@53e25b76" "blockParticleGravity" 1.0
	  "stepSound " "net.minecraft.src.StepSound@73a8dfcc" "maxZ" 1.0
	  "maxY" 1.0 "maxX" 1.0 "minZ" 0.0 "minY" 0.0 "minX" 0.0)
(DEFBLOCK SIGN "blockName" "sign" "blockID" 63 "blockIndexInTexture" 4
	  "isOpaqueCube" NIL "renderAsNormalBlock" NIL "renderType" -1
	  "lightValue" 0 "lightOpacity" 0 "canBlockGrass" NIL "tickOnLoad" NIL
	  "nometanotify" T "isCollidable" NIL "blockResistance" 5.0
	  "blockHardness" 1.0 "slipperiness" 0.6 "blockMaterial"
	  "net.minecraft.src.Material@1c655221" "blockParticleGravity" 1.0
	  "stepSound " "net.minecraft.src.StepSound@58d25a40" "maxZ" 0.75
	  "maxY" 1.0 "maxX" 0.75 "minZ" 0.25 "minY" 0.0 "minX" 0.25)
(DEFBLOCK DOORWOOD "blockName" "doorWood" "blockID" 64 "blockIndexInTexture"
	  97 "isOpaqueCube" NIL "renderAsNormalBlock" NIL "renderType" 7
	  "lightValue" 0 "lightOpacity" 0 "canBlockGrass" NIL "tickOnLoad" NIL
	  "nometanotify" T "isCollidable" T "blockResistance" 15.0
	  "blockHardness" 3.0 "slipperiness" 0.6 "blockMaterial"
	  "net.minecraft.src.Material@1c655221" "blockParticleGravity" 1.0
	  "stepSound " "net.minecraft.src.StepSound@58d25a40" "maxZ" 1.0
	  "maxY" 1.0 "maxX" 1.0 "minZ" 0.0 "minY" 0.0 "minX" 0.0)
(DEFBLOCK LADDER "blockName" "ladder" "blockID" 65 "blockIndexInTexture" 83
	  "isOpaqueCube" NIL "renderAsNormalBlock" NIL "renderType" 8
	  "lightValue" 0 "lightOpacity" 0 "canBlockGrass" T "tickOnLoad" NIL
	  "nometanotify" T "isCollidable" NIL "blockResistance" 2.0
	  "blockHardness" 0.4 "slipperiness" 0.6 "blockMaterial"
	  "net.minecraft.src.MaterialLogic@c2e1f26" "blockParticleGravity" 1.0
	  "stepSound " "net.minecraft.src.StepSound@58d25a40" "maxZ" 1.0
	  "maxY" 1.0 "maxX" 1.0 "minZ" 0.0 "minY" 0.0 "minX" 0.0)
(DEFBLOCK RAIL "blockName" "rail" "blockID" 66 "blockIndexInTexture" 128
	  "isOpaqueCube" NIL "renderAsNormalBlock" NIL "renderType" 9
	  "lightValue" 0 "lightOpacity" 0 "canBlockGrass" T "tickOnLoad" NIL
	  "nometanotify" T "isCollidable" NIL "blockResistance" 3.5
	  "blockHardness" 0.7 "slipperiness" 0.6 "blockMaterial"
	  "net.minecraft.src.MaterialLogic@c2e1f26" "blockParticleGravity" 1.0
	  "stepSound " "net.minecraft.src.StepSound@dcf3e99" "maxZ" 1.0 "maxY"
	  0.125 "maxX" 1.0 "minZ" 0.0 "minY" 0.0 "minX" 0.0)
(DEFBLOCK STAIRSSTONE "blockName" "stairsStone" "blockID" 67
	  "blockIndexInTexture" 16 "isOpaqueCube" NIL "renderAsNormalBlock"
	  NIL "renderType" 10 "lightValue" 0 "lightOpacity" 255
	  "canBlockGrass" NIL "tickOnLoad" NIL "nometanotify" T "isCollidable"
	  T "blockResistance" 30.0 "blockHardness" 2.0 "slipperiness" 0.6
	  "blockMaterial" "net.minecraft.src.Material@53e25b76"
	  "blockParticleGravity" 1.0 "stepSound "
	  "net.minecraft.src.StepSound@73a8dfcc" "maxZ" 1.0 "maxY" 1.0 "maxX"
	  1.0 "minZ" 0.0 "minY" 0.0 "minX" 0.0)
(DEFBLOCK SIGN "blockName" "sign" "blockID" 68 "blockIndexInTexture" 4
	  "isOpaqueCube" NIL "renderAsNormalBlock" NIL "renderType" -1
	  "lightValue" 0 "lightOpacity" 0 "canBlockGrass" NIL "tickOnLoad" NIL
	  "nometanotify" T "isCollidable" NIL "blockResistance" 5.0
	  "blockHardness" 1.0 "slipperiness" 0.6 "blockMaterial"
	  "net.minecraft.src.Material@1c655221" "blockParticleGravity" 1.0
	  "stepSound " "net.minecraft.src.StepSound@58d25a40" "maxZ" 0.75
	  "maxY" 1.0 "maxX" 0.75 "minZ" 0.25 "minY" 0.0 "minX" 0.25)
(DEFBLOCK LEVER "blockName" "lever" "blockID" 69 "blockIndexInTexture" 96
	  "isOpaqueCube" NIL "renderAsNormalBlock" NIL "renderType" 12
	  "lightValue" 0 "lightOpacity" 0 "canBlockGrass" T "tickOnLoad" NIL
	  "nometanotify" T "isCollidable" NIL "blockResistance" 2.5
	  "blockHardness" 0.5 "slipperiness" 0.6 "blockMaterial"
	  "net.minecraft.src.MaterialLogic@c2e1f26" "blockParticleGravity" 1.0
	  "stepSound " "net.minecraft.src.StepSound@58d25a40" "maxZ" 1.0
	  "maxY" 1.0 "maxX" 1.0 "minZ" 0.0 "minY" 0.0 "minX" 0.0)
(DEFBLOCK PRESSUREPLATE "blockName" "pressurePlate" "blockID" 70
	  "blockIndexInTexture" 1 "isOpaqueCube" NIL "renderAsNormalBlock" NIL
	  "renderType" 0 "lightValue" 0 "lightOpacity" 0 "canBlockGrass" NIL
	  "tickOnLoad" T "nometanotify" T "isCollidable" NIL "blockResistance"
	  2.5 "blockHardness" 0.5 "slipperiness" 0.6 "blockMaterial"
	  "net.minecraft.src.Material@53e25b76" "blockParticleGravity" 1.0
	  "stepSound " "net.minecraft.src.StepSound@73a8dfcc" "maxZ" 0.9375
	  "maxY" 0.03125 "maxX" 0.9375 "minZ" 0.0625 "minY" 0.0 "minX" 0.0625)
(DEFBLOCK DOORIRON "blockName" "doorIron" "blockID" 71 "blockIndexInTexture"
	  98 "isOpaqueCube" NIL "renderAsNormalBlock" NIL "renderType" 7
	  "lightValue" 0 "lightOpacity" 0 "canBlockGrass" NIL "tickOnLoad" NIL
	  "nometanotify" T "isCollidable" T "blockResistance" 25.0
	  "blockHardness" 5.0 "slipperiness" 0.6 "blockMaterial"
	  "net.minecraft.src.Material@75a1cd57" "blockParticleGravity" 1.0
	  "stepSound " "net.minecraft.src.StepSound@dcf3e99" "maxZ" 1.0 "maxY"
	  1.0 "maxX" 1.0 "minZ" 0.0 "minY" 0.0 "minX" 0.0)
(DEFBLOCK PRESSUREPLATE "blockName" "pressurePlate" "blockID" 72
	  "blockIndexInTexture" 4 "isOpaqueCube" NIL "renderAsNormalBlock" NIL
	  "renderType" 0 "lightValue" 0 "lightOpacity" 0 "canBlockGrass" NIL
	  "tickOnLoad" T "nometanotify" T "isCollidable" NIL "blockResistance"
	  2.5 "blockHardness" 0.5 "slipperiness" 0.6 "blockMaterial"
	  "net.minecraft.src.Material@1c655221" "blockParticleGravity" 1.0
	  "stepSound " "net.minecraft.src.StepSound@58d25a40" "maxZ" 0.9375
	  "maxY" 0.03125 "maxX" 0.9375 "minZ" 0.0625 "minY" 0.0 "minX" 0.0625)
(DEFBLOCK OREREDSTONE "blockName" "oreRedstone" "blockID" 73
	  "blockIndexInTexture" 51 "isOpaqueCube" T "renderAsNormalBlock" T
	  "renderType" 0 "lightValue" 0 "lightOpacity" 255 "canBlockGrass" NIL
	  "tickOnLoad" NIL "nometanotify" T "isCollidable" T "blockResistance"
	  15.0 "blockHardness" 3.0 "slipperiness" 0.6 "blockMaterial"
	  "net.minecraft.src.Material@53e25b76" "blockParticleGravity" 1.0
	  "stepSound " "net.minecraft.src.StepSound@73a8dfcc" "maxZ" 1.0
	  "maxY" 1.0 "maxX" 1.0 "minZ" 0.0 "minY" 0.0 "minX" 0.0)
(DEFBLOCK OREREDSTONE "blockName" "oreRedstone" "blockID" 74
	  "blockIndexInTexture" 51 "isOpaqueCube" T "renderAsNormalBlock" T
	  "renderType" 0 "lightValue" 9 "lightOpacity" 255 "canBlockGrass" NIL
	  "tickOnLoad" T "nometanotify" T "isCollidable" T "blockResistance"
	  15.0 "blockHardness" 3.0 "slipperiness" 0.6 "blockMaterial"
	  "net.minecraft.src.Material@53e25b76" "blockParticleGravity" 1.0
	  "stepSound " "net.minecraft.src.StepSound@73a8dfcc" "maxZ" 1.0
	  "maxY" 1.0 "maxX" 1.0 "minZ" 0.0 "minY" 0.0 "minX" 0.0)
(DEFBLOCK NOTGATE "blockName" "notGate" "blockID" 75 "blockIndexInTexture" 115
	  "isOpaqueCube" NIL "renderAsNormalBlock" NIL "renderType" 2
	  "lightValue" 0 "lightOpacity" 0 "canBlockGrass" T "tickOnLoad" T
	  "nometanotify" T "isCollidable" T "blockResistance" 0.0
	  "blockHardness" 0.0 "slipperiness" 0.6 "blockMaterial"
	  "net.minecraft.src.MaterialLogic@c2e1f26" "blockParticleGravity" 1.0
	  "stepSound " "net.minecraft.src.StepSound@58d25a40" "maxZ" 1.0
	  "maxY" 1.0 "maxX" 1.0 "minZ" 0.0 "minY" 0.0 "minX" 0.0)
(DEFBLOCK NOTGATE "blockName" "notGate" "blockID" 76 "blockIndexInTexture" 99
	  "isOpaqueCube" NIL "renderAsNormalBlock" NIL "renderType" 2
	  "lightValue" 7 "lightOpacity" 0 "canBlockGrass" T "tickOnLoad" T
	  "nometanotify" T "isCollidable" T "blockResistance" 0.0
	  "blockHardness" 0.0 "slipperiness" 0.6 "blockMaterial"
	  "net.minecraft.src.MaterialLogic@c2e1f26" "blockParticleGravity" 1.0
	  "stepSound " "net.minecraft.src.StepSound@58d25a40" "maxZ" 1.0
	  "maxY" 1.0 "maxX" 1.0 "minZ" 0.0 "minY" 0.0 "minX" 0.0)
(DEFBLOCK BUTTON "blockName" "button" "blockID" 77 "blockIndexInTexture" 1
	  "isOpaqueCube" NIL "renderAsNormalBlock" NIL "renderType" 0
	  "lightValue" 0 "lightOpacity" 0 "canBlockGrass" T "tickOnLoad" T
	  "nometanotify" T "isCollidable" NIL "blockResistance" 2.5
	  "blockHardness" 0.5 "slipperiness" 0.6 "blockMaterial"
	  "net.minecraft.src.MaterialLogic@c2e1f26" "blockParticleGravity" 1.0
	  "stepSound " "net.minecraft.src.StepSound@73a8dfcc" "maxZ" 1.0
	  "maxY" 1.0 "maxX" 1.0 "minZ" 0.0 "minY" 0.0 "minX" 0.0)
(DEFBLOCK SNOW "blockName" "snow" "blockID" 78 "blockIndexInTexture" 66
	  "isOpaqueCube" NIL "renderAsNormalBlock" NIL "renderType" 0
	  "lightValue" 0 "lightOpacity" 0 "canBlockGrass" T "tickOnLoad" T
	  "nometanotify" NIL "isCollidable" T "blockResistance" 0.5
	  "blockHardness" 0.1 "slipperiness" 0.6 "blockMaterial"
	  "net.minecraft.src.MaterialLogic@1eb44e46" "blockParticleGravity"
	  1.0 "stepSound " "net.minecraft.src.StepSound@1ee0005" "maxZ" 1.0
	  "maxY" 0.125 "maxX" 1.0 "minZ" 0.0 "minY" 0.0 "minX" 0.0)
(DEFBLOCK ICE "blockName" "ice" "blockID" 79 "blockIndexInTexture" 67
	  "isOpaqueCube" NIL "renderAsNormalBlock" T "renderType" 0
	  "lightValue" 0 "lightOpacity" 3 "canBlockGrass" NIL "tickOnLoad" T
	  "nometanotify" NIL "isCollidable" T "blockResistance" 2.5
	  "blockHardness" 0.5 "slipperiness" 0.98 "blockMaterial"
	  "net.minecraft.src.Material@6504e3b2" "blockParticleGravity" 1.0
	  "stepSound " "net.minecraft.src.StepSoundStone@7fbe847c" "maxZ" 1.0
	  "maxY" 1.0 "maxX" 1.0 "minZ" 0.0 "minY" 0.0 "minX" 0.0)
(DEFBLOCK SNOW "blockName" "snow" "blockID" 80 "blockIndexInTexture" 66
	  "isOpaqueCube" T "renderAsNormalBlock" T "renderType" 0 "lightValue"
	  0 "lightOpacity" 255 "canBlockGrass" NIL "tickOnLoad" T
	  "nometanotify" NIL "isCollidable" T "blockResistance" 1.0
	  "blockHardness" 0.2 "slipperiness" 0.6 "blockMaterial"
	  "net.minecraft.src.Material@515f550a" "blockParticleGravity" 1.0
	  "stepSound " "net.minecraft.src.StepSound@1ee0005" "maxZ" 1.0 "maxY"
	  1.0 "maxX" 1.0 "minZ" 0.0 "minY" 0.0 "minX" 0.0)
(DEFBLOCK CACTUS "blockName" "cactus" "blockID" 81 "blockIndexInTexture" 70
	  "isOpaqueCube" NIL "renderAsNormalBlock" NIL "renderType" 13
	  "lightValue" 0 "lightOpacity" 0 "canBlockGrass" NIL "tickOnLoad" T
	  "nometanotify" NIL "isCollidable" T "blockResistance" 2.0
	  "blockHardness" 0.4 "slipperiness" 0.6 "blockMaterial"
	  "net.minecraft.src.Material@626b2d4a" "blockParticleGravity" 1.0
	  "stepSound " "net.minecraft.src.StepSound@1ee0005" "maxZ" 1.0 "maxY"
	  1.0 "maxX" 1.0 "minZ" 0.0 "minY" 0.0 "minX" 0.0)
(DEFBLOCK CLAY "blockName" "clay" "blockID" 82 "blockIndexInTexture" 72
	  "isOpaqueCube" T "renderAsNormalBlock" T "renderType" 0 "lightValue"
	  0 "lightOpacity" 255 "canBlockGrass" NIL "tickOnLoad" NIL
	  "nometanotify" NIL "isCollidable" T "blockResistance" 3.0
	  "blockHardness" 0.6 "slipperiness" 0.6 "blockMaterial"
	  "net.minecraft.src.Material@5e91993f" "blockParticleGravity" 1.0
	  "stepSound " "net.minecraft.src.StepSound@aec6354" "maxZ" 1.0 "maxY"
	  1.0 "maxX" 1.0 "minZ" 0.0 "minY" 0.0 "minX" 0.0)
(DEFBLOCK REEDS "blockName" "reeds" "blockID" 83 "blockIndexInTexture" 73
	  "isOpaqueCube" NIL "renderAsNormalBlock" NIL "renderType" 1
	  "lightValue" 0 "lightOpacity" 0 "canBlockGrass" T "tickOnLoad" T
	  "nometanotify" NIL "isCollidable" NIL "blockResistance" 0.0
	  "blockHardness" 0.0 "slipperiness" 0.6 "blockMaterial"
	  "net.minecraft.src.MaterialLogic@1b701da1" "blockParticleGravity"
	  1.0 "stepSound " "net.minecraft.src.StepSound@7e774085" "maxZ" 0.875
	  "maxY" 1.0 "maxX" 0.875 "minZ" 0.125 "minY" 0.0 "minX" 0.125)
(DEFBLOCK JUKEBOX "blockName" "jukebox" "blockID" 84 "blockIndexInTexture" 74
	  "isOpaqueCube" T "renderAsNormalBlock" T "renderType" 0 "lightValue"
	  0 "lightOpacity" 255 "canBlockGrass" NIL "tickOnLoad" NIL
	  "nometanotify" T "isCollidable" T "blockResistance" 30.0
	  "blockHardness" 2.0 "slipperiness" 0.6 "blockMaterial"
	  "net.minecraft.src.Material@1c655221" "blockParticleGravity" 1.0
	  "stepSound " "net.minecraft.src.StepSound@73a8dfcc" "maxZ" 1.0
	  "maxY" 1.0 "maxX" 1.0 "minZ" 0.0 "minY" 0.0 "minX" 0.0)
(DEFBLOCK FENCE "blockName" "fence" "blockID" 85 "blockIndexInTexture" 4
	  "isOpaqueCube" NIL "renderAsNormalBlock" NIL "renderType" 11
	  "lightValue" 0 "lightOpacity" 0 "canBlockGrass" NIL "tickOnLoad" NIL
	  "nometanotify" T "isCollidable" T "blockResistance" 15.0
	  "blockHardness" 2.0 "slipperiness" 0.6 "blockMaterial"
	  "net.minecraft.src.Material@1c655221" "blockParticleGravity" 1.0
	  "stepSound " "net.minecraft.src.StepSound@58d25a40" "maxZ" 1.0
	  "maxY" 1.0 "maxX" 1.0 "minZ" 0.0 "minY" 0.0 "minX" 0.0)
(DEFBLOCK PUMPKIN "blockName" "pumpkin" "blockID" 86 "blockIndexInTexture" 102
	  "isOpaqueCube" T "renderAsNormalBlock" T "renderType" 0 "lightValue"
	  0 "lightOpacity" 255 "canBlockGrass" NIL "tickOnLoad" T
	  "nometanotify" T "isCollidable" T "blockResistance" 5.0
	  "blockHardness" 1.0 "slipperiness" 0.6 "blockMaterial"
	  "net.minecraft.src.Material@1c4af82c" "blockParticleGravity" 1.0
	  "stepSound " "net.minecraft.src.StepSound@58d25a40" "maxZ" 1.0
	  "maxY" 1.0 "maxX" 1.0 "minZ" 0.0 "minY" 0.0 "minX" 0.0)
(DEFBLOCK HELLROCK "blockName" "hellrock" "blockID" 87 "blockIndexInTexture"
	  103 "isOpaqueCube" T "renderAsNormalBlock" T "renderType" 0
	  "lightValue" 0 "lightOpacity" 255 "canBlockGrass" NIL "tickOnLoad"
	  NIL "nometanotify" NIL "isCollidable" T "blockResistance" 2.0
	  "blockHardness" 0.4 "slipperiness" 0.6 "blockMaterial"
	  "net.minecraft.src.Material@53e25b76" "blockParticleGravity" 1.0
	  "stepSound " "net.minecraft.src.StepSound@73a8dfcc" "maxZ" 1.0
	  "maxY" 1.0 "maxX" 1.0 "minZ" 0.0 "minY" 0.0 "minX" 0.0)
(DEFBLOCK HELLSAND "blockName" "hellsand" "blockID" 88 "blockIndexInTexture"
	  104 "isOpaqueCube" T "renderAsNormalBlock" T "renderType" 0
	  "lightValue" 0 "lightOpacity" 255 "canBlockGrass" NIL "tickOnLoad"
	  NIL "nometanotify" NIL "isCollidable" T "blockResistance" 2.5
	  "blockHardness" 0.5 "slipperiness" 0.6 "blockMaterial"
	  "net.minecraft.src.Material@15615099" "blockParticleGravity" 1.0
	  "stepSound " "net.minecraft.src.StepSoundSand@1edf1c96" "maxZ" 1.0
	  "maxY" 1.0 "maxX" 1.0 "minZ" 0.0 "minY" 0.0 "minX" 0.0)
(DEFBLOCK LIGHTGEM "blockName" "lightgem" "blockID" 89 "blockIndexInTexture"
	  105 "isOpaqueCube" T "renderAsNormalBlock" T "renderType" 0
	  "lightValue" 15 "lightOpacity" 255 "canBlockGrass" NIL "tickOnLoad"
	  NIL "nometanotify" NIL "isCollidable" T "blockResistance" 1.5
	  "blockHardness" 0.3 "slipperiness" 0.6 "blockMaterial"
	  "net.minecraft.src.Material@53e25b76" "blockParticleGravity" 1.0
	  "stepSound " "net.minecraft.src.StepSoundStone@7fbe847c" "maxZ" 1.0
	  "maxY" 1.0 "maxX" 1.0 "minZ" 0.0 "minY" 0.0 "minX" 0.0)
(DEFBLOCK PORTAL "blockName" "portal" "blockID" 90 "blockIndexInTexture" 14
	  "isOpaqueCube" NIL "renderAsNormalBlock" NIL "renderType" 0
	  "lightValue" 11 "lightOpacity" 0 "canBlockGrass" T "tickOnLoad" NIL
	  "nometanotify" NIL "isCollidable" NIL "blockResistance" 0.0
	  "blockHardness" -1.0 "slipperiness" 0.6 "blockMaterial"
	  "net.minecraft.src.MaterialPortal@379619aa" "blockParticleGravity"
	  1.0 "stepSound " "net.minecraft.src.StepSoundStone@7fbe847c" "maxZ"
	  1.0 "maxY" 1.0 "maxX" 1.0 "minZ" 0.0 "minY" 0.0 "minX" 0.0)
(DEFBLOCK LITPUMPKIN "blockName" "litpumpkin" "blockID" 91
	  "blockIndexInTexture" 102 "isOpaqueCube" T "renderAsNormalBlock" T
	  "renderType" 0 "lightValue" 15 "lightOpacity" 255 "canBlockGrass"
	  NIL "tickOnLoad" T "nometanotify" T "isCollidable" T
	  "blockResistance" 5.0 "blockHardness" 1.0 "slipperiness" 0.6
	  "blockMaterial" "net.minecraft.src.Material@1c4af82c"
	  "blockParticleGravity" 1.0 "stepSound "
	  "net.minecraft.src.StepSound@58d25a40" "maxZ" 1.0 "maxY" 1.0 "maxX"
	  1.0 "minZ" 0.0 "minY" 0.0 "minX" 0.0)
(DEFBLOCK CAKE "blockName" "cake" "blockID" 92 "blockIndexInTexture" 121
	  "isOpaqueCube" NIL "renderAsNormalBlock" NIL "renderType" 0
	  "lightValue" 0 "lightOpacity" 0 "canBlockGrass" NIL "tickOnLoad" T
	  "nometanotify" T "isCollidable" T "blockResistance" 2.5
	  "blockHardness" 0.5 "slipperiness" 0.6 "blockMaterial"
	  "net.minecraft.src.Material@cac736f" "blockParticleGravity" 1.0
	  "stepSound " "net.minecraft.src.StepSound@1ee0005" "maxZ" 1.0 "maxY"
	  1.0 "maxX" 1.0 "minZ" 0.0 "minY" 0.0 "minX" 0.0)
(DEFBLOCK DIODE "blockName" "diode" "blockID" 93 "blockIndexInTexture" 6
	  "isOpaqueCube" NIL "renderAsNormalBlock" NIL "renderType" 15
	  "lightValue" 0 "lightOpacity" 0 "canBlockGrass" T "tickOnLoad" NIL
	  "nometanotify" T "isCollidable" T "blockResistance" 0.0
	  "blockHardness" 0.0 "slipperiness" 0.6 "blockMaterial"
	  "net.minecraft.src.MaterialLogic@c2e1f26" "blockParticleGravity" 1.0
	  "stepSound " "net.minecraft.src.StepSound@58d25a40" "maxZ" 1.0
	  "maxY" 0.125 "maxX" 1.0 "minZ" 0.0 "minY" 0.0 "minX" 0.0)
(DEFBLOCK DIODE "blockName" "diode" "blockID" 94 "blockIndexInTexture" 6
	  "isOpaqueCube" NIL "renderAsNormalBlock" NIL "renderType" 15
	  "lightValue" 9 "lightOpacity" 0 "canBlockGrass" T "tickOnLoad" NIL
	  "nometanotify" T "isCollidable" T "blockResistance" 0.0
	  "blockHardness" 0.0 "slipperiness" 0.6 "blockMaterial"
	  "net.minecraft.src.MaterialLogic@c2e1f26" "blockParticleGravity" 1.0
	  "stepSound " "net.minecraft.src.StepSound@58d25a40" "maxZ" 1.0
	  "maxY" 0.125 "maxX" 1.0 "minZ" 0.0 "minY" 0.0 "minX" 0.0)
(DEFBLOCK LOCKEDCHEST "blockName" "lockedchest" "blockID" 95
	  "blockIndexInTexture" 26 "isOpaqueCube" T "renderAsNormalBlock" T
	  "renderType" 0 "lightValue" 15 "lightOpacity" 255 "canBlockGrass"
	  NIL "tickOnLoad" T "nometanotify" T "isCollidable" T
	  "blockResistance" 0.0 "blockHardness" 0.0 "slipperiness" 0.6
	  "blockMaterial" "net.minecraft.src.Material@1c655221"
	  "blockParticleGravity" 1.0 "stepSound "
	  "net.minecraft.src.StepSound@58d25a40" "maxZ" 1.0 "maxY" 1.0 "maxX"
	  1.0 "minZ" 0.0 "minY" 0.0 "minX" 0.0)
(DEFBLOCK TRAPDOOR "blockName" "trapdoor" "blockID" 96 "blockIndexInTexture"
	  84 "isOpaqueCube" NIL "renderAsNormalBlock" NIL "renderType" 0
	  "lightValue" 0 "lightOpacity" 0 "canBlockGrass" NIL "tickOnLoad" NIL
	  "nometanotify" T "isCollidable" T "blockResistance" 15.0
	  "blockHardness" 3.0 "slipperiness" 0.6 "blockMaterial"
	  "net.minecraft.src.Material@1c655221" "blockParticleGravity" 1.0
	  "stepSound " "net.minecraft.src.StepSound@58d25a40" "maxZ" 1.0
	  "maxY" 1.0 "maxX" 1.0 "minZ" 0.0 "minY" 0.0 "minX" 0.0)

(defmacro progno (&rest args) (declare (ignore args)))

(defun hashinfo (leblock)
  (maphash (lambda (k v) (print (list* k v)))
	   leblock))

(defun bitarray (size)
  (make-array size :element-type (quote (unsigned-byte 1))))

(defun nibblearray (size)
  (make-array size :element-type (quote (unsigned-byte 4))))

(defun bytearray (size)
  (make-array size :element-type (quote (unsigned-byte 8))))

(defparameter totalblockspace 256)

(defparameter blocksList nil)

(defparameter tickOnLoad nil)
(defparameter opaquecubelooukup nil)
(defparameter isblockcontainer nil)
(defparameter lightopacity nil)
(defparameter canblockgrass nil)
(defparameter lightvalue nil)
(defparameter disableneighbornotifyonmetadatachange nil)

(defparameter blockIndexInTexture nil)
(defparameter blockhardness nil)
(defparameter blockResistance nil)
(defparameter stepSound nil)
(defparameter blockParticleGravity nil)
(defparameter blockMaterial nil)
(defparameter slipperiness nil)
(defparameter blockname nil)

(defparameter isCollidable nil)

(defparameter getrendertype nil)
(defparameter renderasnormalblock nil)

(defparameter colormultiplier nil)

(defun updateblockslist ()
  (setf blockslist (make-array totalblockspace :initial-element nil))
  (maphash
   (lambda (k v)
     (declare (ignore k))
     (setf
      (aref blockslist
	    (gethash "blockID" v))
      v))
   allblocks))

(defmacro aif (test &body stuff)
  `(let ((it ,test))
     (if it
	 ,@stuff)))

(defmacro defblockprop (var nombre type)
  `(progn
     (setf ,var (make-array totalblockspace :element-type ,type))
     (dotimes (n totalblockspace)
       (aif (aref blockslist n)
	 (setf (aref ,var n) (gethash ,nombre it))))))

(defun buildblocks ()
  (updateblockslist)
  (defblockprop tickOnLoad "tickOnLoad" t)
  (defblockprop opaquecubelooukup "isOpaqueCube" t)
  (defblockprop lightOpacity "lightOpacity" 'fixnum)
  (defblockprop canblockgrass "canBlockGrass" t)
  (defblockprop lightvalue "lightValue" '(unsigned-byte 4))
  (defblockprop disableneighbornotifyonmetadatachange "nometanotify" t)
  (defblockprop blockIndexInTexture "blockIndexInTexture" '(unsigned-byte 8))
  (defblockprop blockhardness "blockHardness" 'float)
  (defblockprop blockResistance "blockResistance" 'float)
  (defblockprop stepsound "stepSound " t)
  (defblockprop blockParticleGravity "blockParticleGravity" 'float)
  (defblockprop blockMaterial "blockMaterial" t)
  (defblockprop slipperiness "slipperiness" 'float)
  (defblockprop blockname "blockName" t)
  (defblockprop getrendertype "renderType" 'fixnum)
  (defblockprop renderasnormalblock "renderAsNormalBlock" t)
  (defblockprop isCollidable "isCollidable" t))

(defparameter getBlocktexture (make-array totalblockspace))
(defparameter colormultiplier (make-array totalblockspace))


(defmacro deffunc (array nombre func)
  `(let ((blockid (gethash "blockID" (gethash ,nombre allblocks))))
     (setf (aref ,array blockid) ,func)))

(deffunc getBlocktexture 'grass
  (lambda (x)
    (case x
      (0 2)
      (1 0)
      (t 3))))

(deffunc colormultiplier 'tallgrass
  (lambda () (sandbox::getapixel 0 255 (sandbox::lget sandbox::*g/image* "misc/grasscolor.png"))))
(deffunc colormultiplier 'grass
  (lambda () (sandbox::getapixel 0 255 (sandbox::lget sandbox::*g/image* "misc/grasscolor.png"))))
(deffunc colormultiplier 'leaves
  (lambda () (sandbox::getapixel 0 255 (sandbox::lget sandbox::*g/image* "misc/foliagecolor.png"))))

(buildblocks)
    
