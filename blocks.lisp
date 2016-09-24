(in-package :mc-blocks)

(defparameter allblocks (make-hash-table :test (function equal)))

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

(defmacro progno (&rest args) (declare (ignore args)))

(defun hashinfo (leblock)
  (maphash (lambda (k v) (print (list* k v)))
	   leblock))

(defun aconverter (meh)
  (let ((wow (copy-tree meh)))
    (setf (first wow) (intern (string-upcase (third meh))))
    (push 'defblock wow)
    (print wow)
    wow))

(defblock default 
    "blockName" "default" 
    "slipperiness" 0.6
    "blockMaterial" "net.minecraft.src.Material@46fbb2c1" 
    "blockParticleGravity" 1.0
    "stepSound " "net.minecraft.src.StepSound@1698c449" 
    "maxZ" 1.0
    "maxY" 1.0
    "maxX" 1.0
    "minZ" 0.0
    "minY" 0.0
    "minX" 0.0
    "blockResistance" 30.0
    "blockHardness" 1.5
    "blockID" 0
    "blockIndexInTexture" 0)
(defblock stone
    "blockName" "stone" 
    "slipperiness" 0.6
    "blockMaterial" "net.minecraft.src.Material@46fbb2c1" 
    "blockParticleGravity" 1.0
    "stepSound " "net.minecraft.src.StepSound@1698c449" 
    "maxZ" 1.0
    "maxY" 1.0
    "maxX" 1.0
    "minZ" 0.0
    "minY" 0.0
    "minX" 0.0
    "blockResistance" 30.0
    "blockHardness" 1.5
    "blockID" 1
    "blockIndexInTexture" 1)
(defblock grass
    "blockName" "grass" 
    "slipperiness" 0.6
    "blockMaterial" "net.minecraft.src.Material@5ef04b5" 
    "blockParticleGravity" 1.0
    "stepSound " "net.minecraft.src.StepSound@5f4da5c3" 
    "maxZ" 1.0
    "maxY" 1.0
    "maxX" 1.0
    "minZ" 0.0
    "minY" 0.0
    "minX" 0.0
    "blockResistance" 3.0
    "blockHardness" 0.6
    "blockID" 2
    "blockIndexInTexture" 

    (progn (lambda (x)
	      (case x
		(0 2)
		(1 0)
		(t 3)))))
(defblock dirt
    "blockName" "dirt" 
    "slipperiness" 0.6
    "blockMaterial" "net.minecraft.src.Material@443b7951" 
    "blockParticleGravity" 1.0
    "stepSound " "net.minecraft.src.StepSound@14514713" 
    "maxZ" 1.0
    "maxY" 1.0
    "maxX" 1.0
    "minZ" 0.0
    "minY" 0.0
    "minX" 0.0
    "blockResistance" 2.5
    "blockHardness" 0.5
    "blockID" 3
    "blockIndexInTexture" 2)
(defblock stonebrick
    "blockName" "stonebrick" 
    "slipperiness" 0.6
    "blockMaterial" "net.minecraft.src.Material@46fbb2c1" 
    "blockParticleGravity" 1.0
    "stepSound " "net.minecraft.src.StepSound@1698c449" 
    "maxZ" 1.0
    "maxY" 1.0
    "maxX" 1.0
    "minZ" 0.0
    "minY" 0.0
    "minX" 0.0
    "blockResistance" 30.0
    "blockHardness" 2.0
    "blockID" 4
    "blockIndexInTexture" 16)
(defblock wood
    "blockName" "wood" 
    "slipperiness" 0.6
    "blockMaterial" "net.minecraft.src.Material@69663380" 
    "blockParticleGravity" 1.0
    "stepSound " "net.minecraft.src.StepSound@5b37e0d2" 
    "maxZ" 1.0
    "maxY" 1.0
    "maxX" 1.0
    "minZ" 0.0
    "minY" 0.0
    "minX" 0.0
    "blockResistance" 15.0
    "blockHardness" 2.0
    "blockID" 5
    "blockIndexInTexture" 4)
(defblock sapling
    "blockName" "sapling" 
    "slipperiness" 0.6
    "blockMaterial" "net.minecraft.src.MaterialLogic@4459eb14" 
    "blockParticleGravity" 1.0
    "stepSound " "net.minecraft.src.StepSound@5f4da5c3" 
    "maxZ" 0.8999999761581421
    "maxY" 0.800000011920929
    "maxX" 0.8999999761581421
    "minZ" 0.09999999403953552
    "minY" 0.0
    "minX" 0.09999999403953552
    "blockResistance" 0.0
    "blockHardness" 0.0
    "blockID" 6
    "blockIndexInTexture" 15)
(defblock bedrock
    "blockName" "bedrock" 
    "slipperiness" 0.6
    "blockMaterial" "net.minecraft.src.Material@46fbb2c1" 
    "blockParticleGravity" 1.0
    "stepSound " "net.minecraft.src.StepSound@1698c449" 
    "maxZ" 1.0
    "maxY" 1.0
    "maxX" 1.0
    "minZ" 0.0
    "minY" 0.0
    "minX" 0.0
    "blockResistance" 1.8E7
    "blockHardness" -1.0
    "blockID" 7
    "blockIndexInTexture" 17)
(defblock water
    "blockName" "water" 
    "slipperiness" 0.6
    "blockMaterial" "net.minecraft.src.MaterialLiquid@5a2e4553" 
    "blockParticleGravity" 1.0
    "stepSound " "net.minecraft.src.StepSound@28c97a5" 
    "maxZ" 1.0
    "maxY" 1.0
    "maxX" 1.0
    "minZ" 0.0
    "minY" 0.0
    "minX" 0.0
    "blockResistance" 500.0
    "blockHardness" 100.0
    "blockID" 8
    "blockIndexInTexture" 205)
(defblock water
    "blockName" "water" 
    "slipperiness" 0.6
    "blockMaterial" "net.minecraft.src.MaterialLiquid@5a2e4553" 
    "blockParticleGravity" 1.0
    "stepSound " "net.minecraft.src.StepSound@28c97a5" 
    "maxZ" 1.0
    "maxY" 1.0
    "maxX" 1.0
    "minZ" 0.0
    "minY" 0.0
    "minX" 0.0
    "blockResistance" 500.0
    "blockHardness" 100.0
    "blockID" 9
    "blockIndexInTexture" 205)
(defblock lava
    "blockName" "lava" 
    "slipperiness" 0.6
    "blockMaterial" "net.minecraft.src.MaterialLiquid@6659c656" 
    "blockParticleGravity" 1.0
    "stepSound " "net.minecraft.src.StepSound@28c97a5" 
    "maxZ" 1.0
    "maxY" 1.0
    "maxX" 1.0
    "minZ" 0.0
    "minY" 0.0
    "minX" 0.0
    "blockResistance" 0.0
    "blockHardness" 0.0
    "blockID" 10
    "blockIndexInTexture" 237)

(DEFBLOCK SAND "blockName" "sand" "slipperiness" 0.6 "blockMaterial"
          "net.minecraft.src.Material@6d5380c2" "blockParticleGravity" 1.0
          "stepSound " "net.minecraft.src.StepSoundSand@45ff54e6" "maxZ" 1.0
          "maxY" 1.0 "maxX" 1.0 "minZ" 0.0 "minY" 0.0 "minX" 0.0
          "blockResistance" 2.5 "blockHardness" 0.5 "blockID" 12
          "blockIndexInTexture" 18) 
(DEFBLOCK GRAVEL "blockName" "gravel" "slipperiness" 0.6 "blockMaterial"
          "net.minecraft.src.Material@6d5380c2" "blockParticleGravity" 1.0
          "stepSound " "net.minecraft.src.StepSound@14514713" "maxZ" 1.0 "maxY"
          1.0 "maxX" 1.0 "minZ" 0.0 "minY" 0.0 "minX" 0.0 "blockResistance" 3.0
          "blockHardness" 0.6 "blockID" 13 "blockIndexInTexture" 19) 
(DEFBLOCK OREGOLD "blockName" "oreGold" "slipperiness" 0.6 "blockMaterial"
          "net.minecraft.src.Material@46fbb2c1" "blockParticleGravity" 1.0
          "stepSound " "net.minecraft.src.StepSound@1698c449" "maxZ" 1.0 "maxY"
          1.0 "maxX" 1.0 "minZ" 0.0 "minY" 0.0 "minX" 0.0 "blockResistance"
          15.0 "blockHardness" 3.0 "blockID" 14 "blockIndexInTexture" 32) 
(DEFBLOCK OREIRON "blockName" "oreIron" "slipperiness" 0.6 "blockMaterial"
          "net.minecraft.src.Material@46fbb2c1" "blockParticleGravity" 1.0
          "stepSound " "net.minecraft.src.StepSound@1698c449" "maxZ" 1.0 "maxY"
          1.0 "maxX" 1.0 "minZ" 0.0 "minY" 0.0 "minX" 0.0 "blockResistance"
          15.0 "blockHardness" 3.0 "blockID" 15 "blockIndexInTexture" 33) 
(DEFBLOCK ORECOAL "blockName" "oreCoal" "slipperiness" 0.6 "blockMaterial"
          "net.minecraft.src.Material@46fbb2c1" "blockParticleGravity" 1.0
          "stepSound " "net.minecraft.src.StepSound@1698c449" "maxZ" 1.0 "maxY"
          1.0 "maxX" 1.0 "minZ" 0.0 "minY" 0.0 "minX" 0.0 "blockResistance"
          15.0 "blockHardness" 3.0 "blockID" 16 "blockIndexInTexture" 34) 
(DEFBLOCK LOG "blockName" "log" "slipperiness" 0.6 "blockMaterial"
          "net.minecraft.src.Material@69663380" "blockParticleGravity" 1.0
          "stepSound " "net.minecraft.src.StepSound@5b37e0d2" "maxZ" 1.0 "maxY"
          1.0 "maxX" 1.0 "minZ" 0.0 "minY" 0.0 "minX" 0.0 "blockResistance"
          10.0 "blockHardness" 2.0 "blockID" 17 "blockIndexInTexture" 20) 
(DEFBLOCK LEAVES "blockName" "leaves" "slipperiness" 0.6 "blockMaterial"
          "net.minecraft.src.Material@2328c243" "blockParticleGravity" 1.0
          "stepSound " "net.minecraft.src.StepSound@5f4da5c3" "maxZ" 1.0 "maxY"
          1.0 "maxX" 1.0 "minZ" 0.0 "minY" 0.0 "minX" 0.0 "blockResistance" 1.0
          "blockHardness" 0.2 "blockID" 18 "blockIndexInTexture"
          (IF T
              53
              52)) 
(DEFBLOCK SPONGE "blockName" "sponge" "slipperiness" 0.6 "blockMaterial"
          "net.minecraft.src.Material@bebdb06" "blockParticleGravity" 1.0
          "stepSound " "net.minecraft.src.StepSound@5f4da5c3" "maxZ" 1.0 "maxY"
          1.0 "maxX" 1.0 "minZ" 0.0 "minY" 0.0 "minX" 0.0 "blockResistance" 3.0
          "blockHardness" 0.6 "blockID" 19 "blockIndexInTexture" 48) 
(DEFBLOCK GLASS "blockName" "glass" "slipperiness" 0.6 "blockMaterial"
          "net.minecraft.src.Material@7a4f0f29" "blockParticleGravity" 1.0
          "stepSound " "net.minecraft.src.StepSoundStone@45283ce2" "maxZ" 1.0
          "maxY" 1.0 "maxX" 1.0 "minZ" 0.0 "minY" 0.0 "minX" 0.0
          "blockResistance" 1.5 "blockHardness" 0.3 "blockID" 20
          "blockIndexInTexture" 49) 
(DEFBLOCK ORELAPIS "blockName" "oreLapis" "slipperiness" 0.6 "blockMaterial"
          "net.minecraft.src.Material@46fbb2c1" "blockParticleGravity" 1.0
          "stepSound " "net.minecraft.src.StepSound@1698c449" "maxZ" 1.0 "maxY"
          1.0 "maxX" 1.0 "minZ" 0.0 "minY" 0.0 "minX" 0.0 "blockResistance"
          15.0 "blockHardness" 3.0 "blockID" 21 "blockIndexInTexture" 160) 
(DEFBLOCK BLOCKLAPIS "blockName" "blockLapis" "slipperiness" 0.6
          "blockMaterial" "net.minecraft.src.Material@46fbb2c1"
          "blockParticleGravity" 1.0 "stepSound "
          "net.minecraft.src.StepSound@1698c449" "maxZ" 1.0 "maxY" 1.0 "maxX"
          1.0 "minZ" 0.0 "minY" 0.0 "minX" 0.0 "blockResistance" 15.0
          "blockHardness" 3.0 "blockID" 22 "blockIndexInTexture" 144) 
(DEFBLOCK DISPENSER "blockName" "dispenser" "slipperiness" 0.6 "blockMaterial"
          "net.minecraft.src.Material@46fbb2c1" "blockParticleGravity" 1.0
          "stepSound " "net.minecraft.src.StepSound@1698c449" "maxZ" 1.0 "maxY"
          1.0 "maxX" 1.0 "minZ" 0.0 "minY" 0.0 "minX" 0.0 "blockResistance"
          17.5 "blockHardness" 3.5 "blockID" 23 "blockIndexInTexture" 45) 
(DEFBLOCK SANDSTONE "blockName" "sandStone" "slipperiness" 0.6 "blockMaterial"
          "net.minecraft.src.Material@46fbb2c1" "blockParticleGravity" 1.0
          "stepSound " "net.minecraft.src.StepSound@1698c449" "maxZ" 1.0 "maxY"
          1.0 "maxX" 1.0 "minZ" 0.0 "minY" 0.0 "minX" 0.0 "blockResistance" 4.0
          "blockHardness" 0.8 "blockID" 24 "blockIndexInTexture" 192) 
(DEFBLOCK MUSICBLOCK "blockName" "musicBlock" "slipperiness" 0.6
          "blockMaterial" "net.minecraft.src.Material@69663380"
          "blockParticleGravity" 1.0 "stepSound "
          "net.minecraft.src.StepSound@28c97a5" "maxZ" 1.0 "maxY" 1.0 "maxX"
          1.0 "minZ" 0.0 "minY" 0.0 "minX" 0.0 "blockResistance" 4.0
          "blockHardness" 0.8 "blockID" 25 "blockIndexInTexture" 74) 
(DEFBLOCK BED "blockName" "bed" "slipperiness" 0.6 "blockMaterial"
          "net.minecraft.src.Material@2077d4de" "blockParticleGravity" 1.0
          "stepSound " "net.minecraft.src.StepSound@28c97a5" "maxZ" 1.0 "maxY"
          0.5625 "maxX" 1.0 "minZ" 0.0 "minY" 0.0 "minX" 0.0 "blockResistance"
          1.0 "blockHardness" 0.2 "blockID" 26 "blockIndexInTexture" 134) 
(DEFBLOCK GOLDENRAIL "blockName" "goldenRail" "slipperiness" 0.6
          "blockMaterial" "net.minecraft.src.MaterialLogic@7591083d"
          "blockParticleGravity" 1.0 "stepSound "
          "net.minecraft.src.StepSound@77a567e1" "maxZ" 1.0 "maxY" 0.125 "maxX"
          1.0 "minZ" 0.0 "minY" 0.0 "minX" 0.0 "blockResistance" 3.5
          "blockHardness" 0.7 "blockID" 27 "blockIndexInTexture" 179) 
(DEFBLOCK DETECTORRAIL "blockName" "detectorRail" "slipperiness" 0.6
          "blockMaterial" "net.minecraft.src.MaterialLogic@7591083d"
          "blockParticleGravity" 1.0 "stepSound "
          "net.minecraft.src.StepSound@77a567e1" "maxZ" 1.0 "maxY" 0.125 "maxX"
          1.0 "minZ" 0.0 "minY" 0.0 "minX" 0.0 "blockResistance" 3.5
          "blockHardness" 0.7 "blockID" 28 "blockIndexInTexture" 195) 
(DEFBLOCK PISTONSTICKYBASE "blockName" "pistonStickyBase" "slipperiness" 0.6
          "blockMaterial" "net.minecraft.src.Material@736e9adb"
          "blockParticleGravity" 1.0 "stepSound "
          "net.minecraft.src.StepSound@1698c449" "maxZ" 1.0 "maxY" 1.0 "maxX"
          1.0 "minZ" 0.0 "minY" 0.0 "minX" 0.0 "blockResistance" 2.5
          "blockHardness" 0.5 "blockID" 29 "blockIndexInTexture" 106) 
(DEFBLOCK WEB "blockName" "web" "slipperiness" 0.6 "blockMaterial"
          "net.minecraft.src.Material@6d21714c" "blockParticleGravity" 1.0
          "stepSound " "net.minecraft.src.StepSound@28c97a5" "maxZ" 1.0 "maxY"
          1.0 "maxX" 1.0 "minZ" 0.0 "minY" 0.0 "minX" 0.0 "blockResistance"
          20.0 "blockHardness" 4.0 "blockID" 30 "blockIndexInTexture" 11) 
(DEFBLOCK TALLGRASS "blockName" "tallgrass" "slipperiness" 0.6 "blockMaterial"
          "net.minecraft.src.MaterialLogic@4459eb14" "blockParticleGravity" 1.0
          "stepSound " "net.minecraft.src.StepSound@5f4da5c3" "maxZ" 0.9 "maxY"
          0.8 "maxX" 0.9 "minZ" 0.099999994 "minY" 0.0 "minX" 0.099999994
          "blockResistance" 0.0 "blockHardness" 0.0 "blockID" 31
          "blockIndexInTexture" 39) 
(DEFBLOCK DEADBUSH "blockName" "deadbush" "slipperiness" 0.6 "blockMaterial"
          "net.minecraft.src.MaterialLogic@4459eb14" "blockParticleGravity" 1.0
          "stepSound " "net.minecraft.src.StepSound@5f4da5c3" "maxZ" 0.9 "maxY"
          0.8 "maxX" 0.9 "minZ" 0.099999994 "minY" 0.0 "minX" 0.099999994
          "blockResistance" 0.0 "blockHardness" 0.0 "blockID" 32
          "blockIndexInTexture" 55) 
(DEFBLOCK PISTONBASE "blockName" "pistonBase" "slipperiness" 0.6
          "blockMaterial" "net.minecraft.src.Material@736e9adb"
          "blockParticleGravity" 1.0 "stepSound "
          "net.minecraft.src.StepSound@1698c449" "maxZ" 1.0 "maxY" 1.0 "maxX"
          1.0 "minZ" 0.0 "minY" 0.0 "minX" 0.0 "blockResistance" 2.5
          "blockHardness" 0.5 "blockID" 33 "blockIndexInTexture" 107) 
(DEFBLOCK NULL "blockName" "null" "slipperiness" 0.6 "blockMaterial"
          "net.minecraft.src.Material@736e9adb" "blockParticleGravity" 1.0
          "stepSound " "net.minecraft.src.StepSound@1698c449" "maxZ" 1.0 "maxY"
          1.0 "maxX" 1.0 "minZ" 0.0 "minY" 0.0 "minX" 0.0 "blockResistance" 2.5
          "blockHardness" 0.5 "blockID" 34 "blockIndexInTexture" 107) 
(DEFBLOCK CLOTH "blockName" "cloth" "slipperiness" 0.6 "blockMaterial"
          "net.minecraft.src.Material@2077d4de" "blockParticleGravity" 1.0
          "stepSound " "net.minecraft.src.StepSound@108c4c35" "maxZ" 1.0 "maxY"
          1.0 "maxX" 1.0 "minZ" 0.0 "minY" 0.0 "minX" 0.0 "blockResistance" 4.0
          "blockHardness" 0.8 "blockID" 35 "blockIndexInTexture" 64) 
(DEFBLOCK NULL "blockName" "null" "slipperiness" 0.6 "blockMaterial"
          "net.minecraft.src.Material@736e9adb" "blockParticleGravity" 1.0
          "stepSound " "net.minecraft.src.StepSound@28c97a5" "maxZ" 1.0 "maxY"
          1.0 "maxX" 1.0 "minZ" 0.0 "minY" 0.0 "minX" 0.0 "blockResistance" 0.0
          "blockHardness" -1.0 "blockID" 36 "blockIndexInTexture" 0) 
(DEFBLOCK FLOWER "blockName" "flower" "slipperiness" 0.6 "blockMaterial"
          "net.minecraft.src.MaterialLogic@4459eb14" "blockParticleGravity" 1.0
          "stepSound " "net.minecraft.src.StepSound@5f4da5c3" "maxZ" 0.7 "maxY"
          0.6 "maxX" 0.7 "minZ" 0.3 "minY" 0.0 "minX" 0.3 "blockResistance" 0.0
          "blockHardness" 0.0 "blockID" 37 "blockIndexInTexture" 13) 
(DEFBLOCK ROSE "blockName" "rose" "slipperiness" 0.6 "blockMaterial"
          "net.minecraft.src.MaterialLogic@4459eb14" "blockParticleGravity" 1.0
          "stepSound " "net.minecraft.src.StepSound@5f4da5c3" "maxZ" 0.7 "maxY"
          0.6 "maxX" 0.7 "minZ" 0.3 "minY" 0.0 "minX" 0.3 "blockResistance" 0.0
          "blockHardness" 0.0 "blockID" 38 "blockIndexInTexture" 12) 
(DEFBLOCK MUSHROOM "blockName" "mushroom" "slipperiness" 0.6 "blockMaterial"
          "net.minecraft.src.MaterialLogic@4459eb14" "blockParticleGravity" 1.0
          "stepSound " "net.minecraft.src.StepSound@5f4da5c3" "maxZ" 0.7 "maxY"
          0.4 "maxX" 0.7 "minZ" 0.3 "minY" 0.0 "minX" 0.3 "blockResistance" 0.0
          "blockHardness" 0.0 "blockID" 39 "blockIndexInTexture" 29) 
(DEFBLOCK MUSHROOM "blockName" "mushroom" "slipperiness" 0.6 "blockMaterial"
          "net.minecraft.src.MaterialLogic@4459eb14" "blockParticleGravity" 1.0
          "stepSound " "net.minecraft.src.StepSound@5f4da5c3" "maxZ" 0.7 "maxY"
          0.4 "maxX" 0.7 "minZ" 0.3 "minY" 0.0 "minX" 0.3 "blockResistance" 0.0
          "blockHardness" 0.0 "blockID" 40 "blockIndexInTexture" 28) 
(DEFBLOCK BLOCKGOLD "blockName" "blockGold" "slipperiness" 0.6 "blockMaterial"
          "net.minecraft.src.Material@4ccabbaa" "blockParticleGravity" 1.0
          "stepSound " "net.minecraft.src.StepSound@77a567e1" "maxZ" 1.0 "maxY"
          1.0 "maxX" 1.0 "minZ" 0.0 "minY" 0.0 "minX" 0.0 "blockResistance"
          30.0 "blockHardness" 3.0 "blockID" 41 "blockIndexInTexture" 23) 
(DEFBLOCK BLOCKIRON "blockName" "blockIron" "slipperiness" 0.6 "blockMaterial"
          "net.minecraft.src.Material@4ccabbaa" "blockParticleGravity" 1.0
          "stepSound " "net.minecraft.src.StepSound@77a567e1" "maxZ" 1.0 "maxY"
          1.0 "maxX" 1.0 "minZ" 0.0 "minY" 0.0 "minX" 0.0 "blockResistance"
          30.0 "blockHardness" 5.0 "blockID" 42 "blockIndexInTexture" 22) 
(DEFBLOCK STONESLAB "blockName" "stoneSlab" "slipperiness" 0.6 "blockMaterial"
          "net.minecraft.src.Material@46fbb2c1" "blockParticleGravity" 1.0
          "stepSound " "net.minecraft.src.StepSound@1698c449" "maxZ" 1.0 "maxY"
          1.0 "maxX" 1.0 "minZ" 0.0 "minY" 0.0 "minX" 0.0 "blockResistance"
          30.0 "blockHardness" 2.0 "blockID" 43 "blockIndexInTexture" 6) 
(DEFBLOCK STONESLAB "blockName" "stoneSlab" "slipperiness" 0.6 "blockMaterial"
          "net.minecraft.src.Material@46fbb2c1" "blockParticleGravity" 1.0
          "stepSound " "net.minecraft.src.StepSound@1698c449" "maxZ" 1.0 "maxY"
          0.5 "maxX" 1.0 "minZ" 0.0 "minY" 0.0 "minX" 0.0 "blockResistance"
          30.0 "blockHardness" 2.0 "blockID" 44 "blockIndexInTexture" 6) 
(DEFBLOCK BRICK "blockName" "brick" "slipperiness" 0.6 "blockMaterial"
          "net.minecraft.src.Material@46fbb2c1" "blockParticleGravity" 1.0
          "stepSound " "net.minecraft.src.StepSound@1698c449" "maxZ" 1.0 "maxY"
          1.0 "maxX" 1.0 "minZ" 0.0 "minY" 0.0 "minX" 0.0 "blockResistance"
          30.0 "blockHardness" 2.0 "blockID" 45 "blockIndexInTexture" 7) 
(DEFBLOCK TNT "blockName" "tnt" "slipperiness" 0.6 "blockMaterial"
          "net.minecraft.src.Material@4bf558aa" "blockParticleGravity" 1.0
          "stepSound " "net.minecraft.src.StepSound@5f4da5c3" "maxZ" 1.0 "maxY"
          1.0 "maxX" 1.0 "minZ" 0.0 "minY" 0.0 "minX" 0.0 "blockResistance" 0.0
          "blockHardness" 0.0 "blockID" 46 "blockIndexInTexture" 8) 
(DEFBLOCK BOOKSHELF "blockName" "bookshelf" "slipperiness" 0.6 "blockMaterial"
          "net.minecraft.src.Material@69663380" "blockParticleGravity" 1.0
          "stepSound " "net.minecraft.src.StepSound@5b37e0d2" "maxZ" 1.0 "maxY"
          1.0 "maxX" 1.0 "minZ" 0.0 "minY" 0.0 "minX" 0.0 "blockResistance" 7.5
          "blockHardness" 1.5 "blockID" 47 "blockIndexInTexture" 35) 
(DEFBLOCK STONEMOSS "blockName" "stoneMoss" "slipperiness" 0.6 "blockMaterial"
          "net.minecraft.src.Material@46fbb2c1" "blockParticleGravity" 1.0
          "stepSound " "net.minecraft.src.StepSound@1698c449" "maxZ" 1.0 "maxY"
          1.0 "maxX" 1.0 "minZ" 0.0 "minY" 0.0 "minX" 0.0 "blockResistance"
          30.0 "blockHardness" 2.0 "blockID" 48 "blockIndexInTexture" 36) 
(DEFBLOCK OBSIDIAN "blockName" "obsidian" "slipperiness" 0.6 "blockMaterial"
          "net.minecraft.src.Material@46fbb2c1" "blockParticleGravity" 1.0
          "stepSound " "net.minecraft.src.StepSound@1698c449" "maxZ" 1.0 "maxY"
          1.0 "maxX" 1.0 "minZ" 0.0 "minY" 0.0 "minX" 0.0 "blockResistance"
          6000.0 "blockHardness" 10.0 "blockID" 49 "blockIndexInTexture" 37) 
(DEFBLOCK TORCH "blockName" "torch" "slipperiness" 0.6 "blockMaterial"
          "net.minecraft.src.MaterialLogic@7591083d" "blockParticleGravity" 1.0
          "stepSound " "net.minecraft.src.StepSound@5b37e0d2" "maxZ" 1.0 "maxY"
          1.0 "maxX" 1.0 "minZ" 0.0 "minY" 0.0 "minX" 0.0 "blockResistance" 0.0
          "blockHardness" 0.0 "blockID" 50 "blockIndexInTexture" 80) 
(DEFBLOCK FIRE "blockName" "fire" "slipperiness" 0.6 "blockMaterial"
          "net.minecraft.src.MaterialTransparent@2d38eb89"
          "blockParticleGravity" 1.0 "stepSound "
          "net.minecraft.src.StepSound@5b37e0d2" "maxZ" 1.0 "maxY" 1.0 "maxX"
          1.0 "minZ" 0.0 "minY" 0.0 "minX" 0.0 "blockResistance" 0.0
          "blockHardness" 0.0 "blockID" 51 "blockIndexInTexture" 31) 
(DEFBLOCK MOBSPAWNER "blockName" "mobSpawner" "slipperiness" 0.6
          "blockMaterial" "net.minecraft.src.Material@46fbb2c1"
          "blockParticleGravity" 1.0 "stepSound "
          "net.minecraft.src.StepSound@77a567e1" "maxZ" 1.0 "maxY" 1.0 "maxX"
          1.0 "minZ" 0.0 "minY" 0.0 "minX" 0.0 "blockResistance" 25.0
          "blockHardness" 5.0 "blockID" 52 "blockIndexInTexture" 65) 
(DEFBLOCK STAIRSWOOD "blockName" "stairsWood" "slipperiness" 0.6
          "blockMaterial" "net.minecraft.src.Material@69663380"
          "blockParticleGravity" 1.0 "stepSound "
          "net.minecraft.src.StepSound@5b37e0d2" "maxZ" 1.0 "maxY" 1.0 "maxX"
          1.0 "minZ" 0.0 "minY" 0.0 "minX" 0.0 "blockResistance" 15.0
          "blockHardness" 2.0 "blockID" 53 "blockIndexInTexture" 4) 
(DEFBLOCK CHEST "blockName" "chest" "slipperiness" 0.6 "blockMaterial"
          "net.minecraft.src.Material@69663380" "blockParticleGravity" 1.0
          "stepSound " "net.minecraft.src.StepSound@5b37e0d2" "maxZ" 1.0 "maxY"
          1.0 "maxX" 1.0 "minZ" 0.0 "minY" 0.0 "minX" 0.0 "blockResistance"
          12.5 "blockHardness" 2.5 "blockID" 54 "blockIndexInTexture" 26) 
(DEFBLOCK REDSTONEDUST "blockName" "redstoneDust" "slipperiness" 0.6
          "blockMaterial" "net.minecraft.src.MaterialLogic@7591083d"
          "blockParticleGravity" 1.0 "stepSound "
          "net.minecraft.src.StepSound@28c97a5" "maxZ" 1.0 "maxY" 0.0625 "maxX"
          1.0 "minZ" 0.0 "minY" 0.0 "minX" 0.0 "blockResistance" 0.0
          "blockHardness" 0.0 "blockID" 55 "blockIndexInTexture" 164) 
(DEFBLOCK OREDIAMOND "blockName" "oreDiamond" "slipperiness" 0.6
          "blockMaterial" "net.minecraft.src.Material@46fbb2c1"
          "blockParticleGravity" 1.0 "stepSound "
          "net.minecraft.src.StepSound@1698c449" "maxZ" 1.0 "maxY" 1.0 "maxX"
          1.0 "minZ" 0.0 "minY" 0.0 "minX" 0.0 "blockResistance" 15.0
          "blockHardness" 3.0 "blockID" 56 "blockIndexInTexture" 50) 
(DEFBLOCK BLOCKDIAMOND "blockName" "blockDiamond" "slipperiness" 0.6
          "blockMaterial" "net.minecraft.src.Material@4ccabbaa"
          "blockParticleGravity" 1.0 "stepSound "
          "net.minecraft.src.StepSound@77a567e1" "maxZ" 1.0 "maxY" 1.0 "maxX"
          1.0 "minZ" 0.0 "minY" 0.0 "minX" 0.0 "blockResistance" 30.0
          "blockHardness" 5.0 "blockID" 57 "blockIndexInTexture" 24) 
(DEFBLOCK WORKBENCH "blockName" "workbench" "slipperiness" 0.6 "blockMaterial"
          "net.minecraft.src.Material@69663380" "blockParticleGravity" 1.0
          "stepSound " "net.minecraft.src.StepSound@5b37e0d2" "maxZ" 1.0 "maxY"
          1.0 "maxX" 1.0 "minZ" 0.0 "minY" 0.0 "minX" 0.0 "blockResistance"
          12.5 "blockHardness" 2.5 "blockID" 58 "blockIndexInTexture" 59) 
(DEFBLOCK CROPS "blockName" "crops" "slipperiness" 0.6 "blockMaterial"
          "net.minecraft.src.MaterialLogic@4459eb14" "blockParticleGravity" 1.0
          "stepSound " "net.minecraft.src.StepSound@5f4da5c3" "maxZ" 1.0 "maxY"
          0.25 "maxX" 1.0 "minZ" 0.0 "minY" 0.0 "minX" 0.0 "blockResistance"
          0.0 "blockHardness" 0.0 "blockID" 59 "blockIndexInTexture" 88) 
(DEFBLOCK FARMLAND "blockName" "farmland" "slipperiness" 0.6 "blockMaterial"
          "net.minecraft.src.Material@443b7951" "blockParticleGravity" 1.0
          "stepSound " "net.minecraft.src.StepSound@14514713" "maxZ" 1.0 "maxY"
          0.9375 "maxX" 1.0 "minZ" 0.0 "minY" 0.0 "minX" 0.0 "blockResistance"
          3.0 "blockHardness" 0.6 "blockID" 60 "blockIndexInTexture" 87) 
(DEFBLOCK FURNACE "blockName" "furnace" "slipperiness" 0.6 "blockMaterial"
          "net.minecraft.src.Material@46fbb2c1" "blockParticleGravity" 1.0
          "stepSound " "net.minecraft.src.StepSound@1698c449" "maxZ" 1.0 "maxY"
          1.0 "maxX" 1.0 "minZ" 0.0 "minY" 0.0 "minX" 0.0 "blockResistance"
          17.5 "blockHardness" 3.5 "blockID" 61 "blockIndexInTexture" 45) 
(DEFBLOCK FURNACE "blockName" "furnace" "slipperiness" 0.6 "blockMaterial"
          "net.minecraft.src.Material@46fbb2c1" "blockParticleGravity" 1.0
          "stepSound " "net.minecraft.src.StepSound@1698c449" "maxZ" 1.0 "maxY"
          1.0 "maxX" 1.0 "minZ" 0.0 "minY" 0.0 "minX" 0.0 "blockResistance"
          17.5 "blockHardness" 3.5 "blockID" 62 "blockIndexInTexture" 45) 
(DEFBLOCK SIGN "blockName" "sign" "slipperiness" 0.6 "blockMaterial"
          "net.minecraft.src.Material@69663380" "blockParticleGravity" 1.0
          "stepSound " "net.minecraft.src.StepSound@5b37e0d2" "maxZ" 0.75
          "maxY" 1.0 "maxX" 0.75 "minZ" 0.25 "minY" 0.0 "minX" 0.25
          "blockResistance" 5.0 "blockHardness" 1.0 "blockID" 63
          "blockIndexInTexture" 4) 
(DEFBLOCK DOORWOOD "blockName" "doorWood" "slipperiness" 0.6 "blockMaterial"
          "net.minecraft.src.Material@69663380" "blockParticleGravity" 1.0
          "stepSound " "net.minecraft.src.StepSound@5b37e0d2" "maxZ" 1.0 "maxY"
          1.0 "maxX" 1.0 "minZ" 0.0 "minY" 0.0 "minX" 0.0 "blockResistance"
          15.0 "blockHardness" 3.0 "blockID" 64 "blockIndexInTexture" 97) 
(DEFBLOCK LADDER "blockName" "ladder" "slipperiness" 0.6 "blockMaterial"
          "net.minecraft.src.MaterialLogic@7591083d" "blockParticleGravity" 1.0
          "stepSound " "net.minecraft.src.StepSound@5b37e0d2" "maxZ" 1.0 "maxY"
          1.0 "maxX" 1.0 "minZ" 0.0 "minY" 0.0 "minX" 0.0 "blockResistance" 2.0
          "blockHardness" 0.4 "blockID" 65 "blockIndexInTexture" 83) 
(DEFBLOCK RAIL "blockName" "rail" "slipperiness" 0.6 "blockMaterial"
          "net.minecraft.src.MaterialLogic@7591083d" "blockParticleGravity" 1.0
          "stepSound " "net.minecraft.src.StepSound@77a567e1" "maxZ" 1.0 "maxY"
          0.125 "maxX" 1.0 "minZ" 0.0 "minY" 0.0 "minX" 0.0 "blockResistance"
          3.5 "blockHardness" 0.7 "blockID" 66 "blockIndexInTexture" 128) 
(DEFBLOCK STAIRSSTONE "blockName" "stairsStone" "slipperiness" 0.6
          "blockMaterial" "net.minecraft.src.Material@46fbb2c1"
          "blockParticleGravity" 1.0 "stepSound "
          "net.minecraft.src.StepSound@1698c449" "maxZ" 1.0 "maxY" 1.0 "maxX"
          1.0 "minZ" 0.0 "minY" 0.0 "minX" 0.0 "blockResistance" 30.0
          "blockHardness" 2.0 "blockID" 67 "blockIndexInTexture" 16) 
(DEFBLOCK SIGN "blockName" "sign" "slipperiness" 0.6 "blockMaterial"
          "net.minecraft.src.Material@69663380" "blockParticleGravity" 1.0
          "stepSound " "net.minecraft.src.StepSound@5b37e0d2" "maxZ" 0.75
          "maxY" 1.0 "maxX" 0.75 "minZ" 0.25 "minY" 0.0 "minX" 0.25
          "blockResistance" 5.0 "blockHardness" 1.0 "blockID" 68
          "blockIndexInTexture" 4) 
(DEFBLOCK LEVER "blockName" "lever" "slipperiness" 0.6 "blockMaterial"
          "net.minecraft.src.MaterialLogic@7591083d" "blockParticleGravity" 1.0
          "stepSound " "net.minecraft.src.StepSound@5b37e0d2" "maxZ" 1.0 "maxY"
          1.0 "maxX" 1.0 "minZ" 0.0 "minY" 0.0 "minX" 0.0 "blockResistance" 2.5
          "blockHardness" 0.5 "blockID" 69 "blockIndexInTexture" 96) 
(DEFBLOCK PRESSUREPLATE "blockName" "pressurePlate" "slipperiness" 0.6
          "blockMaterial" "net.minecraft.src.Material@46fbb2c1"
          "blockParticleGravity" 1.0 "stepSound "
          "net.minecraft.src.StepSound@1698c449" "maxZ" 0.9375 "maxY" 0.03125
          "maxX" 0.9375 "minZ" 0.0625 "minY" 0.0 "minX" 0.0625
          "blockResistance" 2.5 "blockHardness" 0.5 "blockID" 70
          "blockIndexInTexture" 1) 
(DEFBLOCK DOORIRON "blockName" "doorIron" "slipperiness" 0.6 "blockMaterial"
          "net.minecraft.src.Material@4ccabbaa" "blockParticleGravity" 1.0
          "stepSound " "net.minecraft.src.StepSound@77a567e1" "maxZ" 1.0 "maxY"
          1.0 "maxX" 1.0 "minZ" 0.0 "minY" 0.0 "minX" 0.0 "blockResistance"
          25.0 "blockHardness" 5.0 "blockID" 71 "blockIndexInTexture" 98) 
(DEFBLOCK PRESSUREPLATE "blockName" "pressurePlate" "slipperiness" 0.6
          "blockMaterial" "net.minecraft.src.Material@69663380"
          "blockParticleGravity" 1.0 "stepSound "
          "net.minecraft.src.StepSound@5b37e0d2" "maxZ" 0.9375 "maxY" 0.03125
          "maxX" 0.9375 "minZ" 0.0625 "minY" 0.0 "minX" 0.0625
          "blockResistance" 2.5 "blockHardness" 0.5 "blockID" 72
          "blockIndexInTexture" 4) 
(DEFBLOCK OREREDSTONE "blockName" "oreRedstone" "slipperiness" 0.6
          "blockMaterial" "net.minecraft.src.Material@46fbb2c1"
          "blockParticleGravity" 1.0 "stepSound "
          "net.minecraft.src.StepSound@1698c449" "maxZ" 1.0 "maxY" 1.0 "maxX"
          1.0 "minZ" 0.0 "minY" 0.0 "minX" 0.0 "blockResistance" 15.0
          "blockHardness" 3.0 "blockID" 73 "blockIndexInTexture" 51) 
(DEFBLOCK OREREDSTONE "blockName" "oreRedstone" "slipperiness" 0.6
          "blockMaterial" "net.minecraft.src.Material@46fbb2c1"
          "blockParticleGravity" 1.0 "stepSound "
          "net.minecraft.src.StepSound@1698c449" "maxZ" 1.0 "maxY" 1.0 "maxX"
          1.0 "minZ" 0.0 "minY" 0.0 "minX" 0.0 "blockResistance" 15.0
          "blockHardness" 3.0 "blockID" 74 "blockIndexInTexture" 51) 
(DEFBLOCK NOTGATE "blockName" "notGate" "slipperiness" 0.6 "blockMaterial"
          "net.minecraft.src.MaterialLogic@7591083d" "blockParticleGravity" 1.0
          "stepSound " "net.minecraft.src.StepSound@5b37e0d2" "maxZ" 1.0 "maxY"
          1.0 "maxX" 1.0 "minZ" 0.0 "minY" 0.0 "minX" 0.0 "blockResistance" 0.0
          "blockHardness" 0.0 "blockID" 75 "blockIndexInTexture" 115) 
(DEFBLOCK NOTGATE "blockName" "notGate" "slipperiness" 0.6 "blockMaterial"
          "net.minecraft.src.MaterialLogic@7591083d" "blockParticleGravity" 1.0
          "stepSound " "net.minecraft.src.StepSound@5b37e0d2" "maxZ" 1.0 "maxY"
          1.0 "maxX" 1.0 "minZ" 0.0 "minY" 0.0 "minX" 0.0 "blockResistance" 0.0
          "blockHardness" 0.0 "blockID" 76 "blockIndexInTexture" 99) 
(DEFBLOCK BUTTON "blockName" "button" "slipperiness" 0.6 "blockMaterial"
          "net.minecraft.src.MaterialLogic@7591083d" "blockParticleGravity" 1.0
          "stepSound " "net.minecraft.src.StepSound@1698c449" "maxZ" 1.0 "maxY"
          1.0 "maxX" 1.0 "minZ" 0.0 "minY" 0.0 "minX" 0.0 "blockResistance" 2.5
          "blockHardness" 0.5 "blockID" 77 "blockIndexInTexture" 1) 
(DEFBLOCK SNOW "blockName" "snow" "slipperiness" 0.6 "blockMaterial"
          "net.minecraft.src.MaterialLogic@5fa7e7ff" "blockParticleGravity" 1.0
          "stepSound " "net.minecraft.src.StepSound@108c4c35" "maxZ" 1.0 "maxY"
          0.125 "maxX" 1.0 "minZ" 0.0 "minY" 0.0 "minX" 0.0 "blockResistance"
          0.5 "blockHardness" 0.1 "blockID" 78 "blockIndexInTexture" 66) 
(DEFBLOCK ICE "blockName" "ice" "slipperiness" 0.98 "blockMaterial"
          "net.minecraft.src.Material@4629104a" "blockParticleGravity" 1.0
          "stepSound " "net.minecraft.src.StepSoundStone@45283ce2" "maxZ" 1.0
          "maxY" 1.0 "maxX" 1.0 "minZ" 0.0 "minY" 0.0 "minX" 0.0
          "blockResistance" 2.5 "blockHardness" 0.5 "blockID" 79
          "blockIndexInTexture" 67) 
(DEFBLOCK SNOW "blockName" "snow" "slipperiness" 0.6 "blockMaterial"
          "net.minecraft.src.Material@27f8302d" "blockParticleGravity" 1.0
          "stepSound " "net.minecraft.src.StepSound@108c4c35" "maxZ" 1.0 "maxY"
          1.0 "maxX" 1.0 "minZ" 0.0 "minY" 0.0 "minX" 0.0 "blockResistance" 1.0
          "blockHardness" 0.2 "blockID" 80 "blockIndexInTexture" 66) 
(DEFBLOCK CACTUS "blockName" "cactus" "slipperiness" 0.6 "blockMaterial"
          "net.minecraft.src.Material@4d76f3f8" "blockParticleGravity" 1.0
          "stepSound " "net.minecraft.src.StepSound@108c4c35" "maxZ" 1.0 "maxY"
          1.0 "maxX" 1.0 "minZ" 0.0 "minY" 0.0 "minX" 0.0 "blockResistance" 2.0
          "blockHardness" 0.4 "blockID" 81 "blockIndexInTexture" 70) 
(DEFBLOCK CLAY "blockName" "clay" "slipperiness" 0.6 "blockMaterial"
          "net.minecraft.src.Material@2d8e6db6" "blockParticleGravity" 1.0
          "stepSound " "net.minecraft.src.StepSound@14514713" "maxZ" 1.0 "maxY"
          1.0 "maxX" 1.0 "minZ" 0.0 "minY" 0.0 "minX" 0.0 "blockResistance" 3.0
          "blockHardness" 0.6 "blockID" 82 "blockIndexInTexture" 72) 
(DEFBLOCK REEDS "blockName" "reeds" "slipperiness" 0.6 "blockMaterial"
          "net.minecraft.src.MaterialLogic@4459eb14" "blockParticleGravity" 1.0
          "stepSound " "net.minecraft.src.StepSound@5f4da5c3" "maxZ" 0.875
          "maxY" 1.0 "maxX" 0.875 "minZ" 0.125 "minY" 0.0 "minX" 0.125
          "blockResistance" 0.0 "blockHardness" 0.0 "blockID" 83
          "blockIndexInTexture" 73) 
(DEFBLOCK JUKEBOX "blockName" "jukebox" "slipperiness" 0.6 "blockMaterial"
          "net.minecraft.src.Material@69663380" "blockParticleGravity" 1.0
          "stepSound " "net.minecraft.src.StepSound@1698c449" "maxZ" 1.0 "maxY"
          1.0 "maxX" 1.0 "minZ" 0.0 "minY" 0.0 "minX" 0.0 "blockResistance"
          30.0 "blockHardness" 2.0 "blockID" 84 "blockIndexInTexture" 74) 
(DEFBLOCK FENCE "blockName" "fence" "slipperiness" 0.6 "blockMaterial"
          "net.minecraft.src.Material@69663380" "blockParticleGravity" 1.0
          "stepSound " "net.minecraft.src.StepSound@5b37e0d2" "maxZ" 1.0 "maxY"
          1.0 "maxX" 1.0 "minZ" 0.0 "minY" 0.0 "minX" 0.0 "blockResistance"
          15.0 "blockHardness" 2.0 "blockID" 85 "blockIndexInTexture" 4) 
(DEFBLOCK PUMPKIN "blockName" "pumpkin" "slipperiness" 0.6 "blockMaterial"
          "net.minecraft.src.Material@23ab930d" "blockParticleGravity" 1.0
          "stepSound " "net.minecraft.src.StepSound@5b37e0d2" "maxZ" 1.0 "maxY"
          1.0 "maxX" 1.0 "minZ" 0.0 "minY" 0.0 "minX" 0.0 "blockResistance" 5.0
          "blockHardness" 1.0 "blockID" 86 "blockIndexInTexture" 102) 
(DEFBLOCK HELLROCK "blockName" "hellrock" "slipperiness" 0.6 "blockMaterial"
          "net.minecraft.src.Material@46fbb2c1" "blockParticleGravity" 1.0
          "stepSound " "net.minecraft.src.StepSound@1698c449" "maxZ" 1.0 "maxY"
          1.0 "maxX" 1.0 "minZ" 0.0 "minY" 0.0 "minX" 0.0 "blockResistance" 2.0
          "blockHardness" 0.4 "blockID" 87 "blockIndexInTexture" 103) 
(DEFBLOCK HELLSAND "blockName" "hellsand" "slipperiness" 0.6 "blockMaterial"
          "net.minecraft.src.Material@6d5380c2" "blockParticleGravity" 1.0
          "stepSound " "net.minecraft.src.StepSoundSand@45ff54e6" "maxZ" 1.0
          "maxY" 1.0 "maxX" 1.0 "minZ" 0.0 "minY" 0.0 "minX" 0.0
          "blockResistance" 2.5 "blockHardness" 0.5 "blockID" 88
          "blockIndexInTexture" 104) 
(DEFBLOCK LIGHTGEM "blockName" "lightgem" "slipperiness" 0.6 "blockMaterial"
          "net.minecraft.src.Material@46fbb2c1" "blockParticleGravity" 1.0
          "stepSound " "net.minecraft.src.StepSoundStone@45283ce2" "maxZ" 1.0
          "maxY" 1.0 "maxX" 1.0 "minZ" 0.0 "minY" 0.0 "minX" 0.0
          "blockResistance" 1.5 "blockHardness" 0.3 "blockID" 89
          "blockIndexInTexture" 105) 
(DEFBLOCK PORTAL "blockName" "portal" "slipperiness" 0.6 "blockMaterial"
          "net.minecraft.src.MaterialPortal@4534b60d" "blockParticleGravity"
          1.0 "stepSound " "net.minecraft.src.StepSoundStone@45283ce2" "maxZ"
          1.0 "maxY" 1.0 "maxX" 1.0 "minZ" 0.0 "minY" 0.0 "minX" 0.0
          "blockResistance" 0.0 "blockHardness" -1.0 "blockID" 90
          "blockIndexInTexture" 14) 
(DEFBLOCK LITPUMPKIN "blockName" "litpumpkin" "slipperiness" 0.6
          "blockMaterial" "net.minecraft.src.Material@23ab930d"
          "blockParticleGravity" 1.0 "stepSound "
          "net.minecraft.src.StepSound@5b37e0d2" "maxZ" 1.0 "maxY" 1.0 "maxX"
          1.0 "minZ" 0.0 "minY" 0.0 "minX" 0.0 "blockResistance" 5.0
          "blockHardness" 1.0 "blockID" 91 "blockIndexInTexture" 102) 
(DEFBLOCK CAKE "blockName" "cake" "slipperiness" 0.6 "blockMaterial"
          "net.minecraft.src.Material@3fa77460" "blockParticleGravity" 1.0
          "stepSound " "net.minecraft.src.StepSound@108c4c35" "maxZ" 1.0 "maxY"
          1.0 "maxX" 1.0 "minZ" 0.0 "minY" 0.0 "minX" 0.0 "blockResistance" 2.5
          "blockHardness" 0.5 "blockID" 92 "blockIndexInTexture" 121) 
(DEFBLOCK DIODE "blockName" "diode" "slipperiness" 0.6 "blockMaterial"
          "net.minecraft.src.MaterialLogic@7591083d" "blockParticleGravity" 1.0
          "stepSound " "net.minecraft.src.StepSound@5b37e0d2" "maxZ" 1.0 "maxY"
          0.125 "maxX" 1.0 "minZ" 0.0 "minY" 0.0 "minX" 0.0 "blockResistance"
          0.0 "blockHardness" 0.0 "blockID" 93 "blockIndexInTexture" 6) 
(DEFBLOCK DIODE "blockName" "diode" "slipperiness" 0.6 "blockMaterial"
          "net.minecraft.src.MaterialLogic@7591083d" "blockParticleGravity" 1.0
          "stepSound " "net.minecraft.src.StepSound@5b37e0d2" "maxZ" 1.0 "maxY"
          0.125 "maxX" 1.0 "minZ" 0.0 "minY" 0.0 "minX" 0.0 "blockResistance"
          0.0 "blockHardness" 0.0 "blockID" 94 "blockIndexInTexture" 6) 
(DEFBLOCK LOCKEDCHEST "blockName" "lockedchest" "slipperiness" 0.6
          "blockMaterial" "net.minecraft.src.Material@69663380"
          "blockParticleGravity" 1.0 "stepSound "
          "net.minecraft.src.StepSound@5b37e0d2" "maxZ" 1.0 "maxY" 1.0 "maxX"
          1.0 "minZ" 0.0 "minY" 0.0 "minX" 0.0 "blockResistance" 0.0
          "blockHardness" 0.0 "blockID" 95 "blockIndexInTexture" 26) 
(DEFBLOCK TRAPDOOR "blockName" "trapdoor" "slipperiness" 0.6 "blockMaterial"
          "net.minecraft.src.Material@69663380" "blockParticleGravity" 1.0
          "stepSound " "net.minecraft.src.StepSound@5b37e0d2" "maxZ" 1.0 "maxY"
          1.0 "maxX" 1.0 "minZ" 0.0 "minY" 0.0 "minX" 0.0 "blockResistance"
          15.0 "blockHardness" 3.0 "blockID" 96 "blockIndexInTexture" 84)

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

(defun updateblockname ()
  (setf blockname (make-array totalblockspace :initial-element nil))
  (dotimes (n totalblockspace)
    (aif (aref blockslist n)
      (setf (aref blockname n) (gethash "blockName" it)))))

(defun updateblockindexintexture ()
  (setf blockIndexInTexture (make-array totalblockspace))
  (dotimes (n totalblockspace)
    (aif (aref blockslist n)
      (setf (aref blockIndexInTexture n) (gethash "blockIndexInTexture" it)))))

(updateblockslist)
(updateblockname)
(updateblockindexintexture)
