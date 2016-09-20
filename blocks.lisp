(in-package :sandbox)

(defparameter blocks
  (list
   (list
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
   (list
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
   (list
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
    "blockIndexInTexture" (lambda (x)
			    (case x
			      (0 2)
			      (1 0)
			      (t 3))))
   (list
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
   (list
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
   (list
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
   (list
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
   (list
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
   (list
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
   (list
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
   (list
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
   (list
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
    "blockResistance" 500.0
    "blockHardness" 100.0
    "blockID" 11
    "blockIndexInTexture" 237)
   (list
    "blockName" "sand" 
    "slipperiness" 0.6
    "blockMaterial" "net.minecraft.src.Material@6d5380c2" 
    "blockParticleGravity" 1.0
    "stepSound " "net.minecraft.src.StepSoundSand@45ff54e6" 
    "maxZ" 1.0
    "maxY" 1.0
    "maxX" 1.0
    "minZ" 0.0
    "minY" 0.0
    "minX" 0.0
    "blockResistance" 2.5
    "blockHardness" 0.5
    "blockID" 12
    "blockIndexInTexture" 18)
   (list
    "blockName" "gravel" 
    "slipperiness" 0.6
    "blockMaterial" "net.minecraft.src.Material@6d5380c2" 
    "blockParticleGravity" 1.0
    "stepSound " "net.minecraft.src.StepSound@14514713" 
    "maxZ" 1.0
    "maxY" 1.0
    "maxX" 1.0
    "minZ" 0.0
    "minY" 0.0
    "minX" 0.0
    "blockResistance" 3.0
    "blockHardness" 0.6
    "blockID" 13
    "blockIndexInTexture" 19)
   (list
    "blockName" "oreGold" 
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
    "blockResistance" 15.0
    "blockHardness" 3.0
    "blockID" 14
    "blockIndexInTexture" 32)
   (list
    "blockName" "oreIron" 
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
    "blockResistance" 15.0
    "blockHardness" 3.0
    "blockID" 15
    "blockIndexInTexture" 33)
   (list
    "blockName" "oreCoal" 
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
    "blockResistance" 15.0
    "blockHardness" 3.0
    "blockID" 16
    "blockIndexInTexture" 34)
   (list
    "blockName" "log" 
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
    "blockResistance" 10.0
    "blockHardness" 2.0
    "blockID" 17
    "blockIndexInTexture" 20)
   (list
    "blockName" "leaves" 
    "slipperiness" 0.6
    "blockMaterial" "net.minecraft.src.Material@2328c243" 
    "blockParticleGravity" 1.0
    "stepSound " "net.minecraft.src.StepSound@5f4da5c3" 
    "maxZ" 1.0
    "maxY" 1.0
    "maxX" 1.0
    "minZ" 0.0
    "minY" 0.0
    "minX" 0.0
    "blockResistance" 1.0
    "blockHardness" 0.2
    "blockID" 18
    "blockIndexInTexture" 52)
   (list
    "blockName" "sponge" 
    "slipperiness" 0.6
    "blockMaterial" "net.minecraft.src.Material@bebdb06" 
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
    "blockID" 19
    "blockIndexInTexture" 48)
   (list
    "blockName" "glass" 
    "slipperiness" 0.6
    "blockMaterial" "net.minecraft.src.Material@7a4f0f29" 
    "blockParticleGravity" 1.0
    "stepSound " "net.minecraft.src.StepSoundStone@45283ce2" 
    "maxZ" 1.0
    "maxY" 1.0
    "maxX" 1.0
    "minZ" 0.0
    "minY" 0.0
    "minX" 0.0
    "blockResistance" 1.5
    "blockHardness" 0.3
    "blockID" 20
    "blockIndexInTexture" 49)
   (list
    "blockName" "oreLapis" 
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
    "blockResistance" 15.0
    "blockHardness" 3.0
    "blockID" 21
    "blockIndexInTexture" 160)
   (list
    "blockName" "blockLapis" 
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
    "blockResistance" 15.0
    "blockHardness" 3.0
    "blockID" 22
    "blockIndexInTexture" 144)
   (list
    "blockName" "dispenser" 
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
    "blockResistance" 17.5
    "blockHardness" 3.5
    "blockID" 23
    "blockIndexInTexture" 45)
   (list
    "blockName" "sandStone" 
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
    "blockResistance" 4.0
    "blockHardness" 0.8
    "blockID" 24
    "blockIndexInTexture" 192)
   (list
    "blockName" "musicBlock" 
    "slipperiness" 0.6
    "blockMaterial" "net.minecraft.src.Material@69663380" 
    "blockParticleGravity" 1.0
    "stepSound " "net.minecraft.src.StepSound@28c97a5" 
    "maxZ" 1.0
    "maxY" 1.0
    "maxX" 1.0
    "minZ" 0.0
    "minY" 0.0
    "minX" 0.0
    "blockResistance" 4.0
    "blockHardness" 0.8
    "blockID" 25
    "blockIndexInTexture" 74)
   (list
    "blockName" "bed" 
    "slipperiness" 0.6
    "blockMaterial" "net.minecraft.src.Material@2077d4de" 
    "blockParticleGravity" 1.0
    "stepSound " "net.minecraft.src.StepSound@28c97a5" 
    "maxZ" 1.0
    "maxY" 0.5625
    "maxX" 1.0
    "minZ" 0.0
    "minY" 0.0
    "minX" 0.0
    "blockResistance" 1.0
    "blockHardness" 0.2
    "blockID" 26
    "blockIndexInTexture" 134)
   (list
    "blockName" "goldenRail" 
    "slipperiness" 0.6
    "blockMaterial" "net.minecraft.src.MaterialLogic@7591083d" 
    "blockParticleGravity" 1.0
    "stepSound " "net.minecraft.src.StepSound@77a567e1" 
    "maxZ" 1.0
    "maxY" 0.125
    "maxX" 1.0
    "minZ" 0.0
    "minY" 0.0
    "minX" 0.0
    "blockResistance" 3.5
    "blockHardness" 0.7
    "blockID" 27
    "blockIndexInTexture" 179)
   (list
    "blockName" "detectorRail" 
    "slipperiness" 0.6
    "blockMaterial" "net.minecraft.src.MaterialLogic@7591083d" 
    "blockParticleGravity" 1.0
    "stepSound " "net.minecraft.src.StepSound@77a567e1" 
    "maxZ" 1.0
    "maxY" 0.125
    "maxX" 1.0
    "minZ" 0.0
    "minY" 0.0
    "minX" 0.0
    "blockResistance" 3.5
    "blockHardness" 0.7
    "blockID" 28
    "blockIndexInTexture" 195)
   (list
    "blockName" "pistonStickyBase" 
    "slipperiness" 0.6
    "blockMaterial" "net.minecraft.src.Material@736e9adb" 
    "blockParticleGravity" 1.0
    "stepSound " "net.minecraft.src.StepSound@1698c449" 
    "maxZ" 1.0
    "maxY" 1.0
    "maxX" 1.0
    "minZ" 0.0
    "minY" 0.0
    "minX" 0.0
    "blockResistance" 2.5
    "blockHardness" 0.5
    "blockID" 29
    "blockIndexInTexture" 106)
   (list
    "blockName" "web" 
    "slipperiness" 0.6
    "blockMaterial" "net.minecraft.src.Material@6d21714c" 
    "blockParticleGravity" 1.0
    "stepSound " "net.minecraft.src.StepSound@28c97a5" 
    "maxZ" 1.0
    "maxY" 1.0
    "maxX" 1.0
    "minZ" 0.0
    "minY" 0.0
    "minX" 0.0
    "blockResistance" 20.0
    "blockHardness" 4.0
    "blockID" 30
    "blockIndexInTexture" 11)
   (list
    "blockName" "tallgrass" 
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
    "blockID" 31
    "blockIndexInTexture" 39)
   (list
    "blockName" "deadbush" 
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
    "blockID" 32
    "blockIndexInTexture" 55)
   (list
    "blockName" "pistonBase" 
    "slipperiness" 0.6
    "blockMaterial" "net.minecraft.src.Material@736e9adb" 
    "blockParticleGravity" 1.0
    "stepSound " "net.minecraft.src.StepSound@1698c449" 
    "maxZ" 1.0
    "maxY" 1.0
    "maxX" 1.0
    "minZ" 0.0
    "minY" 0.0
    "minX" 0.0
    "blockResistance" 2.5
    "blockHardness" 0.5
    "blockID" 33
    "blockIndexInTexture" 107)
   (list
    "blockName" "null" 
    "slipperiness" 0.6
    "blockMaterial" "net.minecraft.src.Material@736e9adb" 
    "blockParticleGravity" 1.0
    "stepSound " "net.minecraft.src.StepSound@1698c449" 
    "maxZ" 1.0
    "maxY" 1.0
    "maxX" 1.0
    "minZ" 0.0
    "minY" 0.0
    "minX" 0.0
    "blockResistance" 2.5
    "blockHardness" 0.5
    "blockID" 34
    "blockIndexInTexture" 107)
   (list
    "blockName" "cloth" 
    "slipperiness" 0.6
    "blockMaterial" "net.minecraft.src.Material@2077d4de" 
    "blockParticleGravity" 1.0
    "stepSound " "net.minecraft.src.StepSound@108c4c35" 
    "maxZ" 1.0
    "maxY" 1.0
    "maxX" 1.0
    "minZ" 0.0
    "minY" 0.0
    "minX" 0.0
    "blockResistance" 4.0
    "blockHardness" 0.8
    "blockID" 35
    "blockIndexInTexture" 64)
   (list
    "blockName" "null" 
    "slipperiness" 0.6
    "blockMaterial" "net.minecraft.src.Material@736e9adb" 
    "blockParticleGravity" 1.0
    "stepSound " "net.minecraft.src.StepSound@28c97a5" 
    "maxZ" 1.0
    "maxY" 1.0
    "maxX" 1.0
    "minZ" 0.0
    "minY" 0.0
    "minX" 0.0
    "blockResistance" 0.0
    "blockHardness" -1.0
    "blockID" 36
    "blockIndexInTexture" 0)
   (list
    "blockName" "flower" 
    "slipperiness" 0.6
    "blockMaterial" "net.minecraft.src.MaterialLogic@4459eb14" 
    "blockParticleGravity" 1.0
    "stepSound " "net.minecraft.src.StepSound@5f4da5c3" 
    "maxZ" 0.699999988079071
    "maxY" 0.6000000238418579
    "maxX" 0.699999988079071
    "minZ" 0.30000001192092896
    "minY" 0.0
    "minX" 0.30000001192092896
    "blockResistance" 0.0
    "blockHardness" 0.0
    "blockID" 37
    "blockIndexInTexture" 13)
   (list
    "blockName" "rose" 
    "slipperiness" 0.6
    "blockMaterial" "net.minecraft.src.MaterialLogic@4459eb14" 
    "blockParticleGravity" 1.0
    "stepSound " "net.minecraft.src.StepSound@5f4da5c3" 
    "maxZ" 0.699999988079071
    "maxY" 0.6000000238418579
    "maxX" 0.699999988079071
    "minZ" 0.30000001192092896
    "minY" 0.0
    "minX" 0.30000001192092896
    "blockResistance" 0.0
    "blockHardness" 0.0
    "blockID" 38
    "blockIndexInTexture" 12)
   (list
    "blockName" "mushroom" 
    "slipperiness" 0.6
    "blockMaterial" "net.minecraft.src.MaterialLogic@4459eb14" 
    "blockParticleGravity" 1.0
    "stepSound " "net.minecraft.src.StepSound@5f4da5c3" 
    "maxZ" 0.699999988079071
    "maxY" 0.4000000059604645
    "maxX" 0.699999988079071
    "minZ" 0.30000001192092896
    "minY" 0.0
    "minX" 0.30000001192092896
    "blockResistance" 0.0
    "blockHardness" 0.0
    "blockID" 39
    "blockIndexInTexture" 29)
   (list
    "blockName" "mushroom" 
    "slipperiness" 0.6
    "blockMaterial" "net.minecraft.src.MaterialLogic@4459eb14" 
    "blockParticleGravity" 1.0
    "stepSound " "net.minecraft.src.StepSound@5f4da5c3" 
    "maxZ" 0.699999988079071
    "maxY" 0.4000000059604645
    "maxX" 0.699999988079071
    "minZ" 0.30000001192092896
    "minY" 0.0
    "minX" 0.30000001192092896
    "blockResistance" 0.0
    "blockHardness" 0.0
    "blockID" 40
    "blockIndexInTexture" 28)
   (list
    "blockName" "blockGold" 
    "slipperiness" 0.6
    "blockMaterial" "net.minecraft.src.Material@4ccabbaa" 
    "blockParticleGravity" 1.0
    "stepSound " "net.minecraft.src.StepSound@77a567e1" 
    "maxZ" 1.0
    "maxY" 1.0
    "maxX" 1.0
    "minZ" 0.0
    "minY" 0.0
    "minX" 0.0
    "blockResistance" 30.0
    "blockHardness" 3.0
    "blockID" 41
    "blockIndexInTexture" 23)
   (list
    "blockName" "blockIron" 
    "slipperiness" 0.6
    "blockMaterial" "net.minecraft.src.Material@4ccabbaa" 
    "blockParticleGravity" 1.0
    "stepSound " "net.minecraft.src.StepSound@77a567e1" 
    "maxZ" 1.0
    "maxY" 1.0
    "maxX" 1.0
    "minZ" 0.0
    "minY" 0.0
    "minX" 0.0
    "blockResistance" 30.0
    "blockHardness" 5.0
    "blockID" 42
    "blockIndexInTexture" 22)
   (list
    "blockName" "stoneSlab" 
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
    "blockID" 43
    "blockIndexInTexture" 6)
   (list
    "blockName" "stoneSlab" 
    "slipperiness" 0.6
    "blockMaterial" "net.minecraft.src.Material@46fbb2c1" 
    "blockParticleGravity" 1.0
    "stepSound " "net.minecraft.src.StepSound@1698c449" 
    "maxZ" 1.0
    "maxY" 0.5
    "maxX" 1.0
    "minZ" 0.0
    "minY" 0.0
    "minX" 0.0
    "blockResistance" 30.0
    "blockHardness" 2.0
    "blockID" 44
    "blockIndexInTexture" 6)
   (list
    "blockName" "brick" 
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
    "blockID" 45
    "blockIndexInTexture" 7)
   (list
    "blockName" "tnt" 
    "slipperiness" 0.6
    "blockMaterial" "net.minecraft.src.Material@4bf558aa" 
    "blockParticleGravity" 1.0
    "stepSound " "net.minecraft.src.StepSound@5f4da5c3" 
    "maxZ" 1.0
    "maxY" 1.0
    "maxX" 1.0
    "minZ" 0.0
    "minY" 0.0
    "minX" 0.0
    "blockResistance" 0.0
    "blockHardness" 0.0
    "blockID" 46
    "blockIndexInTexture" 8)
   (list
    "blockName" "bookshelf" 
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
    "blockResistance" 7.5
    "blockHardness" 1.5
    "blockID" 47
    "blockIndexInTexture" 35)
   (list
    "blockName" "stoneMoss" 
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
    "blockID" 48
    "blockIndexInTexture" 36)
   (list
    "blockName" "obsidian" 
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
    "blockResistance" 6000.0
    "blockHardness" 10.0
    "blockID" 49
    "blockIndexInTexture" 37)
   (list
    "blockName" "torch" 
    "slipperiness" 0.6
    "blockMaterial" "net.minecraft.src.MaterialLogic@7591083d" 
    "blockParticleGravity" 1.0
    "stepSound " "net.minecraft.src.StepSound@5b37e0d2" 
    "maxZ" 1.0
    "maxY" 1.0
    "maxX" 1.0
    "minZ" 0.0
    "minY" 0.0
    "minX" 0.0
    "blockResistance" 0.0
    "blockHardness" 0.0
    "blockID" 50
    "blockIndexInTexture" 80)
   (list
    "blockName" "fire" 
    "slipperiness" 0.6
    "blockMaterial" "net.minecraft.src.MaterialTransparent@2d38eb89" 
    "blockParticleGravity" 1.0
    "stepSound " "net.minecraft.src.StepSound@5b37e0d2" 
    "maxZ" 1.0
    "maxY" 1.0
    "maxX" 1.0
    "minZ" 0.0
    "minY" 0.0
    "minX" 0.0
    "blockResistance" 0.0
    "blockHardness" 0.0
    "blockID" 51
    "blockIndexInTexture" 31)
   (list
    "blockName" "mobSpawner" 
    "slipperiness" 0.6
    "blockMaterial" "net.minecraft.src.Material@46fbb2c1" 
    "blockParticleGravity" 1.0
    "stepSound " "net.minecraft.src.StepSound@77a567e1" 
    "maxZ" 1.0
    "maxY" 1.0
    "maxX" 1.0
    "minZ" 0.0
    "minY" 0.0
    "minX" 0.0
    "blockResistance" 25.0
    "blockHardness" 5.0
    "blockID" 52
    "blockIndexInTexture" 65)
   (list
    "blockName" "stairsWood" 
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
    "blockID" 53
    "blockIndexInTexture" 4)
   (list
    "blockName" "chest" 
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
    "blockResistance" 12.5
    "blockHardness" 2.5
    "blockID" 54
    "blockIndexInTexture" 26)
   (list
    "blockName" "redstoneDust" 
    "slipperiness" 0.6
    "blockMaterial" "net.minecraft.src.MaterialLogic@7591083d" 
    "blockParticleGravity" 1.0
    "stepSound " "net.minecraft.src.StepSound@28c97a5" 
    "maxZ" 1.0
    "maxY" 0.0625
    "maxX" 1.0
    "minZ" 0.0
    "minY" 0.0
    "minX" 0.0
    "blockResistance" 0.0
    "blockHardness" 0.0
    "blockID" 55
    "blockIndexInTexture" 164)
   (list
    "blockName" "oreDiamond" 
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
    "blockResistance" 15.0
    "blockHardness" 3.0
    "blockID" 56
    "blockIndexInTexture" 50)
   (list
    "blockName" "blockDiamond" 
    "slipperiness" 0.6
    "blockMaterial" "net.minecraft.src.Material@4ccabbaa" 
    "blockParticleGravity" 1.0
    "stepSound " "net.minecraft.src.StepSound@77a567e1" 
    "maxZ" 1.0
    "maxY" 1.0
    "maxX" 1.0
    "minZ" 0.0
    "minY" 0.0
    "minX" 0.0
    "blockResistance" 30.0
    "blockHardness" 5.0
    "blockID" 57
    "blockIndexInTexture" 24)
   (list
    "blockName" "workbench" 
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
    "blockResistance" 12.5
    "blockHardness" 2.5
    "blockID" 58
    "blockIndexInTexture" 59)
   (list
    "blockName" "crops" 
    "slipperiness" 0.6
    "blockMaterial" "net.minecraft.src.MaterialLogic@4459eb14" 
    "blockParticleGravity" 1.0
    "stepSound " "net.minecraft.src.StepSound@5f4da5c3" 
    "maxZ" 1.0
    "maxY" 0.25
    "maxX" 1.0
    "minZ" 0.0
    "minY" 0.0
    "minX" 0.0
    "blockResistance" 0.0
    "blockHardness" 0.0
    "blockID" 59
    "blockIndexInTexture" 88)
   (list
    "blockName" "farmland" 
    "slipperiness" 0.6
    "blockMaterial" "net.minecraft.src.Material@443b7951" 
    "blockParticleGravity" 1.0
    "stepSound " "net.minecraft.src.StepSound@14514713" 
    "maxZ" 1.0
    "maxY" 0.9375
    "maxX" 1.0
    "minZ" 0.0
    "minY" 0.0
    "minX" 0.0
    "blockResistance" 3.0
    "blockHardness" 0.6
    "blockID" 60
    "blockIndexInTexture" 87)
   (list
    "blockName" "furnace" 
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
    "blockResistance" 17.5
    "blockHardness" 3.5
    "blockID" 61
    "blockIndexInTexture" 45)
   (list
    "blockName" "furnace" 
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
    "blockResistance" 17.5
    "blockHardness" 3.5
    "blockID" 62
    "blockIndexInTexture" 45)
   (list
    "blockName" "sign" 
    "slipperiness" 0.6
    "blockMaterial" "net.minecraft.src.Material@69663380" 
    "blockParticleGravity" 1.0
    "stepSound " "net.minecraft.src.StepSound@5b37e0d2" 
    "maxZ" 0.75
    "maxY" 1.0
    "maxX" 0.75
    "minZ" 0.25
    "minY" 0.0
    "minX" 0.25
    "blockResistance" 5.0
    "blockHardness" 1.0
    "blockID" 63
    "blockIndexInTexture" 4)
   (list
    "blockName" "doorWood" 
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
    "blockHardness" 3.0
    "blockID" 64
    "blockIndexInTexture" 97)
   (list
    "blockName" "ladder" 
    "slipperiness" 0.6
    "blockMaterial" "net.minecraft.src.MaterialLogic@7591083d" 
    "blockParticleGravity" 1.0
    "stepSound " "net.minecraft.src.StepSound@5b37e0d2" 
    "maxZ" 1.0
    "maxY" 1.0
    "maxX" 1.0
    "minZ" 0.0
    "minY" 0.0
    "minX" 0.0
    "blockResistance" 2.0
    "blockHardness" 0.4
    "blockID" 65
    "blockIndexInTexture" 83)
   (list
    "blockName" "rail" 
    "slipperiness" 0.6
    "blockMaterial" "net.minecraft.src.MaterialLogic@7591083d" 
    "blockParticleGravity" 1.0
    "stepSound " "net.minecraft.src.StepSound@77a567e1" 
    "maxZ" 1.0
    "maxY" 0.125
    "maxX" 1.0
    "minZ" 0.0
    "minY" 0.0
    "minX" 0.0
    "blockResistance" 3.5
    "blockHardness" 0.7
    "blockID" 66
    "blockIndexInTexture" 128)
   (list
    "blockName" "stairsStone" 
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
    "blockID" 67
    "blockIndexInTexture" 16)
   (list
    "blockName" "sign" 
    "slipperiness" 0.6
    "blockMaterial" "net.minecraft.src.Material@69663380" 
    "blockParticleGravity" 1.0
    "stepSound " "net.minecraft.src.StepSound@5b37e0d2" 
    "maxZ" 0.75
    "maxY" 1.0
    "maxX" 0.75
    "minZ" 0.25
    "minY" 0.0
    "minX" 0.25
    "blockResistance" 5.0
    "blockHardness" 1.0
    "blockID" 68
    "blockIndexInTexture" 4)
   (list
    "blockName" "lever" 
    "slipperiness" 0.6
    "blockMaterial" "net.minecraft.src.MaterialLogic@7591083d" 
    "blockParticleGravity" 1.0
    "stepSound " "net.minecraft.src.StepSound@5b37e0d2" 
    "maxZ" 1.0
    "maxY" 1.0
    "maxX" 1.0
    "minZ" 0.0
    "minY" 0.0
    "minX" 0.0
    "blockResistance" 2.5
    "blockHardness" 0.5
    "blockID" 69
    "blockIndexInTexture" 96)
   (list
    "blockName" "pressurePlate" 
    "slipperiness" 0.6
    "blockMaterial" "net.minecraft.src.Material@46fbb2c1" 
    "blockParticleGravity" 1.0
    "stepSound " "net.minecraft.src.StepSound@1698c449" 
    "maxZ" 0.9375
    "maxY" 0.03125
    "maxX" 0.9375
    "minZ" 0.0625
    "minY" 0.0
    "minX" 0.0625
    "blockResistance" 2.5
    "blockHardness" 0.5
    "blockID" 70
    "blockIndexInTexture" 1)
   (list
    "blockName" "doorIron" 
    "slipperiness" 0.6
    "blockMaterial" "net.minecraft.src.Material@4ccabbaa" 
    "blockParticleGravity" 1.0
    "stepSound " "net.minecraft.src.StepSound@77a567e1" 
    "maxZ" 1.0
    "maxY" 1.0
    "maxX" 1.0
    "minZ" 0.0
    "minY" 0.0
    "minX" 0.0
    "blockResistance" 25.0
    "blockHardness" 5.0
    "blockID" 71
    "blockIndexInTexture" 98)
   (list
    "blockName" "pressurePlate" 
    "slipperiness" 0.6
    "blockMaterial" "net.minecraft.src.Material@69663380" 
    "blockParticleGravity" 1.0
    "stepSound " "net.minecraft.src.StepSound@5b37e0d2" 
    "maxZ" 0.9375
    "maxY" 0.03125
    "maxX" 0.9375
    "minZ" 0.0625
    "minY" 0.0
    "minX" 0.0625
    "blockResistance" 2.5
    "blockHardness" 0.5
    "blockID" 72
    "blockIndexInTexture" 4)
   (list
    "blockName" "oreRedstone" 
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
    "blockResistance" 15.0
    "blockHardness" 3.0
    "blockID" 73
    "blockIndexInTexture" 51)
   (list
    "blockName" "oreRedstone" 
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
    "blockResistance" 15.0
    "blockHardness" 3.0
    "blockID" 74
    "blockIndexInTexture" 51)
   (list
    "blockName" "notGate" 
    "slipperiness" 0.6
    "blockMaterial" "net.minecraft.src.MaterialLogic@7591083d" 
    "blockParticleGravity" 1.0
    "stepSound " "net.minecraft.src.StepSound@5b37e0d2" 
    "maxZ" 1.0
    "maxY" 1.0
    "maxX" 1.0
    "minZ" 0.0
    "minY" 0.0
    "minX" 0.0
    "blockResistance" 0.0
    "blockHardness" 0.0
    "blockID" 75
    "blockIndexInTexture" 115)
   (list
    "blockName" "notGate" 
    "slipperiness" 0.6
    "blockMaterial" "net.minecraft.src.MaterialLogic@7591083d" 
    "blockParticleGravity" 1.0
    "stepSound " "net.minecraft.src.StepSound@5b37e0d2" 
    "maxZ" 1.0
    "maxY" 1.0
    "maxX" 1.0
    "minZ" 0.0
    "minY" 0.0
    "minX" 0.0
    "blockResistance" 0.0
    "blockHardness" 0.0
    "blockID" 76
    "blockIndexInTexture" 99)
   (list
    "blockName" "button" 
    "slipperiness" 0.6
    "blockMaterial" "net.minecraft.src.MaterialLogic@7591083d" 
    "blockParticleGravity" 1.0
    "stepSound " "net.minecraft.src.StepSound@1698c449" 
    "maxZ" 1.0
    "maxY" 1.0
    "maxX" 1.0
    "minZ" 0.0
    "minY" 0.0
    "minX" 0.0
    "blockResistance" 2.5
    "blockHardness" 0.5
    "blockID" 77
    "blockIndexInTexture" 1)
   (list
    "blockName" "snow" 
    "slipperiness" 0.6
    "blockMaterial" "net.minecraft.src.MaterialLogic@5fa7e7ff" 
    "blockParticleGravity" 1.0
    "stepSound " "net.minecraft.src.StepSound@108c4c35" 
    "maxZ" 1.0
    "maxY" 0.125
    "maxX" 1.0
    "minZ" 0.0
    "minY" 0.0
    "minX" 0.0
    "blockResistance" 0.5
    "blockHardness" 0.1
    "blockID" 78
    "blockIndexInTexture" 66)
   (list
    "blockName" "ice" 
    "slipperiness" 0.98
    "blockMaterial" "net.minecraft.src.Material@4629104a" 
    "blockParticleGravity" 1.0
    "stepSound " "net.minecraft.src.StepSoundStone@45283ce2" 
    "maxZ" 1.0
    "maxY" 1.0
    "maxX" 1.0
    "minZ" 0.0
    "minY" 0.0
    "minX" 0.0
    "blockResistance" 2.5
    "blockHardness" 0.5
    "blockID" 79
    "blockIndexInTexture" 67)
   (list
    "blockName" "snow" 
    "slipperiness" 0.6
    "blockMaterial" "net.minecraft.src.Material@27f8302d" 
    "blockParticleGravity" 1.0
    "stepSound " "net.minecraft.src.StepSound@108c4c35" 
    "maxZ" 1.0
    "maxY" 1.0
    "maxX" 1.0
    "minZ" 0.0
    "minY" 0.0
    "minX" 0.0
    "blockResistance" 1.0
    "blockHardness" 0.2
    "blockID" 80
    "blockIndexInTexture" 66)
   (list
    "blockName" "cactus" 
    "slipperiness" 0.6
    "blockMaterial" "net.minecraft.src.Material@4d76f3f8" 
    "blockParticleGravity" 1.0
    "stepSound " "net.minecraft.src.StepSound@108c4c35" 
    "maxZ" 1.0
    "maxY" 1.0
    "maxX" 1.0
    "minZ" 0.0
    "minY" 0.0
    "minX" 0.0
    "blockResistance" 2.0
    "blockHardness" 0.4
    "blockID" 81
    "blockIndexInTexture" 70)
   (list
    "blockName" "clay" 
    "slipperiness" 0.6
    "blockMaterial" "net.minecraft.src.Material@2d8e6db6" 
    "blockParticleGravity" 1.0
    "stepSound " "net.minecraft.src.StepSound@14514713" 
    "maxZ" 1.0
    "maxY" 1.0
    "maxX" 1.0
    "minZ" 0.0
    "minY" 0.0
    "minX" 0.0
    "blockResistance" 3.0
    "blockHardness" 0.6
    "blockID" 82
    "blockIndexInTexture" 72)
   (list
    "blockName" "reeds" 
    "slipperiness" 0.6
    "blockMaterial" "net.minecraft.src.MaterialLogic@4459eb14" 
    "blockParticleGravity" 1.0
    "stepSound " "net.minecraft.src.StepSound@5f4da5c3" 
    "maxZ" 0.875
    "maxY" 1.0
    "maxX" 0.875
    "minZ" 0.125
    "minY" 0.0
    "minX" 0.125
    "blockResistance" 0.0
    "blockHardness" 0.0
    "blockID" 83
    "blockIndexInTexture" 73)
   (list
    "blockName" "jukebox" 
    "slipperiness" 0.6
    "blockMaterial" "net.minecraft.src.Material@69663380" 
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
    "blockID" 84
    "blockIndexInTexture" 74)
   (list
    "blockName" "fence" 
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
    "blockID" 85
    "blockIndexInTexture" 4)
   (list
    "blockName" "pumpkin" 
    "slipperiness" 0.6
    "blockMaterial" "net.minecraft.src.Material@23ab930d" 
    "blockParticleGravity" 1.0
    "stepSound " "net.minecraft.src.StepSound@5b37e0d2" 
    "maxZ" 1.0
    "maxY" 1.0
    "maxX" 1.0
    "minZ" 0.0
    "minY" 0.0
    "minX" 0.0
    "blockResistance" 5.0
    "blockHardness" 1.0
    "blockID" 86
    "blockIndexInTexture" 102)
   (list
    "blockName" "hellrock" 
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
    "blockResistance" 2.0
    "blockHardness" 0.4
    "blockID" 87
    "blockIndexInTexture" 103)
   (list
    "blockName" "hellsand" 
    "slipperiness" 0.6
    "blockMaterial" "net.minecraft.src.Material@6d5380c2" 
    "blockParticleGravity" 1.0
    "stepSound " "net.minecraft.src.StepSoundSand@45ff54e6" 
    "maxZ" 1.0
    "maxY" 1.0
    "maxX" 1.0
    "minZ" 0.0
    "minY" 0.0
    "minX" 0.0
    "blockResistance" 2.5
    "blockHardness" 0.5
    "blockID" 88
    "blockIndexInTexture" 104)
   (list
    "blockName" "lightgem" 
    "slipperiness" 0.6
    "blockMaterial" "net.minecraft.src.Material@46fbb2c1" 
    "blockParticleGravity" 1.0
    "stepSound " "net.minecraft.src.StepSoundStone@45283ce2" 
    "maxZ" 1.0
    "maxY" 1.0
    "maxX" 1.0
    "minZ" 0.0
    "minY" 0.0
    "minX" 0.0
    "blockResistance" 1.5
    "blockHardness" 0.3
    "blockID" 89
    "blockIndexInTexture" 105)
   (list
    "blockName" "portal" 
    "slipperiness" 0.6
    "blockMaterial" "net.minecraft.src.MaterialPortal@4534b60d" 
    "blockParticleGravity" 1.0
    "stepSound " "net.minecraft.src.StepSoundStone@45283ce2" 
    "maxZ" 1.0
    "maxY" 1.0
    "maxX" 1.0
    "minZ" 0.0
    "minY" 0.0
    "minX" 0.0
    "blockResistance" 0.0
    "blockHardness" -1.0
    "blockID" 90
    "blockIndexInTexture" 14)
   (list
    "blockName" "litpumpkin" 
    "slipperiness" 0.6
    "blockMaterial" "net.minecraft.src.Material@23ab930d" 
    "blockParticleGravity" 1.0
    "stepSound " "net.minecraft.src.StepSound@5b37e0d2" 
    "maxZ" 1.0
    "maxY" 1.0
    "maxX" 1.0
    "minZ" 0.0
    "minY" 0.0
    "minX" 0.0
    "blockResistance" 5.0
    "blockHardness" 1.0
    "blockID" 91
    "blockIndexInTexture" 102)
   (list
    "blockName" "cake" 
    "slipperiness" 0.6
    "blockMaterial" "net.minecraft.src.Material@3fa77460" 
    "blockParticleGravity" 1.0
    "stepSound " "net.minecraft.src.StepSound@108c4c35" 
    "maxZ" 1.0
    "maxY" 1.0
    "maxX" 1.0
    "minZ" 0.0
    "minY" 0.0
    "minX" 0.0
    "blockResistance" 2.5
    "blockHardness" 0.5
    "blockID" 92
    "blockIndexInTexture" 121)
   (list
    "blockName" "diode" 
    "slipperiness" 0.6
    "blockMaterial" "net.minecraft.src.MaterialLogic@7591083d" 
    "blockParticleGravity" 1.0
    "stepSound " "net.minecraft.src.StepSound@5b37e0d2" 
    "maxZ" 1.0
    "maxY" 0.125
    "maxX" 1.0
    "minZ" 0.0
    "minY" 0.0
    "minX" 0.0
    "blockResistance" 0.0
    "blockHardness" 0.0
    "blockID" 93
    "blockIndexInTexture" 6)
   (list
    "blockName" "diode" 
    "slipperiness" 0.6
    "blockMaterial" "net.minecraft.src.MaterialLogic@7591083d" 
    "blockParticleGravity" 1.0
    "stepSound " "net.minecraft.src.StepSound@5b37e0d2" 
    "maxZ" 1.0
    "maxY" 0.125
    "maxX" 1.0
    "minZ" 0.0
    "minY" 0.0
    "minX" 0.0
    "blockResistance" 0.0
    "blockHardness" 0.0
    "blockID" 94
    "blockIndexInTexture" 6)
   (list
    "blockName" "lockedchest" 
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
    "blockResistance" 0.0
    "blockHardness" 0.0
    "blockID" 95
    "blockIndexInTexture" 26)
   (list
    "blockName" "trapdoor" 
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
    "blockHardness" 3.0
    "blockID" 96
    "blockIndexInTexture" 84)))

(defparameter blocksList
  (coerce 
   (let ((blist nil))
     (dolist (ablock blocks)
       (push (second ablock) blist))
     (nreverse blist))
   'vector))

(defparameter blockIndexInTexture
  (let ((thearray (make-array 256)))
    (let ((counter 0))
      (dolist (ablock blocks)
	(setf (aref thearray counter) (nth 29 ablock))
	(incf counter)))
    thearray))
