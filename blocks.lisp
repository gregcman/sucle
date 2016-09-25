(in-package :mc-blocks)

(defparameter raw
  `((list
     "blockName" "stone" 
     "blockID" 1
     "blockIndexInTexture" 1
     "isOpaqueCube" t
     "renderAsNormalBlock" t
     "renderType" 0
     "lightValue" 0
     "lightOpacity" 255
     "canBlockGrass" nil
     "tickOnLoad" nil
     "nometanotify" nil
     "isCollidable" t
     "blockResistance" 30.0
     "blockHardness" 1.5
     "slipperiness" 0.6
     "blockMaterial" "net.minecraft.src.Material@53e25b76" 
     "blockParticleGravity" 1.0
     "stepSound " "net.minecraft.src.StepSound@73a8dfcc" 
     "maxZ" 1.0
     "maxY" 1.0
     "maxX" 1.0
     "minZ" 0.0
     "minY" 0.0
     "minX" 0.0
     )
    (list
     "blockName" "grass" 
     "blockID" 2
     "blockIndexInTexture" 3
     "isOpaqueCube" t
     "renderAsNormalBlock" t
     "renderType" 0
     "lightValue" 0
     "lightOpacity" 255
     "canBlockGrass" nil
     "tickOnLoad" t
     "nometanotify" nil
     "isCollidable" t
     "blockResistance" 3.0
     "blockHardness" 0.6
     "slipperiness" 0.6
     "blockMaterial" "net.minecraft.src.Material@ea30797" 
     "blockParticleGravity" 1.0
     "stepSound " "net.minecraft.src.StepSound@7e774085" 
     "maxZ" 1.0
     "maxY" 1.0
     "maxX" 1.0
     "minZ" 0.0
     "minY" 0.0
     "minX" 0.0
     )
    (list
     "blockName" "dirt" 
     "blockID" 3
     "blockIndexInTexture" 2
     "isOpaqueCube" t
     "renderAsNormalBlock" t
     "renderType" 0
     "lightValue" 0
     "lightOpacity" 255
     "canBlockGrass" nil
     "tickOnLoad" nil
     "nometanotify" nil
     "isCollidable" t
     "blockResistance" 2.5
     "blockHardness" 0.5
     "slipperiness" 0.6
     "blockMaterial" "net.minecraft.src.Material@3f8f9dd6" 
     "blockParticleGravity" 1.0
     "stepSound " "net.minecraft.src.StepSound@aec6354" 
     "maxZ" 1.0
     "maxY" 1.0
     "maxX" 1.0
     "minZ" 0.0
     "minY" 0.0
     "minX" 0.0
     )
    (list
     "blockName" "stonebrick" 
     "blockID" 4
     "blockIndexInTexture" 16
     "isOpaqueCube" t
     "renderAsNormalBlock" t
     "renderType" 0
     "lightValue" 0
     "lightOpacity" 255
     "canBlockGrass" nil
     "tickOnLoad" nil
     "nometanotify" nil
     "isCollidable" t
     "blockResistance" 30.0
     "blockHardness" 2.0
     "slipperiness" 0.6
     "blockMaterial" "net.minecraft.src.Material@53e25b76" 
     "blockParticleGravity" 1.0
     "stepSound " "net.minecraft.src.StepSound@73a8dfcc" 
     "maxZ" 1.0
     "maxY" 1.0
     "maxX" 1.0
     "minZ" 0.0
     "minY" 0.0
     "minX" 0.0
     )
    (list
     "blockName" "wood" 
     "blockID" 5
     "blockIndexInTexture" 4
     "isOpaqueCube" t
     "renderAsNormalBlock" t
     "renderType" 0
     "lightValue" 0
     "lightOpacity" 255
     "canBlockGrass" nil
     "tickOnLoad" nil
     "nometanotify" t
     "isCollidable" t
     "blockResistance" 15.0
     "blockHardness" 2.0
     "slipperiness" 0.6
     "blockMaterial" "net.minecraft.src.Material@1c655221" 
     "blockParticleGravity" 1.0
     "stepSound " "net.minecraft.src.StepSound@58d25a40" 
     "maxZ" 1.0
     "maxY" 1.0
     "maxX" 1.0
     "minZ" 0.0
     "minY" 0.0
     "minX" 0.0
     )
    (list
     "blockName" "sapling" 
     "blockID" 6
     "blockIndexInTexture" 15
     "isOpaqueCube" nil
     "renderAsNormalBlock" nil
     "renderType" 1
     "lightValue" 0
     "lightOpacity" 0
     "canBlockGrass" t
     "tickOnLoad" t
     "nometanotify" t
     "isCollidable" ,(if nil t nil)
     "blockResistance" 0.0
     "blockHardness" 0.0
     "slipperiness" 0.6
     "blockMaterial" "net.minecraft.src.MaterialLogic@1b701da1" 
     "blockParticleGravity" 1.0
     "stepSound " "net.minecraft.src.StepSound@7e774085" 
     "maxZ" 0.8999999761581421
     "maxY" 0.800000011920929
     "maxX" 0.8999999761581421
     "minZ" 0.09999999403953552
     "minY" 0.0
     "minX" 0.09999999403953552
     )
    (list
     "blockName" "bedrock" 
     "blockID" 7
     "blockIndexInTexture" 17
     "isOpaqueCube" t
     "renderAsNormalBlock" t
     "renderType" 0
     "lightValue" 0
     "lightOpacity" 255
     "canBlockGrass" nil
     "tickOnLoad" nil
     "nometanotify" nil
     "isCollidable" t
     "blockResistance" 1.8E7
     "blockHardness" -1.0
     "slipperiness" 0.6
     "blockMaterial" "net.minecraft.src.Material@53e25b76" 
     "blockParticleGravity" 1.0
     "stepSound " "net.minecraft.src.StepSound@73a8dfcc" 
     "maxZ" 1.0
     "maxY" 1.0
     "maxX" 1.0
     "minZ" 0.0
     "minY" 0.0
     "minX" 0.0
     )
    (list
     "blockName" "water" 
     "blockID" 8
     "blockIndexInTexture" 205
     "isOpaqueCube" nil
     "renderAsNormalBlock" nil
     "renderType" 4
     "lightValue" 0
     "lightOpacity" 3
     "canBlockGrass" nil
     "tickOnLoad" t
     "nometanotify" t
     "isCollidable" ,(if nil t nil)
     "blockResistance" 500.0
     "blockHardness" 100.0
     "slipperiness" 0.6
     "blockMaterial" "net.minecraft.src.MaterialLiquid@726f3b58" 
     "blockParticleGravity" 1.0
     "stepSound " "net.minecraft.src.StepSound@442d9b6e" 
     "maxZ" 1.0
     "maxY" 1.0
     "maxX" 1.0
     "minZ" 0.0
     "minY" 0.0
     "minX" 0.0
     )
    (list
     "blockName" "water" 
     "blockID" 9
     "blockIndexInTexture" 205
     "isOpaqueCube" nil
     "renderAsNormalBlock" nil
     "renderType" 4
     "lightValue" 0
     "lightOpacity" 3
     "canBlockGrass" nil
     "tickOnLoad" nil
     "nometanotify" t
     "isCollidable" ,(if nil t nil)
     "blockResistance" 500.0
     "blockHardness" 100.0
     "slipperiness" 0.6
     "blockMaterial" "net.minecraft.src.MaterialLiquid@726f3b58" 
     "blockParticleGravity" 1.0
     "stepSound " "net.minecraft.src.StepSound@442d9b6e" 
     "maxZ" 1.0
     "maxY" 1.0
     "maxX" 1.0
     "minZ" 0.0
     "minY" 0.0
     "minX" 0.0
     )
    (list
     "blockName" "lava" 
     "blockID" 10
     "blockIndexInTexture" 237
     "isOpaqueCube" nil
     "renderAsNormalBlock" nil
     "renderType" 4
     "lightValue" 15
     "lightOpacity" 255
     "canBlockGrass" nil
     "tickOnLoad" t
     "nometanotify" t
     "isCollidable" ,(if nil t nil)
     "blockResistance" 0.0
     "blockHardness" 0.0
     "slipperiness" 0.6
     "blockMaterial" "net.minecraft.src.MaterialLiquid@ee7d9f1" 
     "blockParticleGravity" 1.0
     "stepSound " "net.minecraft.src.StepSound@442d9b6e" 
     "maxZ" 1.0
     "maxY" 1.0
     "maxX" 1.0
     "minZ" 0.0
     "minY" 0.0
     "minX" 0.0
     )
    (list
     "blockName" "lava" 
     "blockID" 11
     "blockIndexInTexture" 237
     "isOpaqueCube" nil
     "renderAsNormalBlock" nil
     "renderType" 4
     "lightValue" 15
     "lightOpacity" 255
     "canBlockGrass" nil
     "tickOnLoad" t
     "nometanotify" t
     "isCollidable" ,(if nil t nil)
     "blockResistance" 500.0
     "blockHardness" 100.0
     "slipperiness" 0.6
     "blockMaterial" "net.minecraft.src.MaterialLiquid@ee7d9f1" 
     "blockParticleGravity" 1.0
     "stepSound " "net.minecraft.src.StepSound@442d9b6e" 
     "maxZ" 1.0
     "maxY" 1.0
     "maxX" 1.0
     "minZ" 0.0
     "minY" 0.0
     "minX" 0.0
     )
    (list
     "blockName" "sand" 
     "blockID" 12
     "blockIndexInTexture" 18
     "isOpaqueCube" t
     "renderAsNormalBlock" t
     "renderType" 0
     "lightValue" 0
     "lightOpacity" 255
     "canBlockGrass" nil
     "tickOnLoad" nil
     "nometanotify" nil
     "isCollidable" t
     "blockResistance" 2.5
     "blockHardness" 0.5
     "slipperiness" 0.6
     "blockMaterial" "net.minecraft.src.Material@15615099" 
     "blockParticleGravity" 1.0
     "stepSound " "net.minecraft.src.StepSoundSand@1edf1c96" 
     "maxZ" 1.0
     "maxY" 1.0
     "maxX" 1.0
     "minZ" 0.0
     "minY" 0.0
     "minX" 0.0
     )
    (list
     "blockName" "gravel" 
     "blockID" 13
     "blockIndexInTexture" 19
     "isOpaqueCube" t
     "renderAsNormalBlock" t
     "renderType" 0
     "lightValue" 0
     "lightOpacity" 255
     "canBlockGrass" nil
     "tickOnLoad" nil
     "nometanotify" nil
     "isCollidable" t
     "blockResistance" 3.0
     "blockHardness" 0.6
     "slipperiness" 0.6
     "blockMaterial" "net.minecraft.src.Material@15615099" 
     "blockParticleGravity" 1.0
     "stepSound " "net.minecraft.src.StepSound@aec6354" 
     "maxZ" 1.0
     "maxY" 1.0
     "maxX" 1.0
     "minZ" 0.0
     "minY" 0.0
     "minX" 0.0
     )
    (list
     "blockName" "oreGold" 
     "blockID" 14
     "blockIndexInTexture" 32
     "isOpaqueCube" t
     "renderAsNormalBlock" t
     "renderType" 0
     "lightValue" 0
     "lightOpacity" 255
     "canBlockGrass" nil
     "tickOnLoad" nil
     "nometanotify" nil
     "isCollidable" t
     "blockResistance" 15.0
     "blockHardness" 3.0
     "slipperiness" 0.6
     "blockMaterial" "net.minecraft.src.Material@53e25b76" 
     "blockParticleGravity" 1.0
     "stepSound " "net.minecraft.src.StepSound@73a8dfcc" 
     "maxZ" 1.0
     "maxY" 1.0
     "maxX" 1.0
     "minZ" 0.0
     "minY" 0.0
     "minX" 0.0
     )
    (list
     "blockName" "oreIron" 
     "blockID" 15
     "blockIndexInTexture" 33
     "isOpaqueCube" t
     "renderAsNormalBlock" t
     "renderType" 0
     "lightValue" 0
     "lightOpacity" 255
     "canBlockGrass" nil
     "tickOnLoad" nil
     "nometanotify" nil
     "isCollidable" t
     "blockResistance" 15.0
     "blockHardness" 3.0
     "slipperiness" 0.6
     "blockMaterial" "net.minecraft.src.Material@53e25b76" 
     "blockParticleGravity" 1.0
     "stepSound " "net.minecraft.src.StepSound@73a8dfcc" 
     "maxZ" 1.0
     "maxY" 1.0
     "maxX" 1.0
     "minZ" 0.0
     "minY" 0.0
     "minX" 0.0
     )
    (list
     "blockName" "oreCoal" 
     "blockID" 16
     "blockIndexInTexture" 34
     "isOpaqueCube" t
     "renderAsNormalBlock" t
     "renderType" 0
     "lightValue" 0
     "lightOpacity" 255
     "canBlockGrass" nil
     "tickOnLoad" nil
     "nometanotify" nil
     "isCollidable" t
     "blockResistance" 15.0
     "blockHardness" 3.0
     "slipperiness" 0.6
     "blockMaterial" "net.minecraft.src.Material@53e25b76" 
     "blockParticleGravity" 1.0
     "stepSound " "net.minecraft.src.StepSound@73a8dfcc" 
     "maxZ" 1.0
     "maxY" 1.0
     "maxX" 1.0
     "minZ" 0.0
     "minY" 0.0
     "minX" 0.0
     )
    (list
     "blockName" "log" 
     "blockID" 17
     "blockIndexInTexture" 20
     "isOpaqueCube" t
     "renderAsNormalBlock" t
     "renderType" 0
     "lightValue" 0
     "lightOpacity" 255
     "canBlockGrass" nil
     "tickOnLoad" nil
     "nometanotify" t
     "isCollidable" t
     "blockResistance" 10.0
     "blockHardness" 2.0
     "slipperiness" 0.6
     "blockMaterial" "net.minecraft.src.Material@1c655221" 
     "blockParticleGravity" 1.0
     "stepSound " "net.minecraft.src.StepSound@58d25a40" 
     "maxZ" 1.0
     "maxY" 1.0
     "maxX" 1.0
     "minZ" 0.0
     "minY" 0.0
     "minX" 0.0
     )
    (list
     "blockName" "leaves" 
     "blockID" 18
     "blockIndexInTexture" ,(if nil 52 53)
     "isOpaqueCube" t
     "renderAsNormalBlock" t
     "renderType" 0
     "lightValue" 0
     "lightOpacity" 1
     "canBlockGrass" nil
     "tickOnLoad" t
     "nometanotify" t
     "isCollidable" t
     "blockResistance" 1.0
     "blockHardness" 0.2
     "slipperiness" 0.6
     "blockMaterial" "net.minecraft.src.Material@368102c8" 
     "blockParticleGravity" 1.0
     "stepSound " "net.minecraft.src.StepSound@7e774085" 
     "maxZ" 1.0
     "maxY" 1.0
     "maxX" 1.0
     "minZ" 0.0
     "minY" 0.0
     "minX" 0.0
     )
    (list
     "blockName" "sponge" 
     "blockID" 19
     "blockIndexInTexture" 48
     "isOpaqueCube" t
     "renderAsNormalBlock" t
     "renderType" 0
     "lightValue" 0
     "lightOpacity" 255
     "canBlockGrass" nil
     "tickOnLoad" nil
     "nometanotify" nil
     "isCollidable" t
     "blockResistance" 3.0
     "blockHardness" 0.6
     "slipperiness" 0.6
     "blockMaterial" "net.minecraft.src.Material@6996db8" 
     "blockParticleGravity" 1.0
     "stepSound " "net.minecraft.src.StepSound@7e774085" 
     "maxZ" 1.0
     "maxY" 1.0
     "maxX" 1.0
     "minZ" 0.0
     "minY" 0.0
     "minX" 0.0
     )
    (list
     "blockName" "glass" 
     "blockID" 20
     "blockIndexInTexture" 49
     "isOpaqueCube" nil
     "renderAsNormalBlock" t
     "renderType" 0
     "lightValue" 0
     "lightOpacity" 0
     "canBlockGrass" nil
     "tickOnLoad" nil
     "nometanotify" nil
     "isCollidable" t
     "blockResistance" 1.5
     "blockHardness" 0.3
     "slipperiness" 0.6
     "blockMaterial" "net.minecraft.src.Material@1963006a" 
     "blockParticleGravity" 1.0
     "stepSound " "net.minecraft.src.StepSoundStone@7fbe847c" 
     "maxZ" 1.0
     "maxY" 1.0
     "maxX" 1.0
     "minZ" 0.0
     "minY" 0.0
     "minX" 0.0
     )
    (list
     "blockName" "oreLapis" 
     "blockID" 21
     "blockIndexInTexture" 160
     "isOpaqueCube" t
     "renderAsNormalBlock" t
     "renderType" 0
     "lightValue" 0
     "lightOpacity" 255
     "canBlockGrass" nil
     "tickOnLoad" nil
     "nometanotify" nil
     "isCollidable" t
     "blockResistance" 15.0
     "blockHardness" 3.0
     "slipperiness" 0.6
     "blockMaterial" "net.minecraft.src.Material@53e25b76" 
     "blockParticleGravity" 1.0
     "stepSound " "net.minecraft.src.StepSound@73a8dfcc" 
     "maxZ" 1.0
     "maxY" 1.0
     "maxX" 1.0
     "minZ" 0.0
     "minY" 0.0
     "minX" 0.0
     )
    (list
     "blockName" "blockLapis" 
     "blockID" 22
     "blockIndexInTexture" 144
     "isOpaqueCube" t
     "renderAsNormalBlock" t
     "renderType" 0
     "lightValue" 0
     "lightOpacity" 255
     "canBlockGrass" nil
     "tickOnLoad" nil
     "nometanotify" nil
     "isCollidable" t
     "blockResistance" 15.0
     "blockHardness" 3.0
     "slipperiness" 0.6
     "blockMaterial" "net.minecraft.src.Material@53e25b76" 
     "blockParticleGravity" 1.0
     "stepSound " "net.minecraft.src.StepSound@73a8dfcc" 
     "maxZ" 1.0
     "maxY" 1.0
     "maxX" 1.0
     "minZ" 0.0
     "minY" 0.0
     "minX" 0.0
     )
    (list
     "blockName" "dispenser" 
     "blockID" 23
     "blockIndexInTexture" 45
     "isOpaqueCube" t
     "renderAsNormalBlock" t
     "renderType" 0
     "lightValue" 0
     "lightOpacity" 255
     "canBlockGrass" nil
     "tickOnLoad" nil
     "nometanotify" t
     "isCollidable" t
     "blockResistance" 17.5
     "blockHardness" 3.5
     "slipperiness" 0.6
     "blockMaterial" "net.minecraft.src.Material@53e25b76" 
     "blockParticleGravity" 1.0
     "stepSound " "net.minecraft.src.StepSound@73a8dfcc" 
     "maxZ" 1.0
     "maxY" 1.0
     "maxX" 1.0
     "minZ" 0.0
     "minY" 0.0
     "minX" 0.0
     )
    (list
     "blockName" "sandStone" 
     "blockID" 24
     "blockIndexInTexture" 192
     "isOpaqueCube" t
     "renderAsNormalBlock" t
     "renderType" 0
     "lightValue" 0
     "lightOpacity" 255
     "canBlockGrass" nil
     "tickOnLoad" nil
     "nometanotify" nil
     "isCollidable" t
     "blockResistance" 4.0
     "blockHardness" 0.8
     "slipperiness" 0.6
     "blockMaterial" "net.minecraft.src.Material@53e25b76" 
     "blockParticleGravity" 1.0
     "stepSound " "net.minecraft.src.StepSound@73a8dfcc" 
     "maxZ" 1.0
     "maxY" 1.0
     "maxX" 1.0
     "minZ" 0.0
     "minY" 0.0
     "minX" 0.0
     )
    (list
     "blockName" "musicBlock" 
     "blockID" 25
     "blockIndexInTexture" 74
     "isOpaqueCube" t
     "renderAsNormalBlock" t
     "renderType" 0
     "lightValue" 0
     "lightOpacity" 255
     "canBlockGrass" nil
     "tickOnLoad" nil
     "nometanotify" t
     "isCollidable" t
     "blockResistance" 4.0
     "blockHardness" 0.8
     "slipperiness" 0.6
     "blockMaterial" "net.minecraft.src.Material@1c655221" 
     "blockParticleGravity" 1.0
     "stepSound " "net.minecraft.src.StepSound@442d9b6e" 
     "maxZ" 1.0
     "maxY" 1.0
     "maxX" 1.0
     "minZ" 0.0
     "minY" 0.0
     "minX" 0.0
     )
    (list
     "blockName" "bed" 
     "blockID" 26
     "blockIndexInTexture" 134
     "isOpaqueCube" nil
     "renderAsNormalBlock" nil
     "renderType" 14
     "lightValue" 0
     "lightOpacity" 0
     "canBlockGrass" nil
     "tickOnLoad" nil
     "nometanotify" t
     "isCollidable" t
     "blockResistance" 1.0
     "blockHardness" 0.2
     "slipperiness" 0.6
     "blockMaterial" "net.minecraft.src.Material@41975e01" 
     "blockParticleGravity" 1.0
     "stepSound " "net.minecraft.src.StepSound@442d9b6e" 
     "maxZ" 1.0
     "maxY" 0.5625
     "maxX" 1.0
     "minZ" 0.0
     "minY" 0.0
     "minX" 0.0
     )
    (list
     "blockName" "goldenRail" 
     "blockID" 27
     "blockIndexInTexture" 179
     "isOpaqueCube" nil
     "renderAsNormalBlock" nil
     "renderType" 9
     "lightValue" 0
     "lightOpacity" 0
     "canBlockGrass" t
     "tickOnLoad" nil
     "nometanotify" t
     "isCollidable" ,(if nil t nil)
     "blockResistance" 3.5
     "blockHardness" 0.7
     "slipperiness" 0.6
     "blockMaterial" "net.minecraft.src.MaterialLogic@c2e1f26" 
     "blockParticleGravity" 1.0
     "stepSound " "net.minecraft.src.StepSound@dcf3e99" 
     "maxZ" 1.0
     "maxY" 0.125
     "maxX" 1.0
     "minZ" 0.0
     "minY" 0.0
     "minX" 0.0
     )
    (list
     "blockName" "detectorRail" 
     "blockID" 28
     "blockIndexInTexture" 195
     "isOpaqueCube" nil
     "renderAsNormalBlock" nil
     "renderType" 9
     "lightValue" 0
     "lightOpacity" 0
     "canBlockGrass" t
     "tickOnLoad" t
     "nometanotify" t
     "isCollidable" ,(if nil t nil)
     "blockResistance" 3.5
     "blockHardness" 0.7
     "slipperiness" 0.6
     "blockMaterial" "net.minecraft.src.MaterialLogic@c2e1f26" 
     "blockParticleGravity" 1.0
     "stepSound " "net.minecraft.src.StepSound@dcf3e99" 
     "maxZ" 1.0
     "maxY" 0.125
     "maxX" 1.0
     "minZ" 0.0
     "minY" 0.0
     "minX" 0.0
     )
    (list
     "blockName" "pistonStickyBase" 
     "blockID" 29
     "blockIndexInTexture" 106
     "isOpaqueCube" nil
     "renderAsNormalBlock" nil
     "renderType" 16
     "lightValue" 0
     "lightOpacity" 0
     "canBlockGrass" nil
     "tickOnLoad" nil
     "nometanotify" t
     "isCollidable" t
     "blockResistance" 2.5
     "blockHardness" 0.5
     "slipperiness" 0.6
     "blockMaterial" "net.minecraft.src.Material@6d9c638" 
     "blockParticleGravity" 1.0
     "stepSound " "net.minecraft.src.StepSound@73a8dfcc" 
     "maxZ" 1.0
     "maxY" 1.0
     "maxX" 1.0
     "minZ" 0.0
     "minY" 0.0
     "minX" 0.0
     )
    (list
     "blockName" "web" 
     "blockID" 30
     "blockIndexInTexture" 11
     "isOpaqueCube" nil
     "renderAsNormalBlock" nil
     "renderType" 1
     "lightValue" 0
     "lightOpacity" 1
     "canBlockGrass" nil
     "tickOnLoad" nil
     "nometanotify" nil
     "isCollidable" ,(if nil t nil)
     "blockResistance" 20.0
     "blockHardness" 4.0
     "slipperiness" 0.6
     "blockMaterial" "net.minecraft.src.Material@7dc5e7b4" 
     "blockParticleGravity" 1.0
     "stepSound " "net.minecraft.src.StepSound@442d9b6e" 
     "maxZ" 1.0
     "maxY" 1.0
     "maxX" 1.0
     "minZ" 0.0
     "minY" 0.0
     "minX" 0.0
     )
    (list
     "blockName" "tallgrass" 
     "blockID" 31
     "blockIndexInTexture" 39
     "isOpaqueCube" nil
     "renderAsNormalBlock" nil
     "renderType" 1
     "lightValue" 0
     "lightOpacity" 0
     "canBlockGrass" t
     "tickOnLoad" t
     "nometanotify" nil
     "isCollidable" ,(if nil t nil)
     "blockResistance" 0.0
     "blockHardness" 0.0
     "slipperiness" 0.6
     "blockMaterial" "net.minecraft.src.MaterialLogic@1b701da1" 
     "blockParticleGravity" 1.0
     "stepSound " "net.minecraft.src.StepSound@7e774085" 
     "maxZ" 0.8999999761581421
     "maxY" 0.800000011920929
     "maxX" 0.8999999761581421
     "minZ" 0.09999999403953552
     "minY" 0.0
     "minX" 0.09999999403953552
     )
    (list
     "blockName" "deadbush" 
     "blockID" 32
     "blockIndexInTexture" 55
     "isOpaqueCube" nil
     "renderAsNormalBlock" nil
     "renderType" 1
     "lightValue" 0
     "lightOpacity" 0
     "canBlockGrass" t
     "tickOnLoad" t
     "nometanotify" nil
     "isCollidable" ,(if nil t nil)
     "blockResistance" 0.0
     "blockHardness" 0.0
     "slipperiness" 0.6
     "blockMaterial" "net.minecraft.src.MaterialLogic@1b701da1" 
     "blockParticleGravity" 1.0
     "stepSound " "net.minecraft.src.StepSound@7e774085" 
     "maxZ" 0.8999999761581421
     "maxY" 0.800000011920929
     "maxX" 0.8999999761581421
     "minZ" 0.09999999403953552
     "minY" 0.0
     "minX" 0.09999999403953552
     )
    (list
     "blockName" "pistonBase" 
     "blockID" 33
     "blockIndexInTexture" 107
     "isOpaqueCube" nil
     "renderAsNormalBlock" nil
     "renderType" 16
     "lightValue" 0
     "lightOpacity" 0
     "canBlockGrass" nil
     "tickOnLoad" nil
     "nometanotify" t
     "isCollidable" t
     "blockResistance" 2.5
     "blockHardness" 0.5
     "slipperiness" 0.6
     "blockMaterial" "net.minecraft.src.Material@6d9c638" 
     "blockParticleGravity" 1.0
     "stepSound " "net.minecraft.src.StepSound@73a8dfcc" 
     "maxZ" 1.0
     "maxY" 1.0
     "maxX" 1.0
     "minZ" 0.0
     "minY" 0.0
     "minX" 0.0
     )
    (list
     "blockName" "null" 
     "blockID" 34
     "blockIndexInTexture" 107
     "isOpaqueCube" nil
     "renderAsNormalBlock" nil
     "renderType" 17
     "lightValue" 0
     "lightOpacity" 0
     "canBlockGrass" nil
     "tickOnLoad" nil
     "nometanotify" t
     "isCollidable" t
     "blockResistance" 2.5
     "blockHardness" 0.5
     "slipperiness" 0.6
     "blockMaterial" "net.minecraft.src.Material@6d9c638" 
     "blockParticleGravity" 1.0
     "stepSound " "net.minecraft.src.StepSound@73a8dfcc" 
     "maxZ" 1.0
     "maxY" 1.0
     "maxX" 1.0
     "minZ" 0.0
     "minY" 0.0
     "minX" 0.0
     )
    (list
     "blockName" "cloth" 
     "blockID" 35
     "blockIndexInTexture" 64
     "isOpaqueCube" t
     "renderAsNormalBlock" t
     "renderType" 0
     "lightValue" 0
     "lightOpacity" 255
     "canBlockGrass" nil
     "tickOnLoad" nil
     "nometanotify" t
     "isCollidable" t
     "blockResistance" 4.0
     "blockHardness" 0.8
     "slipperiness" 0.6
     "blockMaterial" "net.minecraft.src.Material@41975e01" 
     "blockParticleGravity" 1.0
     "stepSound " "net.minecraft.src.StepSound@1ee0005" 
     "maxZ" 1.0
     "maxY" 1.0
     "maxX" 1.0
     "minZ" 0.0
     "minY" 0.0
     "minX" 0.0
     )
    (list
     "blockName" "null" 
     "blockID" 36
     "blockIndexInTexture" 0
     "isOpaqueCube" nil
     "renderAsNormalBlock" nil
     "renderType" -1
     "lightValue" 0
     "lightOpacity" 0
     "canBlockGrass" nil
     "tickOnLoad" nil
     "nometanotify" nil
     "isCollidable" t
     "blockResistance" 0.0
     "blockHardness" -1.0
     "slipperiness" 0.6
     "blockMaterial" "net.minecraft.src.Material@6d9c638" 
     "blockParticleGravity" 1.0
     "stepSound " "net.minecraft.src.StepSound@442d9b6e" 
     "maxZ" 1.0
     "maxY" 1.0
     "maxX" 1.0
     "minZ" 0.0
     "minY" 0.0
     "minX" 0.0
     )
    (list
     "blockName" "flower" 
     "blockID" 37
     "blockIndexInTexture" 13
     "isOpaqueCube" nil
     "renderAsNormalBlock" nil
     "renderType" 1
     "lightValue" 0
     "lightOpacity" 0
     "canBlockGrass" t
     "tickOnLoad" t
     "nometanotify" nil
     "isCollidable" ,(if nil t nil)
     "blockResistance" 0.0
     "blockHardness" 0.0
     "slipperiness" 0.6
     "blockMaterial" "net.minecraft.src.MaterialLogic@1b701da1" 
     "blockParticleGravity" 1.0
     "stepSound " "net.minecraft.src.StepSound@7e774085" 
     "maxZ" 0.699999988079071
     "maxY" 0.6000000238418579
     "maxX" 0.699999988079071
     "minZ" 0.30000001192092896
     "minY" 0.0
     "minX" 0.30000001192092896
     )
    (list
     "blockName" "rose" 
     "blockID" 38
     "blockIndexInTexture" 12
     "isOpaqueCube" nil
     "renderAsNormalBlock" nil
     "renderType" 1
     "lightValue" 0
     "lightOpacity" 0
     "canBlockGrass" t
     "tickOnLoad" t
     "nometanotify" nil
     "isCollidable" ,(if nil t nil)
     "blockResistance" 0.0
     "blockHardness" 0.0
     "slipperiness" 0.6
     "blockMaterial" "net.minecraft.src.MaterialLogic@1b701da1" 
     "blockParticleGravity" 1.0
     "stepSound " "net.minecraft.src.StepSound@7e774085" 
     "maxZ" 0.699999988079071
     "maxY" 0.6000000238418579
     "maxX" 0.699999988079071
     "minZ" 0.30000001192092896
     "minY" 0.0
     "minX" 0.30000001192092896
     )
    (list
     "blockName" "mushroomBrown" 
     "blockID" 39
     "blockIndexInTexture" 29
     "isOpaqueCube" nil
     "renderAsNormalBlock" nil
     "renderType" 1
     "lightValue" 1
     "lightOpacity" 0
     "canBlockGrass" t
     "tickOnLoad" t
     "nometanotify" nil
     "isCollidable" ,(if nil t nil)
     "blockResistance" 0.0
     "blockHardness" 0.0
     "slipperiness" 0.6
     "blockMaterial" "net.minecraft.src.MaterialLogic@1b701da1" 
     "blockParticleGravity" 1.0
     "stepSound " "net.minecraft.src.StepSound@7e774085" 
     "maxZ" 0.699999988079071
     "maxY" 0.4000000059604645
     "maxX" 0.699999988079071
     "minZ" 0.30000001192092896
     "minY" 0.0
     "minX" 0.30000001192092896
     )
    (list
     "blockName" "mushroomRed" 
     "blockID" 40
     "blockIndexInTexture" 28
     "isOpaqueCube" nil
     "renderAsNormalBlock" nil
     "renderType" 1
     "lightValue" 0
     "lightOpacity" 0
     "canBlockGrass" t
     "tickOnLoad" t
     "nometanotify" nil
     "isCollidable" ,(if nil t nil)
     "blockResistance" 0.0
     "blockHardness" 0.0
     "slipperiness" 0.6
     "blockMaterial" "net.minecraft.src.MaterialLogic@1b701da1" 
     "blockParticleGravity" 1.0
     "stepSound " "net.minecraft.src.StepSound@7e774085" 
     "maxZ" 0.699999988079071
     "maxY" 0.4000000059604645
     "maxX" 0.699999988079071
     "minZ" 0.30000001192092896
     "minY" 0.0
     "minX" 0.30000001192092896
     )
    (list
     "blockName" "blockGold" 
     "blockID" 41
     "blockIndexInTexture" 23
     "isOpaqueCube" t
     "renderAsNormalBlock" t
     "renderType" 0
     "lightValue" 0
     "lightOpacity" 255
     "canBlockGrass" nil
     "tickOnLoad" nil
     "nometanotify" nil
     "isCollidable" t
     "blockResistance" 30.0
     "blockHardness" 3.0
     "slipperiness" 0.6
     "blockMaterial" "net.minecraft.src.Material@75a1cd57" 
     "blockParticleGravity" 1.0
     "stepSound " "net.minecraft.src.StepSound@dcf3e99" 
     "maxZ" 1.0
     "maxY" 1.0
     "maxX" 1.0
     "minZ" 0.0
     "minY" 0.0
     "minX" 0.0
     )
    (list
     "blockName" "blockIron" 
     "blockID" 42
     "blockIndexInTexture" 22
     "isOpaqueCube" t
     "renderAsNormalBlock" t
     "renderType" 0
     "lightValue" 0
     "lightOpacity" 255
     "canBlockGrass" nil
     "tickOnLoad" nil
     "nometanotify" nil
     "isCollidable" t
     "blockResistance" 30.0
     "blockHardness" 5.0
     "slipperiness" 0.6
     "blockMaterial" "net.minecraft.src.Material@75a1cd57" 
     "blockParticleGravity" 1.0
     "stepSound " "net.minecraft.src.StepSound@dcf3e99" 
     "maxZ" 1.0
     "maxY" 1.0
     "maxX" 1.0
     "minZ" 0.0
     "minY" 0.0
     "minX" 0.0
     )
    (list
     "blockName" "stoneSlab" 
     "blockID" 43
     "blockIndexInTexture" 6
     "isOpaqueCube" t
     "renderAsNormalBlock" t
     "renderType" 0
     "lightValue" 0
     "lightOpacity" 255
     "canBlockGrass" nil
     "tickOnLoad" nil
     "nometanotify" nil
     "isCollidable" t
     "blockResistance" 30.0
     "blockHardness" 2.0
     "slipperiness" 0.6
     "blockMaterial" "net.minecraft.src.Material@53e25b76" 
     "blockParticleGravity" 1.0
     "stepSound " "net.minecraft.src.StepSound@73a8dfcc" 
     "maxZ" 1.0
     "maxY" 1.0
     "maxX" 1.0
     "minZ" 0.0
     "minY" 0.0
     "minX" 0.0
     )
    (list
     "blockName" "stoneSlab" 
     "blockID" 44
     "blockIndexInTexture" 6
     "isOpaqueCube" nil
     "renderAsNormalBlock" nil
     "renderType" 0
     "lightValue" 0
     "lightOpacity" 255
     "canBlockGrass" nil
     "tickOnLoad" nil
     "nometanotify" nil
     "isCollidable" t
     "blockResistance" 30.0
     "blockHardness" 2.0
     "slipperiness" 0.6
     "blockMaterial" "net.minecraft.src.Material@53e25b76" 
     "blockParticleGravity" 1.0
     "stepSound " "net.minecraft.src.StepSound@73a8dfcc" 
     "maxZ" 1.0
     "maxY" 0.5
     "maxX" 1.0
     "minZ" 0.0
     "minY" 0.0
     "minX" 0.0
     )
    (list
     "blockName" "brick" 
     "blockID" 45
     "blockIndexInTexture" 7
     "isOpaqueCube" t
     "renderAsNormalBlock" t
     "renderType" 0
     "lightValue" 0
     "lightOpacity" 255
     "canBlockGrass" nil
     "tickOnLoad" nil
     "nometanotify" nil
     "isCollidable" t
     "blockResistance" 30.0
     "blockHardness" 2.0
     "slipperiness" 0.6
     "blockMaterial" "net.minecraft.src.Material@53e25b76" 
     "blockParticleGravity" 1.0
     "stepSound " "net.minecraft.src.StepSound@73a8dfcc" 
     "maxZ" 1.0
     "maxY" 1.0
     "maxX" 1.0
     "minZ" 0.0
     "minY" 0.0
     "minX" 0.0
     )
    (list
     "blockName" "tnt" 
     "blockID" 46
     "blockIndexInTexture" 8
     "isOpaqueCube" t
     "renderAsNormalBlock" t
     "renderType" 0
     "lightValue" 0
     "lightOpacity" 255
     "canBlockGrass" nil
     "tickOnLoad" nil
     "nometanotify" nil
     "isCollidable" t
     "blockResistance" 0.0
     "blockHardness" 0.0
     "slipperiness" 0.6
     "blockMaterial" "net.minecraft.src.Material@3d012ddd" 
     "blockParticleGravity" 1.0
     "stepSound " "net.minecraft.src.StepSound@7e774085" 
     "maxZ" 1.0
     "maxY" 1.0
     "maxX" 1.0
     "minZ" 0.0
     "minY" 0.0
     "minX" 0.0
     )
    (list
     "blockName" "bookshelf" 
     "blockID" 47
     "blockIndexInTexture" 35
     "isOpaqueCube" t
     "renderAsNormalBlock" t
     "renderType" 0
     "lightValue" 0
     "lightOpacity" 255
     "canBlockGrass" nil
     "tickOnLoad" nil
     "nometanotify" nil
     "isCollidable" t
     "blockResistance" 7.5
     "blockHardness" 1.5
     "slipperiness" 0.6
     "blockMaterial" "net.minecraft.src.Material@1c655221" 
     "blockParticleGravity" 1.0
     "stepSound " "net.minecraft.src.StepSound@58d25a40" 
     "maxZ" 1.0
     "maxY" 1.0
     "maxX" 1.0
     "minZ" 0.0
     "minY" 0.0
     "minX" 0.0
     )
    (list
     "blockName" "stoneMoss" 
     "blockID" 48
     "blockIndexInTexture" 36
     "isOpaqueCube" t
     "renderAsNormalBlock" t
     "renderType" 0
     "lightValue" 0
     "lightOpacity" 255
     "canBlockGrass" nil
     "tickOnLoad" nil
     "nometanotify" nil
     "isCollidable" t
     "blockResistance" 30.0
     "blockHardness" 2.0
     "slipperiness" 0.6
     "blockMaterial" "net.minecraft.src.Material@53e25b76" 
     "blockParticleGravity" 1.0
     "stepSound " "net.minecraft.src.StepSound@73a8dfcc" 
     "maxZ" 1.0
     "maxY" 1.0
     "maxX" 1.0
     "minZ" 0.0
     "minY" 0.0
     "minX" 0.0
     )
    (list
     "blockName" "obsidian" 
     "blockID" 49
     "blockIndexInTexture" 37
     "isOpaqueCube" t
     "renderAsNormalBlock" t
     "renderType" 0
     "lightValue" 0
     "lightOpacity" 255
     "canBlockGrass" nil
     "tickOnLoad" nil
     "nometanotify" nil
     "isCollidable" t
     "blockResistance" 6000.0
     "blockHardness" 10.0
     "slipperiness" 0.6
     "blockMaterial" "net.minecraft.src.Material@53e25b76" 
     "blockParticleGravity" 1.0
     "stepSound " "net.minecraft.src.StepSound@73a8dfcc" 
     "maxZ" 1.0
     "maxY" 1.0
     "maxX" 1.0
     "minZ" 0.0
     "minY" 0.0
     "minX" 0.0
     )
    (list
     "blockName" "torch" 
     "blockID" 50
     "blockIndexInTexture" 80
     "isOpaqueCube" nil
     "renderAsNormalBlock" nil
     "renderType" 2
     "lightValue" 14
     "lightOpacity" 0
     "canBlockGrass" t
     "tickOnLoad" t
     "nometanotify" t
     "isCollidable" ,(if nil t nil)
     "blockResistance" 0.0
     "blockHardness" 0.0
     "slipperiness" 0.6
     "blockMaterial" "net.minecraft.src.MaterialLogic@c2e1f26" 
     "blockParticleGravity" 1.0
     "stepSound " "net.minecraft.src.StepSound@58d25a40" 
     "maxZ" 1.0
     "maxY" 1.0
     "maxX" 1.0
     "minZ" 0.0
     "minY" 0.0
     "minX" 0.0
     )
    (list
     "blockName" "fire" 
     "blockID" 51
     "blockIndexInTexture" 31
     "isOpaqueCube" nil
     "renderAsNormalBlock" nil
     "renderType" 3
     "lightValue" 15
     "lightOpacity" 0
     "canBlockGrass" t
     "tickOnLoad" t
     "nometanotify" t
     "isCollidable" nil
     "blockResistance" 0.0
     "blockHardness" 0.0
     "slipperiness" 0.6
     "blockMaterial" "net.minecraft.src.MaterialTransparent@6f2b958e" 
     "blockParticleGravity" 1.0
     "stepSound " "net.minecraft.src.StepSound@58d25a40" 
     "maxZ" 1.0
     "maxY" 1.0
     "maxX" 1.0
     "minZ" 0.0
     "minY" 0.0
     "minX" 0.0
     )
    (list
     "blockName" "mobSpawner" 
     "blockID" 52
     "blockIndexInTexture" 65
     "isOpaqueCube" nil
     "renderAsNormalBlock" t
     "renderType" 0
     "lightValue" 0
     "lightOpacity" 0
     "canBlockGrass" nil
     "tickOnLoad" nil
     "nometanotify" nil
     "isCollidable" t
     "blockResistance" 25.0
     "blockHardness" 5.0
     "slipperiness" 0.6
     "blockMaterial" "net.minecraft.src.Material@53e25b76" 
     "blockParticleGravity" 1.0
     "stepSound " "net.minecraft.src.StepSound@dcf3e99" 
     "maxZ" 1.0
     "maxY" 1.0
     "maxX" 1.0
     "minZ" 0.0
     "minY" 0.0
     "minX" 0.0
     )
    (list
     "blockName" "stairsWood" 
     "blockID" 53
     "blockIndexInTexture" 4
     "isOpaqueCube" nil
     "renderAsNormalBlock" nil
     "renderType" 10
     "lightValue" 0
     "lightOpacity" 255
     "canBlockGrass" nil
     "tickOnLoad" nil
     "nometanotify" t
     "isCollidable" t
     "blockResistance" 15.0
     "blockHardness" 2.0
     "slipperiness" 0.6
     "blockMaterial" "net.minecraft.src.Material@1c655221" 
     "blockParticleGravity" 1.0
     "stepSound " "net.minecraft.src.StepSound@58d25a40" 
     "maxZ" 1.0
     "maxY" 1.0
     "maxX" 1.0
     "minZ" 0.0
     "minY" 0.0
     "minX" 0.0
     )
    (list
     "blockName" "chest" 
     "blockID" 54
     "blockIndexInTexture" 26
     "isOpaqueCube" t
     "renderAsNormalBlock" t
     "renderType" 0
     "lightValue" 0
     "lightOpacity" 255
     "canBlockGrass" nil
     "tickOnLoad" nil
     "nometanotify" t
     "isCollidable" t
     "blockResistance" 12.5
     "blockHardness" 2.5
     "slipperiness" 0.6
     "blockMaterial" "net.minecraft.src.Material@1c655221" 
     "blockParticleGravity" 1.0
     "stepSound " "net.minecraft.src.StepSound@58d25a40" 
     "maxZ" 1.0
     "maxY" 1.0
     "maxX" 1.0
     "minZ" 0.0
     "minY" 0.0
     "minX" 0.0
     )
    (list
     "blockName" "redstoneDust" 
     "blockID" 55
     "blockIndexInTexture" 164
     "isOpaqueCube" nil
     "renderAsNormalBlock" nil
     "renderType" 5
     "lightValue" 0
     "lightOpacity" 0
     "canBlockGrass" t
     "tickOnLoad" nil
     "nometanotify" t
     "isCollidable" ,(if nil t nil)
     "blockResistance" 0.0
     "blockHardness" 0.0
     "slipperiness" 0.6
     "blockMaterial" "net.minecraft.src.MaterialLogic@c2e1f26" 
     "blockParticleGravity" 1.0
     "stepSound " "net.minecraft.src.StepSound@442d9b6e" 
     "maxZ" 1.0
     "maxY" 0.0625
     "maxX" 1.0
     "minZ" 0.0
     "minY" 0.0
     "minX" 0.0
     )
    (list
     "blockName" "oreDiamond" 
     "blockID" 56
     "blockIndexInTexture" 50
     "isOpaqueCube" t
     "renderAsNormalBlock" t
     "renderType" 0
     "lightValue" 0
     "lightOpacity" 255
     "canBlockGrass" nil
     "tickOnLoad" nil
     "nometanotify" nil
     "isCollidable" t
     "blockResistance" 15.0
     "blockHardness" 3.0
     "slipperiness" 0.6
     "blockMaterial" "net.minecraft.src.Material@53e25b76" 
     "blockParticleGravity" 1.0
     "stepSound " "net.minecraft.src.StepSound@73a8dfcc" 
     "maxZ" 1.0
     "maxY" 1.0
     "maxX" 1.0
     "minZ" 0.0
     "minY" 0.0
     "minX" 0.0
     )
    (list
     "blockName" "blockDiamond" 
     "blockID" 57
     "blockIndexInTexture" 24
     "isOpaqueCube" t
     "renderAsNormalBlock" t
     "renderType" 0
     "lightValue" 0
     "lightOpacity" 255
     "canBlockGrass" nil
     "tickOnLoad" nil
     "nometanotify" nil
     "isCollidable" t
     "blockResistance" 30.0
     "blockHardness" 5.0
     "slipperiness" 0.6
     "blockMaterial" "net.minecraft.src.Material@75a1cd57" 
     "blockParticleGravity" 1.0
     "stepSound " "net.minecraft.src.StepSound@dcf3e99" 
     "maxZ" 1.0
     "maxY" 1.0
     "maxX" 1.0
     "minZ" 0.0
     "minY" 0.0
     "minX" 0.0
     )
    (list
     "blockName" "workbench" 
     "blockID" 58
     "blockIndexInTexture" 59
     "isOpaqueCube" t
     "renderAsNormalBlock" t
     "renderType" 0
     "lightValue" 0
     "lightOpacity" 255
     "canBlockGrass" nil
     "tickOnLoad" nil
     "nometanotify" nil
     "isCollidable" t
     "blockResistance" 12.5
     "blockHardness" 2.5
     "slipperiness" 0.6
     "blockMaterial" "net.minecraft.src.Material@1c655221" 
     "blockParticleGravity" 1.0
     "stepSound " "net.minecraft.src.StepSound@58d25a40" 
     "maxZ" 1.0
     "maxY" 1.0
     "maxX" 1.0
     "minZ" 0.0
     "minY" 0.0
     "minX" 0.0
     )
    (list
     "blockName" "crops" 
     "blockID" 59
     "blockIndexInTexture" 88
     "isOpaqueCube" nil
     "renderAsNormalBlock" nil
     "renderType" 6
     "lightValue" 0
     "lightOpacity" 0
     "canBlockGrass" t
     "tickOnLoad" t
     "nometanotify" t
     "isCollidable" ,(if nil t nil)
     "blockResistance" 0.0
     "blockHardness" 0.0
     "slipperiness" 0.6
     "blockMaterial" "net.minecraft.src.MaterialLogic@1b701da1" 
     "blockParticleGravity" 1.0
     "stepSound " "net.minecraft.src.StepSound@7e774085" 
     "maxZ" 1.0
     "maxY" 0.25
     "maxX" 1.0
     "minZ" 0.0
     "minY" 0.0
     "minX" 0.0
     )
    (list
     "blockName" "farmland" 
     "blockID" 60
     "blockIndexInTexture" 87
     "isOpaqueCube" nil
     "renderAsNormalBlock" nil
     "renderType" 0
     "lightValue" 0
     "lightOpacity" 255
     "canBlockGrass" nil
     "tickOnLoad" t
     "nometanotify" nil
     "isCollidable" t
     "blockResistance" 3.0
     "blockHardness" 0.6
     "slipperiness" 0.6
     "blockMaterial" "net.minecraft.src.Material@3f8f9dd6" 
     "blockParticleGravity" 1.0
     "stepSound " "net.minecraft.src.StepSound@aec6354" 
     "maxZ" 1.0
     "maxY" 0.9375
     "maxX" 1.0
     "minZ" 0.0
     "minY" 0.0
     "minX" 0.0
     )
    (list
     "blockName" "furnace" 
     "blockID" 61
     "blockIndexInTexture" 45
     "isOpaqueCube" t
     "renderAsNormalBlock" t
     "renderType" 0
     "lightValue" 0
     "lightOpacity" 255
     "canBlockGrass" nil
     "tickOnLoad" nil
     "nometanotify" t
     "isCollidable" t
     "blockResistance" 17.5
     "blockHardness" 3.5
     "slipperiness" 0.6
     "blockMaterial" "net.minecraft.src.Material@53e25b76" 
     "blockParticleGravity" 1.0
     "stepSound " "net.minecraft.src.StepSound@73a8dfcc" 
     "maxZ" 1.0
     "maxY" 1.0
     "maxX" 1.0
     "minZ" 0.0
     "minY" 0.0
     "minX" 0.0
     )
    (list
     "blockName" "furnace" 
     "blockID" 62
     "blockIndexInTexture" 45
     "isOpaqueCube" t
     "renderAsNormalBlock" t
     "renderType" 0
     "lightValue" 13
     "lightOpacity" 255
     "canBlockGrass" nil
     "tickOnLoad" nil
     "nometanotify" t
     "isCollidable" t
     "blockResistance" 17.5
     "blockHardness" 3.5
     "slipperiness" 0.6
     "blockMaterial" "net.minecraft.src.Material@53e25b76" 
     "blockParticleGravity" 1.0
     "stepSound " "net.minecraft.src.StepSound@73a8dfcc" 
     "maxZ" 1.0
     "maxY" 1.0
     "maxX" 1.0
     "minZ" 0.0
     "minY" 0.0
     "minX" 0.0
     )
    (list
     "blockName" "sign" 
     "blockID" 63
     "blockIndexInTexture" 4
     "isOpaqueCube" nil
     "renderAsNormalBlock" nil
     "renderType" -1
     "lightValue" 0
     "lightOpacity" 0
     "canBlockGrass" nil
     "tickOnLoad" nil
     "nometanotify" t
     "isCollidable" ,(if nil t nil)
     "blockResistance" 5.0
     "blockHardness" 1.0
     "slipperiness" 0.6
     "blockMaterial" "net.minecraft.src.Material@1c655221" 
     "blockParticleGravity" 1.0
     "stepSound " "net.minecraft.src.StepSound@58d25a40" 
     "maxZ" 0.75
     "maxY" 1.0
     "maxX" 0.75
     "minZ" 0.25
     "minY" 0.0
     "minX" 0.25
     )
    (list
     "blockName" "doorWood" 
     "blockID" 64
     "blockIndexInTexture" 97
     "isOpaqueCube" nil
     "renderAsNormalBlock" nil
     "renderType" 7
     "lightValue" 0
     "lightOpacity" 0
     "canBlockGrass" nil
     "tickOnLoad" nil
     "nometanotify" t
     "isCollidable" t
     "blockResistance" 15.0
     "blockHardness" 3.0
     "slipperiness" 0.6
     "blockMaterial" "net.minecraft.src.Material@1c655221" 
     "blockParticleGravity" 1.0
     "stepSound " "net.minecraft.src.StepSound@58d25a40" 
     "maxZ" 1.0
     "maxY" 1.0
     "maxX" 1.0
     "minZ" 0.0
     "minY" 0.0
     "minX" 0.0
     )
    (list
     "blockName" "ladder" 
     "blockID" 65
     "blockIndexInTexture" 83
     "isOpaqueCube" nil
     "renderAsNormalBlock" nil
     "renderType" 8
     "lightValue" 0
     "lightOpacity" 0
     "canBlockGrass" t
     "tickOnLoad" nil
     "nometanotify" t
     "isCollidable" ,(if nil t nil)
     "blockResistance" 2.0
     "blockHardness" 0.4
     "slipperiness" 0.6
     "blockMaterial" "net.minecraft.src.MaterialLogic@c2e1f26" 
     "blockParticleGravity" 1.0
     "stepSound " "net.minecraft.src.StepSound@58d25a40" 
     "maxZ" 1.0
     "maxY" 1.0
     "maxX" 1.0
     "minZ" 0.0
     "minY" 0.0
     "minX" 0.0
     )
    (list
     "blockName" "rail" 
     "blockID" 66
     "blockIndexInTexture" 128
     "isOpaqueCube" nil
     "renderAsNormalBlock" nil
     "renderType" 9
     "lightValue" 0
     "lightOpacity" 0
     "canBlockGrass" t
     "tickOnLoad" nil
     "nometanotify" t
     "isCollidable" ,(if nil t nil)
     "blockResistance" 3.5
     "blockHardness" 0.7
     "slipperiness" 0.6
     "blockMaterial" "net.minecraft.src.MaterialLogic@c2e1f26" 
     "blockParticleGravity" 1.0
     "stepSound " "net.minecraft.src.StepSound@dcf3e99" 
     "maxZ" 1.0
     "maxY" 0.125
     "maxX" 1.0
     "minZ" 0.0
     "minY" 0.0
     "minX" 0.0
     )
    (list
     "blockName" "stairsStone" 
     "blockID" 67
     "blockIndexInTexture" 16
     "isOpaqueCube" nil
     "renderAsNormalBlock" nil
     "renderType" 10
     "lightValue" 0
     "lightOpacity" 255
     "canBlockGrass" nil
     "tickOnLoad" nil
     "nometanotify" t
     "isCollidable" t
     "blockResistance" 30.0
     "blockHardness" 2.0
     "slipperiness" 0.6
     "blockMaterial" "net.minecraft.src.Material@53e25b76" 
     "blockParticleGravity" 1.0
     "stepSound " "net.minecraft.src.StepSound@73a8dfcc" 
     "maxZ" 1.0
     "maxY" 1.0
     "maxX" 1.0
     "minZ" 0.0
     "minY" 0.0
     "minX" 0.0
     )
    (list
     "blockName" "sign" 
     "blockID" 68
     "blockIndexInTexture" 4
     "isOpaqueCube" nil
     "renderAsNormalBlock" nil
     "renderType" -1
     "lightValue" 0
     "lightOpacity" 0
     "canBlockGrass" nil
     "tickOnLoad" nil
     "nometanotify" t
     "isCollidable" ,(if nil t nil)
     "blockResistance" 5.0
     "blockHardness" 1.0
     "slipperiness" 0.6
     "blockMaterial" "net.minecraft.src.Material@1c655221" 
     "blockParticleGravity" 1.0
     "stepSound " "net.minecraft.src.StepSound@58d25a40" 
     "maxZ" 0.75
     "maxY" 1.0
     "maxX" 0.75
     "minZ" 0.25
     "minY" 0.0
     "minX" 0.25
     )
    (list
     "blockName" "lever" 
     "blockID" 69
     "blockIndexInTexture" 96
     "isOpaqueCube" nil
     "renderAsNormalBlock" nil
     "renderType" 12
     "lightValue" 0
     "lightOpacity" 0
     "canBlockGrass" t
     "tickOnLoad" nil
     "nometanotify" t
     "isCollidable" ,(if nil t nil)
     "blockResistance" 2.5
     "blockHardness" 0.5
     "slipperiness" 0.6
     "blockMaterial" "net.minecraft.src.MaterialLogic@c2e1f26" 
     "blockParticleGravity" 1.0
     "stepSound " "net.minecraft.src.StepSound@58d25a40" 
     "maxZ" 1.0
     "maxY" 1.0
     "maxX" 1.0
     "minZ" 0.0
     "minY" 0.0
     "minX" 0.0
     )
    (list
     "blockName" "pressurePlate" 
     "blockID" 70
     "blockIndexInTexture" 1
     "isOpaqueCube" nil
     "renderAsNormalBlock" nil
     "renderType" 0
     "lightValue" 0
     "lightOpacity" 0
     "canBlockGrass" nil
     "tickOnLoad" t
     "nometanotify" t
     "isCollidable" ,(if nil t nil)
     "blockResistance" 2.5
     "blockHardness" 0.5
     "slipperiness" 0.6
     "blockMaterial" "net.minecraft.src.Material@53e25b76" 
     "blockParticleGravity" 1.0
     "stepSound " "net.minecraft.src.StepSound@73a8dfcc" 
     "maxZ" 0.9375
     "maxY" 0.03125
     "maxX" 0.9375
     "minZ" 0.0625
     "minY" 0.0
     "minX" 0.0625
     )
    (list
     "blockName" "doorIron" 
     "blockID" 71
     "blockIndexInTexture" 98
     "isOpaqueCube" nil
     "renderAsNormalBlock" nil
     "renderType" 7
     "lightValue" 0
     "lightOpacity" 0
     "canBlockGrass" nil
     "tickOnLoad" nil
     "nometanotify" t
     "isCollidable" t
     "blockResistance" 25.0
     "blockHardness" 5.0
     "slipperiness" 0.6
     "blockMaterial" "net.minecraft.src.Material@75a1cd57" 
     "blockParticleGravity" 1.0
     "stepSound " "net.minecraft.src.StepSound@dcf3e99" 
     "maxZ" 1.0
     "maxY" 1.0
     "maxX" 1.0
     "minZ" 0.0
     "minY" 0.0
     "minX" 0.0
     )
    (list
     "blockName" "pressurePlate" 
     "blockID" 72
     "blockIndexInTexture" 4
     "isOpaqueCube" nil
     "renderAsNormalBlock" nil
     "renderType" 0
     "lightValue" 0
     "lightOpacity" 0
     "canBlockGrass" nil
     "tickOnLoad" t
     "nometanotify" t
     "isCollidable" ,(if nil t nil)
     "blockResistance" 2.5
     "blockHardness" 0.5
     "slipperiness" 0.6
     "blockMaterial" "net.minecraft.src.Material@1c655221" 
     "blockParticleGravity" 1.0
     "stepSound " "net.minecraft.src.StepSound@58d25a40" 
     "maxZ" 0.9375
     "maxY" 0.03125
     "maxX" 0.9375
     "minZ" 0.0625
     "minY" 0.0
     "minX" 0.0625
     )
    (list
     "blockName" "oreRedstone" 
     "blockID" 73
     "blockIndexInTexture" 51
     "isOpaqueCube" t
     "renderAsNormalBlock" t
     "renderType" 0
     "lightValue" 0
     "lightOpacity" 255
     "canBlockGrass" nil
     "tickOnLoad" nil
     "nometanotify" t
     "isCollidable" t
     "blockResistance" 15.0
     "blockHardness" 3.0
     "slipperiness" 0.6
     "blockMaterial" "net.minecraft.src.Material@53e25b76" 
     "blockParticleGravity" 1.0
     "stepSound " "net.minecraft.src.StepSound@73a8dfcc" 
     "maxZ" 1.0
     "maxY" 1.0
     "maxX" 1.0
     "minZ" 0.0
     "minY" 0.0
     "minX" 0.0
     )
    (list
     "blockName" "oreRedstone" 
     "blockID" 74
     "blockIndexInTexture" 51
     "isOpaqueCube" t
     "renderAsNormalBlock" t
     "renderType" 0
     "lightValue" 9
     "lightOpacity" 255
     "canBlockGrass" nil
     "tickOnLoad" t
     "nometanotify" t
     "isCollidable" t
     "blockResistance" 15.0
     "blockHardness" 3.0
     "slipperiness" 0.6
     "blockMaterial" "net.minecraft.src.Material@53e25b76" 
     "blockParticleGravity" 1.0
     "stepSound " "net.minecraft.src.StepSound@73a8dfcc" 
     "maxZ" 1.0
     "maxY" 1.0
     "maxX" 1.0
     "minZ" 0.0
     "minY" 0.0
     "minX" 0.0
     )
    (list
     "blockName" "notGate" 
     "blockID" 75
     "blockIndexInTexture" 115
     "isOpaqueCube" nil
     "renderAsNormalBlock" nil
     "renderType" 2
     "lightValue" 0
     "lightOpacity" 0
     "canBlockGrass" t
     "tickOnLoad" t
     "nometanotify" t
     "isCollidable" t
     "blockResistance" 0.0
     "blockHardness" 0.0
     "slipperiness" 0.6
     "blockMaterial" "net.minecraft.src.MaterialLogic@c2e1f26" 
     "blockParticleGravity" 1.0
     "stepSound " "net.minecraft.src.StepSound@58d25a40" 
     "maxZ" 1.0
     "maxY" 1.0
     "maxX" 1.0
     "minZ" 0.0
     "minY" 0.0
     "minX" 0.0
     )
    (list
     "blockName" "notGate" 
     "blockID" 76
     "blockIndexInTexture" 99
     "isOpaqueCube" nil
     "renderAsNormalBlock" nil
     "renderType" 2
     "lightValue" 7
     "lightOpacity" 0
     "canBlockGrass" t
     "tickOnLoad" t
     "nometanotify" t
     "isCollidable" t
     "blockResistance" 0.0
     "blockHardness" 0.0
     "slipperiness" 0.6
     "blockMaterial" "net.minecraft.src.MaterialLogic@c2e1f26" 
     "blockParticleGravity" 1.0
     "stepSound " "net.minecraft.src.StepSound@58d25a40" 
     "maxZ" 1.0
     "maxY" 1.0
     "maxX" 1.0
     "minZ" 0.0
     "minY" 0.0
     "minX" 0.0
     )
    (list
     "blockName" "button" 
     "blockID" 77
     "blockIndexInTexture" 1
     "isOpaqueCube" nil
     "renderAsNormalBlock" nil
     "renderType" 0
     "lightValue" 0
     "lightOpacity" 0
     "canBlockGrass" t
     "tickOnLoad" t
     "nometanotify" t
     "isCollidable" ,(if nil t nil)
     "blockResistance" 2.5
     "blockHardness" 0.5
     "slipperiness" 0.6
     "blockMaterial" "net.minecraft.src.MaterialLogic@c2e1f26" 
     "blockParticleGravity" 1.0
     "stepSound " "net.minecraft.src.StepSound@73a8dfcc" 
     "maxZ" 1.0
     "maxY" 1.0
     "maxX" 1.0
     "minZ" 0.0
     "minY" 0.0
     "minX" 0.0
     )
    (list
     "blockName" "snow" 
     "blockID" 78
     "blockIndexInTexture" 66
     "isOpaqueCube" nil
     "renderAsNormalBlock" nil
     "renderType" 0
     "lightValue" 0
     "lightOpacity" 0
     "canBlockGrass" t
     "tickOnLoad" t
     "nometanotify" nil
     "isCollidable" t
     "blockResistance" 0.5
     "blockHardness" 0.1
     "slipperiness" 0.6
     "blockMaterial" "net.minecraft.src.MaterialLogic@1eb44e46" 
     "blockParticleGravity" 1.0
     "stepSound " "net.minecraft.src.StepSound@1ee0005" 
     "maxZ" 1.0
     "maxY" 0.125
     "maxX" 1.0
     "minZ" 0.0
     "minY" 0.0
     "minX" 0.0
     )
    (list
     "blockName" "ice" 
     "blockID" 79
     "blockIndexInTexture" 67
     "isOpaqueCube" nil
     "renderAsNormalBlock" t
     "renderType" 0
     "lightValue" 0
     "lightOpacity" 3
     "canBlockGrass" nil
     "tickOnLoad" t
     "nometanotify" nil
     "isCollidable" t
     "blockResistance" 2.5
     "blockHardness" 0.5
     "slipperiness" 0.98
     "blockMaterial" "net.minecraft.src.Material@6504e3b2" 
     "blockParticleGravity" 1.0
     "stepSound " "net.minecraft.src.StepSoundStone@7fbe847c" 
     "maxZ" 1.0
     "maxY" 1.0
     "maxX" 1.0
     "minZ" 0.0
     "minY" 0.0
     "minX" 0.0
     )
    (list
     "blockName" "snow" 
     "blockID" 80
     "blockIndexInTexture" 66
     "isOpaqueCube" t
     "renderAsNormalBlock" t
     "renderType" 0
     "lightValue" 0
     "lightOpacity" 255
     "canBlockGrass" nil
     "tickOnLoad" t
     "nometanotify" nil
     "isCollidable" t
     "blockResistance" 1.0
     "blockHardness" 0.2
     "slipperiness" 0.6
     "blockMaterial" "net.minecraft.src.Material@515f550a" 
     "blockParticleGravity" 1.0
     "stepSound " "net.minecraft.src.StepSound@1ee0005" 
     "maxZ" 1.0
     "maxY" 1.0
     "maxX" 1.0
     "minZ" 0.0
     "minY" 0.0
     "minX" 0.0
     )
    (list
     "blockName" "cactus" 
     "blockID" 81
     "blockIndexInTexture" 70
     "isOpaqueCube" nil
     "renderAsNormalBlock" nil
     "renderType" 13
     "lightValue" 0
     "lightOpacity" 0
     "canBlockGrass" nil
     "tickOnLoad" t
     "nometanotify" nil
     "isCollidable" t
     "blockResistance" 2.0
     "blockHardness" 0.4
     "slipperiness" 0.6
     "blockMaterial" "net.minecraft.src.Material@626b2d4a" 
     "blockParticleGravity" 1.0
     "stepSound " "net.minecraft.src.StepSound@1ee0005" 
     "maxZ" 1.0
     "maxY" 1.0
     "maxX" 1.0
     "minZ" 0.0
     "minY" 0.0
     "minX" 0.0
     )
    (list
     "blockName" "clay" 
     "blockID" 82
     "blockIndexInTexture" 72
     "isOpaqueCube" t
     "renderAsNormalBlock" t
     "renderType" 0
     "lightValue" 0
     "lightOpacity" 255
     "canBlockGrass" nil
     "tickOnLoad" nil
     "nometanotify" nil
     "isCollidable" t
     "blockResistance" 3.0
     "blockHardness" 0.6
     "slipperiness" 0.6
     "blockMaterial" "net.minecraft.src.Material@5e91993f" 
     "blockParticleGravity" 1.0
     "stepSound " "net.minecraft.src.StepSound@aec6354" 
     "maxZ" 1.0
     "maxY" 1.0
     "maxX" 1.0
     "minZ" 0.0
     "minY" 0.0
     "minX" 0.0
     )
    (list
     "blockName" "reeds" 
     "blockID" 83
     "blockIndexInTexture" 73
     "isOpaqueCube" nil
     "renderAsNormalBlock" nil
     "renderType" 1
     "lightValue" 0
     "lightOpacity" 0
     "canBlockGrass" t
     "tickOnLoad" t
     "nometanotify" nil
     "isCollidable" ,(if nil t nil)
     "blockResistance" 0.0
     "blockHardness" 0.0
     "slipperiness" 0.6
     "blockMaterial" "net.minecraft.src.MaterialLogic@1b701da1" 
     "blockParticleGravity" 1.0
     "stepSound " "net.minecraft.src.StepSound@7e774085" 
     "maxZ" 0.875
     "maxY" 1.0
     "maxX" 0.875
     "minZ" 0.125
     "minY" 0.0
     "minX" 0.125
     )
    (list
     "blockName" "jukebox" 
     "blockID" 84
     "blockIndexInTexture" 74
     "isOpaqueCube" t
     "renderAsNormalBlock" t
     "renderType" 0
     "lightValue" 0
     "lightOpacity" 255
     "canBlockGrass" nil
     "tickOnLoad" nil
     "nometanotify" t
     "isCollidable" t
     "blockResistance" 30.0
     "blockHardness" 2.0
     "slipperiness" 0.6
     "blockMaterial" "net.minecraft.src.Material@1c655221" 
     "blockParticleGravity" 1.0
     "stepSound " "net.minecraft.src.StepSound@73a8dfcc" 
     "maxZ" 1.0
     "maxY" 1.0
     "maxX" 1.0
     "minZ" 0.0
     "minY" 0.0
     "minX" 0.0
     )
    (list
     "blockName" "fence" 
     "blockID" 85
     "blockIndexInTexture" 4
     "isOpaqueCube" nil
     "renderAsNormalBlock" nil
     "renderType" 11
     "lightValue" 0
     "lightOpacity" 0
     "canBlockGrass" nil
     "tickOnLoad" nil
     "nometanotify" t
     "isCollidable" t
     "blockResistance" 15.0
     "blockHardness" 2.0
     "slipperiness" 0.6
     "blockMaterial" "net.minecraft.src.Material@1c655221" 
     "blockParticleGravity" 1.0
     "stepSound " "net.minecraft.src.StepSound@58d25a40" 
     "maxZ" 1.0
     "maxY" 1.0
     "maxX" 1.0
     "minZ" 0.0
     "minY" 0.0
     "minX" 0.0
     )
    (list
     "blockName" "pumpkin" 
     "blockID" 86
     "blockIndexInTexture" 102
     "isOpaqueCube" t
     "renderAsNormalBlock" t
     "renderType" 0
     "lightValue" 0
     "lightOpacity" 255
     "canBlockGrass" nil
     "tickOnLoad" t
     "nometanotify" t
     "isCollidable" t
     "blockResistance" 5.0
     "blockHardness" 1.0
     "slipperiness" 0.6
     "blockMaterial" "net.minecraft.src.Material@1c4af82c" 
     "blockParticleGravity" 1.0
     "stepSound " "net.minecraft.src.StepSound@58d25a40" 
     "maxZ" 1.0
     "maxY" 1.0
     "maxX" 1.0
     "minZ" 0.0
     "minY" 0.0
     "minX" 0.0
     )
    (list
     "blockName" "hellrock" 
     "blockID" 87
     "blockIndexInTexture" 103
     "isOpaqueCube" t
     "renderAsNormalBlock" t
     "renderType" 0
     "lightValue" 0
     "lightOpacity" 255
     "canBlockGrass" nil
     "tickOnLoad" nil
     "nometanotify" nil
     "isCollidable" t
     "blockResistance" 2.0
     "blockHardness" 0.4
     "slipperiness" 0.6
     "blockMaterial" "net.minecraft.src.Material@53e25b76" 
     "blockParticleGravity" 1.0
     "stepSound " "net.minecraft.src.StepSound@73a8dfcc" 
     "maxZ" 1.0
     "maxY" 1.0
     "maxX" 1.0
     "minZ" 0.0
     "minY" 0.0
     "minX" 0.0
     )
    (list
     "blockName" "hellsand" 
     "blockID" 88
     "blockIndexInTexture" 104
     "isOpaqueCube" t
     "renderAsNormalBlock" t
     "renderType" 0
     "lightValue" 0
     "lightOpacity" 255
     "canBlockGrass" nil
     "tickOnLoad" nil
     "nometanotify" nil
     "isCollidable" t
     "blockResistance" 2.5
     "blockHardness" 0.5
     "slipperiness" 0.6
     "blockMaterial" "net.minecraft.src.Material@15615099" 
     "blockParticleGravity" 1.0
     "stepSound " "net.minecraft.src.StepSoundSand@1edf1c96" 
     "maxZ" 1.0
     "maxY" 1.0
     "maxX" 1.0
     "minZ" 0.0
     "minY" 0.0
     "minX" 0.0
     )
    (list
     "blockName" "lightgem" 
     "blockID" 89
     "blockIndexInTexture" 105
     "isOpaqueCube" t
     "renderAsNormalBlock" t
     "renderType" 0
     "lightValue" 15
     "lightOpacity" 255
     "canBlockGrass" nil
     "tickOnLoad" nil
     "nometanotify" nil
     "isCollidable" t
     "blockResistance" 1.5
     "blockHardness" 0.3
     "slipperiness" 0.6
     "blockMaterial" "net.minecraft.src.Material@53e25b76" 
     "blockParticleGravity" 1.0
     "stepSound " "net.minecraft.src.StepSoundStone@7fbe847c" 
     "maxZ" 1.0
     "maxY" 1.0
     "maxX" 1.0
     "minZ" 0.0
     "minY" 0.0
     "minX" 0.0
     )
    (list
     "blockName" "portal" 
     "blockID" 90
     "blockIndexInTexture" 14
     "isOpaqueCube" nil
     "renderAsNormalBlock" nil
     "renderType" 0
     "lightValue" 11
     "lightOpacity" 0
     "canBlockGrass" t
     "tickOnLoad" nil
     "nometanotify" nil
     "isCollidable" ,(if nil t nil)
     "blockResistance" 0.0
     "blockHardness" -1.0
     "slipperiness" 0.6
     "blockMaterial" "net.minecraft.src.MaterialPortal@379619aa" 
     "blockParticleGravity" 1.0
     "stepSound " "net.minecraft.src.StepSoundStone@7fbe847c" 
     "maxZ" 1.0
     "maxY" 1.0
     "maxX" 1.0
     "minZ" 0.0
     "minY" 0.0
     "minX" 0.0
     )
    (list
     "blockName" "litpumpkin" 
     "blockID" 91
     "blockIndexInTexture" 102
     "isOpaqueCube" t
     "renderAsNormalBlock" t
     "renderType" 0
     "lightValue" 15
     "lightOpacity" 255
     "canBlockGrass" nil
     "tickOnLoad" t
     "nometanotify" t
     "isCollidable" t
     "blockResistance" 5.0
     "blockHardness" 1.0
     "slipperiness" 0.6
     "blockMaterial" "net.minecraft.src.Material@1c4af82c" 
     "blockParticleGravity" 1.0
     "stepSound " "net.minecraft.src.StepSound@58d25a40" 
     "maxZ" 1.0
     "maxY" 1.0
     "maxX" 1.0
     "minZ" 0.0
     "minY" 0.0
     "minX" 0.0
     )
    (list
     "blockName" "cake" 
     "blockID" 92
     "blockIndexInTexture" 121
     "isOpaqueCube" nil
     "renderAsNormalBlock" nil
     "renderType" 0
     "lightValue" 0
     "lightOpacity" 0
     "canBlockGrass" nil
     "tickOnLoad" t
     "nometanotify" t
     "isCollidable" t
     "blockResistance" 2.5
     "blockHardness" 0.5
     "slipperiness" 0.6
     "blockMaterial" "net.minecraft.src.Material@cac736f" 
     "blockParticleGravity" 1.0
     "stepSound " "net.minecraft.src.StepSound@1ee0005" 
     "maxZ" 1.0
     "maxY" 1.0
     "maxX" 1.0
     "minZ" 0.0
     "minY" 0.0
     "minX" 0.0
     )
    (list
     "blockName" "diode" 
     "blockID" 93
     "blockIndexInTexture" 6
     "isOpaqueCube" nil
     "renderAsNormalBlock" nil
     "renderType" 15
     "lightValue" 0
     "lightOpacity" 0
     "canBlockGrass" t
     "tickOnLoad" nil
     "nometanotify" t
     "isCollidable" t
     "blockResistance" 0.0
     "blockHardness" 0.0
     "slipperiness" 0.6
     "blockMaterial" "net.minecraft.src.MaterialLogic@c2e1f26" 
     "blockParticleGravity" 1.0
     "stepSound " "net.minecraft.src.StepSound@58d25a40" 
     "maxZ" 1.0
     "maxY" 0.125
     "maxX" 1.0
     "minZ" 0.0
     "minY" 0.0
     "minX" 0.0
     )
    (list
     "blockName" "diode" 
     "blockID" 94
     "blockIndexInTexture" 6
     "isOpaqueCube" nil
     "renderAsNormalBlock" nil
     "renderType" 15
     "lightValue" 9
     "lightOpacity" 0
     "canBlockGrass" t
     "tickOnLoad" nil
     "nometanotify" t
     "isCollidable" t
     "blockResistance" 0.0
     "blockHardness" 0.0
     "slipperiness" 0.6
     "blockMaterial" "net.minecraft.src.MaterialLogic@c2e1f26" 
     "blockParticleGravity" 1.0
     "stepSound " "net.minecraft.src.StepSound@58d25a40" 
     "maxZ" 1.0
     "maxY" 0.125
     "maxX" 1.0
     "minZ" 0.0
     "minY" 0.0
     "minX" 0.0
     )
    (list
     "blockName" "lockedchest" 
     "blockID" 95
     "blockIndexInTexture" 26
     "isOpaqueCube" t
     "renderAsNormalBlock" t
     "renderType" 0
     "lightValue" 15
     "lightOpacity" 255
     "canBlockGrass" nil
     "tickOnLoad" t
     "nometanotify" t
     "isCollidable" t
     "blockResistance" 0.0
     "blockHardness" 0.0
     "slipperiness" 0.6
     "blockMaterial" "net.minecraft.src.Material@1c655221" 
     "blockParticleGravity" 1.0
     "stepSound " "net.minecraft.src.StepSound@58d25a40" 
     "maxZ" 1.0
     "maxY" 1.0
     "maxX" 1.0
     "minZ" 0.0
     "minY" 0.0
     "minX" 0.0
     )
    (list
     "blockName" "trapdoor" 
     "blockID" 96
     "blockIndexInTexture" 84
     "isOpaqueCube" nil
     "renderAsNormalBlock" nil
     "renderType" 0
     "lightValue" 0
     "lightOpacity" 0
     "canBlockGrass" nil
     "tickOnLoad" nil
     "nometanotify" t
     "isCollidable" t
     "blockResistance" 15.0
     "blockHardness" 3.0
     "slipperiness" 0.6
     "blockMaterial" "net.minecraft.src.Material@1c655221" 
     "blockParticleGravity" 1.0
     "stepSound " "net.minecraft.src.StepSound@58d25a40" 
     "maxZ" 1.0
     "maxY" 1.0
     "maxX" 1.0
     "minZ" 0.0
     "minY" 0.0
     "minX" 0.0
     )))

(defmacro defblockthemall ()
  (list* 'progn (mapcar #'aconverter raw)))

(defblockthemall)

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

(defmacro deffunc (array nombre func)
  `(let ((blockid (gethash "blockID" (gethash ,nombre allblocks))))
     (setf (aref ,array blockid) ,func)))

(deffunc getBlocktexture 'grass
  (lambda (x)
    (case x
      (0 2)
      (1 0)
      (t 3))))

(buildblocks)
    
