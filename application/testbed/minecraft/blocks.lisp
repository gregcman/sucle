(in-package :sandbox-sub)

(defun preload ()
  (let ((array (make-array (* 4 7))))
    (dobox ((number 0 4)
	    (name 0 7))
	   (setf (aref array (+ number (* name 4)))
		 (music::load-all
		  (print
		   (concatenate
		    'string
		    "/media/imac/share/space/lispysaves/assets/sandbox-assets/resources/sound3/dig/"
		    
		    (string-downcase (symbol-name
				      (aref #(stone wood gravel grass sand cloth snow)
					    name)))
		    (aref #("1" "2" "3" "4") number
			  )
		    ".ogg"))
		  :mono8)))
    array))


(application::deflazy preloaded-sounds (application::al-context)
  (declare (ignorable application::al-context))
  (print "loading-sounds")
  (preload))

#+nil
(map nil
     (lambda (x) (music::free-preloaded x))
     (application::getfnc 'PRELOADED-SOUNDS))

(defparameter *wot-counter* 0)

(defun wot (value)
  (incf *wot-counter*)
  (aref 
   (application::getfnc 'preloaded-sounds)
   (+ (mod *wot-counter* 4)
      (* 4 (sound-dispatch value)))))


(defun sound-dispatch (value)
  (case value
    (0 0) ;air
    ((1 4 7 14 15 16 21 22 23 24) 0)				;stone,cobble
    ((2 18) 3)				;grass
    ((3 13) 2)				;dirt ,gravel
    ((5 17) 1)				;wood, log
    (12 4) ;sand
    ((35 81) 6)
    (otherwise (random 7))))


(defun blocksound (x y z)
  (let ((blockid (world::getblock x y z)))
    (unless (= blockid 0)
      (music::play-at (wot blockid) 
		      (+ x 0.5) (+ y 0.5) (+ 0.5 z)
		      0.8
		      1.0))))
 

;   (6 "sapling" 15 0 NIL NIL) 
;   (9 "water" 205 0 NIL NIL) 
;   (11 "lava" 237 15 NIL NIL) 
;   (14 "oreGold" 32 0 T T) 
;   (15 "oreIron" 33 0 T T) 
;   (16 "oreCoal" 34 0 T T) 
;   (19 "sponge" 48 0 T T) 
;   (21 "oreLapis" 160 0 T T) 
;   (22 "blockLapis" 144 0 T T) 
;   (23 "dispenser" 45 0 T T) 
;   (25 "musicBlock" 74 0 T T) 
;   (26 "bed" 134 0 T NIL) 
;   (27 "goldenRail" 179 0 NIL NIL) 
;   (28 "detectorRail" 195 0 NIL NIL) 
;   (29 "pistonStickyBase" 106 0 T NIL) 
;   (30 "web" 11 0 NIL NIL) 
;   (31 "tallgrass" 39 0 NIL NIL) 
;   (32 "deadbush" 55 0 NIL NIL) 
;   (33 "pistonBase" 107 0 T NIL) 
;   (34 "null" 107 0 T NIL) 
;   (35 "cloth" 64 0 T T) 
;   (36 "null" 0 0 T NIL) 
;   (37 "flower" 13 0 NIL NIL) 
;   (38 "rose" 12 0 NIL NIL) 
;   (39 "mushroomBrown" 29 1 NIL NIL) 
;   (40 "mushroomRed" 28 0 NIL NIL) 
;   (41 "blockGold" 23 0 T T) 
;   (42 "blockIron" 22 0 T T) 
;   (43 "stoneSlab" 6 0 T T) 
;   (44 "stoneSlab" 6 0 T NIL) 
;   (46 "tnt" 8 0 T T) 
;   (47 "bookshelf" 35 0 T T) 
;   (48 "stoneMoss" 36 0 T T) 
;   (49 "obsidian" 37 0 T T) 
;   (50 "torch" 80 14 NIL NIL) 
;   (51 "fire" 31 15 NIL NIL) 
;   (52 "mobSpawner" 65 0 T NIL) 
;   (53 "stairsWood" 4 0 T NIL) 
;   (54 "chest" 26 0 T T) 
;   (55 "redstoneDust" 164 0 NIL NIL) 
;   (56 "oreDiamond" 50 0 T T) 
;   (57 "blockDiamond" 24 0 T T) 
;   (58 "workbench" 59 0 T T) 
;   (59 "crops" 88 0 NIL NIL) 
;   (60 "farmland" 87 0 T NIL) 
;   (61 "furnace" 45 0 T T) 
;   (62 "furnace" 45 13 T T) 
;   (63 "sign" 4 0 NIL NIL) 
;   (64 "doorWood" 97 0 T NIL) 
;   (65 "ladder" 83 0 NIL NIL) 
;   (66 "rail" 128 0 NIL NIL) 
;   (67 "stairsStone" 16 0 T NIL) 
;   (68 "sign" 4 0 NIL NIL) 
;   (69 "lever" 96 0 NIL NIL) 
;   (70 "pressurePlate" 1 0 NIL NIL) 
;   (71 "doorIron" 98 0 T NIL) 
;   (72 "pressurePlate" 4 0 NIL NIL) 
;   (73 "oreRedstone" 51 0 T T) 
;   (74 "oreRedstone" 51 9 T T) 
;   (75 "notGate" 115 0 T NIL) 
;   (76 "notGate" 99 7 T NIL) 
;   (77 "button" 1 0 NIL NIL) 
;   (80 "snow" 66 0 T T) 
;   (81 "cactus" 70 0 T NIL) 
;   (83 "reeds" 73 0 NIL NIL) 
;   (84 "jukebox" 74 0 T T) 
;   (85 "fence" 4 0 T NIL) 
;   (86 "pumpkin" 102 0 T T) 
;   (87 "hellrock" 103 0 T T) 
;   (88 "hellsand" 104 0 T T) 
;   (90 "portal" 14 11 NIL NIL) 
;   (91 "litpumpkin" 102 15 T T) 
;   (92 "cake" 121 0 T NIL) 
;   (93 "diode" 6 0 T NIL) 
;   (94 "diode" 6 9 T NIL) 
;   (95 "lockedchest" 26 15 T T)
