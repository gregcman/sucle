(defparameter png-resources
  '(""
    "terrain.png"
    "pack.png" "particles.png" 
    ("achievement/"
     "bg.png" "icons.png")
    ("armor/"
     "chain_1.png" "chain_2.png" "cloth_1.png" "cloth_2.png"
     "diamond_1.png" "diamond_2.png"
     "gold_1.png" "gold_2.png"
     "iron_1.png" "iron_2.png" "power.png")
    ("art/"
     "kz.png")
    ("environment/"
     "clouds.png" "rain.png" "snow.png")
    ("font/"
     "default.png")
    ("gui/"
     "background.png" "container.png"
     "crafting.png" "furnace.png" "gui.png" "icons.png"
     "inventory.png"
     "items.png" "logo.png" "particles.png" "slot.png"
     "trap.png" "unknown_pack.png")
    ("item/"
     "arrows.png" "boat.png" "cart.png" "door.png" "sign.png")
    ("misc/"
     "dial.png" "foliagecolor.png" "footprint.png"
     "grasscolor.png"
     "mapbg.png" "mapicons.png" "pumpkinblur.png"
     "shadow.png" "vignette.png" "water.png" "watercolor.png")
    ("mob/"
     "char.png" "chicken.png" "cow.png"  "creeper.png"
     "ghast.png" "ghast_fire.png"
     "pig.png" "pigman.png" "pigzombie.png"
     "saddle.png" "sheep.png" "sheep_fur.png" "silverfish.png"
     "skeleton.png"
     "slime.png" "spider.png" "spider_eyes.png"
     "squid.png" "wolf.png" "wolf_angry.png" "wolf_tame.png"
     "zombie.png")
    ("terrain/"
     "moon.png" "sun.png")
    ("title/"
     "black.png" "mclogo.png" "mojang.png")
    ("skybox/"
     "cheap.png")))

(defparameter simple-resources
  '(""
    "terrain.png"
    ("terrain/"
     "moon.png" "sun.png")
    ("skybox/"
     "cheap.png")
    ("misc/"
     "grasscolor.png")
    ("gui/"
     "gui.png"))) 

;;;takes a tree list structure where the car of each list is the folder name ex: #P"root/"
;;;and the other items in the rest of the list are either more lists or just plain strings.
;;;returns a flat list which is the collection of all the resulting pathnames
(defun expand-paths (dir-list)
   (let (acc) 
     (labels ((rec (x currentpath)
		(if (consp x)
		    (dolist (sub (cdr x))
		      (rec sub (merge-pathnames (car x) currentpath)))
		    (push (merge-pathnames x currentpath) acc))))
       (rec dir-list #P"") acc)))
