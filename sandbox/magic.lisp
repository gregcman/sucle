(in-package #:sandbox)

(defparameter ourdir
  (make-pathname :host (pathname-host #.(or *compile-file-truename*
					    *load-truename*))
		 :directory (pathname-directory #.(or *compile-file-truename*
						      *load-truename*))))
(defparameter dir-resource (merge-pathnames #P"res/" ourdir))
(defparameter dir-shader (merge-pathnames #P"shaders/" dir-resource))
(defparameter dir-mc-assets (merge-pathnames "moreshit/" dir-resource))

;;;;load a single text file and plop it in the global library
(defun load-file-text (path text-name)
  (let ((text (file-string path)))
    (lset *g/text* text-name text)
    text))

;;;;load a single image and plop it in the global library
(defun load-file-image (path image-name)
  (let ((png (load-png path)))
    (flip-image png)
    (lset *g/image* image-name png)
    png))

(defun load-file-images (path-list start-dir)
  (dolist (path path-list)
    (let ((filepath (merge-pathnames path start-dir)))
      (load-file-image filepath (namestring path)))))

(defun shader-path (name)
  (merge-pathnames name dir-shader))

(defparameter ?images nil)
(defparameter ?shaders nil)

(defun load-assets ()
  (unless ?shaders
    (load-file-text (shader-path "blockshader/transforms.vs") :bs-vs)
    (load-file-text (shader-path "blockshader/basictexcoord.frag") :bs-frag)
    (load-file-text (shader-path "simpleshader/transforms.vs") :ss-vs)
    (load-file-text (shader-path "simpleshader/basictexcoord.frag") ::ss-frag)
    (setf ?shaders t))
  (unless ?images
    (load-file-images (expand-paths png-resources) dir-mc-assets)
    (setf ?images t)))

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
     "black.png" "mclogo.png" "mojang.png")))
