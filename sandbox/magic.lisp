(in-package #:sandbox)

;;;;This file: "magic" contains facilities to load images and stuff

(defparameter ourdir
  (make-pathname :host (pathname-host #.(or *compile-file-truename*
					    *load-truename*))
		 :directory (pathname-directory #.(or *compile-file-truename*
						      *load-truename*))))
(defparameter resdir (merge-pathnames #P"res/" ourdir))
(defparameter shaderdir #P "shaders/")

(defparameter texture-library (make-hash-table :test 'equal))
(defparameter picture-library (make-hash-table :test 'equal))
(defun load-png (name filename)
  (setf (gethash name picture-library)
	(flip-image (opticl:read-png-file filename))))

(defun load-shader-file (name)
  (file-string
   (merge-pathnames name (merge-pathnames shaderdir resdir))))

(defun load-shit (tex-data name width height)
  (setf (gethash name texture-library)
	(create-texture-wot tex-data width height)))

(defun get-file-paths-and-metadata (x currentpath)
  (if (consp x)
      (let* ((dirnombre (first x))
	     (babies (cdr x)))
	(mapcar
	 (lambda (n)
	   (get-file-paths-and-metadata
	    n
	    (merge-pathnames dirnombre currentpath)))
	 babies))
      (progn
	(list x (merge-pathnames x currentpath)))))

(defun get-all-mc-textures ()
  (let* ((shit 
	  (flatten
	   (get-file-paths-and-metadata png-resources resdir))))
    (labels ((rec (lizzy)
	       (if lizzy
		   (let ((nombre (first lizzy))
			 (path (second lizzy)))
		     (load-png nombre path)
		     (rec (cddr lizzy))))))
      (rec shit))))


(eval-when (:load-toplevel :execute)
  (defparameter png-resources
    '("moreshit/"
      "terrain.png"
      "pack.png" "particles.png" 
      ("achievement/"
       ("bg.png" "icons.png"))
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
       "black.png" "mclogo.png" "mojang.png"))))

(defparameter texturesloaded? nil)

(defun loadletextures ()
  (if (not texturesloaded?)
      (progn
	(get-all-mc-textures)
	(setf texturesloaded? t))))


(defun load-into-texture-library (name &optional (othername name))
  (let ((thepic (gethash name picture-library)))
    (if thepic
	(let ((dims (array-dimensions thepic)))
	    (load-shit
	     (fatten thepic)
	     othername (first dims) (second dims))))))

(defun create-texture-wot (tex-data width height)
  "creates an opengl texture from data"
  (let ((the-shit (car (gl:gen-textures 1))))
    (gl:bind-texture :texture-2d the-shit)
    (gl:tex-parameter :texture-2d :texture-min-filter :nearest-mipmap-linear)
    (gl:tex-parameter :texture-2d :texture-mag-filter :nearest)
    (gl:tex-parameter :texture-2d :texture-wrap-s :repeat)
    (gl:tex-parameter :texture-2d :texture-wrap-t :repeat)
    (gl:tex-parameter :texture-2d :texture-border-color '(0 0 0 0))
    (gl:tex-parameter :texture-2d :generate-mipmap :true)
    (gl:tex-image-2d
     :texture-2d 0
     :rgba width height 0 :rgba :unsigned-byte tex-data)
    (gl:generate-mipmap :texture-2d)
    the-shit))

;;create image library and a texture library
