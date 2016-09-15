(in-package #:sandbox)

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

(defun file-string (path)
  (with-open-file (stream path)
    (let ((data (make-string (file-length stream))))
      (read-sequence data stream)
      data)))

(defun chunk-byte-offset (x z)
  (* 4
     (+
      (mod x 32)
      (* 32 (mod z 32)))))

(defun byte-read (path)
  (with-open-file (stream path :element-type '(unsigned-byte 8))
    (let* ((len (file-length stream))
	   (data (make-array len :element-type '(unsigned-byte 8))))
      (dotimes (n len)
	(setf (aref data n) (read-byte stream)))
      data)))

;(defparameter testchunk (sandbox::byte-read "/home/imac/Downloads/cNBT-master/testdata/hell.mcr"))
(defun tonum (nums)
  (let ((danum 1))
    (setf nums (reverse nums))
    (dotimes (n (length nums))
      (incf danum (* (expt 256 n) (elt nums n))))
    danum))

(defun my-subseq (array start amount)
  (subseq array start (+ start amount)))

(defun mcr-chunk (mcrdata x z)
  (let* ((initial (chunk-byte-offset x z))
	 (firstdata (subseq mcrdata initial (+ initial 4)))
	 (region-offset (tonum (subseq firstdata 0 3)))
	 (page-length (elt firstdata 3))
	 (time-stamp (tonum (my-subseq mcrdata (+ 4096 initial) 4)))
	 (chunkdata (my-subseq mcrdata (* 4096 region-offset) (* page-length 4096)))
	 (chunklength (tonum (my-subseq chunkdata 0 4)))
	 (compression-type (elt chunkdata 4))
	 (compressed-chunk-data (my-subseq chunkdata 5 (1- chunklength))))
    (print (list region-offset page-length time-stamp))
    (if (= 2 compression-type)
	(chipz:decompress nil 'chipz:zlib compressed-chunk-data)
	(chipz:decompress nil 'chipz:gzip compressed-chunk-data))))

(defun nbt-compound ())
(defun nbt-list ())

(defun flatten (obj)
  (do* ((result (list obj))
        (node result))
       ((null node) (delete nil result))
    (cond ((consp (car node))
           (when (cdar node) (push (cdar node) (cdr node)))
           (setf (car node) (caar node)))
          (t (setf node (cdr node))))))

(defun fatten (some-array)
  (let* ((total-size (array-total-size some-array))
	 (new-array (make-array total-size)))
    (dotimes (x total-size)
      (setf (aref new-array x) (row-major-aref some-array x)))
    new-array))

(defun flip-image(darray)
  (let* ((dims (array-dimensions darray))
	 (myray (make-array dims)))   
    (dotimes (w (first dims))
      (dotimes (h (second dims))
	(dotimes (val (third dims))
	  (setf
	   (aref myray (- (first dims) w 1) h val)
	   (aref darray w h val)))))   
    myray))

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

(eval-when (:load-toplevel :execute)
  (get-all-mc-textures))
