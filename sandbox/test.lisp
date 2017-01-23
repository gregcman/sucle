(in-package :sandbox)

;;;used by none but to test

(defun spill-hash (hash)
  (loop for key being the hash-keys of hash
     using (hash-value value)
     do (format t "~S ~S~%" key value)))

(defun int-scale (int scale)
  (truncate (* int scale)))

(defun fun-setup ()
  (color-grasses)
  (test-world)
  (erase-bottom)
  (goto 16 80 -16))

(defun erase-bottom ()
  (dobox ((x 0 128) (y 0 64) (z -128 0))
	 (plain-setblock x y z 0 0)))

(defun test-world ()
  (dobox ((x 0 8) (y -8 0))
	 (someseq x y)))

(defun spawn ()
  (goto 64 80 -64))

(defun goto (x y z)
  (setf *xpos* x
	*ypos* y
	*zpos* z))

(defun color-grasses ()
  (modify-greens 64 192)
  (modify-greens 80 192)
  (modify-greens 0 240))

(defun ubyte-mult (a b)
  (truncate (* a b) 256))

(defun multiply-into (vecinto other)
  (macrolet ((aux (a b num)
	       `(let ((at (aref ,a ,num))
		      (bt (aref ,b ,num)))
		  (setf (aref ,a ,num) (ubyte-mult at bt)))))
    (aux vecinto other 0)
    (aux vecinto other 1)
    (aux vecinto other 2)
    (aux vecinto other 3)))

;;;grass is 0 240
;;;leaves is [64 80] 192
(defun modify-greens (xpos ypos &optional
				  (color (imagewise:getapixel
					  0 255
					  (get-image"misc/grasscolor.png"))))
  (let ((terrain (get-image "terrain.png")))
    (dobox ((x xpos (+ 16 xpos)) (y ypos (+ 16 ypos)))
	   (multiply-into (imagewise:getapixel y x terrain) color))))


(defun shit (x)
  (print x global-output))

(defun force-quit ()
  (when ourthread
    (SB-THREAD:terminate-thread (sb-thread:terminate-thread ourthread))))

(defun pos? ()
  (print (list *xpos* *ypos* *zpos*)))

(defun vel? ()
  (print (list *xvel* *yvel* *zvel*)))

(defun draw-fistbox ()
  ;;;;draw the fist hitbox
  (progn (with-slots ((minx aabbcc::minx) (miny aabbcc::miny) (minz aabbcc::minz)
		      (maxx aabbcc::maxx) (maxy aabbcc::maxy) (maxz aabbcc::maxz))
	     fist-aabb
	   (draw-box (+ minx fistx -0) (+  miny fisty -0) (+  minz fistz -0)
		     (+ maxx fistx -0) (+  maxy fisty -0) (+  maxz fistz -0)))))

(defun neighbors (x y z)
  (let ((tot 0))
    (macrolet ((aux (i j k)
		 `(unless (zerop (world:getblock (+ x ,i) (+ y ,j) (+ z ,k)))
		   (incf tot))))
      (aux 1 0 0)
      (aux -1 0 0)
      (aux 0 1 0)
      (aux 0 -1 0)
      (aux 0 0 1)
      (aux 0 0 -1))
    tot))

(defun aux-func2 (x dx)
  (if (zerop dx)
      most-positive-double-float
      (if (plusp dx)
	  (/ (- (floor (1+ x)) x) dx)
	  (/ (- (ceiling (1- x)) x) dx))))

;;move to the next closest integer in the direction of the delta
(defun step-next (x y z dx dy dz)
  (values (aux-func2 x dx)
	  (aux-func2 y dy)
	  (aux-func2 z dz)))

(defun aux-step-next (x y z dx dy dz)
  (mvb (i j k) (step-next x y z dx dy dz)
       (mvb (value i? j? k?) (smallest i j k)
	    (values value
		    (+ x (* dx value))
		    (+ y (* dy value))
		    (+ z (* dz value))
		    i?
		    j?
		    k?))))

(defun dosteps (x y z dx dy dz)
  (let ((total 1)
	(pluspdx (plusp dx))
	(pluspdy (plusp dy))
	(pluspdz (plusp dz)))
    (declare (ignorable pluspdx pluspdy pluspdz))
    (tagbody
       rep
       (mvb (ratio newx newy newz i? j? k?) (aux-step-next x y z dx dy dz)
	    (declare (ignorable i? j? k?))
	    (when i?
	      (if pluspdx nil))
	    (when j?
	      (if pluspdy nil))
	    (when k?
	      (if pluspdz nil))
	    (setf x newx y newy z newz)
	    (world:setblock (floor x) (floor y) (floor z) 2)
	    (print (list x y z i? j? k?))
	    (decf total ratio)
	    (when (minusp total) (go end))
	    (go rep))
     end)))

(defun test (a b)
  (multiple-value-bind (c d) (extract-polar-coords (unit-pitch-yaw (coerce (deg-rad a) 'single-float)
								   (coerce (deg-rad b) 'single-float)))
    (values (rad-deg c)
	    (rad-deg d))))

(defun test2 (dx dy)
  (multiple-value-bind (y p) (new-direction
			      (coerce dx 'single-float)
			      (coerce dy 'single-float)
			      (coerce (/  pi 180) 'single-float))
    (values (rad-deg y) (rad-deg p))))

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

(defun load-file-images (path-list start-dir)
  (dolist (path path-list)
    (let ((filepath (merge-pathnames path start-dir)))
      (load-file-image filepath (namestring path)))))

(defun load-file-text (path text-name)
  (let ((text (pathwise:file-string path)))
    (set-text text-name text)
    text))

(defun load-file-image (path image-name)
  (let ((png (imagewise:load-png path)))
    (imagewise:flip-image png)
    (set-image image-name png)
    png))


(defun hash-symbol (symbol)
  (declare (type symbol symbol))
  (sxhash symbol))

(defun funcall-test (func)
  (declare (type (function () (values)) func))
  (funcall func))


(defparameter foob
  `(progn
     (declaim (inline floop))
     (defun floop ()
       (print 345))
     (declaim (notinline floop))
     (values #'floop)))

(declaim (inline test234))
(defun test234 ()
  (floop)
  (floop)
  (floop))
(declaim (notinline test234))


;;;;sbcl does retarded checks when one wants to set a symbol's value cell
;;;;so this function bypasses it
;;;;setvq means "set VALUE quoted"
(declaim (inline setvq))
(defun setvq (symbol new)
  #+sbcl (sb-kernel:%set-symbol-global-value symbol new)
  #-sbcl (set symbol new))

(defun wtf3 (symbol new)
  (sb-kernel:%set-symbol-global-value symbol new))
(defun test234other ()
  (locally (declare (inline floop test234))
    (test234)
    (floop)
    (floop))
  (floop))

(defun set5 (symbol)
  (declare (type symbol symbol)
	   (optimize (speed 3) (safety 0)))
  (sb-impl::%set-symbol-value symbol 5))

(defstruct astruct
  symbol)

(declaim (inline ssetvq))
(defun ssetvq (astruct new)
  (setf (astruct-symbol astruct) new))

(defun wtf? (symbol)
  (declare (type symbol symbol))
  (let ((a symbol))
    (dotimes (X 10000000) (setvq a x)) a))

(defparameter wof nil)
(defun wtf2? ()
  (dotimes (X 10000000) (set 'wop x)))

(defun test56 (a b)
  (declare (type fixnum a b))
  (let ((c (+ a b)))
    (declare (type fixnum c))
    c))

(defstruct (zymbol (:constructor %make-zymbol (name hash)))
  (name (error "must provide name!") :type (or null (simple-array character (*))))
  (hash (error "NO") :type fixnum)
  (value nil :type t))

(defun make-zymbol (string)
  (declare (type (simple-array character *) string))
  (let ((hash-value (sxhash string)))
    (%make-zymbol string hash-value)))

(progn
  (defun pprint-zymbol (stream zymbol)
    (pprint-logical-block (stream nil)
      (format stream "#?~a" (zymbol-name zymbol))))
  (set-pprint-dispatch 'zymbol 'pprint-zymbol))

(progn
  (defun question-mark-reader (stream sub-char numarg)
    (declare (ignore sub-char numarg))
    (make-zymbol (string (read stream t nil t))))
  (set-dispatch-macro-character #\# #\? #'question-mark-reader))

(defparameter *all-zymbols* (make-hash-table :test 'equal))

(defun make-zymbol-hash ()
  (make-hash-table :test 'eq :hash-function (lambda (z) (zymbol-hash z))))

(defun make-cons (num)
  (cons num (sxhash num)))

(defun make-cons-hash ()
  (make-hash-table :test 'eq :hash-function #'cdr))

(defparameter fnv-1a-hash-nums
  (list 16777619 
	2166136261
	1099511628211
	14695981039346656037))

(defconstant +fnv-offset-basis-64+ 14695981039346656037)
(defconstant +fnv-offset+ 860922984064492325)
(defconstant +fnv-prime-64+ 1099511628211)
(defconstant +fnv-first-64+ (logand most-positive-fixnum (*  +fnv-offset+ +fnv-prime-64+)))

(defun fnv-1a-64bit (num)
  (declare (values fixnum)
	   (type fixnum num)
	   (optimize (speed 3) (safety 0)))
  (macrolet ((k (body)
	       `(let ((result ,body))
		  (setf num (ash num -8))
		  (logxor (mod num 256)
			  (sb-vm::*-mod64 +fnv-prime-64+ result))))
	     (the-start ()
	       `(logxor +fnv-first-64+ (mod num 256))))
    (the fixnum
	 (logand most-positive-fixnum
		 (k
		  (k 
		   (k 
		    (k 
		     (k 
		      (k 
		       (k 
			(the-start))))))))))))


(defun wtf (num)
  (declare (type fixnum num)
	   
	   (optimize (speed 3) (safety 0)))
  (let ((a 123))
    (declare (type (unsigned-byte 64) a))
    (the fixnum (* a num))))

(eval-when (:load-toplevel :execute :compile-toplevel)
  (defconstant six-bits-in-a-fixnum (floor (logcount most-positive-fixnum) 6)))
;;;the amount of alpha-numeric characters one can put into a single
;;;fixnum on a given machine. turns out to be 11 on 64 bit machines
;;;and 5 on 32 bit machines.
;;;passing around alphanumeric strings inside integers
;;;how convenient
(defun max-fixnum-string-length ()
  (floor (log most-positive-fixnum 36)))

(defun fresh-fixnum-string ()
  (make-array six-bits-in-a-fixnum :element-type 'character :initial-element #\ ))

(defun string-number (string)
  (let ((correct (fresh-fixnum-string)))
    (dotimes (x (min (length string) six-bits-in-a-fixnum))
      (setf (schar correct x) (char-upcase (schar string x))))
    (values
     (%string-number correct)
     correct)))

(defun %string-number (string)
  (declare (type (simple-array character *)))
  (let ((num 0)
	(thash 0))
    (declare (type fixnum num thash))
    (dotimes (x six-bits-in-a-fixnum)
      (incf num (ash (- (char-code (schar string x)) 32) thash))
      (incf thash 6))
    num))

(defun number-string (number)
  (declare (type fixnum number))
  (let ((string (fresh-fixnum-string)))
    (declare (type (simple-array character #.six-bits-in-a-fixnum)))
    (dotimes (x six-bits-in-a-fixnum)
      (if (zerop number)
	  (return))
      (setf (schar string x) (code-char (+ 32 (mod number 64))))
      (setf number (ash number -6)))
    string))

;;;;turn a string made of letters, [also a minus sign]
;;;;and numbers into a fixnum 
(defun %string-number (string)
  (logand (parse-integer string :radix 36) most-positive-fixnum))

(defparameter *number-names!* (make-hash-table :test 'equal))
(defun name-it (string)
  (setf (gethash string *number-names!*) (string-number string)))
;;;greg - 782008 lisp - 1004137 

(defun print-number-letters (number)
  (let ((*print-base* 36))
    (print number)))

(defun interval (a b)
  (cons a b))

(defun interval-start (interval)
  (car interval))

(defun interval-end (interval)
  (cdr interval))

(declaim (type list *id-free-list*))
(defparameter *id-free-list* (list nil))

(let ((free-cons-cells nil))
  (defun recycle-cons-cell (cell)
    (rplaca (rplacd cell free-cons-cells) 0)
    (setf free-cons-cells cell))
  (defun cons-free-list ()
    free-cons-cells)
  (defun conz (ze1 ze2)
    (if free-cons-cells
	(let ((cell free-cons-cells))
	  (setf free-cons-cells (cdr free-cons-cells))
	  (rplaca (rplacd cell ze2) ze1))
	(cons ze1 ze2))))

;;;the free list is a list with nil as the car.
;;;the first nil is not modified, only the cdr.
;;;intervals are [] inclusive on both ends.
;;;ex: (2 . 2) means [2 , 2] which is just a point.

;;;create a fresh free-list for the ids
(defun fresh-id-free-list ()
  (cons nil (list (cons 0 most-positive-fixnum))))

;;;"use" an id - removing it from the free list
(defun id-use (n free)
  (declare (type fixnum n)
	   (type list free))
  (let ((nextcdr (cdr free)))
    (declare (type list nextcdr))
    (when nextcdr
      (tagbody
       rep
	 (let ((cell (car nextcdr)))
	   (let ((start (car cell))
		 (end (cdr cell)))
	     (declare (type fixnum start end))
	     (if (<= start n) ;;if n is behind, its over because start will only increase
		 (when (<= n end) ;;if it lies between inclusive, check
		   (if (= start n)
		       (progn
			 (if (= start end)
			     (setf (cdr free) (cdr nextcdr))
			     (setf (car cell) (the fixnum (1+ start)))))
		       (if (= end n) ;;n is greater than start
			   (progn
			     (if (= start end)
				 (setf (cdr free) (cdr nextcdr))
				 (setf (cdr cell) (the fixnum (1- end)))))
			   (progn ;;n is between start and end exclusive
			     (setf (car cell) (the fixnum (1+ n)))
			     (setf (cdr free)
				   (cons (cons start (the fixnum (1- n))) nextcdr)))))
		   (return-from id-use n))
		 (go end))))
	 (setf free nextcdr)
	 (setf nextcdr (cdr nextcdr))
	 (when nextcdr (go rep))
       end))))

;;;return an unused id, which is just the lower bound of the smallest interval.
;;;does not modify the free list
(defun id-allocate (free)
  (let ((first-cons (first (cdr free))))
    (car first-cons)))

;;;determine whether or not a specific id is available - unused
(defun id-unused-p (n free)
  (declare (type fixnum n))
  (dolist (cell (cdr free))
    (let ((start (car cell))
	  (end (cdr cell)))
      (declare (type fixnum start end))
      (if (<= start n) 
	  (when (<= n end)
	    (return t))
	  (return nil))))) ;;already past intervals which could contain n

;;;unuse an id, putting it back into the free list
;;;four scenarios can occur:
;;;a one-hole gap, which merges two intervals - remove a cons cell
;;;attach to the start of an interval - change car value to "n"
;;;attach to the end of an interval - change cdr value "n"
;;;not touch any interval - insert a (cons n n)
(defun id-free (n freelist)
  (declare (type fixnum n))
  (let ((n-1 (1- n)))
    (declare (type fixnum n-1))
    (let ((tail (cdr freelist)))
      (if tail ;;;when all the ids have been used ---
	  (let ((cell (car tail)))
	    (let ((first-start (car cell)))
	      (declare (type fixnum first-start))
	      (when (< n first-start)
		(if (= n (1- first-start))
		    (setf (car cell) n) ;;;extend downwards
		    (setf (cdr freelist) (cons (cons n n) tail))) ;;;push new interval
		(return-from id-free t)))
	;;;initial test over, looping between consecutive cells begins
	    (let ((next-tail (cdr tail)))
	      (if next-tail
		  (let* ((next-cell (car next-tail))
			 (left (cdr cell)) ;;the most of the lower interval
			 (right (car next-cell))) ;;the least of the greater interval
		    (declare (type fixnum left right))
		    (tagbody
		     rep
		       (if (<= n left) ;;it is is equal or less than its not valid
			   (return-from id-free nil)
			   (let ((merge-bottom (= left n-1)) ;;whether to merge lower
				 (merge-top (= (1- right) n))) ;;whether to merge higher
			     (if merge-top
				 (if merge-bottom
				     (progn
				       (setf (cdr tail) (cdr next-tail))
				       ;;remove the second interval
				       (setf (cdr cell) (cdr next-cell))
				       ;;combine first interval into a big one
				       (return-from id-free t))
				     (progn
				       (setf (car next-cell) n)
				       ;;lower the top interval
				       (return-from id-free t)))
				 (if merge-bottom
				     (progn
				       (setf (cdr cell) n)
				       ;;raise the bottom interval
				       (return-from id-free t))
				     (when (< n right)
				       ;;when its between but there is no merge, create another interval
				       (setf (cdr tail) (cons (cons n n) next-tail))
				       (return-from id-free t))))))

		       (let ((new-next (cdr next-tail)))
			 (when new-next ;;if it is nil then there is no cell to check
			   (setf tail next-tail
				 next-tail new-next
				 cell next-cell 
				 next-cell (car next-tail)
				 left (cdr cell)
				 right (car next-cell))
			   (go rep)))

	     ;;;;looping between two cells over
		       (let ((last-end (cdr next-cell)))
			 (declare (type fixnum last-end))
			 (when (< last-end n)
			   (if (= n-1 last-end)
			       (setf (cdr next-cell) n) ;;extend the last interval ever
			       (setf (cdr next-tail) (cons (cons n n) nil))) ;;make another
			   (return-from id-free t)))))
	      ;;;when there is only one interval and it is short of the most positive fixnum
		  (let ((first-end (cdr cell)))
		    (declare (type fixnum first-end))
		    (when (< first-end n)
		      (if (= n-1 first-end)
			  (setf (cdr cell) n) ;;extend the last interval ever
			  (setf (cdr tail) (cons (cons n n) nil))) ;;make another
		      (return-from id-free t))))))  
	  (setf (cdr freelist) (cons (cons n n) nil)))))
  (return-from id-free nil))

