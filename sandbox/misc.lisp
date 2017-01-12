(in-package :sandbox)

(defun spill-hash (hash)
  (loop for key being the hash-keys of hash
     using (hash-value value)
     do (format t "~S ~S~%" key value)))


(defun averager (amount)
  (let ((the-array (make-array amount :element-type 'fixnum))
	(index 0)
	(tot 0))
    (lambda (x)
      (let ((old (aref the-array index)))
	(setf tot (+ tot x (- old)))
	(setf (aref the-array index) x))
      (setf index (mod (1+ index) amount))
      (values (/ (coerce tot 'single-float) amount) the-array))))

(defmacro doblocks ((xvar xstart xnum)
		    (yvar ystart ynum)
		    (zvar zstart znum)
		    &body body)
  `(dorange (,xvar ,xstart ,xnum)
	    (dorange (,yvar ,ystart ,ynum)
		     (dorange (,zvar ,zstart ,znum)
			      ,@body))))


(defun fun-setup ()
  (color-grasses)
  (test-world)
  (erase-bottom))

(defun erase-bottom ()
  (doblocks (x 0 128) (y 0 64) (z -128 128) (plain-setblock x y z 0 0)))

(defun test-world ()
   (dorange (x 0 8) (dorange (y -8 8) (someseq x y))))

(defun color-grasses ()
  (modify-greens 64 192)
  (modify-greens 80 192)
  (modify-greens 0 240))
