(defparameter grass-strands
  (vector 3 2 3 3
	  4 1 3 2
	  4 3 4 3
	  2 3 3 2))
(defun grasscolor ()
  (let ((img (get-image "terrain.png"))
	(tot (vector 0 0 0 0))
	(amount 0))
    (dotimes (x 16)
      (let ((xpos (+ 48 x)))
	(dotimes (yoff (aref grass-strands x))
	  (incf amount)
	  (let ((ypos (- 255 yoff)))
	    (let ((pixel (imagewise:getapixel ypos xpos img)))
	      (map-into tot #'+ pixel tot))))))
    (print (list tot amount))))
(defun emptycolor ()
  (let ((img (get-image "terrain.png"))
	(tot (vector 0 0 0 0)))
    (dotimes (x 16)
      (let ((xpos x))
	(dotimes (yoff 16)
	  (let ((ypos (- 255 yoff)))
	    (let ((pixel (imagewise:getapixel ypos xpos img)))
	      (map-into tot #'+ pixel tot))))))
    (print tot)))
 
