(defun line2 (px py pz vx vy vz blockid)
  (sandbox-sub::aabb-collect-blocks
   px py pz (- vx px) (- vy py) (- vz pz)
   sandbox-sub::*fist-aabb*   
   (lambda (x y z)
     (sandbox::plain-setblock x y z blockid 0))))

(defparameter *box* (vector 0 0 0 0 0 0))
(defun make-box (a b)
  (with-vec (x y z) (a)
    (with-vec (x0 y0 z0) (b)
      (vector (min x x0)
	      (1+ (max x x0))
	      (min y y0)
	      (1+ (max y y0))
	      (min z z0)
	      (1+ (max z z0))))))

(defun hollowbox (fun &optional (num 1) (box *box*))
  (with-vec (x0 x1 y0 y1 z0 z1) (box)
    (lambda (x y z)
      (let ((count 0))
	(when (or (= x x0)
		  (= x (1- x1)))
	  (incf count))
	(when (or (= y y0)
		  (= y (1- y1)))
	  (incf count))
	(when (or (= z z0)
		  (= z (1- z1)))
	  (incf count))
	(when (>= count num)
	  (funcall fun x y z))))))

(defun neighbors2 (x y z)
  (let ((tot 0))
    (macrolet ((aux (i j k)
		 `(unless (zerop (world:getblock (+ x ,i) (+ y ,j) (+ z ,k)))
		   (incf tot))))
      (aux 1 0 0)
      
      (aux 1 1 0)
      (aux 1 -1 0)
      (aux 1 0 1)
      (aux 1 0 -1)
      
      (aux -1 0 0)
      
      (aux -1 1 0)
      (aux -1 -1 0)
      (aux -1 0 1)
      (aux -1 0 -1)
      
      (aux 0 1 0)
      
      (aux 0 1 -1)
      (aux 0 1 1)
      
      (aux 0 -1 0)
      
      (aux 0 -1 -1)
      (aux 0 -1 1)
      
      (aux 0 0 1)
      (aux 0 0 -1))
    tot))

(defun scramble (list)
  (let ((a (list-length list))
	(acc nil))
    (setf list (cons nil list))
    (when a
      (loop for i from a above 0 do
	   (progn
	     (let ((cell (nthcdr (random i) list)))
	       (push (pop (cdr cell))
		     acc)))))
    acc))

(defun scram (&optional (n 100))
  (scramble (alexandria:iota n)))

(defun scram2 (&optional (n 100))
  (let ((acc nil))
    (dotimes (i (ash n -1))
      (push i acc)
      (push (- n 1 i) acc))
    acc))

(defun wowwz (&optional (n 100))
  (when (evenp n)
    (let ((xs (scram2 n))
	  (ys (scram2 n))
	  (zs (scram2 n)))
      (loop repeat (ash n -1) do
	   (let ((*box* (make-box (vector (pop xs) (pop ys) (pop zs))
				  (vector (pop xs) (pop ys) (pop zs)))))
	     (map-box (hollowbox (lambda (x y z)
				   (sandbox::plain-setblock x y z *blockid* 0))
				 2)
		      ))))))
 
