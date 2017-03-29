(in-package :sandbox)

(progn
  (eval-always
   (defun duaq (start clockwise-winding form)
     (destructuring-bind (x- x+ y- y+) form
       (if (member start '(1 2 3 4))
	   (let ((vec (vector x+ y+
			      x- y+
			      x- y-
			      x+ y-))
		 (i 0)
		 (ii 2)
		 (iii 4)
		 (iv 6))
	     (declare (dynamic-extent vec))
	     (when clockwise-winding
	       (rotatef ii iv))
	     (let ((end-test (* 2 (1- start))))
	       (do ()
		   ((= end-test i)
		    (list (aref vec i)
			  (aref vec (1+ i))
			  (aref vec ii)
			  (aref vec (1+ ii))
			  (aref vec iii)
			  (aref vec (1+ iii))
			  (aref vec iv)
			  (aref vec (1+ iv))))
		 (rotatef i ii iii iv))))
	   (error "~s is not a plane quadrant" start)))))

  (eval-always
   (defun aalgnqd (start value subform)
     (if (member start '(0 1 2))
	 (let ((places (vector nil nil nil))
	       (count 0))
	   (dolist (form subform)
	     (push form (aref places count))
	     (setf count (- 1 count)))
	   (setf (aref places 2) (list value value value value))
	   (rotatef (aref places 2) (aref places start))
	   (let (acc)
	     (dotimes (x 4)
	       (push (pop (aref places 2)) acc)
	       (push (pop (aref places 1)) acc)
	       (push (pop (aref places 0)) acc))
	     acc))
	 (error "~s is not 0 1 or 2" start))))

  (fuktard:eval-always
   (progn
     (defun quadi+ (i form) ;;jk
       (aalgnqd 0 i (duaq 1 t form)))
     (defun quadi- (i form)
       (aalgnqd 0 i (duaq 3 nil form)))
     (defun quadj+ (j form) ;;ik
       (aalgnqd 1 j (duaq 1 t form)))
     (defun quadj- (j form) 
       (aalgnqd 1 j (duaq 3 nil form)))
     (defun quadk+ (k form) ;;ji
       (aalgnqd 2 k (duaq 1 nil form)))
     (defun quadk- (k form)
       (aalgnqd 2 k (duaq 3 t form))))))
