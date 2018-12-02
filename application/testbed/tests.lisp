
;;less used items below

#+nil
(defun %%%collide (ax0 ay0 az0 ax1 ay1 az1 dx dy dz bx0 by0 bz0 bx1 by1 bz1)
  (%%collide ax0 ay0 az0
	     (+ ax1 ax0) (+ ay1 ay0) (+ az1 az0)
	     dx dy dz
	     bx0 by0 bz0
	     (+ bx1 bx0) (+ by1 by0) (+ bz1 bz0)))

#+nil
(defun testit (&rest args)
  (multiple-value-bind (a b c d)
      (apply #'%%collide args)
    (print (list a b c d))))

#+nil
(defun testit2 (&rest args)
  (multiple-value-bind (a b c d)
      (apply #'%%%collide args)
    (print (list a b c d))))

#+nil
(defun test-cases ()
  (print "unit cubes along the z axis")
  (testit2 0 0 0 1 1 1
	  0 0 0
	  1 0 0 1 1 1)
  (print "corner touching cubes")
  (testit2 0 0 0 1 1 1
	  0 0 0
	  1 1 1 1 1 1)
  (print "corner business")
  (testit2 0 0 0 1 1 1
	  5 5 5
	  1.5 1.5 1.5 1 1 1)
  (print "a test")
  (testit2 0 0 0 1 1 1
	  2 3 10
	  1 2 5 1 1 1))
