(in-package :downgrade-array)

;;;;
;;;;Tests
;;;;

(defun typetest (item test)
  (let ((result (typep item test)))
    (if result
	(format t "~% ~a is ~a" item test)
	(format t "~% ~a is not ~a" item test))))
(defun test ()
  (typetest 1 '(signed-byte 1))
  (typetest -1 '(signed-byte 1))
  (typetest 256 '(signed-byte 8))
  (typetest -256 '(signed-byte 8))
  (typetest 127 '(signed-byte 8))
  (typetest -127 '(signed-byte 8))
  (typetest 128 '(signed-byte 8))
  (typetest -128 '(signed-byte 8)))

(defun test-at-random (&optional (min (- (random most-positive-fixnum)))
			 (max (random most-positive-fixnum)))
  (let ((type (num-type min max)))
    (typetest min type)
    (typetest max type)))

(defun test-again (&optional (s (random 64)))
  (let ((foo (expt 2 (1- s))))
    (test-at-random (- foo) (1+ foo))))

(defun test0 ()
  (print (downgrade-array #(0 1 2 3 4 5)))
  (print (downgrade-array #(0 1 2 3 4 5 34524352345)))
  (print (downgrade-array #(-345345 0 1 2 3 4 5))))

(defun test1 ()
  (let ((*print-readably* t))
    (print
     (list
      (really-downgrade-array #(1 1 0 0 0 1 0 1 0))
      (really-downgrade-array #(1 1 0 0 0 1 0 -1 0))
      (really-downgrade-array #(1 1 0 0 0 1 0 -345345 0))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :voxel-chunks)
(defun test-chunks ()
  
  (let ((acc ()))
    (flet ((add (x y z w)
	     (push (list x y z w) acc)
	     (setf (getobj x y z) w)))

      ;;Not a real test, because blocks can overlap!!!
      #+nil
      (dotimes (i 1000)
	(add (random 10000) (random 10000) (random 10000) (random 10000)))
      (time
       (dotimes (i 10000)
	 (add i i i i)))

      (dolist (item acc)
	(destructuring-bind (x y z w) item
	  (assert (eq w (getobj x y z))))))))

(utility:with-unsafe-speed
  (defun test1 ()
    (time
     (let ((n 2001))
       (dotimes (i (expt 10 6))
	 (setf (getobj n n n) n)))))
  (defun test2 ()
    (time
     (dotimes (i (expt 10 6))
       (setf (getobj (random 64) (random 64) (random 64)) (random 64)))))

  (defun test3 ()
    (time
     (dotimes (i (expt 10 6))
       (let ((x (mod (* i 7) 512))
	     (y (mod (* i 13) 512))
	     (z (mod (* i 19) 512)))
	 (setf (getobj x y z) i))))))
