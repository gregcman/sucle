(in-package :nsb-cga)

;;;;for some reason sb-cga does not have destructive matrix operations, where
;;;;the matrix result is stored in a pre-allocated place.
;;;;the creation of new single-float matrices leads to stress on the garbage
;;;;collector, leading to 10s of megabytes of matrix waste

(set-pprint-dispatch 'sb-cga::matrix nil)
(declaim (ftype (sb-cga::sfunction (sb-cga::matrix
			    single-float single-float single-float single-float
                            single-float single-float single-float single-float
                            single-float single-float single-float single-float
                            single-float single-float single-float single-float)
                           sb-cga::matrix)
                %matrix)
         (inline %matrix))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %matrix (result
		  m11 m12 m13 m14
		  m21 m22 m23 m24
		  m31 m32 m33 m34
		  m41 m42 m43 m44)
    "Construct MATRIX with the given elements (arguments are provided in row
major order.)"
    (macrolet ((put (num)
		 (let ((str (string num)))
		   (let ((num1 (read-from-string (subseq str 1 2)))
			 (num2 (read-from-string (subseq str 2 3))))
		     `(setf (aref result ,(+ -1 num1 (ash (1- num2) 2))) ,num))))
	       (putall (&rest rest)
		 (cons 'progn (mapcar (lambda (x) (list 'put x)) rest))))
      (putall m11 m21 m31 m41
	      m12 m22 m32 m42
	      m13 m23 m33 m43
	      m14 m24 m34 m44)
      result)))


(declaim (ftype (sb-cga::sfunction (sb-cga::matrix) sb-cga::matrix) %zero-matrix)
         (inline %zero-matrix))
(defun %zero-matrix (result)
  "Construct a zero matrix."
  (macrolet ((wow ()
	       (flet ((k (num)
			`(setf (aref result ,num) 0f0)))
		 (let ((acc nil))
		   (dotimes (x 16)
		     (push (k x) acc))
		   (push 'progn acc)))))
    (wow)
    result))

(declaim (ftype (sb-cga::sfunction (sb-cga::matrix) sb-cga::matrix) %identity-matrix))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %identity-matrix (result)
    "Construct an identity matrix."
    (%matrix
     result
     1f0 0f0 0f0 0f0
     0f0 1f0 0f0 0f0
     0f0 0f0 1f0 0f0
     0f0 0f0 0f0 1f0)
    result))

;;;; MATRIX MULTIPLICATION

(declaim (ftype (sb-cga::sfunction
		 (sb-cga::matrix sb-cga::matrix sb-cga::matrix)
		 sb-cga::matrix) %matrix*))
(defun %matrix* (result left right)
  "Multiply MATRICES. The result might not be freshly allocated if all,
or all but one multiplicant is an identity matrix."
  (macrolet ((inline-mul (left right dest)
               `(progn
                  ,@(loop for i below 4
                       append (loop for j below 4
                            collect
                              `(setf
                                (mref ,dest ,i ,j)
                                (+ ,@(loop for k below 4
                                        collect `(* (mref ,left ,i ,k) (mref ,right ,k ,j))))))))))
    (inline-mul left right result)
    result))

;;;; TRANSFORMATIONS

(declaim (ftype
	  (sb-cga::sfunction
	   (sb-cga::matrix single-float single-float single-float) sb-cga::matrix)
	  %translate*))
(defun %translate* (result x y z)
  "Construct a translation matrix from translation factors X, Y and Z."
  (%matrix result
	   1f0 0f0 0f0 x
	   0f0 1f0 0f0 y
	   0f0 0f0 1f0 z
	   0f0 0f0 0f0 1f0))

(declaim (ftype (sb-cga::sfunction (sb-cga::matrix sb-cga::vec) sb-cga::matrix) %translate))
(defun %translate (result vec)
  "Construct a translation matrix using first three elements of VEC as the
translation factors."
  (%translate* result (aref vec 0) (aref vec 1) (aref vec 2)))

(declaim (ftype (sb-cga::sfunction
		 (sb-cga::matrix single-float single-float single-float)
		 sb-cga::matrix) %scale*))
(defun %scale* (result x y z)
  "Construct a scaling matrix from scaling factors X, Y, and Z."
  (%matrix result
	   x    0f0  0f0  0f0
	   0f0  y    0f0  0f0
	   0f0  0f0  z    0f0
	   0f0  0f0  0f0  1f0))

(declaim (ftype (sb-cga::sfunction (sb-cga::matrix sb-cga::vec) sb-cga::matrix) %scale))
(defun %scale (result vec)
  "Construct a scaling matrix using first threee elements of VEC as the
scaling factors."
  (%scale* result (aref vec 0) (aref vec 1) (aref vec 2)))

(declaim (ftype (sb-cga::sfunction
		 (sb-cga::matrix
		  single-float single-float single-float) sb-cga::matrix) %rotate*))
(defun %rotate* (result x y z)
  "Construct a rotation matrix from rotation factors X, Y, Z."
  (let ((rotate (%identity-matrix result)))
    (unless (= 0f0 z)
      (let ((c (cos z))
            (s (sin z)))
        (setf rotate (matrix* rotate
                              (matrix c     (- s) 0f0    0f0
                                      s     c     0f0    0f0
                                      0f0   0f0   1f0    0f0
                                      0f0   0f0   0f0    1f0)))))
    (unless (= 0f0 y)
      (let ((c (cos y))
            (s (sin y)))
        (setf rotate (matrix* rotate
                              (matrix c     0f0   s      0f0
                                      0f0   1f0   0f0    0f0
                                      (- s) 0f0   c      0f0
                                      0f0   0f0   0f0    1f0)))))
    (unless (= 0f0 x)
      (let ((c (cos x))
            (s (sin x)))
        (setf rotate (matrix* rotate
                              (matrix 1f0   0f0   0f0    0f0
                                      0f0   c     (- s)  0f0
                                      0f0   s     c      0f0
                                      0f0   0f0   0f0    1f0)))))
    rotate))

(declaim (ftype (sb-cga::sfunction (sb-cga::matrix sb-cga::vec) sb-cga::matrix) %rotate))
(defun %rotate (result vec)
  "Construct a rotation matrix using first three elements of VEC as the
rotation factors."
  (%rotate* result (aref vec 0) (aref vec 1) (aref vec 2)))

(declaim (ftype (sb-cga::sfunction
		 (sb-cga::matrix sb-cga:vec single-float) sb-cga::matrix) %rotate-around))
(defun %rotate-around (result v radians)
  "Construct a rotation matrix that rotates by RADIANS around VEC V. 4th
element of V is ignored."
  (let ((c (cos radians))
	(s (sin radians))
	(g (- 1f0 (cos radians))))
    (let* ((x (aref v 0))
	   (y (aref v 1))
	   (z (aref v 2))
	   (gxx (* g x x)) (gxy (* g x y)) (gxz (* g x z))
	   (gyy (* g y y)) (gyz (* g y z)) (gzz (* g z z)))
      (%matrix result
	       (+ gxx c)        (- gxy (* s z))  (+ gxz (* s y)) 0f0
	       (+ gxy (* s z))  (+ gyy c)        (- gyz (* s x)) 0f0
	       (- gxz (* s y))  (+ gyz (* s x))  (+ gzz c)       0f0
	       0f0              0f0              0f0             1f0))))

(declaim (ftype (sb-cga::sfunction
		 (sb-cga::matrix single-float
				 single-float
				 single-float
				 single-float) sb-cga::matrix) %rotate-around*)
	 (inline %rotate-around*))
(defun %rotate-around* (result x y z radians)
  "Construct a rotation matrix that rotates by RADIANS around VEC V. 4th
element of V is ignored."
  (let ((c (cos radians))
	(s (sin radians))
	(g (- 1f0 (cos radians))))
    (let* ((gxx (* g x x)) (gxy (* g x y)) (gxz (* g x z))
	   (gyy (* g y y)) (gyz (* g y z)) (gzz (* g z z)))
      (%matrix result
	       (+ gxx c)        (- gxy (* s z))  (+ gxz (* s y)) 0f0
	       (+ gxy (* s z))  (+ gyy c)        (- gyz (* s x)) 0f0
	       (- gxz (* s y))  (+ gyz (* s x))  (+ gzz c)       0f0
	       0f0              0f0              0f0             1f0))))

(declaim (ftype (sb-cga::sfunction
		 (sb-cga::matrix sb-cga::vec sb-cga::vec)
		 sb-cga::matrix) %reorient))
(defun %reorient (result v1 v2)
  "Construct a transformation matrix to reorient V1 with V2."
  (let ((nv1 (normalize v1))
	(nv2 (normalize v2)))
    (if (vec~ nv1 nv2)
	(%identity-matrix result)
	(%rotate-around result
			(normalize (cross-product nv1 nv2))
			(acos (dot-product nv1 nv2))))))

(declaim (ftype (sb-cga::sfunction (sb-cga::matrix sb-cga::matrix) sb-cga::matrix) %transpose-matrix))
(defun %transpose-matrix (result matrix)
  "Transpose of MATRIX."
  (macrolet ((wow ()
	       (flet ((k (i j)
			`(setf (mref result ,i ,j) (mref matrix ,j ,i))))
		 (let ((acc nil))
		   (dotimes (i 4)
		     (dotimes (j 4)
		       (push (k i j) acc)))
		   (cons 'progn acc)))))
    (wow))
  result)

(declaim (ftype (sb-cga::sfunction
		 (sb-cga::matrix sb-cga::matrix)
		 sb-cga::matrix) %inverse-matrix))
(defun %inverse-matrix (result matrix)
  "Inverse of MATRIX. Signals an error if there is no inverse."
  (declare (type sb-cga::matrix result matrix))
  (if (eq matrix +identity-matrix+)
      +identity-matrix+
      (if (and (= 0f0 (mref matrix 3 0) (mref matrix 3 1) (mref matrix 3 2))
               (= 1f0 (mref matrix 3 3)))
          ;; Affine matrix, fast track (and less loss of accuracy from multiplications)
          (let ((inverse (%zero-matrix result)))
            ;; Inverse of the upper left 3x3
            (let ((det (sb-cga::submatrix-determinant matrix)))
              (if (zerop det)
                  ;; If the 3x3 is zero, we're fine -- otherwise punt to the complete
                  ;; implementation.
                  (dotimes (i 3)
                    (dotimes (j 3)
                      (unless (= 0f0 (mref matrix i j))
                        (return-from %inverse-matrix (%generic-inverse-matrix result matrix)))))
                  (macrolet ((inv ((i j) (ai aj bi bj) (ci cj di dj))
                             `(setf (mref inverse ,(1- i) ,(1- j))
                                    (/ (- (* (mref matrix ,(1- ai) ,(1- aj))
                                             (mref matrix ,(1- bi) ,(1- bj)))
                                          (* (mref matrix ,(1- ci) ,(1- cj))
                                             (mref matrix ,(1- di) ,(1- dj))))
                                       det))))
                  (inv (1 1) (2 2 3 3) (2 3 3 2))
                  (inv (1 2) (1 3 3 2) (1 2 3 3))
                  (inv (1 3) (1 2 2 3) (1 3 2 2))
                  (inv (2 1) (2 3 3 1) (2 1 3 3))
                  (inv (2 2) (1 1 3 3) (1 3 3 1))
                  (inv (2 3) (1 3 2 1) (1 1 2 3))
                  (inv (3 1) (2 1 3 2) (2 2 3 1))
                  (inv (3 2) (1 2 3 1) (1 1 3 2))
                  (inv (3 3) (1 1 2 2) (1 2 2 1)))))
            ;; translation: negation after dotting with upper rows
            (let ((x (mref matrix 0 3))
                  (y (mref matrix 1 3))
                  (z (mref matrix 2 3)))
              (dotimes (i 3)
                (setf (mref inverse i 3) (- (+ (* x (mref inverse i 0))
                                               (* y (mref inverse i 1))
                                               (* z (mref inverse i 2)))))))
            ;; affine bottom row (0 0 0 1)
            (setf (mref inverse 3 3) 1f0)
            inverse)
          (%generic-inverse-matrix result matrix))))


;;; KLUDGE: Default is too low to do a good job with GENERIC-INVERSE-MATRIX.
#+sbcl
(eval-when (:compile-toplevel)
  (setf sb-ext:*inline-expansion-limit* 1000))

(defun %generic-inverse-matrix (result matrix)
  (let ((det (matrix-determinant matrix)))
    (if (< (abs det) +default-epsilon+)
        (error "Cannot invert matrix with zero determinant:~%  ~S"
               matrix)
        (macrolet ((a (x y z)
                     (multiple-value-bind (r1 c1) (truncate (- x 11) 10)
                       (multiple-value-bind (r2 c2) (truncate (- y 11) 10)
                         (multiple-value-bind (r3 c3) (truncate (- z 11) 10)
                           `(* (mref matrix ,r1 ,c1)
                               (mref matrix ,r2 ,c2)
                               (mref matrix ,r3 ,c3)))))))
          (let ((m
                 (%matrix result
                  ;; row 1
                  (- (+ (a 22 33 44) (a 23 34 42) (a 24 32 43))
                     (a 22 34 43) (a 23 32 44) (a 24 33 42))
                  (- (+ (a 12 34 43) (a 13 32 44) (a 14 33 42))
                     (a 12 33 44) (a 13 34 42) (a 14 32 43))
                  (- (+ (a 12 23 44) (a 13 24 42) (a 14 22 43))
                     (a 12 24 43) (a 13 22 44) (a 14 23 42))
                  (- (+ (a 12 24 33) (a 13 22 34) (a 14 23 32))
                     (a 12 23 34) (a 13 24 32) (a 14 22 33))
                  ;; row 2
                  (- (+ (a 21 34 43) (a 23 31 44) (a 24 33 41))
                     (a 21 33 44) (a 23 34 41) (a 24 31 43))
                  (- (+ (a 11 33 44) (a 13 34 41) (a 14 31 43))
                     (a 11 34 43) (a 13 31 44) (a 14 33 41))
                  (- (+ (a 11 24 43) (a 13 21 44) (a 14 23 41))
                     (a 11 23 44) (a 13 24 41) (a 14 21 43))
                  (- (+ (a 11 23 34) (a 13 24 31) (a 14 21 33))
                     (a 11 24 33) (a 13 21 34) (a 14 23 31))
                  ;; row 3
                  (- (+ (a 21 32 44) (a 22 34 41) (a 24 31 42))
                     (a 21 34 42) (a 22 31 44) (a 24 32 41))
                  (- (+ (a 11 34 42) (a 12 31 44) (a 14 32 41))
                     (a 11 32 44) (a 12 34 41) (a 14 31 42))
                  (- (+ (a 11 22 44) (a 12 24 41) (a 14 21 42))
                     (a 11 24 42) (a 12 21 44) (a 14 22 41))
                  (- (+ (a 11 24 32) (a 12 21 34) (a 14 22 31))
                     (a 11 22 34) (a 12 24 31) (a 14 21 32))
                  ;; row 4
                  (- (+ (a 21 33 42) (a 22 31 43) (a 23 32 41))
                     (a 21 32 43) (a 22 33 41) (a 23 31 42))
                  (- (+ (a 11 32 43) (a 12 33 41) (a 13 31 42))
                     (a 11 33 42) (a 12 31 43) (a 13 32 41))
                  (- (+ (a 11 23 42) (a 12 21 43) (a 13 22 41))
                     (a 11 22 43) (a 12 23 41) (a 13 21 42))
                  (- (+ (a 11 22 33) (a 12 23 31) (a 13 21 32))
                     (a 11 23 32) (a 12 21 33) (a 13 22 31)))))
            (dotimes (i 4)
              (dotimes (j 4)
                (setf (mref m i j) (/ (mref m i j) det))))
            m)))))
