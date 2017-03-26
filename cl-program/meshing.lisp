(in-package :sandbox)

(defun make-attrib-buffer-data ()
  (make-array 16 :element-type t :initial-element nil))

(defun fill-with-flhats (array)
  (map-into array #'flhat:make-flhat))
(defun make-iterators (buffer result)
  (map-into result (lambda (x) (flhat:make-flhat-iterator x)) buffer))

(defun tally-buffer (iterator-buffer result)
  (map-into result (lambda (x) (iterator-count x)) iterator-buffer))

(defun reset-attrib-buffer-iterators (fill-data)
  (dotimes (x (array-total-size fill-data))
    (flhat:reset-iterator (aref fill-data x))))

(defun iterator-count (iterator)
  (if (iter-ator:p-array iterator)
      (1+ (flhat:iterator-position iterator))
      0))

(defparameter *attrib-buffers* (fill-with-flhats (make-attrib-buffer-data)))
(defparameter *attrib-buffer-iterators*
  (make-iterators *attrib-buffers* (make-attrib-buffer-data)))
(defparameter *attrib-buffer-fill-pointer*
  (tally-buffer *attrib-buffer-iterators* (make-attrib-buffer-data)))


(defun mesh-test42 (times lit tex pos)
  (declare (type iter-ator:iter-ator tex pos))
  (iter-ator:wasabiis ((d lit)
		       (uv tex)
		       (xyz pos))
    (dotimes (x times)
      (%gl:vertex-attrib-1f 8 (d))
      (%gl:vertex-attrib-2f 2 (uv) (uv))
      (%gl:vertex-attrib-3f 0 (xyz) (xyz) (xyz)))))


(defparameter susie (flhat:make-flhat))
(defparameter george (flhat:make-flhat))
(defparameter pauline (flhat:make-flhat))

(defun reset-susie ()
  (setf susie (flhat:make-flhat)))

(defun test6 (times)
  (declare (optimize (speed 3) (safety 0)))
  (let ((spine (flhat:make-flhat-iterator susie)))
    (declare (type iter-ator:iter-ator spine))
    (dotimes (x 1 (flhat:iterator-position spine))
      (flhat:reset-iterator spine)
      (iateor:wasabios ((next spine))
	(dotimes (x times)
	  (next x))))))

(defun test69 ()
  (declare (optimize (speed 3) (safety 0)))
  (let ((spine (flhat:make-flhat-iterator susie))
	(spine2 (flhat:make-flhat-iterator george))
	(spine3 (flhat:make-flhat-iterator pauline)))
    (declare (type iter-ator:iter-ator spine spine2 spine3))
    (dotimes (x 25)
      (deach flhat:reset-iterator spine spine2 spine3)
      (iateor:wasabios ((emit spine)
			(emit2 spine2)
			(emit3 spine3))
	(dotimes (x (floor (expt 10 7) 3))
	  (emit x)
	  (emit2 x)
	  (emit3 x))))))
