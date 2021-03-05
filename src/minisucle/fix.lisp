(defpackage :fix
  (:use :cl))
(in-package :fix)

(defun fix ()
  (crud:use-crud-from-path (merge-pathnames (sucle-temp:path "saves/") "fix.db"))
  (voxel-chunks::clearworld))

(defun seed ()
  (dotimes (x 100)
    (dotimes (y 100)
      (dotimes (z 100)
	(setf (world:getblock x y z) 0))))
  (dotimes (x 16)
    (dotimes (y 16)
      (dotimes (z 16)
	(setf (world:getblock (random 32) (random 32) (random 32))
	      (random 32))))))
