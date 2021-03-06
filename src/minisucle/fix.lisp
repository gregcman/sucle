(defpackage :fix
  (:use :cl))
(in-package :fix)

(defun fix ()
  (crud:use-crud-from-path (merge-pathnames (sucle-temp:path "saves/") "fix.db")))

(defun seed ()
  (dotimes (x 100)
    (dotimes (y 100)
      (dotimes (z 100)
	(setf (voxel-chunks:getobj x y z) 0))))
  (dotimes (x 16)
    (dotimes (y 16)
      (dotimes (z 16)
	(setf (voxel-chunks:getobj (random 32) (random 32) (random 32))
	      (random 32))))))

(defun set-with-update (i j k new)
  (progn
    (setf (voxel-chunks:getobj i j k) new)
    (deflazy:refresh 'render-chunks::chunk)))

(defun update-world-vao ()
  (deflazy:refresh 'render-chunks::chunk))


(defun what ()
  (seed)
  (update-world-vao))


(defun fix-database ()
  (sucle-mp:with-initialize-multiprocessing
    (let ((database::*database* (sucle-temp:path "saves/fix.db")))
      (database::with-open-database2 (database::create-table)))))


