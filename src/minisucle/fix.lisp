(defpackage :fix
  (:use :cl))
(in-package :fix)

(defun fix ()
  (crud:use-crud-from-path (merge-pathnames (sucle-temp:path "saves/") "fix.db")))

(defun seed ()
  (dotimes (x 32)
    (dotimes (y 32)
      (dotimes (z 32)
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

(defun stress ()
  (dotimes (x 1000)
    (setf (voxel-chunks:getobj 0 0 x) (random 256))))

(defun stress2 ()
  (dotimes (x 1000)
    (dotimes (y 1000)
      (setf (voxel-chunks:getobj y 0 x) (random 256)))))
(defun stress2-2 ()
  (dotimes (x 1000)
    (dotimes (y 1000)
      (setf (voxel-chunks:getobj y (+ x y) x) (random 256)))))

(defun fix-database ()
  (sucle-mp:with-initialize-multiprocessing
    (let ((database::*database* (sucle-temp:path "saves/fix.db")))
      (database::with-open-database2 (database::create-table)))))


