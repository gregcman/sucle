(defpackage :fix
  (:use :cl))
(in-package :fix)
#+nil
(defun fix ()
  (crud:use-crud-from-path (merge-pathnames (sucle-temp:path "saves/") "fix.db")))

(defun seed ()
  (dotimes (x 16)
    (dotimes (y 16)
      (dotimes (z 16)
	(setf (voxel-chunks:getobj (random 64) (random 64) (random 64))
	      (random 256)))))
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
  (time
   (dotimes (x 1000)
     (dotimes (y 1000)
       (setf (voxel-chunks:getobj y 0 x) (random 256))))))
(defun stress2-2 ()
  (time
   (dotimes (x 1000)
     (dotimes (y 1000)
       (setf (voxel-chunks:getobj y (+ x y) x) (random 256))))))

(defun fix-database ()
  (sucle-mp:with-initialize-multiprocessing
    (let ((database::*database* (sucle-temp:path "saves/fix.db")))
      (database::with-open-database2 (database::create-table)))))

(deftype key () '(cons fixnum (cons fixnum (cons fixnum t))))
(defun chunks (&optional (name "fix"))
  (let ((database::*database* (sucle-temp:path (merge-pathnames "saves/" (concatenate 'string name ".db")))))
    (remove-if-not (lambda (x) (typep x 'key))
		   (mapcar 'read-from-string
			   (mapcar 'first (database::with-open-database2 (database::all-data)))
			   ))))

(defun chunkat (key &optional (name "fix"))
  (crud:use-crud-from-path (sucle-temp:path (merge-pathnames "saves/" (concatenate 'string name ".db"))))
  (crud:crud-read key))

(defun what2 (&optional (name "new"))
  
  (let ((chunks (chunks name)))
    (dolist (key chunks)
      (let ((data (chunkat key name)))
	(flet ((make-data (data)
		 (let ((chunk-data (coerce data '(simple-array t (*)))))
		   #+nil
		   (assert (= 4096 (length chunk-data))
			   nil
			   "offending chunk ~a"
			   key)
		   (when (= 4096 (length chunk-data))
		     ;;(print "what")
		     (voxel-chunks::with-chunk-key-coordinates (xo zo yo) key
		       (utility:dobox ((xi 0 16)
				       (yi 0 16)
				       (zi 0 16))			     
				      (setf (voxel-chunks::getobj
					     (+ xo xi)
					     (+ yo yi)
					     (+ zo zi))
					    (mod (voxel-chunks::reference-inside-chunk chunk-data xi yi zi)
						 8))))))))
	  (typecase data
	    (list
	     (ecase (length data)
	       (0)
	       (3 ;;[FIXME]does this even work?
		(error "world format invalid"))
	       (1 (make-data (car data)))))
	    (otherwise (make-data data))))))))

(defun worldtest ()
  (time
   (progn (what2 "new")
	  (what2 "fix")
	  ;;(what2 "test")
	  (what2 "meh")
	  )))
