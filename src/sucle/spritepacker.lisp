(defpackage #:spritepacker
  (:use :cl))
(in-package #:spritepacker)

(defparameter *packed-file* (sucle-temp:path "res/terrain.png"))
(defparameter *out-spec* (sucle-temp:path "res/terrain.spec"))
(defparameter *width* 256)
(defparameter *height* 256)
(defun generate-sprite-sheet ()
  (let ((sprites (sucle-temp:path "res/sprites")))
    (patchwork:make-atlas-from-directory sprites
					 :out-file *packed-file*
					 :width *width*
					 :height *height*)))

(defparameter *spec*
  (read-from-string 
   (alexandria:read-file-into-string
    *out-spec*)))

;;Data by name
(defparameter *data* (make-hash-table :test 'equal))
;;Data by number id
(defparameter *data-number* (make-array (length *spec*)))

(defun assign-ids ()
  (let ((number 0))
    (dolist (item *spec*)
      (let ((data (list* :number number item)))
	(setf (gethash (getf item :id) *data*) data)
	(setf (aref *data-number* number) data))
      (incf number))
    number))
;;FIXME::called upon load
(assign-ids)

(defparameter *single-float-array*
  (make-array (* 4 (length *spec*)) :element-type 'single-float))

(defmacro floatf (&rest args)
  `(progn
     ,@(mapcar (lambda (arg)
		 `(setf ,arg (coerce ,arg 'single-float)))
	       args)))

(defun generate-uvs (&aux (arr *single-float-array*))
  (do* ((spec *spec* (cdr spec))
	(item (car spec) (car spec))
	(base 0 (+ base 4)))
       ((null spec))
    ;;[TODO]:duplicate, see bounds-for-name
    (let* ((x0 (getf item :x))
	   (y0 (getf item :y))
	   (x1 (+ x0 (getf item :w)))
	   (y1 (+ y0 (getf item :h))))
      (floatf x0 y0 x1 y1)
      (setf (aref arr (+ base 0)) (/ x0 *width*)
	    (aref arr (+ base 1)) (/ y0 *height*)
	    (aref arr (+ base 2)) (/ x1 *width*)
	    (aref arr (+ base 3)) (/ y1 *height*)))))
;;[FIXME]::called upon load
(generate-uvs)

(defun uvs-for-id (id)
  (utility:with-vec (((* 4 id) u0 v0 u1 v1)) (*single-float-array*)
    (values u0 v0 u1 v1)))
(defun bounds-for-name (name)
  (let ((item (gethash name *data*)))
    ;;[TODO]:duplicate, see generate-uvs
    (let* ((x0 (getf item :x))
	   (y0 (getf item :y))
	   (x1 (+ x0 (getf item :w)))
	   (y1 (+ y0 (getf item :h))))
      (values x0 y0 x1 y1))))

(defun num (name)
  (getf (gethash name *data*) :number))
