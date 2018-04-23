(defpackage #:rectangle
  (:use #:cl)
  (:export
   #:rectangle
   #:x0
   #:x1
   #:y0
   #:y1
   #:rectangle-x0
   #:rectangle-y0
   #:rectangle-x1
   #:rectangle-y1
   #:coordinate-inside-rectangle-p)

  (:export
   #:r-intersect
   #:u
   #:r
   #:l
   #:r
   #:ur
   #:ul
   #:bl
   #:br))

(in-package #:rectangle)

(defclass rectangle ()
  ((x0 :accessor rectangle-x0
       :initform 0.0
       :initarg :x0)
   (y0 :accessor rectangle-y0
       :initform 0.0
       :initarg :y0)
   (x1 :accessor rectangle-x1
       :initform 0.0
       :initarg :x1)
   (y1 :accessor rectangle-y1
       :initform 0.0
       :initarg :y1)))

(defun coordinate-inside-rectangle-p (x y rectangle)
  (with-slots (x0 y0 x1 y1) rectangle
    (and (< x0 x x1)
	 (< y0 y y1))))

;;determine how rectangles intersect .0 
;;means the edges touch. positive number means there is space between
;;negative means it is past. the symbols u r l and b represent the top,
;;right, left, and bottom of the first rectangle. nil means none at all
;;t means an area intersection
(defun r-intersect (ax0 ay0 ax1 ay1 bx0 by0 bx1 by1)
  (let ((dbottom (- ay0 by1))
	(dright (- bx0 ax1))
	(dup (- by0 ay1))
	(dleft (- ax0 bx1)))
    (let ((db (zerop dbottom))
	  (dr (zerop dright))
	  (du (zerop dup))
	  (dl (zerop dleft)))
      (if (or db dr du dl)
	  (cond ((and du dr) 'ur)
		((and du dl) 'ul)
		((and db dl) 'bl)
		((and db dr) 'br)
		(db 'b)
		(dr 'r)
		(du 'u)
		(dl 'l))
	  (not (or (plusp dbottom)
		   (plusp dright)
		   (plusp dup)
		   (plusp dleft)))))))

#+nil
(defclass render-area ()
    ((x :accessor render-area-x
	:initform 0
	:initarg :x)
     (y :accessor render-area-y
	:initform 0
	:initarg :y)
     (width :accessor render-area-width
	    :initform 0
	    :initarg :width)
     (height :accessor render-area-height
	     :initform 0
	     :initarg :height)))
