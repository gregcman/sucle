(defpackage :sketch-util
  (:use :cl))
(in-package :sketch-util)

(struct-to-clos:struct->class
 (defstruct (font-info)
   filename
   size))
(defun vecto-data ()
  (if (boundp 'vecto::*graphics-state*)
      (vecto::image 
       vecto::*graphics-state*)
      (error "no vecto data. Try using VECTO:WITH-CANVAS ?")))
(defun render-text (typeface text r g b a)
  (zpb-ttf:with-font-loader (loader (font-info-filename typeface))
    (let* ((data
	    (vecto:string-bounding-box text (font-info-size typeface) loader))
	   (minx (aref data 0))
	   (miny (aref data 1))
	   (maxx (aref data 2))
	   (maxy (aref data 3)))
      (let ((width (ceiling (- maxx minx)))
	    (height (ceiling (- maxy miny))))
	(vecto:with-canvas (:width width :height height)
	  (vecto:set-font loader (font-info-size typeface))
	  (vecto:set-rgba-fill r g b a)
	  (vecto:translate (- minx) (- miny))
	  (vecto:draw-string 0 0 text)
	  (unflatten-for-image-utility (zpng:image-data (vecto-data))
				       width height))))))

(defun unflatten-for-image-utility (array width height)
  (let ((type (array-element-type array)))
    (assert (equal type '(unsigned-byte 8)))
    (let ((new
	   (make-array
	    (list height width 4)
	    :element-type '(unsigned-byte 8))))
      (dotimes (w width)
	(dotimes (h height)
	  (dotimes (i 4)
	    (setf (aref new h w i)
		  (aref array (+ (* h 4 width) (* w 4) i))))))
      new)))
