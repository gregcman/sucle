(in-package z:sandbox)

(progno
 #:cl-freetype2)
(progno
 (:file "fonts"))
(defparameter *face*
  (freetype2:new-face
   (case 1
     (0 "/usr/share/fonts/truetype/ubuntu-font-family/UbuntuMono-R.ttf")
     (1 "/usr/share/fonts/truetype/dejavu/DejaVuSansMono.ttf"))))

(defparameter *face-bold*
  (freetype2:new-face
   (if t
       "/usr/share/fonts/truetype/dejavu/DejaVuSansMono-Bold.ttf"
       "/usr/share/fonts/truetype/ubuntu-font-family/UbuntuMono-B.ttf")))

(defun test78 ()
  (freetype2:set-char-size *face* (* 8 64) (* 10 64) 96 96)
  (let ((height (ceiling (print (freetype2::string-pixel-height *face* "█"))))
	(width (ceiling (print (freetype2::string-pixel-width *face*  "█")))))
    (print (list width height))
    (let ((raster-array (make-array (list (* 16 height) (* 16 width) 4)
				    :element-type '(unsigned-byte 8))))
      (funland:dobox ((char 0 256))
	     (multiple-value-bind (array xoffset yoffset)
		 (toy-string-to-array *face* (string (code-char char)))
	       (multiple-value-bind (ybase xbase) (floor char 16)
		 (setf xbase (* xbase width)
		       ybase (* ybase height))
		 (incf xbase xoffset)
		 (incf ybase yoffset)
		 (destructuring-bind (height width) (array-dimensions array)
		   (funland:dobox ((y 0 height)
			   (x 0 width))
			  (let ((ax (+ x xbase))
				(ay (+ y ybase))
				(ans (aref array y x)))
			    (dotimes (channel 4)
			      (setf (aref raster-array
					  ay
					  ax
					  channel)
				    ans))))))))
      (opticl:write-png-file "/home/imac/quicklisp/local-projects/symmetrical-umbrella/cl-program/src/res/font/achar2.png"
			     raster-array))))

"ii██"
"ii██"
"ii██"


(defun toy-string-to-array (face string)
  (let (ans xans yans)
    (freetype2:do-string-render (face string bitmap x y)
      (setf (values ans xans yans)
	    (values
	     (bitmap-to-array bitmap) x y)))
    (values ans xans yans)))


(defun ablit (arr1 arr2 &key (x 0) (y 0))
  "Destructivly copy arr2 into arr1 for 2- and 3-dimensional (Y:X, Y:X:RGB(A))
arrays.  X and Y may be specified as a 2D offset into ARR1."
  (assert (= (array-rank arr1) (array-rank arr2)))
  (let ((flat1 (freetype2::flat-array arr1))
	(flat2 (freetype2::flat-array arr2))
	(height1 (freetype2::row-width arr1))
	(height2 (freetype2::row-width arr2))
	(width1 (array-dimension arr1 1))
	(width2 (array-dimension arr2 1))
	(xoff (* x (if (= (array-rank arr1) 3)
		       (array-dimension arr1 2)
		       1))))
    (loop for y2 from 0 below height2
       for y1 from y below height1
       do (let ((x1 (+ (* y1 width1) xoff))
		(x2 (* y2 width2)))
	    (replace flat1 flat2
		     :start1 (max 0 x1)
		     :end1 (* (1+ y1) width1)
		     :start2 (max 0 x2)
		     :end2 (+ x2 width2)))))
  arr1)

(defun bitmap-to-array (bitmap)
  "=> ARRAY

Convert `BITMAP` from internal `FT_Bitmap`'s internal representation to
a native array.  This is specified for a `FT-BITMAP-PIXEL-FORMAT` of `:MONO`,
`:GRAY`, `:LCD`, and `:LCD-V`.

Note that for :LCD and :LCD-V, the result is a either 3\\*width or
3\\*height, respectively.  This may change in the future."
  (let ((buffer (freetype2::ft-bitmap-buffer bitmap))
	(rows (freetype2::ft-bitmap-rows bitmap))
	(width (freetype2::ft-bitmap-width bitmap))
	(pitch (freetype2::ft-bitmap-pitch bitmap))
	(format (freetype2::ft-bitmap-pixel-mode bitmap)))
    (let ((pixel-fn (ecase format
		      (:mono #'freetype2::nth-mono-pixel)
		      (:gray #'freetype2::nth-gray-pixel)
		      (:lcd #'freetype2::nth-gray-pixel)
		      (:lcd-v #'freetype2::nth-gray-pixel)))
	  (array (make-array (list rows width) :element-type '(unsigned-byte 8))))
      (declare (function pixel-fn))
      #+-(format t "buffer: ~A rows: ~A width: ~A pitch: ~A format: ~A~%"
		 buffer rows width pitch format)
      (loop for i from 0 below rows
	 as ptr = (freetype2::inc-pointer buffer (* i pitch))
	 do (loop for j from 0 below width
	       do (setf (aref array i j) (funcall pixel-fn ptr j)))
	 finally (return (values array format))))))
  
