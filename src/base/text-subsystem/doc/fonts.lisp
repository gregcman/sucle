(defpackage #:text-subsystem-generate-font
  (:use :cl)
  (:import-from
   #:freetype2
   #:flat-array
   #:row-width
   #:ft-bitmap-buffer
   #:ft-bitmap-rows
   #:ft-bitmap-pitch
   #:ft-bitmap-width
   #:ft-bitmap-pixel-mode
   #:string-pixel-width
   #:string-pixel-height
   #:inc-pointer
   #:nth-gray-pixel
   #:nth-mono-pixel))

(in-package :text-subsystem-generate-font)

(defparameter *face-path*
  ;;Put your regular and bold monospace font file here
  nil
  #+nil
  '("/usr/share/fonts/truetype/ubuntu/UbuntuMono-R.ttf"
    "/usr/share/fonts/truetype/ubuntu/UbuntuMono-B.ttf")
  #+nil
  '("/usr/share/fonts/truetype/dejavu/DejaVuSansMono.ttf"
    "/usr/share/fonts/truetype/dejavu/DejaVuSansMono-Bold.ttf"))

(defparameter *freetype-face-object* nil)

(defmacro with-face ((face-path) &body body)
  `(freetype2:with-open-face (*freetype-face-object* ,face-path)
     ,@body))

(defun power-of-2-ceiling (n)
  (ash 1 (ceiling (log n 2))))

(defun setpt-helper (max-height &optional (face *freetype-face-object*))
  (multiple-value-bind (pt width) (find-max-width-points max-height face)
    (set-size pt 1)
    ;;We assume that the block character takes up the maximum space in the monospace font.
    (let ((height (ceiling (string-pixel-height *freetype-face-object*  "█"))))
      (values pt height width))))

(defun dump-font () ;;Run this function in order to dump the font.png
  (if *face-path*
      (apply 'test420 (append *face-path*
			      ;;width 8 and aspect 2 means width 8 and 8*2 = 16 height
			      (list 8 2)))
      (error "no font files!")))

(defun test420 (regular-path bold-path glyph-height target-aspect)
  (with-face (regular-path)
    (multiple-value-bind (pt real-height width) (setpt-helper glyph-height)
      (setf width (power-of-2-ceiling width))
      ;;height twice width
      (let* (
	     (yscale (print (* (/ width real-height) target-aspect)))
	     (grid-x 16)
	     (grid-y 8)
	     (raster-array (make-array (list (* 2 target-aspect grid-y width)
					     (* 2 grid-x width))
				       :element-type '(unsigned-byte 8)
				       :initial-element 255)))
	(let ((height (* target-aspect width)))
	  (set-size pt yscale)
	  (draw-to-array raster-array grid-x grid-y width height 0 0)
	  (draw-to-array raster-array grid-x grid-y width height grid-x 0)
	  (draw-to-array raster-array grid-x grid-y width height grid-x 0 :underline-p t)

	  (with-face (bold-path)
	    (set-size pt yscale)
	    (draw-to-array raster-array grid-x grid-y width height 0 grid-y)
	    (draw-to-array raster-array grid-x grid-y width height grid-x grid-y)
	    (draw-to-array raster-array grid-x grid-y width height grid-x grid-y :underline-p t))
	  (opticl:write-png-file
	   (merge-pathnames "font.png" (asdf:system-source-directory :text-subsystem-generate-font))
	   raster-array))))))

(defun set-size (n scale &optional (face *freetype-face-object*))
  (freetype2:set-char-size face
			   (* n 64) 0;(floor (* n 64 scale))
			   64
			   (floor (* 64 scale))
			   ))

(defun find-max-width-points (&optional (max-width 16) (face *freetype-face-object*))
  "find the maximum size font that fits under max-width pixels. 
fonts are generally taller than they are wide. Return (values pt height)"
  (let ((pt 0)
	;; (char-height 16)
	(last-width nil)
	(last-pt nil))
    (block out
      (loop
	 (set-size pt 1 face)
	 (let ((new-width
		(ceiling (string-pixel-width face "█"))))
	   (when (> new-width max-width)
	     (return-from out))
	   (incf pt)
	   ;;(print char-width)
	   (setf last-width new-width
		 last-pt pt))))
    (values last-pt
	    last-width)))

(defun legit-array-index (array &rest indices)
  (block out
    (mapc (lambda (max value)
	    (unless (> max value -1)
	      (return-from out nil)))
	  (array-dimensions array)
	  indices)
    t))

;;grid-x and grid-y are the size in charactesr of the grid
;;width and height are size of individual texture in pixels
(defun draw-to-array
    (raster-array
     grid-x grid-y
     width height
     char-x-offset char-y-offset
     &key (face *freetype-face-object*) underline-p)
  (utility:dobox
   ((char 0 (* grid-x grid-y)))
   (multiple-value-bind (array xoffset yoffset)
       (toy-string-to-array face
			    (if underline-p
				"_" ;;draw an underline for underline font			     
				(string
				 (code-char 
				  char))))
     ;;   (print char)
     ;;   (print (list xoffset yoffset array))
     (multiple-value-bind (ybase xbase) (floor char grid-x)
       (setf xbase (* xbase width)
	     ybase (* ybase height))
       (incf xbase xoffset)
       (incf ybase yoffset)
       (incf xbase (* char-x-offset width))
       (incf ybase (* char-y-offset height))
       (destructuring-bind (sub-height sub-width) (array-dimensions array)
	 (utility:dobox
	  ((y 0 sub-height)
	   (x 0 sub-width))
	  (let ((ax (+ x xbase))
		(ay (+ y ybase))
		(ans (aref array y x)))
	    ;;  (print (list x y))
	    (if (legit-array-index raster-array ay ax)
		;;(dotimes (channel 4))
		(setf (aref raster-array
			    ay
			    ax
			 ;;   channel
			    )
		      
		      (- 255 ans)
		      ;;(- 255 ans)
		      )
		;;	(print "out of bounds")
		))))))))

;;"ii██"
;;"ii██"
;;"ii██"


(defun toy-string-to-array (face string)
  (let (ans xans yans)
    (freetype2:do-string-render (face string bitmap x y :baseline-y-p nil :offsets-p t;nil
				      )
      (setf (values ans xans yans)
	    (values
	     (bitmap-to-array bitmap) x y)))
    (values ans xans yans)))


(defun ablit (arr1 arr2 &key (x 0) (y 0))
  "Destructivly copy arr2 into arr1 for 2- and 3-dimensional (Y:X, Y:X:RGB(A))
arrays.  X and Y may be specified as a 2D offset into ARR1."
  (assert (= (array-rank arr1) (array-rank arr2)))
  (let ((flat1 (flat-array arr1))
	(flat2 (flat-array arr2))
	(height1 (row-width arr1))
	(height2 (row-width arr2))
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
  (let ((buffer (ft-bitmap-buffer bitmap))
	(rows (ft-bitmap-rows bitmap))
	(width (ft-bitmap-width bitmap))
	(pitch (ft-bitmap-pitch bitmap))
	(format (ft-bitmap-pixel-mode bitmap)))
    (let ((pixel-fn (ecase format
		      (:mono #'nth-mono-pixel)
		      (:gray #'nth-gray-pixel)
		      (:lcd #'nth-gray-pixel)
		      (:lcd-v #'nth-gray-pixel)))
	  (array (make-array (list rows width) :element-type '(unsigned-byte 8))))
      (declare (function pixel-fn))
      #+-(format t "buffer: ~A rows: ~A width: ~A pitch: ~A format: ~A~%"
		 buffer rows width pitch format)
      (loop for i from 0 below rows
	 as ptr = (inc-pointer buffer (* i pitch))
	 do (loop for j from 0 below width
	       do (setf (aref array i j) (funcall pixel-fn ptr j)))
	 finally (return (values array format))))))
  
