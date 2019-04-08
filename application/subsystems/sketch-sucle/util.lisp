(defpackage :sketch-util
  (:use :cl))
(in-package :sketch-util)

(defun file-name-extension (name)
  ;; taken from dto's xelf code
  (let ((pos (position #\. name :from-end t)))
    (when (numberp pos)
      (subseq name (1+ pos)))))

(defun opticl-load-image (file)
  (let ((extension (file-name-extension file)))
    (cond ((string= extension "png")
	   (opticl:read-png-file file))
	  ((string= extension "jpeg")
	   (opticl:read-jpeg-file file))
	  ((string= extension "tiff")
	   (opticl:read-tiff-file file))
	  ((string= extension "pnm")
	   (opticl:read-pnm-file file))
	  ((string= extension "pbm")
	   (opticl:read-pbm-file file))
	  ((string= extension "gif")
	   (opticl:read-gif-file file))
	  (t (error "unsupported file format ~s ~s" extension file)))))

(struct-to-clos:struct->class
 (defstruct (opticl-loaded-surface)
   data
   width
   height))

(defun convert (opticl-data)
  (let ((dimensions (array-dimensions opticl-data))
	(type (array-element-type opticl-data)))
    (when (or (not (eq 'unsigned-byte
		       (first type)))
	      (member type '(single-float double-float fixnum)))
      (error "type not supported"))
    (let ((channels (or (third dimensions)
			1))
	  (width (first dimensions))
	  (height (second dimensions)))
      (let ((new (make-array (* width height 4)
			     :element-type '(unsigned-byte 8))))
	;;FIXME::bits not correct? 
	;;32 -> 8 = ash n -24
	;;16 -> 8 = ash n -8
	;;8 -> 8 = identity n
	;;4 -> 8 = 15 -> 255 = * n 255/15 = * n 17
	;;2 -> 8 = 3 -> 255 = * n 85
	;;1 -> 8 = 1 -> 255 = * n 255
	(flet ((u32->8 (n)
		 (declare (optimize (speed 3) (safety 0)))
		 (declare (type (unsigned-byte 32) n))
		 (ash n -24))
	       (u16->8 (n)
		 (declare (optimize (speed 3) (safety 0)))
		 (declare (type (unsigned-byte 16) n))
		 (ash n -8))
	       (u8->8 (n)
		 n)
	       (u4->8 (n)
		 (declare (optimize (speed 3) (safety 0)))
		 (declare (type (unsigned-byte 4) n))
		 (* n 17))
	       (u2->8 (n)
		 (declare (optimize (speed 3) (safety 0)))
		 (declare (type (unsigned-byte 2) n))
		 (* n 85))
	       (u1->8 (n)
		 (declare (optimize (speed 3) (safety 0)))
		 (declare (type (unsigned-byte 1) n))
		 (* n 255)))
	  (let ((convert-fun (ecase (second type)
			       (32 #'u32->8)
			       (16 #'u16->8)
			       (8 #'u8->8)
			       (4 #'u4->8)
			       (2 #'u2->8)
			       (1 #'u1->8))))
	    (flet ((convert-value (value)
		     (funcall convert-fun value)))
	      (flet ((dump-pixels-1 (w h)
		       (let ((gray
			      (convert-value (aref opticl-data w h))))
			 ;;FIXME::include alpha channel or not?
			 (values
			  gray
			  gray
			  gray
			  255)))
		     (dump-pixels-2 (w h)
		       (let ((gray (convert-value (aref opticl-data w h 0)))
			     (alpha (convert-value (aref opticl-data w h 1))))
			 (values gray gray alpha)))
		     (dump-pixels-3 (w h)
		       (values
			(convert-value (aref opticl-data w h 0))
			(convert-value (aref opticl-data w h 1))
			(convert-value (aref opticl-data w h 2))
			255))
		     (dump-pixels-4 (w h)
		       (values
			(convert-value (aref opticl-data w h 0))
			(convert-value (aref opticl-data w h 1))
			(convert-value (aref opticl-data w h 2))
			(convert-value (aref opticl-data w h 3)))))
		(let ((dump-pixels-fun (ecase channels
					  (1 #'dump-pixels-1)
					  (2 #'dump-pixels-2)
					  (3 #'dump-pixels-3)
					  (4 #'dump-pixels-4))))
		  (dotimes (w width)
		    (dotimes (h height)
		      (multiple-value-bind (r g b a) (funcall dump-pixels-fun w h)
			(let ((base (* 4 (+ h (* height w)))))
			  (setf (aref new (+ base 0)) r
				(aref new (+ base 1)) g
				(aref new (+ base 2)) b
				(aref new (+ base 3)) a))))))))))
	(values
	 new
	 width
	 height)))))

(defparameter *path* (merge-pathnames
		      "res/lenna.png"
		      (asdf:system-source-directory :sketch)))
(defparameter *test* nil;;(sketch-util::opticl-load-image *path*)
  )
(defun make-opticl-data (&optional (path *path*))
  (multiple-value-bind (data width height)
      (convert (opticl-load-image path))
    (make-opticl-loaded-surface
     :data data
     :width width
     :height height)))

;;a -> a a a 1.0 or a a a a?
;;ra -> r r r a
;;rgb -> r g b 1.0
;;rgba -> r g b a


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
	  (make-opticl-loaded-surface
	   :data (zpng:image-data (vecto-data))
	   :width width
	   :height height))))))

