(defpackage #:text-sub-test
  (:use #:cl))
(in-package #:text-sub-test)

(defun fuzz ()
  "draw random color foreground, background, character, and underline/bold to the text data"
  (let ((width text-sub::*text-data-width*)
	(height text-sub::*text-data-height*))
    (cffi:with-foreign-object (b :uint8 (* width height 4))
      (let ((xbase (random 256))
	    (ybase (random 256)))
	(dotimes (ypos height)
	  (dotimes (xpos width)
	    (let ((base (the fixnum (+ xpos (the fixnum (* ypos width))))))
	      (let ((offset (the fixnum (* 4 base))))
		(let ((num
		       #+nil
			(logior (char-code (aref *foo* (mod base 1024)))
				(ash 0 8)
				(ash 255 16))
					;	#+nil

			(ash (random (1- (ash 1 32))) -2)
			
			#+nil
			(get-char-num
			 (get-char (the fixnum (+ xpos xstart))
				   (the fixnum (+ ypos ystart))))))
		  (setf (cffi:mem-aref b :uint8 (+ offset 0)) (ldb (byte 8 16) num)
			(cffi:mem-aref b :uint8 (+ offset 1)) (mod (+ ybase ypos) 256)
			;;(ldb (byte 8 8) num)
			(cffi:mem-aref b :uint8 (+ offset 2)) (mod (+ xbase xpos) 256)
			;;(logand 255 num) 
			(cffi:mem-aref b :uint8 (+ offset 3)) (ldb (byte 2 24) num))
		  ))))))
      (progn
	(gl:bind-texture :texture-2d (text-sub::get-text-texture))
	(gl:tex-sub-image-2d :texture-2d 0 0 0 width height :rgba :unsigned-byte b)))))
 
