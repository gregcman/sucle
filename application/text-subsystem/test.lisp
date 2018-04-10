#+nil
(defun copy-array-buf ()
  (let ((width 256)
	(height 256))
    (cffi:with-foreign-object (b :uint8 (etouq (* 256 256 4)))
      (with-unsafe-speed
	  (dobox ((ypos 0 height)
		  (xpos 0 width))
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
			     (cffi:mem-aref b :uint8 (+ offset 1)) (ldb (byte 8 8) num)
			     (cffi:mem-aref b :uint8 (+ offset 2)) (logand 255 num) 
			     (cffi:mem-aref b :uint8 (+ offset 3)) (ldb (byte 8 24) num))
		       )))))
      (progn
	(gl:bind-texture :texture-2d (glhelp::texture (application::getfnc 'text-sub::text-data)))
	(gl:tex-sub-image-2d :texture-2d 0 0 0 width height :bgra :unsigned-byte b)))))
 
