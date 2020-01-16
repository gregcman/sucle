(defpackage #:ncurses-clone-for-lem
  (:use :cl)
  (:export
   #:*glyph-height*
   #:*glyph-width*
   #:*redraw-display-p*
   #:*resized-p*
   #:init
   #:redraw-display
   #:render
   #:window-size))
(in-package :ncurses-clone-for-lem)
(defparameter *glyph-height* 16.0)
(defparameter *glyph-width* 8.0)

(defparameter *resized-p* nil)
(deflazy::deflazy virtual-window ((w application::w) (h application::h))
  (setf *resized-p* t)
  (setf ncurses-clone::*columns* (floor w *glyph-width*)
	ncurses-clone::*lines* (floor h *glyph-height*))
  ;;(ncurses-clone::reset-standard-screen)
  (ncurses-clone::with-virtual-window-lock
    (ncurses-clone::ncurses-wresize ncurses-clone::*std-scr*
				    ncurses-clone::*lines*
				    ncurses-clone::*columns*)
    #+nil
    (setf ncurses-clone::*virtual-window*
	  (ncurses-clone::make-virtual-window))))

(defun window-size ()
  (mapcar 'floor
	  (list (* ncurses-clone::*columns*
		   *glyph-width*)
		(* ncurses-clone::*lines*
		   *glyph-height*))))

(defun update-resize ()
  (setf *resized-p* t))

(defun init ()
  (setf ncurses-clone::*columns* 80
	ncurses-clone::*lines* 25)
  (set-glyph-dimensions 8 16)
  (setf text-sub::*text-data-what-type* :texture-2d)
  (deflazy::refresh 'virtual-window)
  (update-resize)
  (text-sub::change-color-lookup
   ;;'text-sub::color-fun
   'lem.term::color-fun
   #+nil
   (lambda (n)
     (values-list
      (print (mapcar (lambda (x) (/ (utility::floatify x) 255.0))
		     (nbutlast (aref lem.term::*colors* n))))))
   ))

(defun set-glyph-dimensions (w h)
  ;;Set the pixel dimensions of a 1 wide by 1 high character
  (setf *glyph-width* w)
  (setf text-sub::*block-width* w)
  (setf *glyph-height* h)
  (setf text-sub::*block-height* h)
  (progn
    ;;FIXME::Better way to organize this? as of now manually determining that
    ;;these two depend on the *block-height* and *block-width* variables
    (deflazy::refresh 'text-sub::render-normal-text-indirection)
    (deflazy::refresh 'virtual-window)))

(defparameter *redraw-display-p* nil)
(defun redraw-display ()
  (setf *redraw-display-p* t))


(defun render
    (&key (ondraw (lambda ())) (big-glyph-fun 'identity))
  ;;Make sure the virtual window has the correct specs
  (deflazy::getfnc 'virtual-window)
  #+nil
  (;;text-sub::with-data-shader (uniform rebase)
   ;; (gl:clear :color-buffer-bit)
   ;;   (gl:disable :depth-test)
   #+nil
   (rebase -128.0 -128.0))
  #+nil
  (gl:point-size 1.0)

  ;;;;what? this is to replace (gl:with-primitives :points ...body)
  ;;;; to find bug where resizing the lem window over and over causes crash
  #+nil
  (unwind-protect (progn
		    (gl:begin :points)
		    (opengl-immediate::mesh-vertex-color))
    (gl:end))
  ;;(glhelp:set-render-area 0 0 window:*width* window:*height*)
  ;;(gl:clear-color 0.0 0.0 0.0 0.0)
  ;;(gl:clear :color-buffer-bit)
  (gl:polygon-mode :front-and-back :fill)
  (gl:disable :cull-face)
  (gl:disable :depth-test)
  (gl:disable :blend)
    
  (when ncurses-clone::*update-p*
    (setf ncurses-clone::*update-p* nil)
    (funcall ondraw)
    ;;;Copy the virtual screen to a c-array,
    ;;;then send the c-array to an opengl texture
    (let* ((c-array-lines
	    (min text-sub::*text-data-height* ;do not send data larger than text data
		 (+ 1 ncurses-clone::*lines*)))              ;width or height
	   (c-array-columns
	    (min text-sub::*text-data-width*
		 (+ 1 ncurses-clone::*columns*)))
	   (c-array-len (* 4
			   c-array-columns
			   c-array-lines)))
      (cffi:with-foreign-object
       (arr :uint8 c-array-len)
       (flet ((color (r g b a x y)
		(let ((base (* 4 (+ x (* y c-array-columns)))))
		  (setf (cffi:mem-ref arr :uint8 (+ 0 base)) r
			(cffi:mem-ref arr :uint8 (+ 1 base)) g
			(cffi:mem-ref arr :uint8 (+ 2 base)) b
			(cffi:mem-ref arr :uint8 (+ 3 base)) a))))
	 (progn
	   (let ((foox (- c-array-columns 1))
		 (bary (- c-array-lines 1)))
	     (flet ((blacken (x y)
		      (color 0 0 0 0 x y)))
	       (blacken foox bary)
	       (dotimes (i bary)
		 (blacken foox i))
	       (dotimes (i foox)
		 (blacken i bary)))))
	 
	 (let ((len ncurses-clone::*lines*))
	   (dotimes (i len)
	     (let ((array (aref (ncurses-clone::win-data ncurses-clone::*std-scr*) (- len i 1)))
		   (index 0))
	       (block out
		 (do ()
		     ((>= index ncurses-clone::*columns*))
		   (let* ((glyph (aref array index)))

		     ;;This occurs if the widechar is overwritten, but the placeholders still remain.
		     ;;otherwise it would be skipped.
		     (when (eq glyph ncurses-clone::*widechar-placeholder*)
		       (setf glyph ncurses-clone::*clear-glyph*))
		     
		     (let* ((glyph-character (ncurses-clone::glyph-value glyph))
			    (width (ncurses-clone::char-width-at glyph-character index)))

		       ;;FIXME::for some reason, when resizing really quickly,
		       ;;this can get screwed up, so bail out
		       (when (<= (+ 1 ncurses-clone::*columns*)
				 (+ width index))
			 (return-from out))
		       
		       (when (typep glyph 'ncurses-clone::extra-big-glyph)
			 (let ((x index)
			       (y i))
			   (setf (ncurses-clone::extra-big-glyph-x glyph) x
				 (ncurses-clone::extra-big-glyph-y glyph) y))
			 (funcall big-glyph-fun glyph)
			 ;;(print (sucle-attribute-overlay glyph))
			 )
		       (let* ((attributes (ncurses-clone::glyph-attributes glyph))
			      #+nil
			      (pair (ncurses-clone::ncurses-color-pair
				     (ldb (byte 8 0) attributes))))
			 (let ((realfg
				;;FIXME::fragile bit layout
				(if (zerop (ldb (byte 1 8) attributes))
				    ncurses-clone::*fg-default*
				    (ldb (byte 8 0) attributes))
				#+nil
				(let ((fg (car pair)))
				  (if (or
				       (not pair)
				       (= -1 fg))
				      ncurses-clone::*fg-default* ;;FIXME :cache?
				      fg)))
			       (realbg
				(if (zerop (ldb (byte 1 (+ 1 8 8)) attributes))
				    ncurses-clone::*bg-default*
				    (ldb (byte 8 (+ 1 8)) attributes))
				 #+nil
				(let ((bg (cdr pair)))
				  (if (or
				       (not pair)
				       (= -1 bg))
				      ncurses-clone::*bg-default* ;;FIXME :cache?
				      bg))))
			   (when (logtest ncurses-clone::A_reverse attributes)
			     (rotatef realfg realbg))
			   (dotimes (offset width)
			     (block abort-writing
			       (color 
				;;FIXME::this is temporary, to chop off extra unicode bits
				(let ((code (char-code glyph-character)))
				  (if (ncurses-clone::n-char-fits-in-glyph code)
				      code
				      ;;Draw nice-looking placeholders for unimplemented characters.
				      ;;1 wide -> #
				      ;;n wide -> {@...}
				      (case width
					(1 (load-time-value (char-code #\#)))
					(otherwise
					 (cond 
					   ((= offset 0)
					    (load-time-value (char-code #\{)))
					   (t
					    (let ((old-thing (aref array (+ index offset))))
					      (if (eq ncurses-clone::*widechar-placeholder*
						      old-thing)
						  (if (= offset (- width 1))
						      (+ (load-time-value (char-code #\})))
						      (load-time-value (char-code #\@)))
						  ;;if its not a widechar-placeholder, the placeholder
						  ;;was overwritten, so don't draw anything.
						  (return-from abort-writing)))))))))
				realfg
				realbg
				(text-sub::char-attribute
				 (logtest ncurses-clone::A_bold attributes)
				 (logtest ncurses-clone::A_Underline attributes)
				 t)
				(+ offset index)
				i)))))
		       (incf index width)))))))))
       ;;;;write the data out to the texture
       (let ((texture (text-sub::get-text-texture)))
	 (gl:bind-texture :texture-2d texture)
	 (gl:tex-sub-image-2d :texture-2d 0 0 0
			      c-array-columns
			      c-array-lines
			      :rgba :unsigned-byte arr)))))
  (text-sub::with-text-shader (uniform)
    (gl:uniform-matrix-4fv
     (uniform :pmv)
     (load-time-value (nsb-cga:identity-matrix))
     nil)   
    (glhelp::bind-default-framebuffer)
    (glhelp:set-render-area 0 0
			    (deflazy::getfnc 'application::w)
			    (deflazy::getfnc 'application::h))
    #+nil
    (progn
      (gl:enable :blend)
      (gl:blend-func :src-alpha :one-minus-src-alpha))

    (text-sub::draw-fullscreen-quad)
    ))

