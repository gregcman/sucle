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
(deflazy:deflazy virtual-window ((w application:w) (h application:h))
  (setf *resized-p* t)
  (let ((new-columns (floor w *glyph-width*))
	(new-lines (floor h *glyph-height*)))
    ;;(ncurses-clone:reset-standard-screen)
    (ncurses-clone:with-virtual-window-lock
      (ncurses-clone:ncurses-wresize
       ncurses-clone:*std-scr*
       new-lines
       new-columns)
      #+nil
      (setf ncurses-clone:*virtual-window*
	    (ncurses-clone:make-virtual-window)))))

(defun window-size (&optional (win ncurses-clone::*std-scr*))
  (values
   (floor (* (ncurses-clone:win-cols win)
	     *glyph-width*))
   (floor (* (ncurses-clone:win-lines win)
	     *glyph-height*))))
(defun window-pos (&optional (win ncurses-clone::*std-scr*))
  (values
   (floor (* (ncurses-clone:win-x win)
	     *glyph-width*))
   (floor (* (ncurses-clone:win-y win)
	     *glyph-height*))))

(defun update-resize ()
  (setf *resized-p* t))

(defun init ()
  (set-glyph-dimensions 8 16)
  (setf text-sub:*text-data-what-type* :texture-2d)
  (update-resize)
  (text-sub:change-color-lookup
   ;;'text-sub:color-fun
   'lem.term:color-fun
   #+nil
   (lambda (n)
     (values-list
      (print (mapcar (lambda (x) (/ (utility:floatify x) 255.0))
		     (nbutlast (aref lem.term:*colors* n))))))
   ))

(defun set-glyph-dimensions (w h)
  ;;Set the pixel dimensions of a 1 wide by 1 high character
  (setf *glyph-width* w)
  (setf text-sub:*block-width* w)
  (setf *glyph-height* h)
  (setf text-sub:*block-height* h)
  (text-sub::block-dimension-change)
  (progn
    ;;[FIXME]Better way to organize this? as of now manually determining that
    ;;these two depend on the *block-height* and *block-width* variables
    (deflazy:refresh
     'text-sub:indirection
     ;;'text-sub:render-normal-text-indirection
     )
    (deflazy:refresh 'virtual-window)))

(defparameter *redraw-display-p* nil)
(defun redraw-display ()
  (setf *redraw-display-p* t))

(defun win->port (win)
  (multiple-value-bind (x y) (window-pos win)
    (multiple-value-bind (w h) (window-size win)
      (text-sub::port x y w h))))
(defun render
    (&key
       (win ncurses-clone:*std-scr*)
       (port (win->port win) port-supplied-p)
       (ondraw (lambda ()))
       (big-glyph-fun 'identity)
       (update-data nil))
  ;;Make sure the virtual window has the correct specs
  (when (or update-data
	    ;;the port has not been written to,
	    ;;so definitely write to it.
	    (not (text-sub::port-sync port)))
    (funcall ondraw)
    ;;Copy the virtual screen to a c-array,
    ;;then send the c-array to an opengl texture
    ;;While reading the virtual screen, save any special
    ;;glyphs.
    (ncurses->gl (text-sub::port-data port)
		 :win win
		 :big-glyph-fun big-glyph-fun)
    (setf (text-sub::port-sync port) t))
  (text-sub::draw-port port)
  ;;Destroy the temporary port created for this function,
  ;;not provided from elsewhere.
  (unless port-supplied-p
    (text-sub::destroy-port port)))

(defun ncurses->gl (texture &key 
			      (win ncurses-clone:*std-scr*)
			      (big-glyph-fun (constantly nil)))
  (let* ((c-array-lines
	  (min text-sub:*text-data-height* ;do not send data larger than text data
	       (+ 1 (ncurses-clone:win-lines win))))              ;width or height
	 (c-array-columns
	  (min text-sub:*text-data-width*
	       (+ 1 (ncurses-clone:win-cols win))))
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
	
	(let ((len (ncurses-clone:win-lines win)))
	  (dotimes (i len)
	    (let ((array (aref (ncurses-clone:win-data win)
			       (- len i 1)))
		  (index 0))
	      (block out
		(do ()
		    ((>= index (ncurses-clone:win-cols win)))
		  (let* ((glyph (aref array index)))

		    ;;This occurs if the widechar is overwritten, but the placeholders still remain.
		    ;;otherwise it would be skipped.
		    (when (eq glyph ncurses-clone:*widechar-placeholder*)
		      (setf glyph ncurses-clone:*clear-glyph*))
		    
		    (let* ((glyph-character (ncurses-clone:glyph-value glyph))
			   (width (ncurses-clone:char-width-at glyph-character index)))

		      ;;[FIXME]for some reason, when resizing really quickly,
		      ;;this can get screwed up, so bail out
		      (when (<= (+ 1 (ncurses-clone:win-cols win))
				(+ width index))
			(return-from out))
		      
		      (when (typep glyph 'ncurses-clone:extra-big-glyph)
			(let ((x index)
			      (y i))
			  (setf (ncurses-clone:extra-big-glyph-x glyph) x
				(ncurses-clone:extra-big-glyph-y glyph) y))
			(funcall big-glyph-fun glyph)
			;;(print (sucle-attribute-overlay glyph))
			)
		      (let* ((attributes (ncurses-clone:glyph-attributes glyph))
			     #+nil
			     (pair (ncurses-clone:ncurses-color-pair
				    (ldb (byte 8 0) attributes))))
			(let ((realfg
			       ;;[FIXME]fragile bit layout
			       (if (zerop (ldb (byte 1 8) attributes))
				   ncurses-clone:*fg-default*
				   (ldb (byte 8 0) attributes))
				#+nil
				(let ((fg (car pair)))
				  (if (or
				       (not pair)
				       (= -1 fg))
				      ncurses-clone:*fg-default* ;;[FIXME] :cache?
				      fg)))
			      (realbg
			       (if (zerop (ldb (byte 1 (+ 1 8 8)) attributes))
				   ncurses-clone:*bg-default*
				   (ldb (byte 8 (+ 1 8)) attributes))
				#+nil
				(let ((bg (cdr pair)))
				  (if (or
				       (not pair)
				       (= -1 bg))
				      ncurses-clone:*bg-default* ;;[FIXME] :cache?
				      bg))))
			  (when (logtest ncurses-clone:A_reverse attributes)
			    (rotatef realfg realbg))
			  (dotimes (offset width)
			    (block abort-writing
			      (color 
			       ;;[FIXME]this is temporary, to chop off extra unicode bits
			       (let ((code (char-code glyph-character)))
				 (if (ncurses-clone:n-char-fits-in-glyph code)
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
					     (if (eq ncurses-clone:*widechar-placeholder*
						     old-thing)
						 (if (= offset (- width 1))
						     (+ (load-time-value (char-code #\})))
						     (load-time-value (char-code #\@)))
						 ;;if its not a widechar-placeholder, the placeholder
						 ;;was overwritten, so don't draw anything.
						 (return-from abort-writing)))))))))
			       realfg
			       realbg
			       (text-sub:char-attribute
				(logtest ncurses-clone:A_bold attributes)
				(logtest ncurses-clone:A_Underline attributes)
				t)
			       (+ offset index)
			       i)))))
		      (incf index width)))))))))
       ;;;;write the data out to the texture
      (text-sub:submit-text-data
       arr c-array-columns c-array-lines
       texture
       ;;text-data
       ))))
