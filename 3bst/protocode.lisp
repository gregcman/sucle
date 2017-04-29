(in-package :3bst)

(defmacro progno (&rest args) (declare (ignore args)))

(progno
 (defun wcwidth (c)
   (let ((cc (char-code c)))
     (when (zerop cc)
       (return-from wcwidth 0))
     (when (control-p cc)
       (return-from wcwidth -1))
     (when (= cc #xad) ;; soft-hyphen
       (return-from wcwidth 1))
     (when (member (sb-unicode:general-category c) '(:Mn :Me :Cf))
       (return-from wcwidth 0))
     (when (= cc #x200b) ;; zero width space
       (return-from wcwidth 0))
     (when (<= #x1160 cc #x11ff) ;; hangul jamo medial vowels, final consonents
       (return-from wcwidth 0))
     (when (member (sb-unicode:east-asian-width c) '(:w :f))
       (return-from wcwidth 2))
     1)))

(progno
 (defun map-screen (screen function)
   (loop for line across screen
      for y from 0
      do (loop for glyph across line
	    for x from 0
	    do (funcall function glyph y x)))))

(progno
 (defun tresize (columns rows &key (term *term*))
   (let ((minrow (min rows (rows term)))
	 (mincol (min columns (columns term)))
	 (slide (1+ (- (y (cursor term)) rows))))
     (when (or (< columns 1) (< rows 1))
       (warn "tresize: error resizing to ~ax~a" rows columns)
       (return-from tresize nil))
     ;; free uneeded rows
     (when (plusp slide)
       ;; slide screen to keep cursor where we expect it - tscrollup
       ;; would work here, but we can optimize to memmove because we're
       ;; freeing the earlier lines
       (replace (screen term) (screen term) :start1 slide :start2 0)
       (replace (alternate-screen term) (alternate-screen term)
		:start1 slide :start2 0))
     ;; possibly should make these adjustable arrays?
     (setf (slot-value term 'screen) (adjust-array (screen term) rows))
     (setf (slot-value term 'alternate-screen)
	   (adjust-array (alternate-screen term) rows))
     ;; don't need to copy DIRTY array since we flag it all later
     (setf (slot-value term 'dirty) (adjust-array (dirty term) rows))
     (setf (slot-value term 'tabs ) (adjust-array (tabs term) columns
						  :initial-element 0))

     ;; resize each row to new width, zero-pad if needed
     (loop for i below minrow
	do (flet ((r (s)
		    (setf (aref s i)
			  (adjust-array (aref s i) columns))
		    (loop for j from mincol below columns
		       do (setf (aref (aref s i) j)
				(make-instance 'glyph)))))
	     (r (screen term))
	     (r (alternate-screen term))))
     ;; allocate any new rows
     (loop for i from minrow below rows
	do (flet ((n (s)
		    (setf (aref s i)
			  (make-array rows
				      :element-type '(vector glyph *)
				      :initial-contents
				      (coerce
				       (loop repeat columns
					  collect (make-instance 'glyph))
				       '(vector glyph))))))
	     (n (screen term))
	     (n (alternate-screen term))))
     (when (> columns (columns term))
       (loop with last-tab = (position 1 (tabs term) :from-end t)
	  for i from (+ *tab-spaces* (or last-tab 0))
	  below columns by *tab-spaces*
	  do (setf (aref (tabs term) i) 1)))
     ;; update terminal size
     (setf (slot-value term 'columns) columns
	   (slot-value term 'rows) rows)
     ;; reset scrolling region
     (tsetscroll 0 (1- rows) :term term)
     ;; make use of the LIMIT in tmoveto
     (tmoveto (x (cursor term)) (y (cursor term)))
     ;; Clearing both screens (it makes dirty all lines)
     (let ((c (cursor term)))
       (loop repeat 2
	  do (when (and (< mincol columns) (< 0 minrow))
	       (tclearregion mincol 0 (1- columns) (1- minrow) :term term))
	    (when (and (< 0 columns) (< minrow rows))
	      (tclearregion 0 minrow (1- columns) (1- rows) :term term))
	    (tswapscreen :term term)
	    (tcursor :cursor-load :term term))
       (setf (cursor term) c)))))

(progno
;;; these are used for keybindings, possibly should return a closure instead
;;; if still being used that way?
 (defun toggleprinter (arg &key (term *term*))
   (declare (ignore arg))
   (setf (mode term) (logxor (mode term) +mode-print+)))

 (defun printscreem (arg &key (term *term*))
   (declare (ignore arg))
   (tdump :term term))

 (defun printsel (arg &key (term *term*))
   (declare (ignore arg term))
   #++(tdumpsel :term term))

 #++
 (defun tdumpsel (&key (term *term*))
   (tprinter (getsel :term term) :term term)))

(progno
 (defun strreset (str)
   ;; fixme: move this to reinitialize-instance?
   (setf (fill-pointer (buffer str)) 0
	 (priv str) nil
	 (fill-pointer (arguments str)) 0
	 (str-type str) (code-char 0))))

(progno
 (defun csireset (csi)
   (setf (fill-pointer (buffer csi)) 0
	 (priv csi) nil
	 (fill-pointer (arguments csi)) 0
	 (mode csi) 0)))

(progno
 #++
 (defun tty-resize ()
   ;; todo: implement some way of passing this to caller in case it has a TTY
   ;; and wants to do ioctl(..., TIOCSWINSZ, ...) or similar
   )

 (defun tattrset (attr &key (term *term*))
   (loop for line across (screen term)
      thereis (loop for glyph across line
		 thereis (logtest attr (mode glyph))))))

(progno
 (defun tty-send (characters &key (term *term*))
   (tty-write characters :term term)
   (when (attribute-set-p +mode-echo+ :term term)
     (techo characters :term term))))

(progno
 #++(let ((a (make-array 6 :fill-pointer 0)))
      (ensure-aref a 0 2)
      (ensure-aref a 1 3)
      a))


(progno
 (defun attribute/= (a b)
   ;; assuming mode is an int with flag bits for now
   (or (/= (mode a) (mode b))
       (/= (fg a) (fg b))
       (/= (bg a) (bg b)))))

(progno
;;;; todo: mouse/selection stuff

;;;; todo: utils for running a shell with env etc? handle child closed, etc
 ;; (probably mostly let uiop deal with that)

 (defun handle-input-raw (octets &key (term *term*))
   "process OCTETS as (possibly incomplete) UTF8 encoded input from
child process"
   (declare (ignore octets term))
   (error "not done yet, use character input...")))

(progno
 (defun tsetdirtattr (attr &key (term *term*))
   (loop for line across (screen term)
      for i from 0
      do (loop for glyph across line
	    when (logtest attr (mode glyph))
	    do (tsetdirt i i :term term)
	    and return nil))))
(progno
 (defun tsetdirt (top bottom &key (term *term*))
   (loop for i from (limit top 0 (1- (rows term)))
      below (limit bottom 0 (1- (rows term)))
      do (setf (aref (dirty term) i) 1))))

(progno
 (defmethod glyph-attributes ((g glyph))
   (loop for mask in (etouq (quote (list +ATTR-NULL+ +ATTR-BOLD+ +ATTR-FAINT+ +ATTR-ITALIC+
					 +ATTR-UNDERLINE+ +ATTR-BLINK+ +ATTR-REVERSE+
					 +ATTR-INVISIBLE+ +ATTR-STRUCK+ +ATTR-WRAP+
					 +ATTR-WIDE+ +ATTR-WDUMMY+)))
      for key in '(:NULL :BOLD :FAINT :ITALIC :UNDERLINE :BLINK
		   :REVERSE :INVISIBLE :STRUCK :WRAP :WIDE :WDUMMY)
      when (logtest mask (mode g))
      collect key)))
