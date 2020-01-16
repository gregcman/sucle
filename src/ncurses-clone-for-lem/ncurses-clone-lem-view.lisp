(defpackage #:ncurses-clone-lem-view
  (:use :cl)
  (:export
   #:clear
   #:clear-eob
   #:clear-eol
   #:delete-view
   #:display-background-mode
   #:display-height
   #:display-width
   #:make-view
   #:print-into-view
   #:scroll
   #:set-view-pos
   #:set-view-size
   #:update-background
   #:update-display
   #:update-foreground
   #:print-modeline
   #:redraw-view-after
   
   ;;FIXME::HACK
   #:ncurses-view-parent-window))
(in-package #:ncurses-clone-lem-view)
;;;;Window mangement code below?

(struct-to-clos:struct->class
 (defstruct ncurses-view
   scrwin
   modeline-scrwin
   x
   y
   width
   height
   lock
   ;;FIXME::this variable is for determining what lem window
   ;;it is in. But the internals of lem are unknown?
   parent-window
   ))
(set-pprint-dispatch
 'ncurses-view
 (lambda (stream obj)
   (print (ncurses-view-scrwin obj) stream)
   (print (ncurses-view-modeline-scrwin obj) stream)))

(defun display-background-mode ()
  (lem.term:background-mode))
(defun update-foreground (color-name)
  (lem.term:term-set-foreground color-name))
(defun update-background (color-name)
  (lem.term:term-set-background color-name))

(defun display-width ()
  (max 5
       ncurses-clone::*columns*
       ;;charms/ll:*cols*
       ))
(defun display-height ()
  (max 3
       ncurses-clone::*lines*
       ;;charms/ll:*lines*
       ))
(defun make-view (x y width height use-modeline)
  (flet ((newwin (nlines ncols begin-y begin-x main-screen)
           (declare (ignore main-screen))
           (let ((win
		  (;;charms/ll:newwin
		   ncurses-clone::ncurses-newwin
		   nlines ncols begin-y begin-x)))
	     #+nil;;What is this for? keypad?
	     (when use-modeline (;;charms/ll:keypad
				 ncurses-clone::ncurses-keypad
				 win 1))
             ;; (when main-screen
             ;;   (charms/ll:idlok win 1)
             ;;   (charms/ll:scrollok win 1))
             win)))
    (make-ncurses-view
     :scrwin (newwin height width y x nil)
     :modeline-scrwin (when use-modeline (newwin 1 width (+ y height) x nil))
     :x x
     :y y
     :width width
     :height height
     :lock (bt:make-recursive-lock "window-lock"))))
(defmacro with-view-lock (view &body body)
  (utility::with-gensyms (lock)
    `(let ((,lock (ncurses-view-lock ,view)))
       (bt:with-recursive-lock-held (,lock)
	 ,@body))))
(defun delete-view (view)
  (with-view-lock view
    (;;charms/ll:delwin
     ncurses-clone::ncurses-delwin
     (ncurses-view-scrwin view))
    (when (ncurses-view-modeline-scrwin view)
      (;;charms/ll:delwin
       ncurses-clone::ncurses-delwin
       (ncurses-view-modeline-scrwin view)))))
(defun clear (view)
  ;;;https://linux.die.net/man/3/clearok
  ;;#+nil;;FIXME::what is this for?
  (with-view-lock view
    (ncurses-clone::clear-win (ncurses-view-scrwin view))
    #+nil
    (progn
      (;;charms/ll:clearok
       ncurses-clone::ncurses-clearok
       (ncurses-view-scrwin view) 1)
      (when (ncurses-view-modeline-scrwin view)
	(;;charms/ll:clearok
	 ncurses-clone::ncurses-clearok
	 (ncurses-view-modeline-scrwin view) 1)))))

(defun set-view-size (view width height &optional (minibuffer-window-height 1))
  (with-view-lock view
    (setf (ncurses-view-width view) width)
    (setf (ncurses-view-height view) height)
    (;;charms/ll:wresize
     ncurses-clone::ncurses-wresize
     (ncurses-view-scrwin view) height width)
    (when (ncurses-view-modeline-scrwin view)
      (;;charms/ll:mvwin
       ncurses-clone::ncurses-mvwin
       (ncurses-view-modeline-scrwin view)
       (+ (ncurses-view-y view) height)
       (ncurses-view-x view))
      (;;charms/ll:wresize
       ncurses-clone::ncurses-wresize
       (ncurses-view-modeline-scrwin view)
       minibuffer-window-height
       width))))
(defun set-view-pos (view x y)
  (with-view-lock view
    (setf (ncurses-view-x view) x)
    (setf (ncurses-view-y view) y)
    (;;charms/ll:mvwin
     ncurses-clone::ncurses-mvwin
     (ncurses-view-scrwin view) y x)
    (when (ncurses-view-modeline-scrwin view)
      (;;charms/ll:mvwin
       ncurses-clone::ncurses-mvwin
       (ncurses-view-modeline-scrwin view)
       (+ y (ncurses-view-height view))
       x))))
(defun scroll (view n)
  (with-view-lock view
    (;;charms/ll:wscrl
     ncurses-clone::ncurses-wscrl
     (ncurses-view-scrwin view)
     n)))
(defun clear-eol (view x y)
  (with-view-lock view
    (;;charms/ll:wmove
     ncurses-clone::ncurses-wmove
     (ncurses-view-scrwin view) y x)
    (;;charms/ll:wclrtoeol
     ncurses-clone::ncurses-wclrtoeol
     (ncurses-view-scrwin view))))
(defun clear-eob (view x y)
  (with-view-lock view
    (;;charms/ll:wmove
     ncurses-clone::ncurses-wmove
     (ncurses-view-scrwin view) y x)
    (;;charms/ll:wclrtobot
     ncurses-clone::ncurses-wclrtobot
     (ncurses-view-scrwin view))))
(defun update-display ()
    ;;FIXME::why does this do nothing?
  #+nil
  (flet ((render-window (window)
	   (let ((view (lem:window-view window)))
	     (with-view-lock view
	       (let ((scrwin (ncurses-view-scrwin view)))
		 (if (lem::covered-with-floating-window-p
		      window
		      lem::*cursor-x* lem::*cursor-y*)
		     (;;charms/ll:curs-set
		      ncurses-clone::ncurses-curs-set
		      0)
		     (progn
		       (;;charms/ll:curs-set
			ncurses-clone::ncurses-curs-set
			1)
		       (;;charms/ll:wmove
			ncurses-clone::ncurses-wmove
			scrwin lem::*cursor-y* lem::*cursor-x*)))
		 ;;FIXME
		 (;;charms/ll:wnoutrefresh
		  ncurses-clone::ncurses-wnoutrefresh
		  scrwin)
		 ;;;FIXME::does not mirror the lem/ncurses code
		 (ncurses-clone::ncurses-curs-set 0)
		 )))))
    #+nil
    (ncurses-clone::ncurses-wnoutrefresh
     ncurses-clone::*std-scr*)
    #+nil
    (map nil #'render-window (lem:window-list))
    ;;#+nil
    ;;(render-window (lem:current-window))
    )
  (;;charms/ll:doupdate
   ncurses-clone::ncurses-doupdate))

(defun print-into-view (view x y string)
  (with-view-lock view
    #+nil
    (;;charms/ll:wattron
     ncurses-clone::ncurses-wattron
     (ncurses-view-scrwin view)
     attr)
    #+nil
    (when (typep attribute 'sucle-attribute)
      (let ((overlay (sucle-attribute-overlay attribute)))
	(print (list
		(lem:points-to-string
		 (lem:overlay-start overlay)
		 (lem:overlay-end overlay))))))
    
    ;;(charms/ll:scrollok (ncurses-view-scrwin view) 0)
    (;;charms/ll:mvwaddstr
     ncurses-clone::ncurses-mvwaddstr
     (ncurses-view-scrwin view) y x string)
    ;;(charms/ll:scrollok (ncurses-view-scrwin view) 1)

    #+nil
    (;;charms/ll:wattroff
     ncurses-clone::ncurses-wattroff
     (ncurses-view-scrwin view)
     attr)))

(defun print-modeline (view x y string)
  (with-view-lock view
      #+nil
      (;;charms/ll:wattron
       ncurses-clone::ncurses-wattron
       (ncurses-view-modeline-scrwin view) attr)
      (;;charms/ll:mvwaddstr
       ncurses-clone::ncurses-mvwaddstr
       (ncurses-view-modeline-scrwin view) y x string)
      #+nil
      (;;charms/ll:wattroff
       ncurses-clone::ncurses-wattroff
       (ncurses-view-modeline-scrwin view) attr)))

(defun redraw-view-after (view)
  (with-view-lock view
    ;;#+nil ;;;FIXME 
    #+nil
    (;;charms/ll:attron
     ncurses-clone::ncurses-attron
     attr)
    ;;#+nil ;;FIXME:: disabling
    ;;(print view)
    ;;(print ncurses-clone::*std-scr*)
    (when (and (ncurses-view-modeline-scrwin view)
	       (< 0 (ncurses-view-x view)))
      (;;charms/ll:move
       ncurses-clone::ncurses-move
       (ncurses-view-y view)
       (1- (ncurses-view-x view)))
      (;;charms/ll:vline
       ncurses-clone::ncurses-vline
       #\space		    
       (1+ (ncurses-view-height view))))
    ;;(print view)
    ;;(print ncurses-clone::*std-scr*)
    #+nil
    (;;charms/ll:attroff
     ncurses-clone::ncurses-attroff
     attr)
    #+nil;;FIXME
    (;;charms/ll:wnoutrefresh
     ncurses-clone::ncurses-wnoutrefresh
     ;;charms/ll:*stdscr*
     ncurses-clone::*std-scr*)
    (;;charms/ll:wnoutrefresh
     ncurses-clone::ncurses-wnoutrefresh
     (ncurses-view-scrwin view))
    (when (ncurses-view-modeline-scrwin view)
      (;;charms/ll:wnoutrefresh
       ncurses-clone::ncurses-wnoutrefresh
       (ncurses-view-modeline-scrwin view))
      ;;   (ncurses-clone::print-virtual-window ncurses-clone::*virtual-window* *no*)
      )))

