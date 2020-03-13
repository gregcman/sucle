(in-package :lem-sucle)

(defparameter *saved-session* nil)
(defun input-loop (&optional (editor-thread lem-sucle::*editor-thread*))
  (setf application::*main-subthread-p* nil)
  (destructuring-bind (width height) (window-size)
    (application::main
     (lambda ()
       (init)
       (block out
	 (handler-case
	     (let ((out-token (list "good" "bye")))
	       (catch out-token
		 (loop
		    (livesupport:update-repl-link)
		    (livesupport:continuable
		      (per-frame editor-thread out-token)))))
	   (exit-editor (c) (return-from out c)))))
     :width width
     :height height
     :title "lem is an editor for Common Lisp"
     :resizable t)))


(defparameter *last-scroll* 0)
(defparameter *scroll-difference* 0)
(defparameter *scroll-speed* 5)
(defparameter *run-sucle* nil)
(defun per-frame (editor-thread out-token)
  (declare (ignorable editor-thread))
  (application::on-session-change *saved-session*
    (window::set-vsync t))
  (application:poll-app)
  (when *run-sucle*
    (unwind-protect
	 (application::with-quit-token ()
	   (funcall sucle::*sucle-app-function*))
      (setf *run-sucle* nil)
      (window:get-mouse-out)))
  (let ((newscroll (floor window::*scroll-y*)))
    (setf *scroll-difference* (- newscroll *last-scroll*))
    (setf *last-scroll* newscroll))
  
  (handler-case
      (progn
	(when window::*status*
	  ;;(bt:thread-alive-p editor-thread)
	  (throw out-token nil))
	(when *resized-p*
	  (setf *resized-p* nil)
	  (lem:send-event :resize))
	(scroll-event)
	(input-events)
	;;#+nil
	(calculate-cursor-coordinate)
	(handle-dropped-files)
	(left-click-event)
	#+nil
	(let ((event))
	  
	  (if (eq event :abort)
	      (send-abort-event editor-thread nil)
	      ;;(send-event event)
	      )))
    #+nil
    #+sbcl
    (sb-sys:interactive-interrupt (c)
      (declare (ignore c))
      (lem:send-abort-event editor-thread t)))

  ;;Flush the changes made to the ncurses-clone display
  (when *redraw-display-p*
    (setf *redraw-display-p* nil)
    (lem:redraw-display))

  ;;Rendering. Comes after input handling because things could have changed
  (render
   :ondraw
   (lambda ()
     ;;Set the title of the window to the name of the current buffer
     (window:set-caption (lem-base:buffer-name (lem:current-buffer)))
     (setf *some-data* nil)
     #+nil
     (clrhash *some-data*))
   :big-glyph-fun
   'save-special-glyphs
   ))
(defparameter *some-data*
  nil
  ;;(make-hash-table :test 'eq)
  )


(defun save-special-glyphs (glyph)
  (let ((data (ncurses-clone::extra-big-glyph-attribute-data glyph)))   
    (destructuring-bind (attribute view) data
      (let ((window (ncurses-clone-lem-view:ncurses-view-parent-window
		     view))
	    (overlay (sucle-attribute-overlay attribute)))
	(push glyph (getf (getf *some-data* window) overlay))
	#+nil
	(multiple-value-bind (overlay-hash existsp) 
	    (unless existsp
	      (let ((value (make-hash-table :test 'eq)))
		(setf (gethash window *some-data*)
		      value)
		(setf overlay-hash value))))
	#+nil
	(when (zerop (random 200))
	  (print (list
		  window
		  overlay
		  attribute
		  x y)))))))

(defparameter *output* *standard-output*)
(defparameter *mouse-last-position* nil)

(defparameter *mouse-mode* nil)

(defun reset-mouse-mode ()
  (setf *mouse-mode* nil))

(defun same-buffer-points (a b)
  (eq 
   (lem:point-buffer a)
   (lem:point-buffer b)))

(defparameter *grid-mouse-x* nil)
(defparameter *grid-mouse-y* nil)
(defun calculate-cursor-coordinate ()
  ;;For some reason, the coordinate of the mouse is off by 1,1?
  (setf *grid-mouse-x*
	(floor (- window::*mouse-x* 1)
	       *glyph-width*))
  (setf *grid-mouse-y*
	(floor (- 
		window::*mouse-y*
		;;There's a little space between the edge of the window and the area lem uses,
		;;since the window coordinates are not necessary multiples of the glyph size
		;;This causes the y position of the cursor to become messed up, if not accounted for
		(mod window::*height*
		     *glyph-height*)
		1)
	       *glyph-height*)))

(struct-to-clos:struct->class
 (defstruct window-intersection
   window
   intersection-type))

(defparameter *null-window-intersection* (make-window-intersection))
(defparameter *window-last-clicked-at* *null-window-intersection*)
(defun clear-window-intersection ()
  (setf *window-last-clicked-at* *null-window-intersection*))
(defun left-click-event ()
  (let ((just-pressed (window:button :mouse :pressed :left window::*control-state*))
	(just-released (window:button :mouse :released :left window::*control-state*))
	(pressing (window:button :mouse :down :left window::*control-state*)))
    (let* ((coord (list *grid-mouse-x* *grid-mouse-y*))
	   (coord-change
	    (not (equal coord
			*mouse-last-position*))))
      ;;FIXME::better logic? comments?
      ;;TODO::handle selections across multiple windows?
      (when just-pressed
	;;(print "cancelling")
	(multiple-value-bind (window intersection-type)
	    (detect-mouse-window-intersection)
	  ;;reset the mouse mode before, not after because
	  ;;the intersection type can decide the mode
	  (case *mouse-mode*
	    ((:drag-resize-window :marking) (reset-mouse-mode)))
	  (if (null intersection-type)
	      (clear-window-intersection)
	      (let ((intersection-data
		     (make-window-intersection :window window
					       :intersection-type intersection-type)))
		(setf *window-last-clicked-at* intersection-data)
		(setf (lem:current-window) window)
		(case intersection-type
		  (:center
		   (move-window-cursor-to-mouse window))
		  ((:vertical :horizontal)
		   (setf *mouse-mode* :drag-resize-window)))
		(redraw-display))))
	(handle-multi-click coord)
	(handle-multi-click-selection))
      (let ((window (window-intersection-window *window-last-clicked-at*)))
	(when (and pressing
		   window)
	  (ecase (window-intersection-intersection-type *window-last-clicked-at*)
	    (:center
	     (let ((y (- *grid-mouse-y*
			 (lem:window-y window) ;;is move-window-cursor-to-mouse redundant?
			 )))
	       (let ((scroll-down-offset
		      (cond
			((> 0 y)
			 y)
			((>= y (rectified-window-height window))
			 (+ 1 (- y (rectified-window-height window))))
			(t 0))))
		 (when (or
			;;when scrolled by mouse
			(not (zerop *scroll-difference*))
			;;when it is scrolled
			(not (zerop scroll-down-offset))
			;;when its dragging
			coord-change)
		   (lem:scroll-down scroll-down-offset)
		   (move-window-cursor-to-mouse window
						*grid-mouse-x*
						(- *grid-mouse-y* scroll-down-offset))	      
		   (redraw-display)))))
	    (:horizontal
	     (handler-case
		 (let* ((p1 (window-horizontal-edge-coord window))
			(p2 *grid-mouse-x*)
			(difference (- p2 p1)))
		   (unless (zerop difference)
		     (reorder-window-tree)
		     (if (plusp difference)
			 (dotimes (i difference)			   
			   (lem:grow-window-horizontally 1))
			 (dotimes (i (- difference))
			   (lem:shrink-window-horizontally 1)))
		     (redraw-display)))
	       (lem:editor-error (c)
		 (declare (ignorable c)))))
	    (:vertical
	     (handler-case
		 (let* ((p1 (window-vertical-edge-coord window))
			(p2 *grid-mouse-y*)
			(difference (- p2 p1)))
		   (unless (zerop difference)
		     (reorder-window-tree)
		     (if (plusp difference)
			 (dotimes (i difference)
			   (lem:grow-window 1))
			 (dotimes (i (- difference))			   
			   (lem:shrink-window 1)))
		     (redraw-display)))
	       (lem:editor-error (c)
		 (declare (ignorable c))))))))
      (when just-released
	(case *mouse-mode*
	  ((:marking :drag-resize-window) (reset-mouse-mode))))
      (handle-drag-select-region pressing just-pressed)
      ;;save the mouse position for next tick
      (setf *mouse-last-position* coord))))

(defun reorder-window-tree (&optional (window-tree (lem::window-tree)))
  (labels ((f (tree)
             (cond ((lem::window-tree-leaf-p tree)
                    ;;(funcall fn tree)
		    )
                   (t
		    (one-swap-window tree)
                    (f (lem::window-node-car tree))
                    (f (lem::window-node-cdr tree))))))
    (f window-tree)
    (values)))

(defun one-swap-window (instance)
  (let ((car (lem::window-node-car instance)))
    (when (and (lem::window-node-p car)
	       (eq (lem::window-node-split-type car)
		   (lem::window-node-split-type instance)))
      ;;(print "reordering windows")
      (let ((a (lem::window-node-car car))
	    (b (lem::window-node-cdr car))
	    (c (lem::window-node-cdr instance)))
	(setf (lem::window-node-car instance) a)
	(setf (lem::window-node-cdr instance) car)
	(setf (lem::window-node-car car) b)
	(setf (lem::window-node-cdr car) c)))))

(defun safe-point= (point-a point-b)
  (and
   ;;make sure they are in the same buffer
   (same-buffer-points point-a point-b)
   ;;then check whether they are equal
   (lem:point= point-a point-b)))

(defparameter *point-at-last* nil)
(defun handle-drag-select-region (pressing just-pressed)
  (let* ((last-point *point-at-last*)
	 (point (lem:current-point))
	 (point-coord-change
	  (not (and
		;;it exists
		*point-at-last* 
		;;its the same position as point
		(safe-point= *point-at-last* point)))))
    (when point-coord-change
      (setf *point-at-last*
	    (lem:copy-point point
			    :temporary)))
    (when (and
	   pressing
	   (null *mouse-mode*)
	    ;;if it was just pressed, there's going to be a point-coord jump
	   (not just-pressed)
	   ;;selecting a single char should not start marking
	   point-coord-change)
      ;;beginning to mark
      (let ((current-point (lem:current-point)))
	(if (and
	     last-point
	     (same-buffer-points current-point last-point))		
	    (progn
	      (lem:set-current-mark last-point))
	    (progn
	      ;;(print "234234")
	      ;;FIXME? when does this happen? when the last point is null or
	      ;;exists in a different buffer? allow buffer-dependent selection?
	      (lem:set-current-mark current-point))))
      (setf *mouse-mode* :marking))))

(defun handle-dropped-files ()
  ;;switch to window that the mouse is hovering over, and find that file
  (when window::*dropped-files*
    (let ((window
	   (detect-mouse-window-intersection)))
      (when window
	(setf (lem:current-window) window)))
    (unless
	;;Do not drop a file into the minibuffer
	(eq lem::*minibuf-window*
	    (lem:current-window))
      (dolist (file window::*dropped-files*)
	(lem:find-file file))
      (redraw-display))))

(defparameter *last-clicked-at* nil) ;;to detect double and triple clicks etc...
(defparameter *clicked-at-times* 0)
(defun handle-multi-click (coord)
  (if (equal *last-clicked-at* coord)
      (incf *clicked-at-times*)
      (progn
	(setf *last-clicked-at* coord)
	(setf *clicked-at-times* 1))))

(defparameter *point-clicked-at* nil) ;;point representing the starting location in the buffer
(defparameter *click-selection-count* 0)
;;the number of times clicks consecutively at a position,
;;starting with 1
(defun handle-multi-click-selection ()
  (flet ((cancel-click-selection ()
	   (lem:buffer-mark-cancel (lem:current-buffer))
	   (setf *click-selection-count* 0)))
    (cond
      ((= 1 *clicked-at-times*)
       (cancel-click-selection)
       (setf *point-clicked-at*
	     (lem:copy-point (lem:current-point)
			     :temporary)))
      ((< 1 *clicked-at-times*)
       ;;(print *clicked-at-times*)
       
       ;;(lem:save-excursion)
       (let ((successp t))
	 (incf *click-selection-count*)
	 (let (inside
	       (on-last-paren nil))
	   (lem:with-point ((start *point-clicked-at*)
			    (end *point-clicked-at*))
	     (handler-case (progn				 
			     (lem:move-point (lem:current-point) *point-clicked-at*)
			     (lem:forward-sexp) ;;fails if on a closing paren
			     (lem:move-point end (lem:current-point))
			     (lem:backward-sexp) ;;fails at first char in list
			     (lem:move-point start (lem:current-point))
			     (setf inside
				   (and (lem:point<= start *point-clicked-at*)
					(lem:point< *point-clicked-at* end))))
	       (lem:editor-error (c)
		 (declare (ignorable c))
		 (setf on-last-paren t)))
	     (let ((iteration-count *click-selection-count*))
	       ;;(print (list inside on-last-paren))
	       (when inside
		 (decf iteration-count))
	       (dotimes (i iteration-count)
		 (handler-case (progn
				 ;;(lem:save-excursion
				 (lem:backward-up-list)
				 )
		   (lem:editor-error (c)
		     (declare (ignorable c))
		     ;;turn this on to select the whole buffer 
		     ;;(lem::mark-set-whole-buffer)
		     (setf successp nil)))))
	     (if successp
		 (progn
		   ;;(lem:move-point (lem:current-point) start)
		   (lem:set-current-mark (lem:copy-point (lem:current-point)
							 :temporary))
		   (lem:mark-sexp))
		 (progn (cancel-click-selection)
			(lem:move-point (lem:current-point)
					*point-clicked-at*))))))))))

(defun move-window-cursor-to-mouse (window &optional (x1 *grid-mouse-x*) (y1 *grid-mouse-y*))
  (let ((x (lem:window-x window))
	(y (lem:window-y window)))
    (mouse-move-to-cursor window (- x1 x) (- y1 y))))

(defun mouse-move-to-cursor (window x y)
  (let ((point (lem:current-point))
	(view-point (lem::window-view-point window)))
    ;;view-point is in the very upper right
    (when (same-buffer-points point view-point)
      (lem:move-point point view-point)
      (lem:move-to-next-virtual-line point y)
      (lem:move-to-virtual-line-column point x))))
#+nil
(defun mouse-get-window-rect (window)
  (values (lem:window-x      window)
          (lem:window-y      window)
          (lem:window-width  window)
          (lem:window-height window)))

(defun horizontally-between (window &optional (x1 *grid-mouse-x*))
  (let ((x (lem:window-x window))
	(w (lem:window-width window)))
    (and (<= x x1) (< x1 (+ x w)))))
(defun rectified-window-height (window)
  (+ (lem::window-height window)
     (if (lem::window-use-modeline-p window)
	 -1
	 0)))

(defun vertically-between (window &optional (y1 *grid-mouse-y*))
  (let ((y (lem:window-y window)))
    (and (<= y y1)
	 (< y1
	    (+ y (rectified-window-height window))))))
(defun centered-between (window &optional (x1 *grid-mouse-x*) (y1 *grid-mouse-y*))
  (and (horizontally-between window x1)
       (vertically-between window y1)))
(defun window-vertical-edge-coord (window)
  #+nil
  (- (lem:window-y window) 1)
  (+ (lem:window-y window)
     (rectified-window-height window)))

(defun window-horizontal-edge-coord (window)
  #+nil
  (- (lem:window-x window) 1)
  (+ (lem:window-x window)
     (lem:window-width window)))

(defun detect-mouse-window-intersection
    (&optional (x1 *grid-mouse-x*) (y1 *grid-mouse-y*)
       ;; &optional (press nil)
	    )
  ;;find the window which the coordinates x1 and y1 intersect at and return the window
  ;;and intersection type
  ;;returns (values window[if found window, otherwise nil] intersection-type)
  ;;intersection is one of :vertical, :horizontal, :center, or nil
  (let ((windows (lem:window-list)))
    #+nil;;FIXME::what does this variable do in lem?
    (when lem::*minibuffer-calls-window*
      (push lem::*minibuffer-calls-window* windows))
    (when lem::*minibuf-window*
      (push lem::*minibuf-window* windows))
    (block return 
      (dolist (window windows)  
	#+nil
	(when (eq window lem::*minibuf-window*)
	  ;;(print (list x y w h x1 y1))
	  )	  
	(cond
	  ;; vertical dragging window
	  ((and (= y1 (window-vertical-edge-coord window))
		(horizontally-between window x1))
	   ;;(setf *dragging-window* (list window 'y))
	   (return-from return (values window :vertical)))
	  ;; horizontal dragging window	    
	  ((and (= x1 (window-horizontal-edge-coord window))
		(vertically-between window y1))
	   ;;(setf *dragging-window* (list window 'x))
	   (return-from return (values window :horizontal)))
	  ((centered-between window x1 y1)
	   (return-from return (values window :center)))
	  (t)))
      (values nil nil))))

(defun scroll-event ()
  ;;scrolling
  (let ((scroll *scroll-difference*))
    (unless (zerop scroll)
      (lem:scroll-up (* *scroll-speed* scroll))
      (redraw-display)
      )))
(defun input-events ()
  ;;(print (list window::*control* window::*alt* window::*super*))
  ;;unicode input
  (dolist (press window::*char-keys*)
    (destructuring-bind (byte mods) press
      (let ((key (code-to-key byte)))
	(unless
	    ;;FIXME::better logic to handle this? ;;This is because space gets sent twice,
	    ;;once as a unicode char and once as a control key. The control key is for
	    ;;exampe C-Space
	    (member byte (load-time-value (list (char-code #\Space))))
	  (lem:send-event
	   (lem:make-key
	    :sym (lem:key-sym key)
	    :ctrl (or (lem:key-ctrl key)
		      (logtest window::+control+ mods)
		      (window:button :key :down :escape)
		      ;;FIXME:: escape used as substitute for control, specifically windows.
		      ;;see below for same info.
		      )
	    :shift (or (lem:key-shift key)
		       ;;window::*shift* ;;FIXME::why is this here?
		       )
	    :meta (or (lem:key-meta key)
		      (logtest window::+alt+ mods))
	    :super (or (lem:key-super key)
		       (logtest window::+super+ mods))))))))
  ;;control key input, such as Tab, delete, enter
  (let ((array (window::control-state-jp-or-repeat window::*control-state*)))
    (declare (type window::mouse-keyboard-input-array array))
    (dotimes (code 128)
      (let ((true-p (= 1 (sbit array code))))
	(when true-p
	  (multiple-value-bind (name type) (window::back-value code)
	    ;;(print (list name type))
	    (case type
	      (:key ;;FIXME::add mouse support?
	       (cond ((and (window::character-key-p code)
			   (not (member name '(:space)));;;FIXME::better logic to handle this?
			   ))
		     (t
		      (if (member name
				  '(:left-shift :left-control :left-super :left-alt
				    :right-shift :right-control :right-super :right-alt
				    :escape ;;FIXME::escape used as substitute for control,
				    ;;specifically for windows
				    ))
			  ;;FIXME::more efficient test?
			  nil ;;;ignore the modifier keys for shift, super, alt, control
			  (let ((key (get-sym-from-glfw3-code name)))
			    (if key
				(lem:send-event (lem:make-key
						 :sym key
						 :meta window::*alt*
						 :super window::*super*
						 :shift window::*shift*
						 :ctrl window::*control*))
				(format *error-output*
					"~s key unimplemented" name))))))))))))))

