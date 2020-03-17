(in-package :lem-sucle) 

(defclass sucle (lem:implementation)
  ()
  (:default-initargs
   :native-scroll-support nil
    :redraw-after-modifying-floating-window t))
(setf lem:*implementation* (make-instance 'sucle))

(define-condition exit-editor (lem:editor-condition)
  ((value
    :initarg :value
    :reader exit-editor-value
    :initform nil)))

(defun sync-lem-windows-and-ncurses-views ()
  (lem::window-tree-map
   (lem::window-tree)
   (lambda (window)
     (let ((view (lem:window-view window)))
       (when view ;;FIXME::what can this be?
	 (setf (ncurses-clone-lem-view:ncurses-view-parent-window view)
	       window))))))

;;;;
(defclass sucle-attribute (lem:attribute)
  ((overlay
    :initarg :overlay
    :initform nil
    :accessor sucle-attribute-overlay)))
;;;;


(defparameter *editor-thread* nil)
(defun invoke (function)
  (when (or (null *editor-thread*)
	    (not (bt:thread-alive-p *editor-thread*)))
    (print "starting editor thread")
    (let (;;(result nil)
	  (input-thread (bt:current-thread)))
      (setf ncurses-clone::*char-width-at-fun* #'lem-base:char-width)
      (setf *editor-thread*
	    (funcall function
		     nil
		     (lambda (report)
		       (bt:interrupt-thread
			input-thread
			(lambda ()
			  (print report)
			  (error 'exit-editor :value report))))))
      #+nil
      (setf result (input-loop editor-thread))
      #+nil
      (when (and (typep result 'exit-editor)
		 (exit-editor-value result))
	(format t "~&~A~%" (exit-editor-value result))))))
(defmethod lem-if:invoke ((implementation sucle) function)
  ;;FIXME::Factor out term-init?
  (invoke function))
(defmethod lem-if:display-background-mode ((implementation sucle))
  (ncurses-clone-lem-view:display-background-mode))
(defmethod lem-if:update-foreground ((implementation sucle) color-name)
  (ncurses-clone-lem-view:update-foreground color-name))
(defmethod lem-if:update-background ((implementation sucle) color-name)
  (ncurses-clone-lem-view:update-background color-name))
(defmethod lem-if:display-width ((implementation sucle))
  (ncurses-clone-lem-view:display-width))
(defmethod lem-if:display-height ((implementation sucle))
  (ncurses-clone-lem-view:display-height))
(defmethod lem-if:make-view ((implementation sucle) window x y width height use-modeline)
  (declare (ignorable window))
  (ncurses-clone-lem-view:make-view x y width height use-modeline))
(defmethod lem-if:delete-view ((implementation sucle) view)
  (ncurses-clone-lem-view:delete-view view))
(defmethod lem-if:clear ((implementation sucle) view)
  (ncurses-clone-lem-view:clear view))
(defmethod lem-if:set-view-size ((implementation sucle) view width height)
  (ncurses-clone-lem-view:set-view-size view width height (lem:minibuffer-window-height)))
(defmethod lem-if:set-view-pos ((implementation sucle) view x y)
  (ncurses-clone-lem-view:set-view-pos view x y))

(defun attribute-to-bits (attribute-or-name)
  (let ((attribute (lem:ensure-attribute attribute-or-name nil))
        ;;(cursorp (eq attribute-or-name 'lem:cursor))
	)
    (if (null attribute)
        0
        (or (lem::attribute-%internal-value attribute)
            (let ((bits
		   (lem.term::get-attribute-bits-2
		    (lem:attribute-foreground attribute)
		    (lem:attribute-background attribute)
		    (lem::attribute-bold-p attribute)
		    (lem::attribute-underline-p attribute)
		    (lem::attribute-reverse-p attribute))))
              (setf (lem::attribute-%internal-value attribute) bits)
              bits)))))

(defmacro with-attribute-and-view ((attribute view) &body body)
  (alexandria:once-only (attribute)
    `(ncurses-clone::with-attributes
	 ((attribute-to-bits ,attribute) (list ,attribute ,view)
	  (typep ,attribute 'sucle-attribute))
       ,@body)))
(defmethod lem-if:print ((implementation sucle) view x y string attribute)
  ;;FIXME::different names
  (with-attribute-and-view (attribute view)
    (ncurses-clone-lem-view:print-into-view view x y string)))
(defmethod lem-if:print-modeline ((implementation sucle) view x y string attribute)
  (with-attribute-and-view (attribute view)
    (ncurses-clone-lem-view:print-modeline view x y string)))
(defmethod lem-if:clear-eol ((implementation sucle) view x y)
  (ncurses-clone-lem-view:clear-eol view x y))
(defmethod lem-if:clear-eob ((implementation sucle) view x y)
  (ncurses-clone-lem-view:clear-eob view x y))

;;(defparameter *no* *standard-output*)
(defmethod lem-if:redraw-view-after ((implementation sucle) view focus-window-p)
  (declare (ignore focus-window-p))
  (with-attribute-and-view ('lem:modeline view)
    (ncurses-clone-lem-view:redraw-view-after view)))
(defmethod lem-if:update-display ((implementation sucle))
  (ncurses-clone-lem-view:update-display)
  (sync-lem-windows-and-ncurses-views))
(defmethod lem-if:scroll ((implementation sucle) view n)
  (ncurses-clone-lem-view:scroll view n))
(defmethod lem-if:clipboard-paste ((implementation sucle))
  (trivial-clipboard:text))
(defmethod lem-if:clipboard-copy ((implementation sucle) text)
  (trivial-clipboard:text text))

(pushnew :lem-sucle *features*)
