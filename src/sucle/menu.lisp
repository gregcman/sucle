(defpackage #:menu
  (:use :cl)
  (:export
   #:*data*
   #:*w*
   #:*h*
   #:tick
   #:use
   #:start))
(in-package #:menu)
;;Ripped from sucle-test essentially.

(defparameter *view*
  ;;dimensions insignificant: overwritten later
  (ncurses-clone:ncurses-newwin 5 50 0 0))
(defun menu-app ()
  (app:push-mode 'tick)
  (ncurses-clone-for-lem:init)
  (app:default-loop))
(defun start ()
  (app:enter 'menu-app))
(defparameter *menu*
  `((((:key :pressed #\q) .
      ,(lambda () (app:quit)))
     ((:key :pressed #\Escape) .
      ,(lambda () (app:quit))))
    ((:hello
       "
Press q/escape to quit








Bottom Text
" 4 4 :bold t))))
(defun tick ()
  (destructuring-bind (&optional (x0 0) (y0 0) (x1 14) (y1 14)) (menu-dimensions *menu*)
      (ncurses-clone-for-lem::easy-frame x0 y0 x1 y1 *view*))
  (simulate-menu *menu*)
  (ncurses-clone-for-lem:render :update-data t :win *view*))

(defun use (&optional (menu *menu*))
  (setf *menu* menu))

(defun draw-string (str x y
		    &key (view *view*) (fg "white") (bg "black")
		      (underline nil) (bold nil) (reverse nil))
  (lem.term:with-attribute
      (:fg fg :bg bg :underline underline :bold bold :reverse reverse)
    (ncurses-clone:ncurses-mvwaddstr view y x str)))

;;;;MENU
;;-> inspired by html dom?
(defvar *data*)
(defvar *h*)
(defvar *w*)
(defun simulate-menu (&optional (menu *menu*))
  ;;do buttons
  (let
      ;;give buttons access to the DOM
      ((*data* (menu-data menu))
       (*w* (ncurses-clone:win-lines *view*))
       (*h* (ncurses-clone:win-cols *view*)))
    ;;FIXME:move run-buttons somewhere?
    (sucle::run-buttons (menu-buttons menu))
    (let ((fun (menu-tick menu)))
      (when fun
	(funcall fun))))
  ;;do items
  (let ((menu-data (menu-data menu)))
    (dolist (item menu-data)
      (apply 'draw-string (cdr item)))))
(defun menu-buttons (&optional (menu *menu*))
  (first menu))
(defun menu-data (&optional (menu *menu*))
  (second menu))
(defun menu-dimensions (&optional (menu *menu*))
  (third menu))
(defun menu-tick (&optional (menu *menu*))
  (fourth menu))
;;;MENU
;;;;************************************************************************;;;; 
