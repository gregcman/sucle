(in-package :sucle)
(defparameter *start-menu*
  `(;;keys bound to functions
    (((:key :pressed #\f) .
      ,(lambda () (print "Paying Respects")))
     ((:key :pressed #\q) .
      ,(lambda () (app:quit)))
     ((:key :pressed #\Escape) .
      ,(lambda () (app:quit)))
     ((:key :pressed #\p) .
      ,(lambda () (app:pop-mode)))
     ((:key :pressed #\o) .
      ,(lambda () (app:push-mode 'menu:tick)))
     ((:key :pressed #\s) .
      ,(lambda ()
	 (app:push-mode 'sucle-per-frame)))
     ((:key :pressed #\c) .
      ,(lambda ()
	 (print "Clearing...")
	 (let ((clear (assoc :clear menu:*data*)))
	   (setf (second clear)
		 (with-output-to-string (str)
		   (let ((clearstr
			  (make-string menu:*w*
				       :initial-element #\space)))
		     (dotimes (y menu:*h*)
		       (terpri str)
		       (write-string clearstr str))))))))
     ((:key :released #\c) .
      ,(lambda ()
	 (print "Clearing Done!")
	 (let ((clear (assoc :clear menu:*data*)))
	   (setf (second clear)
		 "")))))
    ;;data to render
    ((:hello
      "
Press s to start the game

Press c to clear

Press h for help

Press F to pay respects [not really]

Press q/escape to quit
" 4 4 :bold t)
     ;;(:hello "world" 8 16 :fg "green" :bg "red" :reverse t :bold t)
     (:clear "" 0 0  :bold t))
    (1 1 11 11)))

(defparameter *start-menu2*
  `(;;keys bound to functions
    (
     ((:key :pressed #\Space) .
      ,(lambda ()
	 (menu:use (make-menu-file-picker))
	 ))
    )
    ;;data to render
    ((:clear "                                             
                                             
                                             
                                             
                                             
                                             
                                             
                                             
                                             
                                             
                                             
                                             
                                             
                                             
                                             " 0 0  :bold t)
     (:hello
      "
                   _____ _    _  _____ _      ______ 
                  / ____| |  | |/ ____| |    |  ____|
                 | (___ | |  | | |    | |    | |__   
                  \\\___ \\\| |  | | |    | |    |  __|  
                  ____) | |__| | |____| |____| |____ 
                 |_____/ \\\____/ \\\_____|______|______|












                                
                                     
" 0 14 :bold t)
     (:press_s "
                              press space" 0 20 :bold t :fg "black" :bg "white" :reverse t))
    ()
    ,(lambda ()
       (let ((data (assoc :press_s menu:*data*)))
	 (if (zerop (mod (floor (fps:microseconds) 1000000) 2))
	     (setf (getf data :bg) "white")
	     (setf (getf data :bg) "black"))))))

(defun saves-list ()
  (append (uiop:directory* (sucle-temp:path "saves/*"))
	  (uiop:directory-files (sucle-temp:path "saves/"))))
(defparameter *menu-file-picker*
  `(;;keys bound to functions
    (
     ((:key :pressed #\Space) .
      ,(lambda ()
	 (app:push-mode 'sucle-per-frame)))
    )
    ;;data to render
    ((:press_s "
                              press space" 0 20 :bold t :fg "black" :bg "white" :reverse t))
    ()
    ,(lambda ()
       (let ((data (assoc :press_s menu:*data*)))
	 (if (zerop (mod (floor (fps:microseconds) 1000000) 2))
	     (setf (getf data :bg) "white")
	     (setf (getf data :bg) "black"))))))

(defun make-menu-file-picker (&optional (index 0))
  (let ((saves (saves-list)))
    (setf index (mod index (length saves)))
    `(;;keys bound to functions
      (((:key :repeat :up) .
	,(lambda ()
	   (menu:use (make-menu-file-picker (1- index)))))
       ((:key :repeat :down) .
	,(lambda ()
	   (menu:use (make-menu-file-picker (1+ index)))))
       ((:key :pressed :enter) .
	,(lambda ()
	   (crud:use-crud-from-path (elt saves index))
	   (vocs:clearworld)
	   (update-world-vao2)
	   (app:push-mode 'sucle-per-frame)))
       )
      ;;data to render
      ((:what "      saves; press enter to select" 0 4 :bg "black" :fg "cyan" :bold t)
       ,@(mapcar (lambda (save list-index)
		   `(:data ,(write-to-string save) 0 ,(+ 7 list-index) :bold t :fg "black" :bg "white" :reverse
			   ,(if (equal index list-index)
				nil t
				)))
		 saves
		 (alexandria:iota (length saves))))
      ()
      ,(lambda () (menu:clear)))))
