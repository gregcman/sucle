(in-package :sucle)

(defparameter *start-menu2*
  `(;;keys bound to functions
    (
     ((:key :pressed #\Space) .
      ,(lambda ()
	 (app:push-mode 'sucle-per-frame)))
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
