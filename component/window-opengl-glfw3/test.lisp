#+nil
(defun num-key-jp (&optional (control-state window::*control-state*))
  (etouq
   (cons
    'cond
    (mapcar
     (lambda (n)
       `((window::skey-j-p
	  (window::keyval ,n)
	  control-state) ,n))
     '(0 1 2 3 4 5 6 7 8 9)))))
 
