(in-package :sandbox)

(defun loud-thread (func name output input)
  "makes a thread that reads and writes to stdio"
  (sb-thread:make-thread 
   #'(lambda (standard-output standard-input)    
       (let ((*standard-output* standard-output)
	     (*standard-input* standard-input))
	 (funcall func)))
   :arguments (list output input)
   :name name))
