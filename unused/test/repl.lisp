(in-package :sandbox)

;;;;repl1
(defmacro handling-errors (&body body)
  `(handler-case (progn ,@body)
     (simple-condition (err) 
       (format *error-output* "~&~A: ~%" (class-name (class-of err)))
       (apply (function format) *error-output*
              (simple-condition-format-control   err)
              (simple-condition-format-arguments err))
       (format *error-output* "~&"))
     (condition (err) 
       (format *error-output* "~&~A: ~%  ~S~%"
               (class-name (class-of err)) err))))
(defun repl2 ()
  (do ((+eof+ (gensym))
       (hist 1 (1+ hist)))
      (nil)
    (format t "~%~A[~D]> " (package-name *package*) hist)
    (handling-errors
     (setf +++ ++   ++ +   + -   - (read *standard-input* nil +eof+))
     (when (or (eq - +eof+)
               (member - '((quit)(exit)(continue)) :test (function equal)))
       (return-from repl))
     (setf /// //   // /   / (multiple-value-list (eval -)))
     (setf *** **   ** *   * (first /))
     (format t "~& --> ~{~S~^ ;~%     ~}~%" /)))) 

;;;;repl2
(defun repl2 (&optional (end :end))
  (catch end
    (loop
       (progn
;	 (princ (package-name *package*))
;	 (write-char #\>)
;	 (write-char #\Space)
	 )
       (force-output)
       (let* ((form (read))
	      (results
	       (multiple-value-list
		(eval form))))
	 (setf - form)
	 (shiftf /// // / results)
	 (shiftf +++ ++ + -)
	 (shiftf *** ** * (first results))

	 (dolist (item results)
	   (print item))

	 (terpri)))))
