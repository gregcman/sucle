(defmacro my-setf (place values-form &environment env)
  (multiple-value-bind (vars vals stores setter getter)
      (get-setf-expansion place env)
    (declare (ignorable getter))
    `(let* (,@ (mapcar #'list vars vals))
       (multiple-value-bind ,stores ,values-form
	 ,setter))))
