(defparameter *fixnum-compare*
  #+sbcl 'eq
  #-sbcl 'eql) 
