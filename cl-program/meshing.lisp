(in-package :sandbox)

(defmacro with-simply-bound-iterator ((next place iterator) &body body)
  (let ((array (gensym))
	(index (gensym)))
    `(iter-ator:with-bound-iterator (,next ,place (,array) (,index)) ,iterator
       ,@body)))

(defmacro wasabi ((emit iterator) &body body)
  (let ((next (gensym))
	(place (gensym)))
    `(with-simply-bound-iterator (,next ,place ,iterator)
       (macrolet ((,emit (value)
		    (list (quote progn)
			  (quote (,next))
			  (list (quote setf) (quote ,place) value))))
	 ,@body))))
