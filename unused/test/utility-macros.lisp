(defmacro nif (test-form else if)
  `(if ,test-form
       ,if
       ,else)) 
