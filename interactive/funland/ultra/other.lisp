(in-package :funland)

;;from uiop
(defmacro nest (&rest things)
    "Macro to do keep code nesting and indentation under control." ;; Thanks to mbaringer
    (reduce #'(lambda (outer inner) `(,@outer ,inner))
            things :from-end t))

(export '(nest))
