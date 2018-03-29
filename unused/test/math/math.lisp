(defun ease (x target fraction)
  (+ x (* fraction (- target x))))

(defun int-scale (int scale)
  (truncate (* int scale)))
