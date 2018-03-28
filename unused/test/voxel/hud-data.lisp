(defun injection2 ()
  (map-box #(0 0 20 5)
	   (lambda (x y)
	     (scwu nil x y)))
  (progn
    (copy-string-to-world 0 3 (print-to-buf "xpos: " #'princ))
    (copy-string-to-world 0 2 (print-to-buf "ypos: " #'princ))
    (copy-string-to-world 0 1 (print-to-buf "zpos: " #'princ)))

  (progn
    (copy-string-to-world 6 3 (print-to-buf sandbox::*xpos*))
    (copy-string-to-world 6 2 (print-to-buf sandbox::*ypos*))
    (copy-string-to-world 6 1 (print-to-buf sandbox::*zpos*))

    (when sandbox::fist?
      (copy-string-to-world 0 0 (print-to-buf "blockid: " #'princ))
      (copy-string-to-world 9 0 (print-to-buf (aref mc-blocks::names
						    (world::getblock sandbox::fist-side-x
								     sandbox::fist-side-y
								     sandbox::fist-side-z))
					      #'princ)))))
