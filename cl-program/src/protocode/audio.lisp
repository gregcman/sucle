(in-package :sandbox)

(progno (progno
	 #:cl-openal
	 #:cl-alc
	 #:cl-alut))

(defun alut-hello-world ()
  (alut:with-init
      (al:with-source (source)
      (let ((buffer (if t
			(print (alut:create-buffer-from-file "/home/terminal256/Music/Bustin.wav"))
			(alut:create-buffer-hello-world))))
	(print (alut:get-error))
        (al:source source :buffer buffer)
        (al:source-play source)
        (alut:sleep 500)))))
