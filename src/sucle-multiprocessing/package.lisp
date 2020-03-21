(defpackage #:sucle-multiprocessing
  (:use :cl)
  (:nicknames :sucle-mp :sump)
  (:export
   #:submit-unique-task
   #:remove-unique-task-key
   #:job-task-data
   #:job-task-return-values
   #:*current-job-task*
   #:with-initialize-multiprocessing
   #:with-kernel
   #:do-queue-iterator
   #:submit
   #:flush-job-tasks))
