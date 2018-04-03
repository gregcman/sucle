;;;;;;;;;;;;;;;
(defparameter *command-buffer* (lparallel.queue:make-queue))
(defun run-command-buffer ()
  (let ((queue *command-buffer*))
    (lparallel.queue:with-locked-queue queue
      (dotimes (x (/ (lparallel.queue:queue-count/no-lock queue) 2))
	(let ((var (lparallel.queue:try-pop-queue/no-lock queue))
	      (var2 (lparallel.queue:try-pop-queue/no-lock queue)))
	  (apply var var2))))))
(defun send-command (fun &rest args)
  (let ((queue *command-buffer*))
    (lparallel.queue:with-locked-queue queue
      (lparallel.queue:push-queue/no-lock fun queue)
      (lparallel.queue:push-queue/no-lock args queue))))
 
