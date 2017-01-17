(defpackage #:hook
  (:use #:cl)
  (:export
   #:create-hook
   #:add-hook
   #:remove-hook
   #:run-hook
   #:run-hooks
   #:run-hook-with-args
   #:run-hook-with-args-until-failure
   #:run-hook-with-args-until-success))
