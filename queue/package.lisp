(defpackage #:queue
  (:use #:cl)
  (:nicknames #:q)
  (:export 
#:get-queue
#:q-push
#:q-pop
#:make-uniq-q
#:uniq-push
#:uniq-pop
#:kv-uniq-push
#:kv-uniq-pop
#:uniq-length))
