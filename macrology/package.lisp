(defpackage #:macrology
  (:use #:cl)
  (:nicknames #:coge)

;;;general macros
  (:export
   #:toggle
   #:dorange
   #:progno
   #:ret
   #:rename)

;;;common rebindings
  (:export
   #:dp
   #:l
   #:mvb)

;;;code generation
  (:export
   #:gen-spec
   #:add-spec #:spec-assoc #:is-param 
   #:rp #:legalp #:get-actual-args #:defspec))
