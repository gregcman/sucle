(defpackage #:macrology
  (:use #:cl)
  (:nicknames #:coge)

;;;general macros
  (:export
   #:toggle
   #:dorange
   #:dobox
   #:progno
   #:ret
   #:rename
   #:null!)

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
