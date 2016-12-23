(defpackage #:code-generation
  (:use #:cl)
  (:nicknames #:coge)
  (:export
	#:gen-spec
#:add-spec #:spec-assoc #:is-param 
#:rp #:legalp #:get-actual-args #:defspec))
