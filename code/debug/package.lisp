(cl:defpackage #:incrementalist.debug
   (:use
    #:cl)

   (:shadow
    #:log)

   (:export
    #:*enabled-categories*
    #:log)

   #+incrementalist-debug
   (:export
    #:define-invariant))
