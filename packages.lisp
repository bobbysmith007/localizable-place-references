(cl:defpackage :localizable-place-references
  (:nicknames :place-ref )
  (:use :cl :cl-user :iterate)
  (:export
   #:localizable-place-reference
   #:get-place-value
   #:set-place-value
   #:always-search?
   #:set-local-copy?

   #:reference-place
   #:reference-place-fns
   #:make-localizable-place-finder
   #:context-slot-accessors
   #:slot-value-place-finder-test
   #:object-traversal-place-finder

   #:place-getter
   #:place-setter
   #:place-finder
   #:local-copy

   ))