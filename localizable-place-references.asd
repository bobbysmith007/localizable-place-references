;; -*- lisp -*-

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :localizable-place-references.system)
    (defpackage :localizable-place-references.system (:use :common-lisp :asdf))))

(in-package :localizable-place-references.system)

(defsystem :localizable-place-references
  :description "A library providing for finding and creating references to places
   ( usually in an object tree/graph )"
  :licence "BSD"
  :version "0.2"
  :serial t
  :components ((:file "packages")
               (:file "localizable-place-references"))
  :depends-on (:iterate :alexandria :access))

(defsystem :localizable-place-references-test
  :description "Tests for the localizable-place-references library"
  :licence "BSD"
  :version "0.2"
  :serial t
  :components ((:module :test
                        :serial t
                        :components ((:file "localizable-place-references"))))
  :depends-on (:localizable-place-references :lisp-unit))

(defmethod asdf:perform ((o asdf:test-op) (c (eql (find-system :localizable-place-references))))
  (asdf:oos 'asdf:load-op :localizable-place-references-test)
  (let ((*package* (find-package :localizable-place-references-test)))
    (eval (read-from-string "(run-tests :all)"))))
