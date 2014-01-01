(in-package :localizable-place-references)

(defclass tree-node-with-data (editable-context data-context)
  ((name :accessor name :initarg :name :initform nil)
   (parent :accessor parent :initarg :parent :initform nil)
   (children :accessor children :initarg :children :initform nil)))

(defclass editable-context ()
  ((editablep :initarg editablep)))
(context-slot-accessors
 o editable-context editablep
 (lambda () (object-traversal-place-finder
        o #'parent
        (slot-value-place-finder-test 'editablep))))

(defclass data-context ()
  ((data :initarg :data)))
(context-slot-accessors
 o data-context data
 (lambda () (object-traversal-place-finder
        o #'parent
        (slot-value-place-finder-test 'data)))
 :set-local-copy? nil)

(defparameter +place+ nil)
(defparameter +indirect0+ nil)
(defparameter +indirect1+ nil)

(defparameter +g-node+ nil)
(defparameter +e-node+ nil)
(defparameter +indirection-tree+ nil)
(defun setup-indirection-tree ()
  (setf +indirection-tree+
        (flet ((mn (name data &rest kids)
                 (let ((n (make-instance 'tree-node-with-data :name name :children kids)))
                   (when data (setf (data n) data))
                   (iter (for k in kids) (setf (parent k) n))
                   n)))
          (mn :a 42
              (mn :b nil
                  (mn :c nil)
                  (mn :d nil
                      (setf +e-node+ (mn :e nil ))))
              (mn :f nil
                  (setf +g-node+
                        (mn :g nil
                            (mn :h nil )
                            (mn :i nil )))
                  (mn :j nil )))))
  +indirection-tree+)

(lisp-unit2:define-test test-basic-indirection ()
  (setf +place+ nil)
  (setf +indirect0+ (make-instance
                     'localizable-place-reference
                     :place-reference (reference-place +place+)
                     :set-local-copy? nil))
  (setf +indirect1+ (make-instance
                     'localizable-place-reference
                     :place-reference  (reference-place +place+)
                     :set-local-copy? t))
  (lisp-unit2:assert-false (or (get-place-value +indirect0+)
                              (get-place-value +indirect1+)))
  (set-place-value 42 +indirect0+)
  (lisp-unit2:assert-eql 42 (get-place-value +indirect1+))
  (lisp-unit2:assert-eql 42 +place+)
  (set-place-value 16 +indirect1+)
  (lisp-unit2:assert-eql 16 (get-place-value +indirect1+))
  (lisp-unit2:assert-eql 42 +place+)
  (lisp-unit2:assert-eql 42 (get-place-value +indirect0+)))

(lisp-unit2:define-test indirection-tree-data ()
  (setup-indirection-tree)
  (setf (data +indirection-tree+) 42)
  (lisp-unit2:assert-eql 42 (data +indirection-tree+))
  (lisp-unit2:assert-eql 42 (data +e-node+))
  (lisp-unit2:assert-eql 42 (data +g-node+))
  (lisp-unit2:assert-eql 42 (data (first (children +g-node+))))
  (setf (data +e-node+) 84)
  (lisp-unit2:assert-eql 84 (data +indirection-tree+))
  (lisp-unit2:assert-eql 84 (data +e-node+))
  (lisp-unit2:assert-eql 84 (data +g-node+))
  (lisp-unit2:assert-eql 84 (data (first (children +g-node+))))
  (localize (slot-value +g-node+ 'data) 21)
  (lisp-unit2:assert-eql 84 (data +indirection-tree+) :after-localize)
  (lisp-unit2:assert-eql 84 (data +e-node+) :after-localize)
  (lisp-unit2:assert-eql 21 (data +g-node+))
  (lisp-unit2:assert-eql 21 (data (first (children +g-node+)))))

(lisp-unit2:define-test indirection-tree-editablep ()
  (setup-indirection-tree)
  (setf (editablep +indirection-tree+) t)
  (lisp-unit2:assert-eql t (editablep +indirection-tree+))
  (lisp-unit2:assert-eql t (editablep +e-node+))
  (lisp-unit2:assert-eql t (editablep +g-node+))
  (lisp-unit2:assert-eql t (editablep (first (children +g-node+))))
  (setf (editablep +g-node+) nil)
  (lisp-unit2:assert-eql t (editablep +indirection-tree+))
  (lisp-unit2:assert-eql t (editablep +e-node+))
  (lisp-unit2:assert-eql nil (editablep +g-node+))
  (lisp-unit2:assert-eql nil (editablep (first (children +g-node+)))))

