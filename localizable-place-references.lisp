(in-package :localizable-place-references)

(defclass place-reference ()
  ((form :accessor form :initarg :form :initform nil
         :documentation "The form that was captured by the place function")
   (place-getter
    :accessor place-getter :initarg :place-getter :initform nil
    :documentation
    "A function that references a place, when called returns the value of place")
   (place-setter
    :accessor place-setter :initarg :place-setter :initform nil
    :documentation
    "A function that references a place, when called with an arg sets the value of place")))

(defclass object-slot-reference (place-reference)
  ((instance
    :accessor instance :initarg :instance :initform nil
    :documentation "the object that we are referencing")
   (slot-name
    :accessor slot-name :initarg :slot-name :initform nil
    :documentation "the slot-name on the object that we are referencing")))

(defclass localizable-place-reference ()
  ((place-reference :accessor place-reference :initarg :place-reference :initform nil
    :documentation "some type of place reference")
   (place-finder
    :accessor place-finder :initarg :place-finder :initform nil
    :documentation
    "A function that in the absense of a place or local-copy, will locate the place
     we are meant to reference and returns (values value-of-place place-getter place-setter)")
   (local-copy
    :accessor local-copy :initarg :local-copy
    :documentation "A storage location for a local copy of this data")
   (always-search?
    :accessor always-search? :initarg :always-search? :initform nil
    :documentation
    "When we use the place finder, should we save the returned
    place-reference-fn for future use or should always search using the
    place-finder.  For many uses always searching is preferable, however if
    the search was expensive and it is unlikely the results will change, then
    saving the place-fn can save significant computation")
   (set-local-copy?
    :accessor set-local-copy? :initarg :set-local-copy? :initform nil
    :documentation
    "When we set-place-value associated with this, should we set the local copy
     or should we set the referenced copy "))
  (:documentation "An object that represents a reference to another place
   localizable: we can stop referencing the distant place and start using a local value
   place: the standard CL place: essentially any form, but mostly ones that can
          be in (setf place value)
   reference: the standard definition: an object that can retrieve or set a value that
              would not normally be in scope"))

(defmethod place-getter ((o localizable-place-reference)
                         &aux (ref (place-reference o)))
  (when ref (place-getter ref)))


(defmethod place-setter ((o localizable-place-reference)
                         &aux (ref (place-reference o)))
  (when ref (place-setter ref)))

(defgeneric get-place-value (o)
  (:method ((o place-reference))
    (funcall (place-getter o)))
  (:method ((o localizable-place-reference)
            &aux (has-local-copy? (slot-boundp o 'local-copy)))
    (access:with-access
        (place-getter place-finder place-reference always-search?) o
      (cond
        (has-local-copy? (local-copy o))
        (place-getter (values (funcall place-getter) place-reference))
        (place-finder
         (multiple-value-bind (value new-place-reference)
             (funcall place-finder)
           (when (and new-place-reference (not always-search?))
             (setf place-reference new-place-reference))
           (values value new-place-reference)))
        (T (Error "No local-copy, place or place-finder ~A ~S" o o))))))

(defgeneric set-place-value (new o)
  (:documentation "Given a localizable place reference, and a new value
     set either the local-copy or the place with that value depending on
     current object configuration ")
  (:method (new (o place-reference))
    (funcall (place-setter o) new))
  (:method (new (o localizable-place-reference)
            &aux (has-local-copy? (slot-boundp o 'local-copy)))
    (access:with-access
        (place-setter place-finder place-reference set-local-copy? always-search?)
        o
      ;; we set local in a variety of cases, but it was harder to express that logically
      ;; as one expression than setf'ing three times
      (cond
        ((or has-local-copy? set-local-copy?)
         (setf (local-copy o) new))
        (place-setter (funcall place-setter new))
        (place-finder
         (multiple-value-bind (value new-place-ref)
             (funcall place-finder)
           (when (and new-place-ref (not always-search?))
             (setf place-reference new-place-ref))
           ;; if we could not find a place to set then we will set ourselves
           (if (and new-place-ref (place-setter new-place-ref))
               (funcall (place-setter new-place-ref) new)
               (setf (local-copy o) new))
           (values value new-place-ref)))
        (T
         (warn "In the absense of both a place-setter and a place-finder ~A is setting locally.
              ~%This is probably a configuration mistake" o)
         (setf (local-copy o) new))))))

(defgeneric localize (o &optional value)
  (:method ((o t) &optional value)
    "Things that are not place references cant be localized, this is nice because we can
     treat slot-values as place references even if they are not"
    (or o value))
  (:method ((o localizable-place-reference) &optional (value nil value?))
    "Localizes the place reference to an object to either value, or the value of the place"
    (setf (local-copy o) (if value? value (funcall o)))))

(defmacro reference-place (place)
  "Create a closure that references a place, when called with no args the
   closure returns the value of the place; when called with args, it sets the
   place"
  (when place
    `(make-instance 'place-reference
      :form ',place
      :place-getter (lambda () ,place)
      :place-setter (lambda (n) (setf ,place n)))))

(defun reference-object-slot (object slot-name)
  (when (and object slot-name)
    (make-instance
     'object-slot-reference
     :instance object
     :slot-name slot-name
     :place-getter (lambda () (slot-value object slot-name))
     :place-setter (lambda (new) (setf (slot-value object slot-name) new)))))

(defun make-localizable-place-finder
    (place-finder &key (always-search? t) (set-local-copy? t))
  (make-instance
   'localizable-place-reference
   :place-finder place-finder
   :always-search? always-search?
   :set-local-copy? set-local-copy?))

(defun slot-value-place-finder-test ( slot-name )
  "cant use accessors because of infinite recursion
   (this is usually called inside of the accessor)"
  (lambda (o)
    ;; this should be ignoring slot-unboundedness and missing slots
    (let ((val (ignore-errors (slot-value o slot-name))))
      (when val
        (let ((place-ref (reference-object-slot o slot-name)))
          (typecase val
            (localizable-place-reference
             ;; dont latch onto a reference keep going for the original
             ;; unless this reference has been set
             (when (slot-boundp val 'local-copy)
               (values (local-copy val) place-ref)))
            (t (values val place-ref))))))))

(defun object-traversal-place-finder ( o travel-fn &rest tests)
  "o is an object for which calling travel-fn is valid
   travel-fn turns o into a new o eg: (setf o (funcall #'parent o))
   tests is a series of tests to run on the object.  Usually we check the slot
   value (via a slot-value-place-finder-test)

   cant use accessors because of infinite recursion (this is usually called inside of the accessor)"
  (iter
    (while o)
    (iter (for test in tests)
      (let ((rtn (multiple-value-list (funcall test o))))
        (when (or (< 1 (length rtn)) (first rtn))
          (return-from object-traversal-place-finder
            (apply #'values rtn)))))
    (setf o (funcall travel-fn o))))

(defun non-defered-slot-value (o slot-value))

(defmacro context-slot-accessors (instance class-name slot-name &rest place-finder-args)
  `(progn
    (defmethod ,slot-name ((,instance ,class-name))
      ;; dont think this is possible, but might be worth it as a way to reset this
      ;; slot if set to unbound
      (unless (slot-boundp ,instance ',slot-name)
        (setf (slot-value ,instance ',slot-name)
              (make-localizable-place-finder ,@place-finder-args)))
      (let ((value (slot-value ,instance ',slot-name)))
        (typecase value
          (localizable-place-reference (get-place-value value))
          (t value))))

    (defmethod (setf ,slot-name) (new (,instance ,class-name))
      (unless (slot-boundp ,instance ',slot-name)
        (setf (slot-value ,instance ',slot-name)
              (make-localizable-place-finder ,@place-finder-args)))
      (let ((value (slot-value ,instance ',slot-name)))
        (typecase value
          (localizable-place-reference (set-place-value new value))
          (t (setf (slot-value ,instance ',slot-name) new)))))))
