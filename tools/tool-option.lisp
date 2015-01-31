#|
This file is a part of Parasol
(c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.parasol.tools)
(named-readtables:in-readtable :qtools)

(defgeneric change (option value))

(define-widget tool-option (QWidget)
  ((label :initarg :label :initform NIL :allocation class :accessor label)
   (description :initarg :description :initform NIL :allocation class :accessor description)
   (tool :initarg :tool :initform (error "TOOL required.") :accessor tool)
   (on-change :initarg :on-change :initform NIL :accessor on-change)
   (slot :initarg :slot :initform NIL :accessor slot))
  (:documentation "Tool options are widgets that the user can interact with to change the tool's settings."))

(define-slot (tool-option change) ((new-value "const QString&"))
  (call-next-method))

(define-slot (tool-option change) ((new-value double))
  (call-next-method))

(define-slot (tool-option change) ((new-value int))
  (call-next-method))

(define-slot (tool-option change) ((new-value bool))
  (call-next-method))

(define-initializer (tool-option setup)
  (unless label
    (setf label (capitalize-on #\- (class-name (class-of tool-option)) #\Space T)))
  (#_setToolTip tool-option (format NIL "~a~@[: ~a~]" label description)))

(defmethod change ((option tool-option) new-value)
  (with-slots-bound (option tool-option)
    (when on-change
      (setf new-value (funcall on-change tool option new-value)))
    (when slot
      (setf (slot-value tool slot) new-value))))

(defmacro define-tool-option (name (qt-class &rest direct-superclasses) direct-slots &body options)
  (destructuring-bind (name &optional (label (capitalize-on #\- name #\Space T))
                                      (description ""))
      (if (listp name) name (list name))
    (when (loop for superclass in direct-superclasses
                never (typep superclass 'tool-option))
      (push 'tool-option direct-superclasses))
    (flet ((set-initarg (arg value)
             (let ((args (cdr (assoc :default-initargs options))))
               (setf (getf args arg) (or (getf args arg) value))
               (if (assoc :default-initargs options)
                   (setf (cdr (assoc :default-initargs options)) args)
                   (push `(:default-initargs . ,args) options)))))
      (set-initarg :label label)
      (set-initarg :description description))
    
    `(define-widget ,name (,qt-class ,@direct-superclasses)
       ,direct-slots
       ,@options)))

(indent:define-indentation define-tool-option
    (4 (&whole 6 &rest)
       (&whole 2 (&whole 0 0 &rest 2))
       &rest (&whole 2 2 &rest (&whole 2 2 4 &body))))
