#|
This file is a part of Parasol
(c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.parasol.tools)
(named-readtables:in-readtable :qtools)

(defgeneric change (option value))

(with-widget-environment
  (define-widget tool-option (QWidget)
    ((label :initarg :label :initform NIL :accessor label)
     (description :initarg :description :initform NIL :accessor description)
     (tool :initarg :tool :initform (error "TOOL required.") :accessor tool)
     (on-change :initarg :on-change :initform NIL :accessor on-change)
     (slot :initarg :slot :initform NIL :accessor slot))
    (:documentation "Tool options are widgets that the user can interact with to change the tool's settings."))

  (define-slot change (option (new-value bool int double "const QString&"))
    (declare (method))
    (when on-change
      (setf new-value (funcall on-change tool option new-value)))
    (when slot
      (setf (slot-value tool slot) new-value)))

  (define-initializer option 100
    (unless label
      (setf label (capitalize-on #\- (class-name (class-of option)) #\Space T)))
    (#_setToolTip option (format NIL "~a~@[: ~a~]" label description))))

(defmacro define-tool-option (name (qt-class &rest direct-superclasses) direct-slots &body options)
  (destructuring-bind (name &optional (label (capitalize-on #\- name #\Space T))
                                      (description ""))
      (if (listp name) name (list name))
    (when (loop for superclass in direct-superclasses
                never (typep superclass 'tool-option))
      (push 'tool-option direct-superclasses))
    (unless (assoc :label options)
      (push (list :label label) options))
    (unless (assoc :description options)
      (push (list :description description) options))
    `(define-widget ,name (,qt-class ,@direct-superclasses)
       ,direct-slots
       ,@options)))

(indent:define-indentation define-tool-option
    (4 (&whole 6 &rest)
       (&whole 2 (&whole 0 0 &rest 2))
       &rest (&whole 2 2 &rest (&whole 2 2 4 &body))))

(declare-environment-widget-form 'define-tool-option)
