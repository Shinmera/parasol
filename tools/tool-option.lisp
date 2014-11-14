#|
This file is a part of Parasol
(c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.parasol)
(named-readtables:in-readtable :qtools)

(with-widget-environment
  (define-widget tool-option (QWidget)
    ((label :initarg :label :initform NIL :accessor label)
     (description :initarg :description :initform NIL :accessor description)
     (tool :initarg :tool :initform (error "TOOL required.") :accessor tool)
     (on-change :initarg :on-change :initform NIL :accessor on-change)
     (slot :initarg :slot :initform NIL :accessor slot))
    (:documentation "Tool options are widgets that the user can interact with to change the tool's settings."))

  (define-slot change (tool-option (new-value bool int double "const QString&"))
    (declare (method))
    (when (on-change option)
      (setf new-value (funcall (on-change option) (parent option) option new-value)))
    (when (slot option)
      (setf (slot-value (tool option) (slot option)) new-value)))

  (define-initializer option 100
    (unless (label option)
      (setf (label option) (capitalize-on #\- (class-name (class-of option)) #\Space T)))))
