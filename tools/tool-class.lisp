#|
This file is a part of Parasol
(c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.parasol)
(named-readtables:in-readtable :qtools)

(defclass tool-class (widget-class)
  ((direct-options :initform () :initarg :options :accessor tool-direct-options)
   (effective-options :initform () :accessor tool-effective-options)
   (icon :initform NIL :initarg :icon :accessor tool-icon)
   (label :initform NIL :initarg :label :accessor tool-label)
   (description :initform NIL :initarg :description :accessor tool-description))
  (:documentation "Metaclass for tools that operate on the document. Required for special tool options definition."))

(defmethod c2mop:validate-superclass ((class tool-class) (superclass t))
  nil)

(defmethod c2mop:validate-superclass ((class standard-class) (superclass tool-class))
  nil)

(defmethod c2mop:validate-superclass ((class tool-class) (superclass standard-class))
  t)

(defmethod c2mop:validate-superclass ((class tool-class) (superclass tool-class))
  t)

(defun check-option (class name &key label description type slot &allow-other-keys)
  (when (and slot (not (find slot (c2mop:class-slots class) :key #'c2mop:slot-definition-name)))
    (error "Cannot find an effective slot named ~s on class ~s, but it is set as the target slot for tool option ~s."
           slot class name))
  (unless (and type (find-class type))
    (error "No class of name ~s found for tool option ~s on ~s."
           type name class))
  (unless (typep (find-class type) 'tool-option)
    (error "The class ~s specified for the type of the tool option ~s on ~s is not a subclass of TOOL-OPTION."
           type name class))
  (etypecase label (null) (string))
  (etypecase description (null) (string)))

;; During initialisation we just make sure the options are proper.
(defun initialize-tool-class (class next-method &rest args &key options &allow-other-keys)
  (dolist (option options)
    (apply #'check-option class option))
  (apply next-method class :allow-other-keys T args))

(defmethod initialize-instance :around ((class tool-class) &rest initargs)
  (apply #'initialize-tool-class class #'call-next-method initargs))

(defmethod reinitialize-instance :around ((class tool-class) &rest initargs)
  (apply #'initialize-tool-class class #'call-next-method initargs))

(defmethod initialize-instance :after ((class tool-class) &key)
  (unless (tool-label class)
    (setf (tool-label class)
          (capitalize-on #\- (class-name class) #\Space T))))

;; Collect all inherited and direct options.
(defun collect-inherited-option-definitions (class &optional (direct-superclasses (c2mop:class-direct-superclasses class)))
  (loop for superclass in direct-superclasses
        unless (c2mop:class-finalized-p superclass)
        do (c2mop:finalize-inheritance superclass)
        when (typep superclass 'tool-class)
        append (loop for option in (tool-effective-options superclass)
                     unless (find (car option) options :key #'car)
                     collect option) into options))

(defun compute-effective-options (class &key (direct-superclasses (c2mop:class-direct-superclasses class))
                                             (options (tool-direct-options class)))
  (append (remove-if #'(lambda (a) (find a options :key #'car))
                     (collect-inherited-option-definitions class direct-superclasses))
          (remove :remove options :test #'find)))

;; This way we probably recalculate options quite often
;; it's compile time so it doesn't matter much, but still.
(defun cascade-option-changes (class)
  (setf (tool-effective-options class) (compute-effective-options class))
  (loop for sub-class in (c2mop:class-direct-subclasses class)
        when (and (typep sub-class 'brush-class)
                  (c2mop:class-finalized-p sub-class))
        do (cascade-option-changes sub-class)))

;; Hook in here so we can compute the effective options.
(defmethod c2mop:finalize-inheritance :after ((class tool-class))
  (dolist (super (c2mop:class-direct-superclasses class))
    (unless (c2mop:class-finalized-p super)
      (c2mop:finalize-inheritance super)))
  (cascade-option-changes class))
