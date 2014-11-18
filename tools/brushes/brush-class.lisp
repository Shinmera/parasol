#|
 This file is a part of Parasol
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.parasol)
(named-readtables:in-readtable :qtools)

;; This whole file is mostly a copy of what TOOL-CLASS does,
;; with some very minor modifications.

(defclass brush-class (standard-class)
  ((direct-options :initform () :initarg :options :accessor brush-direct-options)
   (effective-options :initform () :accessor brush-effective-options)
   (icon :initform NIL :initarg :icon :accessor brush-icon)
   (label :initform NIL :initarg :label :accessor brush-label))
  (:documentation ""))

(defmethod c2mop:validate-superclass ((class brush-class) (superclass t))
  nil)

(defmethod c2mop:validate-superclass ((class standard-class) (superclass brush-class))
  nil)

(defmethod c2mop:validate-superclass ((class brush-class) (superclass standard-class))
  t)

(defmethod c2mop:validate-superclass ((class brush-class) (superclass brush-class))
  t)

(defun check-option (class name &key label type slot &allow-other-keys)
  (when (and slot (not (find (eval slot) (c2mop:class-slots class) :key #'c2mop:slot-definition-name)))
    (error "Cannot find an effective slot named ~s on class ~s, but it is set as the target slot for tool option ~s."
           slot class name))
  (unless (and type (find-class type))
    (error "No class of name ~s found for tool option ~s on ~s."
           type name class))
  (unless (c2mop:subclassp (find-class type) (find-class 'tool-option))
    (error "The class ~s specified for the type of the tool option ~s on ~s is not a subclass of TOOL-OPTION."
           type name class))
  (etypecase label (null) (string)))

;; During initialisation we just make sure the options are proper.
(defun initialize-brush-class (class next-method &rest args &key options label &allow-other-keys)
  (remf args :label)
  (apply next-method class
         :allow-other-keys T
         :label (if (listp label) (first label) label)
         args)
  (c2mop:finalize-inheritance class)
  (dolist (option options)
    (apply #'check-option class option)))

(defmethod initialize-instance :around ((class brush-class) &rest initargs)
  (apply #'initialize-brush-class class #'call-next-method initargs))

(defmethod reinitialize-instance :around ((class brush-class) &rest initargs)
  (apply #'initialize-brush-class class #'call-next-method initargs))

(defmethod initialize-instance :after ((class brush-class) &key)
  (unless (brush-label class)
    (setf (brush-label class)
          (capitalize-on #\- (class-name class) #\Space T))))

;; Collect all inherited and direct options.
(defun collect-inherited-option-definitions (class &optional (direct-superclasses (c2mop:class-direct-superclasses class)))
  (loop for superclass in direct-superclasses
        unless (c2mop:class-finalized-p superclass)
        do (c2mop:finalize-inheritance superclass)
        when (typep superclass 'brush-class)
        append (loop for option in (brush-effective-options superclass)
                     unless (find (car option) options :key #'car)
                     collect option) into options))

(defun compute-effective-options (class &key (direct-superclasses (c2mop:class-direct-superclasses class))
                                             (options (brush-direct-options class)))
  (append (remove-if #'(lambda (a) (find a options :key #'car))
                     (collect-inherited-option-definitions class direct-superclasses))
          (remove :remove options :test #'find)))

;; Hook in here so we can compute the effective options.
(defmethod c2mop:finalize-inheritance :after ((class brush-class))
  (labels ((cascade-option-changes (class)
             (setf (brush-effective-options class) (compute-effective-options class))
             (loop for sub-class in (c2mop:class-direct-subclasses class)
                   when (and (typep sub-class 'brush-class)
                             (c2mop:class-finalized-p sub-class))
                   do (cascade-option-changes sub-class))))
    (dolist (super (c2mop:class-direct-superclasses class))
      (unless (c2mop:class-finalized-p super)
        (c2mop:finalize-inheritance super)))
    (cascade-option-changes class)))
