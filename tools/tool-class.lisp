#|
 This file is a part of Parasol
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.parasol.tools)
(named-readtables:in-readtable :qtools)

(defclass tool-class (standard-class)
  ((title :initarg :title :accessor tool-title :type string)
   (description :initarg :description :accessor tool-description :type string)
   (display :initarg :display :accessor tool-display :type list))
  (:default-initargs
   :name (error "NAME required.")
   :description "")
  (:documentation "Metaclass for tools that operate on the document. Required for special tool options definition."))

(defmethod c2mop:validate-superclass ((class tool-class) (superclass t))
  NIL)

(defmethod c2mop:validate-superclass ((class standard-class) (superclass tool-class))
  T)

(defmethod c2mop:validate-superclass ((class tool-class) (superclass standard-class))
  T)

(defmethod c2mop:validate-superclass ((class tool-class) (superclass tool-class))
  T)

(defun check-display-slot (class name)
  (when (not (find name (c2mop:class-slots class) :key #'c2mop:slot-definition-name))
    (error "Cannot find an effective slot named ~s on class ~s, but it is set as a display slot."
           slot class)))

;; During initialisation we just make sure the options are proper.
(defun initialize-tool-class (class next-method &rest args &key title description display &allow-other-keys)
  (when (consp title) (setf (getf args :title) (first title)))
  (when (consp description) (setf (getf args :description) (first description)))
  (apply next-method class :allow-other-keys T args)
  (c2mop:finalize-inheritance class)
  (dolist (slot display)
    (check-display-slot class slot)))

(defmethod initialize-instance :around ((class tool-class) &rest initargs)
  (apply #'initialize-tool-class class #'call-next-method initargs))

(defmethod reinitialize-instance :around ((class tool-class) &rest initargs)
  (apply #'initialize-tool-class class #'call-next-method initargs))
