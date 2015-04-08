#|
 This file is a part of Parasol
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.parasol.tools)
(named-readtables:in-readtable :qtools)

(defclass configurable-class (finalizable-class)
  ((configurable :initarg :configurable :accessor configurable-slots :type list))
  (:default-initargs
   :configurable ())
  (:documentation "Metaclass for classes that are externally configurable through a list of known slots."))

(defun check-configurable-slot (class name)
  (when (not (find name (c2mop:class-slots class) :key #'c2mop:slot-definition-name))
    (error "Cannot find an effective slot named ~s on class ~s, but it is set as a display slot."
           name class)))

(defun initialize-configurable-class (class next-method &rest args &key configurable &allow-other-keys)
  (apply next-method class :allow-other-keys T args)
  (c2mop:finalize-inheritance class)
  (dolist (slot configurable)
    (check-configurable-slot class slot)))

(defmethod initialize-instance :around ((class configurable-class) &rest initargs)
  (apply #'initialize-configurable-class class #'call-next-method initargs))

(defmethod reinitialize-instance :around ((class configurable-class) &rest initargs)
  (apply #'initialize-configurable-class class #'call-next-method initargs))

(defclass configurable (finalizable)
  ()
  (:metaclass configurable-class))

(defmacro define-superclass-method-wrapper (class method &optional (target-method method))
  `(defmethod ,method ((,class ,class))
     (,target-method (class-of ,class))))

(define-superclass-method-wrapper configurable configurable-slots)


(defclass descriptive-class (finalizable-class)
  ((title :initarg :title :accessor object-title :type string)
   (description :initarg :description :accessor object-description :type string))
  (:documentation "Metaclass for tools that operate on the document. Required for special tool options definition."))

(defun concat (&rest args)
  (reduce #'(lambda (a b) (concatenate 'string a b)) args))

(defun initialize-descriptive-class (class next-method &rest args &key name title description &allow-other-keys)
  (when (consp title) (setf (getf args :title) (concat title)))
  (when (consp description) (setf (getf args :description) (concat title)))
  (unless title (setf (getf args :title) (symbol-name name)))
  (unless description (setf (getf args :description) ""))
  (apply next-method class :allow-other-keys T args))

(defmethod initialize-instance :around ((class descriptive-class) &rest initargs)
  (apply #'initialize-descriptive-class class #'call-next-method initargs))

(defmethod reinitialize-instance :around ((class descriptive-class) &rest initargs)
  (apply #'initialize-descriptive-class class #'call-next-method initargs))
