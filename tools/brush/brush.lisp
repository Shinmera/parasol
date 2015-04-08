#|
 This file is a part of Parasol
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.parasol.tools.brush)
(named-readtables:in-readtable :qtools)

(defclass brush-class (configurable-class descriptive-class)
  ()
  (:documentation ""))

(defclass abstract-brush ()
  ()
  (:documentation "Superclass for brushes that only exist in-code and are not visible to the user."))

(define-finalizable brush (configurable abstract-brush)
  ((options :initform () :accessor brush-options))
  (:metaclass brush-class)
  (:documentation "Superclass for all user-usable brushes."))

(defmethod copy ((brush brush))
  (let ((copy (make-instance (class-name (class-of brush)))))
    (loop for slot in (c2mop:class-slots (class-of brush))
          for name = (c2mop:slot-definition-name slot)
          do (setf (slot-value copy name)
                   (slot-value brush name)))
    copy))

(define-superclass-method-wrapper brush brush-title object-title)

;; Wrapper to make it neater and automatically assign proper meta/classes
(defmacro define-brush (name direct-superclasses direct-slots &body options)
  (destructuring-bind (name &optional (title (capitalize-on #\- name #\Space T)) (description ""))
      (if (listp name) name (list name))
    (unless (apply #'parasol-tools::has-superclass 'brush direct-superclasses)
      (push 'brush direct-superclasses))
    (unless (assoc :title options)
      (push (list :title title) options))
    (unless (assoc :description options)
      (push (list :description description) options))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (defclass ,name ,direct-superclasses
         ,direct-slots
         (:metaclass brush-class)
         ,@options))))

(indent:define-indentation define-brush
    (4 (&whole 6 &rest)
       (&whole 2 (&whole 0 0 &rest 2))
       &rest (&whole 2 2 &rest (&whole 2 2 4 &body))))

(defun find-brushes ()
  (let ((classes ()))
    (labels ((scan (class)
               (c2mop:finalize-inheritance class)
               (unless (find 'abstract-brush (c2mop:class-direct-superclasses class)
                             :key #'class-name)
                 (pushnew class classes))
               (dolist (subclass (c2mop:class-direct-subclasses class))
                 (scan subclass))))
      (scan (find-class 'brush)))
    (sort classes #'string< :key #'class-name)))
