#|
 This file is a part of Parasol
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.parasol.tools.brush)
(named-readtables:in-readtable :qtools)

(defclass brush-class (finalizable-class)
  ((title :initarg :title :accessor brush-title)
   (display :initarg :display :accessor brush-option-display))
  (:default-initargs
   :title (error "TITLE required.")
   :display ())
  (:documentation ""))

(defun initialize-brush-class (class next-method &rest args &key title display &allow-other-keys)
  (when (consp title) (setf (getf args :title) (first title)))
  (apply next-method class :allow-other-keys T args)
  (c2mop:finalize-inheritance class)
  (dolist (slot display)
    (check-display-slot class slot)))

(defmethod initialize-instance :around ((class brush-class) &rest initargs)
  (apply #'initialize-brush-class class #'call-next-method initargs))

(defmethod reinitialize-instance :around ((class brush-class) &rest initargs)
  (apply #'initialize-brush-class class #'call-next-method initargs))

(defclass abstract-brush ()
  ()
  (:documentation "Superclass for brushes that only exist in-code and are not visible to the user."))

(defclass brush (finalizable abstract-brush)
  ((options :initform () :accessor brush-options))
  (:metaclass brush-class)
  (:label "Brush")
  (:documentation "Superclass for all user-usable brushes."))

(defmethod copy ((brush brush))
  (let ((copy (make-instance (class-name (class-of brush)))))
    (loop for slot in (c2mop:class-slots (class-of brush))
          for name = (c2mop:slot-definition-name slot)
          do (setf (slot-value copy name)
                   (slot-value brush name)))
    copy))

(defmacro define-superclass-method-wrapper (method)
  `(defmethod ,method ((brush brush))
     (,method (class-of brush))))

(define-superclass-method-wrapper brush-title)

(defgeneric draw-stroke (brush stroke target &optional offset))

;; Wrapper to make it neater and automatically assign proper meta/classes
(defmacro define-brush (name direct-superclasses direct-slots &body options)
  (destructuring-bind (name &optional (title (capitalize-on #\- name #\Space T)))
      (if (listp name) name (list name))
    (unless (apply #'parasol-tools::has-superclass 'brush direct-superclasses)
      (push 'brush direct-superclasses))
    (unless (assoc :title options)
      (push (list :title title) options))
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
