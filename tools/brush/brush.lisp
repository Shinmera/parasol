#|
 This file is a part of Parasol
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.parasol.tools.brush)
(named-readtables:in-readtable :qtools)

;; Again, mostly what TOOL does, but with some adaptations that
;; make it more suited towards the brush env.

(defclass abstract-brush ()
  ()
  (:documentation "Superclass for brushes that only exist in-code and are not visible to the user."))

(defclass brush (abstract-brush)
  ((options :initform (make-hash-table :test 'eql) :reader brush-options))
  (:metaclass brush-class)
  (:label "Tool")
  (:documentation "Superclass for all user-usable brushes."))

(defmethod print-object ((brush brush) stream)
  (print-unreadable-object (brush stream :type T)
    (format stream "~s" (brush-label brush)))
  brush)

(defmacro define-superclass-method-wrapper (method)
  `(defmethod ,method ((brush brush))
     (,method (class-of brush))))

(define-superclass-method-wrapper brush-label)
(define-superclass-method-wrapper brush-icon)

(defmethod initialize-instance :after ((brush brush) &key)
  ;; Instantiate all the options
  (loop for option in (brush-effective-options (class-of brush))
        do (destructuring-bind (name &rest args &key type &allow-other-keys) option
             (let ((args (copy-list args)))
               (remf args :type)
               (loop for cons on args by #'cddr
                     do (setf (cadr cons) (eval (cadr cons))))
               (setf (gethash name (brush-options brush))
                     (apply #'make-instance type :brush brush args))))))

(defmethod finalize :after ((brush brush))
  (dolist (option (brush-options brush))
    (finalize option)))

(defgeneric draw-stroke (brush stroke target &optional offset))

;; Wrapper to make it neater and automatically assign proper meta/classes
(defmacro define-brush (name direct-superclasses direct-slots &body options)
  (destructuring-bind (name &optional (label (capitalize-on #\- name #\Space T)))
      (if (listp name) name (list name))
    (unless (apply #'parasol-tools::has-superclass 'brush direct-superclasses)
      (push 'brush direct-superclasses))
    (unless (assoc :label options)
      (push (list :label label) options))
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
