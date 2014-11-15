#|
 This file is a part of Parasol
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.parasol)
(named-readtables:in-readtable :qtools)

(defclass tool ()
  ((options :initform (make-hash-table :test 'eql) :reader tool-options))
  (:metaclass tool-class)
  (:qt-superclass "QPushButton")
  (:label "Tool")
  (:documentation "Superclass for all document-manipulating tools.")
  (:slots ("change(bool)" change)))

(defmethod change ((tool tool) activated)
  (if activated
      (select tool)
      (deselect tool)))

(defmacro define-superclass-method-wrapper (method)
  `(defmethod ,method ((tool tool))
     (,method (class-of tool))))

(define-superclass-method-wrapper tool-label)
(define-superclass-method-wrapper tool-description)
(define-superclass-method-wrapper tool-icon)

(defmethod initialize-instance :after ((tool tool) &key)
  ;; Instantiate all the options
  (loop for option in (tool-effective-options (class-of tool))
        do (destructuring-bind (name &rest args &key type &allow-other-keys) option
             (remf args :type)
             (setf (gethash name (tool-options tool))
                   (apply #'make-instance type :tool tool args))))
  (#_setCheckable tool T)
  (#_setTooltip (format NIL "~a~@[: ~a~]" (tool-label tool) (tool-description tool)))
  (if (tool-icon tool)
      (#_setIcon tool (tool-icon tool))
      (#_setText tool (tool-label tool)))
  (connect! tool (toggled bool) tool (change bool)))

(defmethod finalize :after ((tool tool))
  (dolist (option (tool-options tool))
    (finalize option)))

;; Tool method stubs
(defgeneric select (tool)
  (:method ((tool tool))))

(defgeneric deselect (tool)
  (:method ((tool tool))))

(defgeneric begin (tool pen)
  (:method ((tool tool) pen)))

(defgeneric move (tool pen)
  (:method ((tool tool) pen)))

(defgeneric end (tool pen)
  (:method ((tool tool) pen)))

;; Wrapper to make it neater and automatically assign proper meta/classes
(defmacro define-tool (name direct-superclasses direct-slots &body options)
  (destructuring-bind (name &optional (label (capitalize-on #\- name #\Space T))
                                      (description ""))
      (if (listp name) name (list name))
    (when (loop for superclass in direct-superclasses
                never (typep superclass 'tool))
      (push 'tool direct-superclasses))
    (unless (assoc :label options)
      (push (list :label label) options))
    (unless (assoc :description options)
      (push (list :description description) options))
    `(defclass ,name ,direct-superclasses
       ,direct-slots
       (:metaclass tool-class)
       ,@options)))

(indent:define-indentation define-tool
    (4 (&whole 6 &rest)
       (&whole 2 (&whole 0 0 &rest 2))
       &rest (&whole 2 2 &rest (&whole 2 2 4 &body))))
