#|
 This file is a part of Parasol
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.parasol.tools)
(named-readtables:in-readtable :qtools)

(defclass tool-class (configurable-class descriptive-class)
  ()
  (:documentation "Metaclass for tools that operate on the document. Required for special tool options definition."))

(defclass tool (configurable)
  ()
  (:metaclass tool-class)
  (:documentation "Superclass for all document-manipulating tools."))

(define-superclass-method-wrapper tool configurable-slots)
(define-superclass-method-wrapper tool tool-title object-title)
(define-superclass-method-wrapper tool tool-description object-description)

;; Tool method stubs
(defgeneric select (tool)
  (:method ((tool tool))
    (v:debug :tool "[STUB] (SELECT ~s)" tool)))

(defgeneric deselect (tool)
  (:method ((tool tool))
    (v:debug :tool "[STUB] (DESELECT ~s)" tool)))

(defgeneric begin (tool pen document)
  (:method ((tool tool) pen document)
    (v:debug :tool "[STUB] (BEGIN ~s ~s ~s)" tool pen document)))

(defgeneric move (tool pen document)
  (:method ((tool tool) pen document)
    (v:debug :tool "[STUB] (MOVE ~s ~s ~s)" tool pen document)))

(defgeneric end (tool pen document)
  (:method ((tool tool) pen document)
    (v:debug :tool "[STUB] (END ~s ~s ~s)" tool pen document)))

(defun has-superclass (superclass &rest classes)
  (let ((superclass (etypecase superclass
                      (class superclass)
                      (symbol (find-class superclass)))))
    (loop for name in classes
          for class = (etypecase name
                        (class name)
                        (symbol (find-class name)))
          do (c2mop:finalize-inheritance class)
          thereis (c2mop:subclassp class superclass))))

;; Wrapper to make it neater and automatically assign proper meta/classes
(defmacro define-tool (name direct-superclasses direct-slots &body options)
  (destructuring-bind (name &optional (title (capitalize-on #\- name #\Space T))
                                      (description ""))
      (if (listp name) name (list name))
    (unless (apply #'has-superclass 'tool direct-superclasses)
      (push 'tool direct-superclasses))
    (unless (assoc :title options)
      (push (list :title title) options))
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

(defun find-tools ()
  (let ((classes ()))
    (labels ((scan (class)
               (c2mop:finalize-inheritance class)
               (dolist (subclass (c2mop:class-direct-subclasses class))
                 (pushnew subclass classes)
                 (scan subclass))))
      (scan (find-class 'tool)))
    (sort classes #'string< :key #'class-name)))
