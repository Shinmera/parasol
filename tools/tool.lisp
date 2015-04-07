#|
 This file is a part of Parasol
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.parasol.tools)
(named-readtables:in-readtable :qtools)

(defclass tool-class (finalizable-class)
  ((title :initarg :title :accessor tool-title :type string)
   (description :initarg :description :accessor tool-description :type string)
   (display :initarg :display :accessor tool-display :type list))
  (:default-initargs
   :title (error "TITLE required.")
   :description ""
   :display ())
  (:documentation "Metaclass for tools that operate on the document. Required for special tool options definition."))

(defun check-display-slot (class name)
  (when (not (find name (c2mop:class-slots class) :key #'c2mop:slot-definition-name))
    (error "Cannot find an effective slot named ~s on class ~s, but it is set as a display slot."
           name class)))

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


(defclass tool (finalizable)
  ()
  (:metaclass tool-class)
  (:title "Tool")
  (:documentation "Superclass for all document-manipulating tools."))

(defmacro define-superclass-method-wrapper (method)
  `(defmethod ,method ((tool tool))
     (,method (class-of tool))))

(define-superclass-method-wrapper tool-title)
(define-superclass-method-wrapper tool-description)
(define-superclass-method-wrapper tool-display)

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
