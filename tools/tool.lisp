#|
 This file is a part of Parasol
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.parasol.tools)
(named-readtables:in-readtable :qtools)

(defclass tool (widget)
  ((options :initform (make-hash-table :test 'eql) :reader tool-options))
  (:metaclass tool-class)
  (:qt-superclass "QPushButton")
  (:label "Tool")
  (:documentation "Superclass for all document-manipulating tools.")
  (:slots ("change(bool)" change))
  (:initializer
   (tool 50
         ;; Instantiate all the options
         (loop for option in (tool-effective-options (class-of tool))
               do (destructuring-bind (name &rest args &key type &allow-other-keys) option
                    (let ((args (copy-list args)))
                      (remf args :type)
                      (loop for cons on args by #'cddr
                            do (setf (cadr cons) (eval (cadr cons))))
                      (setf (tool-option name tool)
                            (apply #'make-instance type :tool tool args)))))
         ;; Default init
         (#_setCheckable tool T)
         (#_setToolTip tool (format NIL "~a~@[: ~a~]" (tool-label tool) (tool-description tool)))
         (if (tool-icon tool)
             (#_setIcon tool (tool-icon tool))
             (#_setText tool (tool-label tool)))
         (connect! tool (toggled bool) tool (change bool)))))

(defmethod print-object ((tool tool) stream)
  (print-unreadable-object (tool stream :type T)
    (format stream "~s" (tool-label tool)))
  tool)

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

(defun tool-option (name tool)
  (gethash name (tool-options tool)))

(defun (setf tool-option) (option name tool)
  (setf (gethash name (tool-options tool))
        option))

(defmethod finalize :after ((tool tool))
  (dolist (option (tool-options tool))
    (finalize option)))

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
  (destructuring-bind (name &optional (label (capitalize-on #\- name #\Space T))
                                      (description ""))
      (if (listp name) name (list name))
    (unless (apply #'has-superclass 'tool direct-superclasses)
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

(declare-environment-widget-form 'define-tool)
