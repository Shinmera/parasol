#|
 This file is a part of Parasol
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.parasol.tools)
(named-readtables:in-readtable :qtools)

(defun initialize-options (tool)
  (loop for option in (if (slot-boundp (class-of tool) 'order)
                          (tool-option-order (class-of tool))
                          (mapcar #'car (tool-effective-options (class-of tool))))
        collect (destructuring-bind (name &rest args &key type &allow-other-keys)
                    (or (assoc option (tool-effective-options (class-of tool)))
                        (error "No such option ~s found on tool ~s" option tool))
                  (let ((args (copy-list args)))
                    (remf args :type)
                    (loop for cons on args by #'cddr
                          do (setf (cadr cons) (eval (cadr cons))))
                    (cons name (apply #'make-instance type :tool tool args)))) into options
        finally (setf (tool-options tool) options)))

(define-widget tool (QPushButton widget)
  ((options :initform () :accessor tool-options))
  (:metaclass tool-class)
  (:label "Tool")
  (:documentation "Superclass for all document-manipulating tools."))

(define-slot (tool change) ((activated bool))
  (if activated
      (select tool)
      (deselect tool)))

(define-initializer (tool setup 5)
  (initialize-options tool)
  ;; Default init
  (#_setMinimumWidth tool 50)
  (#_setSizePolicy tool (#_QSizePolicy::Maximum) (#_QSizePolicy::Maximum))
  (#_setCheckable tool T)
  (#_setFlat tool T)
  (#_setToolTip tool (format NIL "~a~@[: ~a~]" (tool-label tool) (tool-description tool)))
  (if (tool-icon tool)
      (#_setIcon tool (tool-icon tool))
      (#_setText tool (tool-label tool)))
  (connect! tool (toggled bool) tool (change bool)))

(defmethod print-object ((tool tool) stream)
  (print-unreadable-object (tool stream :type T)
    (format stream "~s" (tool-label tool)))
  tool)

(defun tool-option (name tool)
  (cdr (assoc name (tool-options tool))))

(defmacro define-superclass-method-wrapper (method)
  `(defmethod ,method ((tool tool))
     (,method (class-of tool))))

(define-superclass-method-wrapper tool-label)
(define-superclass-method-wrapper tool-description)
(define-superclass-method-wrapper tool-icon)

(defmethod finalize :after ((tool tool))
  (loop for (name . option) in (tool-options tool)
        do (finalize option)))

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
    `(define-widget ,name (QPushButton ,@direct-superclasses)
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
