#|
 This file is a part of Parasol
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.parasol.dev)

(defvar *hooks* (make-hash-table :test 'eql))

(defclass hook ()
  ((name :initarg :name :accessor name)
   (func :initarg :func :accessor func)
   (priority :initarg :priority :reader priority))
  (:default-initargs
   :name (error "NAME required.")
   :func (error "FUNC required.")
   :priority 0))

(defun hooks (trigger)
  (gethash trigger *hooks*))

(defun (setf hooks) (trigger hooks)
  (setf (gethash trigger *hooks*)
        (sort hooks #'> :key #'priority)))

(defun add-hook (trigger hook)
  (when (find (name hook) (hooks trigger) :key #'name)
    (remove-hook trigger hook))
  (push hook (hooks trigger)))

(defun remove-hook (trigger hook)
  (setf (hooks trigger)
        (remove hook (hooks trigger))))

(defmacro define-hook ((trigger name &optional (priority 0)) args &body body)
  `(add-hook ',trigger (make-instance 'hook
                                      :name ',name
                                      :priority ,priority
                                      :func (lambda ,args ,@body))))

(defun trigger (trigger &rest args)
  (dolist (hook (hooks trigger))
    (apply (func hook) args)))
