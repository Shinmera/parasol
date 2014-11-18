#|
 This file is a part of Parasol
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.parasol.document)

(defclass history-item ()
  ((document :initarg :document :initform (error "Document required.") :reader document)))

(defgeneric undo (history-item))

(defgeneric redo (history-item))

(defclass history ()
  ((items :initarg :items :initform () :accessor items)))

(defgeneric record (item history)
  (:method ((item history-item) (history history))
    (push item (items history))
    item))

(defgeneric rewind (history)
  (:method ((history history))
    (let ((item (pop (items history))))
      (undo item)
      item)))

(defgeneric size (history)
  (:method ((history history))
    (length (items history))))
