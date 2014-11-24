#|
 This file is a part of Parasol
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.parasol.document)

(define-finalizable metadata ()
  ((fields :initform (make-hash-table :test 'eql) :reader fields :finalized T)))

(defgeneric field (field metadata)
  (:method (field (data metadata))
    (gethash field (fields data))))

(defgeneric (setf field) (value field metadata)
  (:method (value field (data metadata))
    (setf (gethash field (fields data)) value)))

(defmacro define-metadata-accessor (accessor &optional (field accessor))
  `(defmethod ,accessor ((data metadata))
     (gethash ,(intern (string field) "KEYWORD") (fields data))))

(defgeneric matches (metadata field &optional value)
  (:method ((data metadata) field &optional (value NIL v-p))
    (multiple-value-bind (val found) (field field data)
      (when found
        (or (not v-p)
            (equal value val))))))

(defmacro make-metadata-instance (class (&rest metadata) &rest args)
  (let ((instance (gensym "INSTANCE")))
    `(let ((,instance (make-instance ,class ,@args)))
       ,@(loop for (key val) on metadata by #'cddr
               collect `(setf (field ,key ,instance) ,val))
       ,instance)))
