#|
 This file is a part of Parasol
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.parasol)

(defclass metadata ()
  ((fields :initform (make-hash-table :test 'eql) :reader fields)))

(defgeneric field (field metadata)
  (:method (field (data metadata))
    (gethash field (fields data))))

(defgeneric (setf field) (value field metadata)
  (:method (value field (data metadata))
    (setf (gethash field (fields data)) value)))

(defmacro define-metadata-accessor (accessor &optional (field accessor))
  `(defmethod ,accessor ((data metadata))
     (gethash ,(intern (string field) "KEYWORD") (fields data))))
