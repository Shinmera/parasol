#|
 This file is a part of Parasol
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.parasol.document)

(define-finalizable document (metadata meta-layer history)
  ((layer-counter :initform 0 :accessor layer-counter)))

(defmethod initialize-instance :after ((document document) &key)
  (add-layer document 0))

(defmethod draw ((document document) target)
  (loop for drawable across (drawables document)
        do (draw drawable target)))

(defgeneric current-layer (document)
  (:method ((document document))
    (current-drawable document)))

(defgeneric add-layer (document &optional at)
  (:method ((document document) &optional (at (1+ (current-index document))))
    (activate
     (insert (make-metadata-instance 'layer (:name (format NIL "Layer ~d" (incf (layer-counter document))))) document at)
     document)))

(defmethod extract :after ((index fixnum) (document document))
  (when (= 0 (length (drawables document)))
    (insert (make-instance 'layer) document))
  (setf (current-index document)
        (if (<= (size document) index)
            (1- index)
            index)))
