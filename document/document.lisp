#|
 This file is a part of Parasol
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.parasol.document)

(defclass document (metadata meta-layer history)
  ())

(defmethod initialize-instance :after ((document document) &key)
  (insert (make-instance 'layer) document))

(defmethod draw ((document document) target)
  (loop for drawable across (drawables document)
        do (draw drawable target)))

(defgeneric current-layer (document)
  (:method ((document document))
    (current-drawable document)))

(defmethod extract :after (drawable (document document))
  (when (= 0 (length (drawables document)))
    (insert (make-instance 'layer) document)))
