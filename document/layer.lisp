#|
 This file is a part of Parasol
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.parasol)

(defclass meta-layer (drawable)
  ((drawables :initform (make-array 20 :element-type 'drawable :adjustable T :fill-pointer 0) :reader drawables)
   (current-drawable :initform NIL :accessor current-drawable)))

(defmethod insert (drawable layer &optional position)
  (:method ((drawable drawable) (layer meta-layer) &optional position)
    (if position
        (vector-push-extend-position drawable (drawables layer) position)
        (vector-push-extend drawable (drawables layer)))))

(defmethod extract (drawable layer)
  (:method ((drawable drawable) (layer meta-layer))
    (extract (find drawable (drawables layer)) layer))

  (:method ((index fixnum) (layer meta-layer))
    (vector-pop-position (drawables layer) index)))

(defmethod drawable-at (index layer)
  (:method ((index fixnum) (layer meta-layer))
    (aref (drawables layer) index)))

(defmethod activate (drawable layer)
  (:method ((drawable drawable) (layer meta-layer))
    (setf (current-drawable layer) drawable))

  (:method ((index fixnum) (layer meta-layer))
    (activate (drawable-at index layer) layer)))

(defmethod size (layer)
  (:method ((layer meta-layer))
    (length (drawables layer))))

(defclass layer (meta-layer buffered positioned metadata)
  ((opacity :initarg :opacity :initform 1 :accessor opacity)
   (mode :initarg :mode :initform 0 #|source-over|# :accessor mode)
   (visible :initarg :visible :initform T :accessor visible)))
