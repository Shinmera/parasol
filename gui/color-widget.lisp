#|
 This file is a part of Parasol
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:parasol)
(named-readtables:in-readtable :qt)

(defun %make-color-slider ()
  (let ((slider (#_new QSlider (#_Qt::Horizontal))))
    (#_setTickInterval slider 1)
    (#_setMaximum slider 255)
    (#_setMinimum slider 0)
    slider))

(defclass color-widget ()
  ((%r :initform (%make-color-slider) :accessor r)
   (%g :initform (%make-color-slider) :accessor g)
   (%b :initform (%make-color-slider) :accessor b))
  (:metaclass qt-class)
  (:qt-superclass "QWidget")
  (:slots ("updateColor(int)" update-color)))

(defmethod initialize-instance :after ((widget color-widget) &key)
  (new widget)
  (let ((layout (#_new QVBoxLayout))
        (r (r widget))
        (g (g widget))
        (b (b widget)))
    (#_addWidget layout r)
    (#_addWidget layout g)
    (#_addWidget layout b)
    (#_setLayout widget layout)
    (connect r "valueChanged(int)" widget "updateColor(int)")
    (connect g "valueChanged(int)" widget "updateColor(int)")
    (connect b "valueChanged(int)" widget "updateColor(int)")))

(defmethod update-color ((widget color-widget) val)
  (declare (ignore val))
  (let ((color (#_new QColor (#_value (r widget)) (#_value (g widget)) (#_value (b widget)))))
    (setf (color *current-brush*) color)))
