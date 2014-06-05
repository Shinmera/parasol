#|
 This file is a part of Parasol
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:parasol)
(named-readtables:in-readtable :qt)

(defun %make-size-slider ()
  (let ((slider (#_new QSlider (#_Qt::Horizontal))))
    (#_setTickInterval slider 1)
    (#_setMaximum slider 1000)
    (#_setMinimum slider 0)
    slider))

(defclass brush-widget ()
  ((%size :initform (%make-size-slider) :accessor size))
  (:metaclass qt-class)
  (:qt-superclass "QWidget")
  (:slots ("updateSize(int)" update-size)))

(defmethod initialize-instance :after ((widget brush-widget) &key)
  (new widget)
  (let ((layout (#_new QVBoxLayout))
        (size (size widget)))
    (#_addWidget layout size)
    (#_setLayout widget layout)
    (connect size "valueChanged(int)" widget "updateSize(int)")))

(defmethod update-size ((widget brush-widget) size)
  (setf (base-size *current-brush*) (/ size 100)))
