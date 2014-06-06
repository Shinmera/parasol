#|
 This file is a part of Parasol
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:parasol)
(named-readtables:in-readtable :qt)

(defun %make-size-slider ()
  (make-instance 'ex-slider-widget :max 5000 :default 500 :on-change #'brush-widget-update))

(defclass brush-widget ()
  ((%size :accessor size))
  (:metaclass qt-class)
  (:qt-superclass "QWidget"))

(defmethod initialize-instance :after ((widget brush-widget) &key)
  (new widget)
  (let ((layout (#_new QVBoxLayout))
        (size (%make-size-slider)))
    (#_setSpacing layout 0)
    (#_setContentsMargins layout 0 0 0 0)
    (#_addWidget layout size)
    (#_setLayout widget layout)
    (setf (size widget) size)))

(defun brush-widget-update (size)
  (setf (base-size *current-brush*) (/ size 100)))
