#|
 This file is a part of Parasol
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:parasol)
(named-readtables:in-readtable :qt)

(defun %make-color-slider (color-widget)
  (make-instance 'ex-slider-widget :max 255 :on-change #'(lambda (val) (declare (ignore val)) (color-widget-update color-widget))))

(defclass color-widget ()
  ((%r :accessor r)
   (%g :accessor g)
   (%b :accessor b))
  (:metaclass qt-class)
  (:qt-superclass "QWidget")
  (:slots ("updateColor(int)" update-color)))

(defmethod initialize-instance :after ((widget color-widget) &key)
  (new widget)
  (let ((layout (#_new QVBoxLayout))
        (r (%make-color-slider widget))
        (g (%make-color-slider widget))
        (b (%make-color-slider widget)))
    (#_setContentsMargins layout 0 0 0 0)
    (#_addWidget layout r)
    (#_addWidget layout g)
    (#_addWidget layout b)
    (#_setLayout widget layout)
    (setf (r widget) r
          (g widget) g
          (b widget) b)))

(defun color-widget-update (widget)
  (let ((color (#_new QColor (value (r widget)) (value (g widget)) (value (b widget)))))
    (setf (color *current-brush*) color)))
