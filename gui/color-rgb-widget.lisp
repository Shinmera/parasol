#|
 This file is a part of Parasol
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:parasol)
(named-readtables:in-readtable :qt)

(defun %make-color-slider (color-widget)
  (make-instance 'ex-slider-widget :max 255 :on-change #'(lambda (val) (declare (ignore val)) (color-rgb-widget-update color-widget))))

(defclass color-rgb-widget ()
  ((%r :accessor r)
   (%g :accessor g)
   (%b :accessor b)
   (%parent :initarg :parent :initform (error "Parent required") :accessor parent))
  (:metaclass qt-class)
  (:qt-superclass "QWidget"))

(defmethod initialize-instance :after ((widget color-rgb-widget) &key)
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

(defmethod color ((widget color-rgb-widget))
  (#_new QColor (value (r widget)) (value (g widget)) (value (b widget))))

(defun color-rgb-widget-update (widget)
  (color-widget-update (parent widget) (color widget)))

(defmethod color-widget-update ((widget color-rgb-widget) new-color)
  (exs-update (r widget) (#_red new-color))
  (exs-update (g widget) (#_green new-color))
  (exs-update (b widget) (#_blue new-color)))
