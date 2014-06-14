#|
 This file is a part of Parasol
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:parasol)
(named-readtables:in-readtable :qt)

(defclass brush-widget ()
  ((%brush :accessor brush))
  (:metaclass qt-class)
  (:qt-superclass "QWidget")
  (:slots ("setBrush(const QString)" brush-widget-set)))

(defmethod initialize-instance :after ((widget brush-widget) &key)
  (new widget)
  (let ((layout (#_new QGridLayout))
        (brush (#_new QComboBox)))
    (setf (brush widget) brush)
    (brush-widget-update-choices widget)
    (#_addWidget layout (#_new QLabel "Brush") 0 0)
    (#_addWidget layout brush 2 0)
    (#_setLayout widget layout)
    (connect brush "currentIndexChanged(const QString)" widget "setBrush(const QString)")))

(defun brush-widget-update-choices (widget)
  (#_clear (brush widget))
  (loop for class in (closer-mop:class-direct-subclasses (find-class 'brush))
        do (#_addItem (brush widget) (string-downcase (class-name class)))))

(defun brush-widget-set (widget name)
  (let ((class-name (find-symbol (string-upcase name) "PARASOL")))
    (when class-name
      (setf (current-brush *window*) (make-instance class-name)))))

(defmethod finalize ((widget brush-widget))
  (finalize (size widget))
  (maybe-delete-qobject (brush widget))
  (setf (size widget) NIL
        (brush widget) NIL))
