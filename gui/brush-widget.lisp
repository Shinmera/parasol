#|
 This file is a part of Parasol
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:parasol)
(named-readtables:in-readtable :qt)

(defclass brush-widget ()
  ((%brush :accessor brush)
   (%brush-layout :accessor brush-layout)
   (%brush-elements :initform () :accessor brush-elements))
  (:metaclass qt-class)
  (:qt-superclass "QWidget")
  (:slots ("setBrush(const QString)" brush-widget-set)))

(defmethod initialize-instance :after ((widget brush-widget) &key)
  (new widget)
  (let ((layout (#_new QGridLayout))
        (brush-layout (#_new QVBoxLayout))
        (brush (#_new QComboBox)))
    (setf (brush widget) brush
          (brush-layout widget) brush-layout)
    (#_addWidget layout (#_new QLabel "Brush") 0 0)
    (#_addLayout layout brush-layout 2 0)
    (#_addWidget layout brush 1 0)
    (#_setLayout widget layout)
    (connect brush "currentIndexChanged(const QString)" widget "setBrush(const QString)")
    (brush-widget-update-choices widget)
    (brush-widget-set widget (string (class-name (class-of (current-brush *window*)))))))

(defun brush-widget-update-choices (widget)
  (#_clear (brush widget))
  (loop for class in (closer-mop:class-direct-subclasses (find-class 'brush))
        do (#_addItem (brush widget) (string-downcase (class-name class)))))

(defun brush-widget-set (widget name)
  (let ((class-name (find-symbol (string-upcase name) "PARASOL")))
    (when class-name
      (let ((brush (make-instance class-name)))
        (dolist (element (brush-elements widget))
          (#_removeWidget (brush-layout widget) element)
          (finalize element)
          (maybe-delete-qobject element))
        (setf (current-brush *window*) brush)
        (let ((elements (brush-ui brush)))
          (setf (brush-elements widget) elements)
          (dolist (element elements)
            (#_addWidget (brush-layout widget) element)))))))

(defmethod finalize ((widget brush-widget))
  (dolist (element (brush-elements widget))
    (finalize element)
    (maybe-delete-qobject element))
  (maybe-delete-qobject (brush widget))
  (maybe-delete-qobject (brush-layout widget))
  (setf (brush-layout widget) NIL
        (brush-elements widget) NIL
        (brush widget) NIL))
