#|
 This file is a part of Parasol
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:parasol)
(named-readtables:in-readtable :qt)

(defclass layer-widget ()
  ((%list-widget :initform (#_new QListWidget) :accessor list-widget))
  (:metaclass qt-class)
  (:qt-superclass "QWidget"))

(defmethod initialize-instance :after ((widget layer-widget) &key)
  (new widget)
  (let ((layout (#_new QGridLayout))
        (button-add (#_new QPushButton "+"))
        (button-remove (#_new QPushButton "-")))
    (#_addWidget layout (#_new QLabel "Layers") 0 0 1 2)
    (#_addWidget layout (list-widget widget) 1 0 1 2)
    (#_addWidget layout button-add 2 0)
    (#_addWidget layout button-removes 2 1)
    (#_setLayout widget layout)))
