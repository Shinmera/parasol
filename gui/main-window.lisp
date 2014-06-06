#|
 This file is a part of Parasol
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:parasol)
(named-readtables:in-readtable :qt)

(defclass main-window ()
  ((%canvas-widget :accessor canvas-widget))
  (:metaclass qt-class)
  (:qt-superclass "QMainWindow"))

(defmethod initialize-instance :after ((window main-window) &key)
  (new window)
  (let ((canvas-widget (make-instance 'canvas-widget)))
    (setf (canvas-widget window) canvas-widget)
    (#_setWindowTitle window (format NIL "Parasol v~a" (asdf:component-version (asdf:find-system :parasol))))
    (#_resize window 500 500)

    (let ((central-splitter (#_new QSplitter (#_Qt::Horizontal)))
          (right-splitter (#_new QSplitter (#_Qt::Vertical))))
      (#_addWidget central-splitter canvas-widget)
      (#_addWidget central-splitter right-splitter)

      (#_setChildrenCollapsible right-splitter NIL)
      (#_addWidget right-splitter (make-instance 'brush-widget))
      (#_addWidget right-splitter (make-instance 'color-widget))
      (#_addWidget right-splitter (#_new QWidget))
      (#_setStretchFactor right-splitter 2 10)
      
      (#_setCentralWidget window central-splitter))))
