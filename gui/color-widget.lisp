#|
 This file is a part of Parasol
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:parasol)
(named-readtables:in-readtable :qt)

(defclass color-widget ()
  ((%rgb-widget :accessor rgb-widget)
   (%hsv-widget :accessor hsv-widget)
   (%color-history :initform (make-array (length (color-history *window*))) :accessor color-history))
  (:metaclass qt-class)
  (:qt-superclass "QWidget"))

(defmethod initialize-instance :after ((widget color-widget) &key)
  (new widget)
  (let ((rgb (make-instance 'color-rgb-widget :parent widget))
        (hsv (make-instance 'color-hsv-widget :parent widget))
        (tabs (#_new QTabWidget))
        (layout (#_new QGridLayout)))    
    (#_addTab tabs rgb "rgb")
    (#_addTab tabs hsv "hsv")

    (#_setSpacing layout 1)
    (#_addWidget layout (#_new QLabel "Colour") 0 0 1 (length (color-history widget)))
    (#_addWidget layout tabs 1 0 1 (length (color-history widget)))
    (loop for i from 0 below (length (color-history widget))
          do (let ((label (#_new QLabel)))
               (setf (aref (color-history widget) i) label)
               (#_setAutoFillBackground label T)
               (#_setFixedHeight label 20)
               (#_addWidget layout label 2 i)))  
    (#_setLayout widget layout)

    (setf (rgb-widget widget) rgb
          (hsv-widget widget) hsv)
    (color-widget-update widget (color *window*))))

(defmethod color-widget-update ((widget color-widget) new-color)
  (setf (color *window*) new-color)
  (loop for widget across (color-history widget)
        for color across (color-history *window*)
        do (#_setColor (#_palette widget) (#_QPalette::Background) color)
           (#_update widget))

  (color-widget-update (rgb-widget widget) new-color)
  (color-widget-update (hsv-widget widget) new-color))
