#|
 This file is a part of Parasol
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:parasol)
(named-readtables:in-readtable :qt)

(defvar *current-brush* NIL)

(defclass brush ()
  ((%color :initarg :color :initform (#_new QColor 0 0 0) :accessor color)
   (%base-size :initarg :base-size :initform 5 :accessor base-size)))

(setf *current-brush* (make-instance 'brush))
