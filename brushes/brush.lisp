#|
 This file is a part of Parasol
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:parasol)
(named-readtables:in-readtable :qt)

(defclass brush ()
  ((%base-size :initarg :base-size :initform 5 :accessor base-size)
   (%base-color :initarg :base-color :initform NIL :accessor base-color)))

(defmethod assume-form ((brush brush))
  (make-instance 'brush
                 :base-size (base-size brush)
                 :base-color (#_new QColor (color *window*))))

(defmethod finalize ((brush brush))
  (maybe-delete-qobject (base-color brush)))
