#|
 This file is a part of Parasol
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:parasol)
(named-readtables:in-readtable :qt)

(defclass soft-brush (abstract-brush)
  ((%brush-size :initarg :brush-size :initform 10 :accessor brush-size)
   (%hardness :initarg :hardness :initform 0.5 :accessor hardness)
   (%gradient :initform NIL :accessor gradient))
  (:metaclass brush-class)
  (:fields (brush-size :type :float :range (0.01 200.0 1.0) :slot %brush-size)
           (hardness :type :float :range (0.0 1.0 0.1) :slot %hardness)))

(defmethod initialize-instance :after ((brush soft-brush) &key)
  (when (base-color brush)
    (let ((gradient (#_new QRadialGradient))
           (transparent (#_new QColor (base-color brush))))
      (#_setAlpha transparent 0)
      (#_setColorAt gradient 0.0 (base-color brush))
      (if (< (hardness brush) 1)
          (progn
            (#_setColorAt gradient (hardness brush) (base-color brush))
            (#_setColorAt gradient 1.0 transparent))
          (#_setColorAt gradient 1.0 (base-color brush)))
      (setf (gradient brush) gradient))))

(defmethod finalize ((brush soft-brush))
  (finalize-and-delete (gradient brush))
  (call-next-method))

(defmethod draw-point :around ((brush soft-brush) painter x y xt yt p)
  (declare (ignore xt yt))
  (let ((len (* p (brush-size brush))))
    (#_setCenterRadius (gradient brush) len)
    (with-objects ((qbrush (#_new QBrush (gradient brush))))
      (#_setBrush painter qbrush)
      (call-next-method))))

(defclass softened-brush (soft-brush brush)
  ()
  (:metaclass brush-class))
