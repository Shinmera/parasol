#|
 This file is a part of Parasol
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:parasol)
(named-readtables:in-readtable :qt)

(defclass complex-brush (brush rotational-brush compositing-brush)
  ((%brush-size :initarg :brush-size :initform 10 :accessor brush-size))
  (:metaclass brush-class)
  (:fields (brush-size :type :float :range (0.01 200.0 1.0) :slot %brush-size)))

(defmethod draw-point ((brush complex-brush) painter x y xt yt p)
  (let ((len (* p (brush-size brush))))
    (with-objects ((point (#_new QPointF 0 0)))
      (#_drawEllipse painter point (* len (1+ xt)) (* len (1+ yt))))))
