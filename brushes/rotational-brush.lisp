#|
 This file is a part of Parasol
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:parasol)
(named-readtables:in-readtable :qt)

(defclass rotational-brush (abstract-brush)
  ((%name :initform "Rotational Brush" :accessor name)
   (%rotation :initform 10 :initarg :rotation :accessor rotation)
   (%current-rotation :initform 0 :accessor current-rotation))
  (:metaclass brush-class)
  (:fields (rotation :type :float :range (0.01 360.0 1.0) :slot %rotation)))

(defmethod draw-curve :before ((brush rotational-brush) painter curve from to)
  (setf (current-rotation brush)
        (* from (rotation brush))))

(defmethod draw-point :before ((brush rotational-brush) painter x y xt yt p)
  (#_rotate painter (incf (current-rotation brush) (rotation brush))))

(defclass rotated-texture-brush (rotational-brush textured-brush brush)
  ()
  (:metaclass brush-class))
