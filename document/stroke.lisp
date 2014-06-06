#|
 This file is a part of Parasol
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:parasol)
(named-readtables:in-readtable :qt)

(defvar *curve-type* 'spline)

(defclass stroke ()
  ((%curve :initform (make-curve *curve-type*) :accessor curve)
   (%brush :initform *current-brush* :accessor brush)
   (%last-index :initform 0 :accessor last-index)))

(defgeneric draw (stroke painter)
  (:documentation "Redraws the full STROKE using the PAINTER object.")
  (:method ((stroke stroke) painter)
    (setf (last-index stroke) 0)
    (draw-incremental stroke painter)))

(defgeneric draw-incremental (stroke painter)
  (:documentation "Only draws new STROKE parts that have not been drawn before using the PAINTER.")
  (:method ((stroke stroke) painter)
    (let ((point-count (point-count (curve stroke))))
      (when (< 0 point-count)
        (#_setColor (#_brush painter) (color (brush stroke)))
        (#_setColor (#_pen painter) (color (brush stroke)))
        (map-points (curve stroke)
                    #'(lambda (x y xt yt p)
                        (declare (ignore xt yt))
                        (let ((len (* p (base-size (brush stroke)))))
                          (#_drawEllipse painter (#_new QRectF (- x len) (- y len) (* 2 len) (* 2 len)))))
                    :from (last-index stroke)
                    :to point-count)
        (setf (last-index stroke) (1- point-count))))))

(defmethod record-point ((stroke stroke) x y xt yt p)
  (record-point (curve stroke) (float x) (float y) xt yt p))
