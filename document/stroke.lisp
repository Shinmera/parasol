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
   (%brush :initform (assume-form (current-brush *window*)) :accessor brush)
   (%last-index :initform 0 :accessor last-index)))

(defmethod draw ((stroke stroke) painter)
  "Redraws the full STROKE using the PAINTER object."
  (setf (last-index stroke) 0)
  (draw-incremental stroke painter))

(defgeneric draw-incremental (stroke painter)
  (:documentation "Only draws new STROKE parts that have not been drawn before using the PAINTER.")
  (:method ((stroke stroke) painter)
    (let ((point-amount (point-amount (curve stroke))))
      (when (< 0 point-amount)
        (#_setColor (#_brush painter) (base-color (brush stroke)))
        (#_setColor (#_pen painter) (base-color (brush stroke)))
        (map-points (curve stroke)
                    #'(lambda (x y xt yt p)
                        (declare (ignore xt yt))
                        (let ((len (* p (base-size (brush stroke)))))
                          (with-objects ((point (#_new QPointF x y)))
                            (#_drawEllipse painter point len len))))
                    :from (last-index stroke)
                    :to point-amount)
        (setf (last-index stroke) (1- point-amount))))))

(defmethod record-point ((stroke stroke) x y xt yt p)
  (record-point (curve stroke) (float x) (float y) xt yt p))

(defmethod finalize ((stroke stroke))
  (finalize (curve stroke))
  (finalize (brush stroke))
  (setf (curve stroke) NIL
        (brush stroke) NIL))
