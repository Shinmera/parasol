#|
This file is a part of Parasol
(c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.parasol)
(named-readtables:in-readtable :qbrushs)

(define-brush linearly-sampled-brush (abstract-brush)
  ((distance :initarg :distance :initform 1.0 :accessor distance))
  (:options (distance :type double-option :min 0.1 :max 100000.0 :step 0.5 :default 1.0 :slot 'distance)))

(defmethod draw-stroke ((brush linearly-sampled-brush) (stroke stroke) target)
  (loop for i from 1 below (length (points stroke))
        for start = (aref (points stroke) (1- i))
        for end = (aref (points stroke) i)
        do (loop with length = (sqrt (+ (expt (- (x end) (x start)) 2)
                                        (expt (- (y end) (y start)) 2)))
                 for i from 0 below length by (distance brush)
                 do (draw-penpoint brush (linear-interpolate start end (/ i length)) target))))

(define-brush single-colored-brush (abstract-brush)
  ((color :initarg :color :initform (#_new QColor 0 0 0) :accessor color))
  (:options (color :type color-option :slot 'color)))

(defmethod draw-stroke :before ((brush single-colored-brush) (stroke stroke) target)
  (#_setPen target (#_Qt::NoPen))
  (#_setBrush target (#_new QBrush (color brush))))

(define-brush sized-circle-brush (abstract-brush)
  ((size :initarg :size :initform 2.0 :accessor size))
  (:options (size :type double-option :min 0.1 :max 1000000.0 :step 0.5 :default 2.0 :slot 'size)))

(defmethod draw-penpoint ((brush sized-circle-brush) (pen pen) target)
  (#_drawEllipse target (#_new QPointF (x pen) (y pen)) (size brush) (size brush)))

(define-brush pressured-size-brush (sized-circle-brush abstract-brush)
  ())

(defmethod draw-penpoint ((brush sized-circle-brush) (pen pen) target)
  (#_drawEllipse target (#_new QPointF (x pen) (y pen))
                 (* (size brush) (pressure brush))
                 (* (size brush) (pressure brush))))

(define-brush basic-brush (linearly-sampled-brush single-colored-brush pressured-size-brush)
  ())
