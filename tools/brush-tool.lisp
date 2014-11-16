#|
 This file is a part of Parasol
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.parasol)
(named-readtables:in-readtable :qtools)

(defclass stroke (drawable)
  ((points :initform (make-array 0 :element-type 'pen :adjustable T :fill-pointer 0) :accessor points)
   ;; should be a brush property. But we don't have brushes yet, so stub it.
   (distance :initform 1.0 :accessor distance)))

(defmethod add-point ((pen pen) (stroke stroke))
  (vector-push-extend pen (points stroke))
  stroke)

(defmethod draw ((stroke stroke) target)
  (#_setPen target (#_new QColor 0 0 0 255))
  (loop for i from 1 below (length (points stroke))
        for start = (aref (points stroke) (1- i))
        for end = (aref (points stroke) i)
        do (loop with length = (sqrt (+ (expt (- (x end) (x start)) 2)
                                        (expt (- (y end) (y start)) 2)))
                 for i from 0 below length by (distance stroke)
                 for x = (+ (x start) (* (- (x end) (x start)) (/ i length)))
                 for y = (+ (y start) (* (- (y end) (y start)) (/ i length)))
                 do (#_drawEllipse target (#_new QPointF x y) 2.0 2.0))))

(define-tool (brush-tool "Brush" "Paint onto the canvas.") ()
  ((size :accessor size)
   (color :accessor color))
  (:options
    (size :type double-option :slot 'size :min 0.1 :max 100.0 :step 0.5)
    (color :type color-option :slot 'color :default (#_new QColor 0 0 0 255))))

(defun translate-pen (pen)
  (let ((pen (copy pen)))
    (incf (x pen) (x (current-view)))
    (incf (y pen) (y (current-view)))
    pen))

(defmethod begin ((tool brush-tool) pen)
  (v:info :brush-tool "Beginning stroke at ~s" pen)
  (let ((stroke (make-instance 'stroke))
        (layer (current-layer (current-document)))
        (pen (translate-pen pen)))
    (ensure-fitting (x pen) (y pen) layer)
    (insert (add-point pen stroke) layer)
    (rebuffer layer)
    (#_repaint (current-view))))

(defmethod move ((tool brush-tool) pen)
  (let ((layer (current-layer (current-document)))
        (pen (translate-pen pen)))
    (add-point pen (current-drawable layer))
    (ensure-fitting (x pen) (y pen) layer)
    (rebuffer layer)
    (#_repaint (current-view))))

(defmethod end ((tool brush-tool) pen)
  )
