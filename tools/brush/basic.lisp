#|
This file is a part of Parasol
(c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.parasol.tools.brush)
(named-readtables:in-readtable :qtools)

(define-brush linearly-sampled-brush (abstract-brush)
  ((distance :initarg :distance :initform 1.0 :accessor distance))
  (:options (distance :type double-option :min 0.1 :max 100000.0 :step 0.5 :default 1.0 :slot 'distance)))

(defgeneric draw-penpoint (brush pen target))

(defmethod draw-stroke ((brush linearly-sampled-brush) (stroke stroke) target &optional (from 0))
  (loop for i from from below (1- (length (points stroke)))
        for start = (aref (points stroke) i)
        for end = (aref (points stroke) (1+ i))
        do (loop with length = (sqrt (+ (expt (- (x end) (x start)) 2)
                                        (expt (- (y end) (y start)) 2)))
                 for i from 0 below length by (distance brush)
                 do (draw-penpoint brush (linear-interpolate start end (/ i length)) target))))

(define-brush single-colored-brush (abstract-brush)
  ((color :initarg :color :initform (#_new QColor 0 0 0) :accessor color))
  (:options (color :type color-option :slot 'color)))

(defmethod draw-stroke :before ((brush single-colored-brush) (stroke stroke) target &optional from)
  (declare (ignore from))
  (#_setPen target (#_Qt::NoPen))
  (#_setBrush target (#_new QBrush (color brush))))

;; This needs to be generalised so I can use it both for brushes and textures and all
;; maybe a superclass that translates and this scales?
(define-brush sized-brush (abstract-brush)
  ((size :initarg :size :initform 2.0 :accessor size))
  (:options (size :type double-option :min 0.1 :max 1000000.0 :step 0.5 :default 2.0 :slot 'size)))

(define-brush pressured-size-brush (sized-brush abstract-brush)
  ())

(defmethod draw-penpoint :around ((brush pressured-size-brush) (pen pen) target)
  (let ((orig-size (size brush)))
    (setf (size brush) (* orig-size (pressure pen)))
    (unwind-protect
         (call-next-method)
      (setf (size brush) orig-size))))

(define-brush circle-tip-brush (sized-brush abstract-brush)
  ())

(defmethod draw-penpoint ((brush circle-tip-brush) (pen pen) target)
  (#_drawEllipse target (#_new QPointF (x pen) (y pen)) (size brush) (size brush)))

(define-brush texture-brush (abstract-brush)
  ((texture :initarg :texture :initform NIL :accessor texture)))

(defmethod draw-penpoint ((brush texture-brush) (pen pen) target)
  (#_drawImage target (texture brush) (x pen) (y pen)))

(define-brush basic-brush (linearly-sampled-brush single-colored-brush circle-tip-brush pressured-size-brush)
  ())
