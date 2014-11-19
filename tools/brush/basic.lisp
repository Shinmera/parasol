#|
This file is a part of Parasol
(c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.parasol.tools.brush)
(named-readtables:in-readtable :qtools)

(define-brush linearly-sampled-brush (abstract-brush)
  ((distance :initarg :distance :initform 1.0 :accessor distance))
  (:options (distance :type double-option :min 0.1 :max 100000.0 :step 0.5 :default 1.0 :slot 'distance
              :label "Distance")))

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
  (:options (color :type color-option :slot 'color
              :label "Color")))

(defmethod draw-stroke :before ((brush single-colored-brush) (stroke stroke) target &optional from)
  (declare (ignore from))
  (#_setPen target (#_Qt::NoPen))
  (#_setBrush target (#_new QBrush (color brush))))

;; This needs to be generalised so I can use it both for brushes and textures and all
;; maybe a superclass that translates and this scales?
(define-brush sized-brush (abstract-brush)
  ((size :initarg :size :initform 2.0 :accessor size))
  (:options (size :type double-option :min 0.1 :max 1000000.0 :step 0.5 :default 2.0 :slot 'size
              :label "Size")))

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

(defmethod initialize-instance :after ((brush texture-brush) &key)
  (let ((texture (texture brush)))
    (setf (texture brush)
          (etypecase texture
            (qobject texture)
            (pathname (#_new QImage (uiop:native-namestring texture)))
            (string (#_new QImage (uiop:native-namestring (asdf:system-relative-pathname :parasol texture))))))))

(defmethod draw-penpoint ((brush texture-brush) (pen pen) target)
  (let ((texture (texture brush)))
    (#_drawImage target (#_new QPointF
                               (- (x pen) (/ (#_width texture) 2))
                               (- (y pen) (/ (#_height texture) 2))) texture)))

(define-brush basic-brush (linearly-sampled-brush single-colored-brush circle-tip-brush pressured-size-brush)
  ())

(define-brush pepper-brush (linearly-sampled-brush texture-brush)
  ((texture :initform "assets/pepper.png" :accessor texture)))

(define-brush jalapeno-brush (linearly-sampled-brush texture-brush)
  ((texture :initform "assets/jalapeno.png" :accessor texture)))
