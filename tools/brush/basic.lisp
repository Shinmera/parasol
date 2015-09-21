#|
This file is a part of Parasol
(c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.parasol.tools.brush)
(named-readtables:in-readtable :qtools)

(define-brush linearly-sampled-brush (abstract-brush)
  ((distance :initarg :distance :initform 1.0 :accessor distance
             :type (float 0.1 1000000.0))))

(defgeneric draw-penpoint (brush pen target))

(defmethod draw-stroke ((brush linearly-sampled-brush) (stroke stroke) target &optional (from 0))
  (loop for i from from below (1- (length (points stroke)))
        for start = (aref (points stroke) i)
        for end = (aref (points stroke) (1+ i))
        do (loop with length = (sqrt (+ (expt (- (x end) (x start)) 2)
                                        (expt (- (y end) (y start)) 2)))
                 for i from 0 below length by (distance brush)
                 for pen = (linear-interpolate start end (/ i length))
                 do (with-transformation (target)
                      (q+:translate target (q+:make-qpointf (x pen) (y pen)))
                      (draw-penpoint brush pen target)))))

(define-brush single-colored-brush (abstract-brush)
  ;; Fix color dependency issue
  ((color :initarg :color :initform (make-instance 'color) :accessor color
          :type color)))

(defmethod draw-stroke :before ((brush single-colored-brush) (stroke stroke) target &optional from)
  (declare (ignore from))
  (setf (q+:pen target) (q+:qt.no-pen))
  (setf (q+:brush target) (q+:make-qbrush (to-qcolor (color brush)))))

;; This needs to be generalised so I can use it both for brushes and textures and all
;; maybe a superclass that translates and this scales?
(define-brush sized-brush (abstract-brush)
  ((size :initarg :size :initform 1.0 :accessor size
         :type (float 0.1 1000000.0))))

(defmethod draw-penpoint :before ((brush sized-brush) (pen pen) target)
  (q+:scale target (size brush) (size brush)))

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
  (q+:draw-ellipse target 0 0 1 1))

(define-brush texture-brush (abstract-brush)
  ((texture :initarg :texture :initform NIL :accessor texture)))

(defmethod initialize-instance :after ((brush texture-brush) &key)
  (let ((texture (texture brush)))
    (setf (texture brush)
          (etypecase texture
            (qobject texture)
            (pathname (make-target-from texture))
            (string (make-target-from (asdf:system-relative-pathname :parasol texture)))))))

(defmethod draw-penpoint ((brush texture-brush) (pen pen) target)
  (let ((texture (texture brush)))
    (with-transformation (target)
      (q+:translate target (q+:make-qpointf
                                 (- (/ (width texture) 2))
                                 (- (/ (height texture) 2))))
      (draw texture target))))

(define-brush basic-brush (linearly-sampled-brush single-colored-brush circle-tip-brush pressured-size-brush)
  ()
  (:default-initargs :size 5.0)
  (:configurable color size distance))

(define-brush pepper-brush (linearly-sampled-brush sized-brush texture-brush)
  ((texture :initform "assets/pepper.png" :accessor texture))
  (:configurable size))

(define-brush jalapeno-brush (linearly-sampled-brush sized-brush texture-brush)
  ((texture :initform "assets/jalapeno.png" :accessor texture))
  (:configurable size))
